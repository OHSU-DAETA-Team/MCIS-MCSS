library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(openxlsx)
library(redcapAPI)
source("MCIS-MCSS-Code/scripts/MCSS/ss_functions.R")

###### Checkbox values #######
# - race_ethnicity
# - dhs_status
# - presenting_issue
# - resources
# - other_diagnoses
# - reason_for_closure
# - program_admission
# - barriers
##############################

# MCSS

checkbox_vars <- convert_table |> 
  filter(kind == "checkbox")

cb_vars <- unique(checkbox_vars$variable)

mcss_clean(2023)

intake_fin <-
  list.files(path = "data/0-Cleaned Data/SS/intake/",
             pattern = "*.csv",
             recursive = T,
             full.names = T) |>
  map_df(~read_mapped_csv(.)) |>
  filter(!is.na(ss_team)) |>
  mutate(dob = as_date(dob))

discharge_fin <-
  list.files(path = "data/0-Cleaned Data/SS/discharge/",
             pattern = "*.csv",
             recursive = T, 
             full.names = T) |>
  map_df(~read_mapped_csv(.)) |>
  filter(!is.na(ss_team)) |>
  mutate(dob = as_date(dob))

t <- intake_fin |> 
  left_join(discharge_fin,
            by = c("first_name", "last_name", "dob", "ss_team", "session")) |>
  rename("quarter" = "quarter.x")

fin <- rowid_to_column(t) |> 
  group_by(first_name, last_name, dob) |>
  mutate(grp_id = cur_group_id()) |>
  ungroup() |>
  mutate(across(contains("___"), as.character)) |>
  mutate(record_id = if_else(is.na(first_name), as.integer(paste0(grp_id, rowid)), grp_id)) |>
  mutate(across(all_of(cb_vars), recode_cb))

dummies <- fin |>
  pivot_longer(cols = contains("___"),
               names_to = c("base_name", "f_s_t"), 
               names_pattern = "(.*)___(.*)", 
               values_to = "column_number") |>
  mutate(val = if_else(!is.na(column_number), 1, 0),
         new_colname = if_else(is.na(column_number), 
                               paste0(base_name, "___NA"), 
                               paste0(base_name, paste0("___", column_number)))) |> 
  filter(!is.na(column_number)) |>
  select(-column_number) |> 
  pivot_wider(id_cols = record_id, names_from = new_colname, values_from = val, values_fn = mean)

# 

to_upload <- fin |>
  left_join(dummies, by = "record_id") |>
  mutate(across(contains("_total"), as.character),
         across(contains("_total"), .fns = ~replace_na(., ""))) |>
  mutate(across(contains("___"), as.character),
         across(contains("___"), .fns = ~replace_na(., ""))) |>
  mutate(across(contains("_subscore_"), as.character),
         across(contains("_subscore_"), .fns = ~replace_na(., ""))) |>
  mutate(redcap_repeat_instance = "new",
         dob = as_date(dob)) |>
  select(-ends_with("___first"),
         -ends_with("___second"),
         -ends_with("___third"))

View(to_upload)

## REDCAP

redcap_url <- "https://octri.ohsu.edu/redcap/api/"
ss_token <- read_csv("~/Desktop/MCIS_MCSS/RCtok.csv") |> filter(project == "ss")
rcon <- redcapConnection(url=redcap_url,
                         token=ss_token$token[1])

importRecords(rcon,
              to_upload,
              overwriteBehavior = c("normal"),
              batch.size = 100,
              returnContent = "auto_ids",
              #SET RETURNDATA T to CHECK FOR DATA ISSUES; THIS SHOULD BE DEFAULT
              returnData = T,
              force_auto_number = F)

ss_r <- tibble(exportRecordsTyped(rcon))

ss_r <- ss_r |>
  mutate(age = year(as_date("2023-12-31")) - year(as_date(dob)))

# deleteRecords(rcon, unique(ss_r$record_id))

x_path <- "~/../../private/tmp/nguphiliVolumes/OHSU/OHSU Shared/Restricted/SHARED/PSYCH/Child Psych Clinic/DAETA Team/MRSS and 988/Stabilization Services Quarterly Reports/Tableau/"
save_name <-"2023-Q2-Q4-SS-Tableau Data.xlsx"
#openxlsx::write.xlsx(ss_r, paste0(x_path, save_name))

openxlsx::write.xlsx(ss_r, paste0(save_name))

# ss_r |> skimr::skim()
