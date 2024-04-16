library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(openxlsx)
library(redcapAPI)

lookup_ss <- read_csv("mcss_lookup.csv")
convert_table <- read_csv("mcss_convert.csv")
col_rename <- read_csv("MCSS_rename.csv")

recode_vals <- function(d){
  colnames(d) <- dplyr::recode(
    colnames(d), 
    !!!setNames(as.character(col_rename$new), col_rename$old)
  )
  d
}

recode_cb <- function(x) {
  
  recode_vec <- convert_table |>
    filter(kind == "checkbox") |>
    filter(variable == cur_column()) |>
    pull(label, name = value)
  
  dplyr::recode(x, !!!recode_vec)
}

drop_empty_rows <- function(df){
  df[rowSums(is.na(df)) != ncol(df),]
}

read_mapped_csv <- function(x){
  f <- read_csv(x, col_types = cols(.default = "c"))
  clean_f <- f |> mutate(quarter = str_extract(x, "Q[1-4]"))
  clean_f
}

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

home_dir <- list.dirs(full.names = F)

## create large df

d <- list()

# MCSS

checkbox_vars <- convert_table |> 
  filter(kind == "checkbox")

cb_vars <- unique(checkbox_vars$variable)

inta <- list()
disc <- list()

for (dir in home_dir){
  if(grepl("(SS)/Q[1234]", dir)){
    for (f in list.files(dir, pattern = ".xlsx")){
      ss_dir <- paste(dir, f, sep = "/")
      print(paste("Currently:", ss_dir))
      file_name <- ss_dir
      
      intake <- read_excel(file_name,
                           skip = 3, 
                           sheet = 1,
                           col_names = T,
                           .name_repair = "unique_quiet",
                           trim_ws = T) |> 
        # clean column names
        clean_names() |> 
        # drop empty rows
        drop_empty_rows() |> 
        # recode values
        recode_vals() |>
        mutate(dob = as_date(dob))
      
      discharge <- read_excel(file_name, 
                              skip = 3,
                              sheet = 2,
                              col_names = T,
                              .name_repair = "unique_quiet",
                              trim_ws = T) |> 
        clean_names() |>
        rename_with(.cols = contains("scale"), ~paste0(.x, "_clos")) |>
        drop_empty_rows() |> 
        recode_vals() |>
        mutate(dob = as_date(dob))
      
      discharge_in_intake <- discharge[!(names(discharge) %in% names(intake))] |> names()
      intake_in_discharge <- intake[!(names(intake) %in% names(discharge))] |> names()
      
      # if(nrow(intake) > 0 & nrow(discharge) == 0){
      #   # print(paste("nrow(intake) > 0"))
      #   cols_add <- names(discharge |> select(discharge_in_intake))
      #   t <- intake |> 
      #     mutate(!!!setNames(rep(NA, length(cols_add)), cols_add))
      #   
      # } else if (nrow(intake) == 0 & nrow(discharge) > 0) {
      #   # print(paste("nrow(discharge) > 0"))
      #   cols_add <- names(intake |> select(intake_in_discharge))
      #   t <- discharge |> 
      #     mutate(!!!setNames(rep(NA, length(cols_add)), cols_add))
      # } else if (nrow(intake) == 0 & nrow(discharge) == 0){
      #   break
      # } else {
      #   # print(paste("nrow(discharge) > 0 AND nrow(intake) > 0"))
      #   t <- intake |>
      #     full_join(discharge |> select(c(first_name, last_name, ss_team, dob, discharge_in_intake)), 
      #               by = c("first_name", "last_name", "dob", "ss_team"))
      # }
      
      cleaned_name <- paste(dir, paste0(str_remove(f, ".xlsx"), ".csv"), sep = "/")
      cleaned_split <- str_split_1(cleaned_name, pattern = "/")
      
      if(nrow(intake) > 0){
        intake_n <- paste("intake", paste(cleaned_split[3], paste(cleaned_split[1], cleaned_split[4], sep = "__"), sep = "__"), sep = "__")
        cleaned_intake <- paste("0-Cleaned Data-2023/SS/intake/", intake_n, sep = "")
        write_csv(intake, file = cleaned_intake)
        # inta[[cleaned_intake]] <- intake
      }
      
      if(nrow(discharge) > 0){
        discharge_n <- paste("discharge", paste(cleaned_split[3], paste(cleaned_split[1], cleaned_split[4], sep = "__"), sep = "__"), sep = "__")
        cleaned_discharge <- paste("0-Cleaned Data-2023/SS/discharge/", discharge_n, sep = "")
        write_csv(discharge |> select(c(first_name, last_name, ss_team, date_of_data_entry_mm_dd_yyyy, dob, discharge_in_intake)), file = cleaned_discharge)
        # disc[[cleaned_discharge]] <- discharge
      }
    }
  }
}

intake_fin <-
  list.files(path = "0-Cleaned Data-2023/SS/intake",
             pattern = "*.csv", 
             full.names = T) |>
  map_df(~read_mapped_csv(.)) |>
  filter(!is.na(ss_team)) |>
  mutate(dob = as_date(dob))

discharge_fin <-
  list.files(path = "0-Cleaned Data-2023/SS/discharge",
             pattern = "*.csv", 
             full.names = T) |>
  map_df(~read_mapped_csv(.)) |>
  filter(!is.na(ss_team)) |>
  mutate(dob = as_date(dob))

intake_fin |> View()
discharge_fin |> View()

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

z <- lapply(to_upload |> select(-contains("date"),
                                -contains("name"),
                                -"first_preferred",
                                -contains("_first"),
                                -contains("_last"),
                                -contains("email"),
                                -contains("phone"),
                                -"record_id",
                                -"dob", -"age",
                                -"grp_id",
                                -"ss_team",
                                -"rowid",
                                -"redcap_repeat_instance",
                                -"quarter",
                                -contains("zip")), unique)

# 
baddie_detector <- function(x, to_upload){
  to_upload |>
    filter(!(!!sym(x) %in% convert_table$value)) |>
    filter(!is.na(!!sym(x))) |>
    select(ss_team, !!sym(x)) |>
    rename(value = ss_team) |>
    inner_join(convert_table |> filter(variable == "ss_team"), by = "value") |>
    rename(team = label) |>
    mutate(variable = x,
           value = !!sym(x)) |>
    # select(team, var) |>
    unique()
}

baddies <- z |>
  names() |>
  map(~baddie_detector(., to_upload)) |>
  map_df(~. |> select(team, variable, value)) |>
  arrange(team) |>
  filter(value != "") |>
  filter(!grepl("___", variable)) |>
  filter(!grepl("_clos", variable)) |>
  filter(!grepl("_total", variable))

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
              #SET RETURNDATA T to CHECK FOR DATA ISSUES
              returnData = F,
              force_auto_number = F)

ss_r <- tibble(exportRecordsTyped(rcon))

ss_r <- ss_r |>
  mutate(age = year(as_date("2023-12-31")) - year(as_date(dob)))

# deleteRecords(rcon, unique(ss_r$record_id))

x_path <- "~/../../private/tmp/nguphiliVolumes/OHSU/OHSU Shared/Restricted/SHARED/PSYCH/Child Psych Clinic/DAETA Team/MRSS and 988/Stabilization Services Quarterly Reports/Tableau/"
save_name <-"2023-Q2-Q4-SS-Tableau Data.xlsx"
#openxlsx::write.xlsx(ss_r, paste0(x_path, save_name))

openxlsx::write.xlsx(ss_r, paste0(save_name))

ss_r |> skimr::skim()
