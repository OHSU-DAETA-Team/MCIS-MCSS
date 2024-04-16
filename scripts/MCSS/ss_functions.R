lookup_ss <- read_csv("MCIS-MCSS-Code/assets/MCSS/mcss_lookup.csv")
convert_table <- read_csv("MCIS-MCSS-Code/assets/MCSS/mcss_convert.csv")
col_rename <- read_csv("MCIS-MCSS-Code/assets/MCSS/MCSS_rename.csv")

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
  clean_f <- f |> mutate(quarter = str_extract(x, "Q[1-4]"),
                         year = str_extract(x, "(202[3-6])"))
  clean_f
}

mcss_clean <- function(year){

  file_path <- paste("(SS)", year, "Q[1234]", sep = "/")
  save_dir <- paste0("data/0-Cleaned Data/SS/")
  home_dir <- list.dirs(full.names = F)

  for (dir in home_dir){
    if(grepl(file_path, dir)){
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
        
        cleaned_name <- paste(dir, paste0(str_remove(f, ".xlsx"), ".csv"), sep = "/")
        cleaned_split <- str_split_1(cleaned_name, pattern = "/")
        
        if(nrow(intake) > 0){
          intake_n <- paste("intake", paste(cleaned_split[5], cleaned_split[3], paste(cleaned_split[2], cleaned_split[4], sep = "__"), sep = "__"), sep = "__")
          cleaned_intake <- paste(save_dir, paste0("intake/", year, "/"), intake_n, ".csv", sep = "")
          write_csv(intake, file = cleaned_intake)
        }
        
        if(nrow(discharge) > 0){
          discharge_n <- paste("discharge", paste(cleaned_split[5], cleaned_split[3], paste(cleaned_split[2], cleaned_split[4], sep = "__"), sep = "__"), sep = "__")
          cleaned_discharge <- paste(save_dir, paste0("discharge/", year, "/"), discharge_n, ".csv", sep = "")
          write_csv(discharge |> select(c(first_name, last_name, ss_team, date_of_data_entry_mm_dd_yyyy, dob, discharge_in_intake)), 
                    file = cleaned_discharge)
        }
      }
    }
  }
}
