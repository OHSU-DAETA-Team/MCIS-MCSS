### SELF-DEFINED FUNCTIONS FOR UPLOADING MCIS DATA ###

## LOOKUP Table - for converting labels into numerically coded values

lookup <- read_csv("MCIS-MCSS-Code/assets/MCIS/lookup-mcis-data-labels.csv")

### variable names
cols_vec <- unique(lookup$variable_name)
col_names <- read_csv("MCIS-MCSS-Code/assets/MCIS/mcis-col-names49.csv")

## The excel sheets have extra empty rows in them. Takes in a DF and drops the extra empty rows.
clean_xlsx <- function(df){
  df[rowSums(is.na(df)) != ncol(df), ]
}


## Recode raw values to coded values

recode_col <- function(x) {
  recode_vec <- lookup |>
    filter(variable_name == cur_column()) |>
    pull(value, name = label)
  
  dplyr::recode(x, !!!recode_vec)
}

# function for mapping files into one large file
read_file_mapped <- function(x){
  
  f <- read_excel(x, col_types = col_types)
  
  # create quarter and year columns from name of the file
  clean_f <- f |> mutate(quarter = str_extract(x, "Q[1-4]"),
                         year = str_extract(x, "(202[3-6])"))
  
  clean_f
}

# function for initial data clean #
## Goes through data in OneDrive, does first clean, save files into new directory ##

mcis_clean <- function(year){
  
  file_path <- paste("(MCIS)", year, "Q[1234]", sep = "/")
  save_dir <- paste0("data/0-Cleaned Data/MCIS/", year, "/")
  home_dir <- list.dirs(full.names = F)

  for (dir in home_dir){
    if(grepl(file_path, dir)){
      for (f in list.files(dir, pattern = ".xlsx")){
        dir_n <- paste(dir, f, sep = "/")
        print(paste("Currently:", dir_n))
        file_name <- dir_n
        
        # Klamath and Multnomah have been submitting their data in non-standard format, i.e. not using our data dictionary
        
        if(grepl("18-Klamath Basin Behavioral Health", dir) | grepl("28-Multnomah County Behavioral Health Division", dir)){
          df <- read_excel(file_name, sheet = 1) |> 
            clean_names()
        } else {
          df <- read_excel(file_name, skip = 3, sheet = 1) |> 
            clean_names()
        }
        
        if(ncol(df) > 49){
          df <- df |>
            select(-contains("minute"))
          
          # rename columns/variables
          names(df) <- col_names$variable_name
          # drop extra rows, clean data
          clean_df <- clean_xlsx(df) |>
            ### standardized column names
            rename(any_of(col_renames)) |> 
            ### turn every value into character type for easier wrangling
            mutate(across(-contains("datetime"), as.character)) |> 
            ### recode the values in cols_vec 
            mutate(across(any_of(cols_vec), recode_col)) |>
            ### recode NULL values as empty cells
            mutate(across(everything(), ~ str_replace(., "NULL", ""))) |>
            ### recode empty cells as NA type
            mutate_all(na_if, "") |>
            mutate(legal_first_name = str_extract(legal_first_name, ".*[A-Z].*[a-z].*"),
                   home_zip = str_extract(home_zip, "\\d{5}"))
        } else {
          df <- clean_xlsx(df) |>
            select(-"responder_number_3") |>
            add_column("enr_stabilization_services" = NA)
          
          names(df) <- col_names$variable_name
          
          clean_df <- df |>
            rename(any_of(col_renames)) |>
            mutate(across(-contains("datetime"), as.character)) |>
            mutate(across(any_of(cols_vec), recode_col)) |>
            mutate(legal_first_name = str_extract(legal_first_name, ".*[A-Z].*[a-z].*"),
                   home_zip = str_extract(home_zip, "\\d{5}"))
        }
        
        # SAVE DATA for further processing #
        
        final_df <- clean_df |> 
          select(-"event_name", -"date_of_data_entry_mm_dd_yyyy") |>
          mutate(redcap_repeat_instance = "new",
                 redcap_repeat_instrument = "mobile_crisis_intervention") |>
          mutate(across(-contains("datetime"), ~na_if(., '\"\"'))) |>
          mutate(across(contains("datetime"), as_datetime)) |>
          mutate(dob = as_date(dob))
        
        cleaned_name <- paste(dir, paste0(str_remove(f, ".xlsx"), ".xlsx"), sep = "/")
        cleaned_split <- str_split_1(cleaned_name, pattern = "/")
        fin_name <- paste(cleaned_split[4], paste(cleaned_split[2], cleaned_split[5], sep = "__"), sep = "__")
        cleaned_fname <- paste(save_dir, paste0(fin_name, ".xlsx"), sep = "")
        write.xlsx(final_df, file = cleaned_fname, na = "")
      }
    }
  }
}

# baddie_detector <- function(x, fin_tbl){
#   fin_tbl |>
#     filter(!(!!sym(x) %in% lookup$value) & !(!!sym(x) %in% lookup$label)) |> 
#     filter(!is.na(!!sym(x))) |> 
#     select(mcis_team, !!sym(x)) |> 
#     rename(value = mcis_team) |>
#     inner_join(lookup |> filter(variable_name == "mcis_team"), by = "value") |>
#     rename(team = label) |>
#     mutate(variable = x,
#            value = !!sym(x)) |>
#     unique()
# }

# Function to convert label values to raw values based on the lookup table
convert_label_to_raw <- function(df, lookup_table) {
  df_new <- df
  for (col in colnames(df)) {
    if (col %in% lookup_table$variable_name) {
      look <- lookup_table[lookup_table$variable_name == col, ]
      df_new[[col]] <- ifelse(df[[col]] %in% look$label, 
                              look$value[match(df[[col]], look$label)], 
                              df[[col]])
    }
  }
  return(df_new)
}

