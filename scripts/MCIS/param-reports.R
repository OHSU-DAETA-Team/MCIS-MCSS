library(tidyverse)
library(glue)
library(quarto)
library(purrr)

programs <- readxl::read_excel("2023_Q2-Q4_Tableau Data.xlsx") |> 
  select(mcis_team) |> 
  unique() |> 
  pull()

walk(1:length(programs), function(i) {
  program <- programs[i]
  
  outfile <- glue("{program}_rawCounts.pdf") # gives the filename a date and the municipality name
  
  quarto_render(input = "2023mcis-analysis.qmd", 
                execute_params = list("program" = program), 
                output_file = outfile,
                output_format = "pdf")
})
  
