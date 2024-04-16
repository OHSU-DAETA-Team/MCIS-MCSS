library(shiny)
library(tidyverse)
library(reactable)
library(glue)
library(gt)
library(maps)
library(mapdata)
library(zipcodeR)

file_name <- "~/OneDrive - Oregon Health & Science University/MCIS-MCSS-data/2023/2023_Q2-Q4_Tableau Data.xlsx"

if(file.exists(file_name)){
  r <- readxl::read_xlsx(file_name)
} else {
  redcap_url <- "https://octri.ohsu.edu/redcap/api/"
  mcis_token <- read_csv("~/Desktop/MCIS_MCSS/RCtok.csv") |> filter(project == "mcis")
  rcon <- redcapAPI::redcapConnection(url=redcap_url, token=mcis_token$token[1])
  r <- tibble(redcapAPI::exportRecordsTyped(rcon, batch_size = 500))
}

factor_choices <- unique(read_csv("../../a-data-template.csv")[,1]) |> pull()

# MAP

OR_counties <- map_data("county") |>
  filter(region == "oregon") |>
  mutate(subregion = str_to_title(subregion))

zip_centroids <- geocode_zip(r$dispatch_zip) |>
  rename(dispatch_zip = zipcode,
         zlat = lat,
         zlng = lng)

zip_centroids <- r |> 
  mutate(dispatch_zip = as.character(dispatch_zip)) |>
  left_join(zip_centroids, by = "dispatch_zip") 

sf_data <- st_as_sf(zip_centroids |> filter(!is.na(zlat) & !is.na(zlng)), coords = c("zlng", "zlat"))

# Returns a county-named list of label points
getLabelPoint <- function(county) {Polygon(county[c('long', 'lat')])@labpt}

county_centroids = by(OR_counties, OR_counties$subregion, getLabelPoint)
county_centroids2 <- do.call("rbind.data.frame", county_centroids)
county_centroids2$county = rownames(county_centroids)
names(county_centroids2) <- c('clong', 'clat', "county")

dispatch_count_q3 <- zip_centroids |>
  filter(quarter == "Q3") |>
  group_by(mcis_county) |>
  count() |>
  mutate(county = mcis_county,
         county_n = paste0(mcis_county, "\n(", paste0(n, ")"))) |>
  ungroup() |>
  left_join(county_centroids2, by = "county")

OR_base_dispatch <- OR_counties |>
  mutate(county = subregion) |>
  left_join(dispatch_count_q3, by = "county")

OR_base <- 
  ggplot() +
  geom_polygon(data = OR_base_dispatch, aes(long, lat, fill = "white", group = group)) +
  coord_fixed(1.3) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "hide") +
  geom_text(
    data = OR_base_dispatch, 
    aes(
      x = clong,
      y = clat,
      label = county_n
    ),
    color = "black",
    size = 2.25
  ) +
  ggthemes::theme_map()

# OR_base +
#   geom_polygon(data = OR_counties, fill = NA, color = "white") +
#   geom_polygon(color = "black", fill = NA)
# 

# TABLE

r_wide <- r |> 
  group_by(mcis_team, quarter) |>
  summarize(`# of Dispatches` = n(),
            `Individuals Served` = n_distinct(record_id),
            .groups = "drop") |>
  pivot_wider(names_from = quarter, values_from = c(`# of Dispatches`, `Individuals Served`)) |>
  select(mcis_team, `# of Dispatches_Q2`, `Individuals Served_Q2`, `# of Dispatches_Q3`, `Individuals Served_Q3`) |>
  mutate(across(.cols = -1, .fns = ~replace_na(., replace = 0)),
         mcis_team = as.character(mcis_team),
         across(.cols = -1, .fns = as.numeric)) |>
  rowwise() |>
  mutate(`# of Dispatches_gt` = sum(c_across(contains("# of Dispatches"))),
         `Individuals Served_gt` = sum(c_across(contains("Individuals Served")))) |>
  ungroup()

actual_colnames <- colnames(r_wide)
actual_colnames

desired_colnames <- actual_colnames |> 
  str_remove('_(Q2|Q3|gt)') |> 
  str_to_title()

names(desired_colnames) <- actual_colnames
desired_colnames[1] <- "MCIS Team"

gt_table <- r_wide |>
  # gt(groupname_col = 'geo_designation', rowname_col = 'mcis_team') |>
  gt(rowname_col = 'Mcis_team') |>
  tab_spanner(label = md("**Q2**"),
              columns = 2:3) |>
  tab_spanner(label = md("**Q3**"),
              columns = 4:5) |>
  tab_spanner(label = md("**Grand Total**"),
              columns = 6:7) |>
  tab_header(
    title = 'Volume by Program and Quarter'
    # subtitle = 'Data is courtesy of the {palmerpenguins} R package'
  ) |>
  cols_label(.list = desired_colnames) |>
  sub_zero(zero_text = '-') |>
  grand_summary_rows(
    columns = 2:7,
    fns = list(
      Total = ~sum(.)
    ),
    bold_labels = TRUE
  )

ui <- fluidPage(
  titlePanel("MCIS Data Explorer"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput("factor_column", "Variable:", choices = factor_choices),
                 radioButtons("na_included", "Include missing values?", choices = c("Y", "N"), selected = "N")
    ),
    mainPanel(width = 10,
              fluidRow(
                column(width = 8,
                       plotOutput("bar_chart", width = "100%")
                       ),
                column(width = 4,
                       tableOutput("table")
                       )
                )
              )
    ),
  gt_output("gt_table")
  )

# Define server logic
server <- function(input, output) {
  
  counts <- reactive({
    df_counts <- r |>
      #here we're referring to input$factor_column directly
      group_by_at(input$factor_column) |>
      summarise(count = n()) |>
      arrange(desc(count)) |>
      # input$factor_column here is masking the variable we want to call
      # so we have to use .data to refer to it
      filter(!is.na(.data[[input$factor_column]]) | input$na_included == "Y")
  })
  
  output$table <- renderTable({
    counts()
  })
  
  # output$gt_table <- render_gt(expr = gt_table)
  
  output$bar_chart <- renderPlot({
    ggplot(counts(), aes(x = fct_reorder(.data[[input$factor_column]], count), y = count)) +
    geom_col() +
      labs(x = "",
           y = "n") +
      coord_flip() +
      theme_minimal()
    }, res = 96)
}

shinyApp(ui, server)
