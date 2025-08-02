
# About this script -------------------------------------------------------

# Dashboard for displaying selected data from the Irish Health Survey 2024 that is available by region
# https://www.cso.ie/en/releasesandpublications/ep/p-ihsmr/irishhealthsurvey2024-mainresults/data/

# This app:
## (1) Imports the required data (already pulled in pull_cso_data.R script and saved)
## (2) Selects an appropriate bar chart for each indicator based on the data 
## (3) Creates a bar chart and table for each indicator
## (4) Displays the chart and table, with user options to select indicator, region, and chart layout

# Author: Cian Dowling-Cullen
# Last updated: 31 July 2025



# Load packages -----------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(munsell)   



# Import data -------------------------------------------------------------

# Data was pulled from CSO website using csodata R package and saved
meta               <- readRDS("ihs_data.rds")
tidy_list          <- meta$tidy_list
mixed_codes        <- meta$mixed_codes
facet_stat_codes   <- meta$facet_stat_codes
ihs_codes          <- names(tidy_list)



# Create plots ------------------------------------------------------------

# Function to choose fill variable 
pick_fill_var <- function(dat) {
  
  ## if multiple levels in Statistic, return Statistic as fill variable
  if(nlevels(dat$Statistic) > 1) {
    return(list(data = dat, var = "Statistic"))
  }
  
  ## find other multi-level factors that might be additional variables
  candidate_vars <- dat %>% 
    select(where(~ is.factor(.x) && nlevels(.x) > 1)) %>%    # keep multi-level factors
    select(-any_of(c("Statistic", "Indicator", "Title", "HSE.Health.Regions"))) %>%   # ignore those that we know aren't relevant
    names()
  
  ## if any candidate variables meeting above criteria - return that. Else, return Statistic
  list(data = dat,
       var  = ifelse(length(candidate_vars), candidate_vars[1], "Statistic"))   
}


# Function to decide bar chart layout based on rules:
## 1. If units = Number: side-by-side
## 2. If mixed % and number (ad hoc defined at start): side-by-side
## 3. If units = % and total for any region > 100 (+ tol): side-by-side
## 4. Otherwise: stacked
decide_bar_layout <- function(dat, 
                              layout = "auto", 
                              tol = 0.5,
                              is_mixed = FALSE) {
  unit <- dat$Units[1]                       # "%" or "Number"
  
  ## if unit is %, check if total per region is >100 + tolerance 
  over_100 <- FALSE
  if (unit == "%") {
    over_100 <- dat %>% 
      group_by(HSE.Health.Regions) %>% 
      summarise(total = sum(`2024`, na.rm = TRUE), .groups = "drop") %>% 
      summarise(any(total > 100 + tol)) %>% 
      pull()
  }
  
  ## define automatic position based on above rules
  auto_pos <- case_when(
    unit == "Number"         ~ "dodge",
    is_mixed                 ~ "dodge",
    unit == "%" & over_100   ~ "dodge",
    TRUE                     ~ "stack"
  )
  
  ## return position and subtitles
  list(
    position = ifelse(layout == "auto", auto_pos, layout), # position can be overriden by user
    subtitle_lab = case_when(
      unit == "Number"         ~ "Number",
      is_mixed                 ~ "% & number",
      TRUE                     ~ "%"
    )
  )  
}


# Function to create bar chart for selected indicator
plot_indicator_2024 <- function(tbl_list,
                                code,
                                layout = "auto",   # bar chart layout (user override or 'auto')
                                tol = 0.5) {       # tolerance for deciding stacked vs side-by-side (how much >100% is allowable for stacked)
  dat <- tbl_list[[code]]
  
  ## get values using functions above
  pf <- pick_fill_var(dat)
  dat <- pf$data
  fill_var <- pf$var
  lay <- decide_bar_layout(dat, layout, tol, code %in% mixed_codes)
  
  ## build the base plot
  p <- ggplot(dat, aes(x = HSE.Health.Regions,
                       y = `2024`,
                       fill = .data[[fill_var]])) +
    geom_col(position = lay$position) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
    labs(title    = dat$Title[1],
         subtitle = lay$subtitle_lab,
         x = NULL, y = NULL)
  
  ## change colour scheme for IH437 (required as it has many categories)
  if (code == "IH437") {
    n_cols <- nlevels(dat[[fill_var]])
    if (n_cols <= 12) {
      p <- p + scale_fill_brewer(palette = "Set3", drop = FALSE)
    } else {
      p <- p + scale_fill_manual(
        values = scales::hue_pal(h = c(15, 375), c = 100, l = 65)(n_cols),
        drop   = FALSE
      )
    }
  }
  
  ## faceting for special cases
  if (code %in% mixed_codes) {
    p <- p + facet_wrap(~ Statistic, nrow = 1)
  } else if (code %in% facet_stat_codes) {
    p <- p + facet_wrap(~ Category, nrow = 1)
  }
  
  p
}


# Clean indicator names for dropdown menu
indicator_choices <- set_names(
  ihs_codes,                                                    
  str_glue("{ihs_codes} – {map_chr(ihs_codes, ~ tidy_list[[.x]]$Title[1])}")
)

# Clean region names for dropdown menu
region_levels  <- tidy_list[[1]] %>%                           
  arrange(match(HSE.Health.Regions,         
                c("All HSE Regions",
                  "HSE Dublin and Midlands",
                  "HSE Dublin and North East",
                  "HSE Mid West",
                  "HSE North West",
                  "HSE South",
                  "HSE South East"))) %>% 
  distinct(HSE.Health.Regions) %>% 
  pull()
region_choices <- set_names(region_levels)                  



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # app title
  titlePanel("Selected Irish Health Survey 2024 Results by HSE Region"),
  
  tabsetPanel(
    
    ## tab 1: Dashboard
    tabPanel("Dashboard",
             
             # sidebar with dropdowns to select indicator and region
             sidebarLayout(
               sidebarPanel(
                 
                 ## searchable indicator selector
                 selectizeInput(
                   inputId  = "indicator",
                   label    = "Indicator:",
                   choices  = indicator_choices,
                   multiple = TRUE,
                   selected = NULL,
                   options  = list(
                     maxItems       = 1,
                     placeholder    = "Type to search…",
                     openOnFocus    = TRUE,
                     dropdownParent = 'body'
                   )
                 ),
                 
                 ## searchable region selector
                 selectizeInput(
                   inputId  = "regions",
                   label    = "Region(s):",
                   choices  = region_choices,          
                   multiple = TRUE,                    # allow multiple regions to be selected
                   selected = character(0),            # start with nothing chosen
                   options  = list(placeholder = "Select region(s)")
                 ),
                 
                 radioButtons("layout", "Bar layout:",
                              choices = c("Auto" = "auto", "Stack" = "stack", "Side by side" = "dodge"),
                              inline  = TRUE
                 )
               ),
               
               ## show plot for selected indicator
               mainPanel(
                 plotOutput("ih_plot", height = 450),
                 br(),
                 tableOutput("raw_table")
               )
             )
    ),
    
    ## tab 2: About
    tabPanel("About",
             tags$div(
               style = "max-width: 900px; margin-top: 20px;",
               tags$p(
                 "This dashboard displays Central Statistics Office data from the ",
                 tags$a(
                   href = "https://www.cso.ie/en/releasesandpublications/ep/p-ihsmr/irishhealthsurvey2024-mainresults/data/",
                   "Irish Health Survey 2024", target = "_blank"
                 ),
                 ". It displays selected results from the survey that are broken down by HSE Region and uses publicly available data retrieved with the ",
                 tags$a(href = "https://cran.r-project.org/package=csodata",
                        "csodata R package", target = "_blank"),
                 "."
               ),
               tags$p(
                 "Full source code and additional notes are available on ",
                 tags$a(href = "https://github.com/<your‑GitHub‑repo>",
                        "GitHub", target = "_blank"), "."
               )
             )
    )   
    
  )      
)




# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # reactive list: data for the chosen regions 
  filtered_list <- reactive({
    req(length(input$regions) > 0)            # wait until user selects at least one region
    
    map(tidy_list,                            # keep only the rows selected
        ~ .x |> filter(HSE.Health.Regions %in% input$regions))
  })
  
  # bar chart
  output$ih_plot <- renderPlot({
    validate(need(length(input$indicator) == 1,
                  "Please select an indicator"),
             need(length(input$regions)    > 0,
                  "Please select one or more regions"))
    
    plot_indicator_2024(
      filtered_list(),
      input$indicator[1],    
      layout = input$layout
    )
  })
  
  # table
  output$raw_table <- renderTable({
    validate(
      need(length(input$indicator) == 1, "Please select an indicator"),
      need(length(input$regions)   > 0,  "Please select one or more regions")
    )
    
    filtered_list()[[ input$indicator[1] ]] |>
      rename(Value = `2024`) |>
      select(Statistic, HSE.Health.Regions, everything()) |>
      select(-any_of(c("Title", "Indicator", "Units"))) |>
      arrange(Statistic, HSE.Health.Regions) |>
      ## clean table headings
      rename_with(~ gsub("\\.", " ", .x))        
  }, rownames = FALSE)
  
}

# Run ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)


