

# About this script -------------------------------------------------------

# Pulls data for dashboard that will display selected data from the Irish Health Survey 2024 that is available by region
# https://www.cso.ie/en/releasesandpublications/ep/p-ihsmr/irishhealthsurvey2024-mainresults/data/
# This script pulls the required data using the csodata package

# Author: Cian Dowling-Cullen
# Last updated: 31 July 2025



# Load packages -----------------------------------------------------------

pacman::p_load(
  shiny,
  tidyverse,
  purrr,
  stringr,
  dplyr,
  csodata
)



# Import data -------------------------------------------------------------

# Define required tables
ihs_codes <- c(
  "IH361","IH363","IH366","IH370","IH373","IH376","IH382","IH388","IH391","IH394","IH397",
  "IH401","IH405","IH408","IH411","IH413","IH415","IH422","IH425","IH428","IH431","IH434",
  "IH437","IH442","IH451","IH453","IH455","IH458","IH461","IH464","IH468","IH470","IH474",
  "IH477","IH480","IH483","IH486","IH489","IH492","IH495"
)


# Classify IH382 as mixed. Necassary as units given as "%" in metadata for IH382
# but it actually has a numberic variable as well in table. 
mixed_codes <- c("IH382")


# Also classify IH437 for faceting (has multiple mutually-exclusive categories in the table)
facet_stat_codes <- c("IH437") 


# Function to pull required tables using csodata package, and corresponding titles and units from metadata
pull_cso_data <- function(code) {
  meta <- cso_get_meta(code)
  dat <- cso_get_data(code) %>% 
    rename(HSE.Health.Regions = any_of("HSE.Health.Region")) %>%    # change singular to plural, as some tables have singular
    mutate(
      Indicator = code,
      Title     = meta$Title,        # pull title from the metadata
      Units     = meta$Units         # needed for deciding chart layout later
    )
  
  # add category prefix for faceting if table in facet_stat_codes
  if (code %in% facet_stat_codes) {
    cat_lab <- function(x) {
      x |>
        str_replace("\\s+[–-]\\s+.*$", "") |>  # remove suffix
        str_squish()
    }
    dat <- dat %>%
      mutate(
        Category = factor(cat_lab(Statistic),
                          levels = unique(cat_lab(Statistic)))
      )
  }
  dat                    
}

# Create tidy list including all of the required data tables
tidy_list <- map(set_names(ihs_codes), pull_cso_data)

meta <- list(
  tidy_list        = tidy_list,
  mixed_codes      = mixed_codes,
  facet_stat_codes = facet_stat_codes
)

saveRDS(meta, "ihs_data.rds")

