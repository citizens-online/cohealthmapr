library(dplyr)
library(purrr)
library(sf)
library(jogger)


get_rgn_lsoa_bounds <- function(region) {
  eng_lsoa_de_data %>%
    filter(rgn20nm == region) %>%
    pull(lsoa11cd) %>%
    jogger::geo_get(
      bounds_level = "lsoa11cd",
      within = .,
      within_cd = TRUE,
      return_style = "simple",
      shape_fields = TRUE,     # not included in original run
      quiet_read = TRUE) %>%
    left_join(eng_lsoa_de_data) %>%
    sf::st_transform(crs = 27700)
}


regional_lsoa_boundaries <- regions %>%
  map(get_rgn_lsoa_bounds)
