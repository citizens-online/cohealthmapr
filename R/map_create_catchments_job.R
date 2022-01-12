library(purrr)
library(dplyr)
library(sf)


gp_lsoa_catchments <- list(
  real_home_lsoas,
  voronoi_intersects_filtered,
  regional_lsoa_boundaries
) %>%
  pmap(~ map_create_catchments(..1, ..2, ..3))
