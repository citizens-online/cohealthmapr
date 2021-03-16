library(dplyr)
library(purrr)
library(sf)

voronoi_intersects <- voronoi_home_lsoas_sf %>%
  # this step needed because we want to test st_intersects for each single
  # polygon, not for each regional net as a whole (which intersects
  # everything!). st_geometry (I think) strips out the individual geometries
  # as a list.
  map(st_geometry) %>%
  map2(real_other_lsoas, ~ st_intersects(.x, .y) %>% unlist())


voronoi_home_geometry <- map(voronoi_home_lsoas_sf, st_geometry) %>%
  map(~ st_cast(., "GEOMETRY"))
other_lsoas_geometry <- map(real_other_lsoas, st_geometry)

# v1_test <- voronoi_home_geometry[[1]]
# o1_test <- other_lsoas_geometry[[1]]
#
# voronoi_intersec_test <- map(v1_test, ~ st_intersects(., o1_test) %>% unlist())

voronoi_intersections <- map2(voronoi_home_geometry, other_lsoas_geometry,
                              ~ map(.x, ~ st_intersects(., .y) %>% unlist()))

# voronoi_home_lsoas_sf[[1]] %>%
#   map(st_geometry) %>%
#   map(~ st_intersects(., st_geometry(real_other_lsoas[[1]])) %>% unlist())

