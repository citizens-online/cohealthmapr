library(purrr)
library(dplyr)
library(tidyr)
library(sf)

voronoi_intersects_tidy <- st_geometry(sw_voronoi_lsoas) %>%
  map( ~ st_intersects(., st_geometry(sw_remainder_lsoas)) %>% unlist()) %>%
  tibble::enframe(name = NULL, value = "lsoa11cd") %>%
  bind_cols(st_drop_geometry(sw_voronoi_lsoas)) %>%
  tidyr::unnest_longer(lsoa11cd) %>%
  filter(!is.na(lsoa11cd)) %>%
  mutate(across(lsoa11cd, ~ `[`(sw_remainder_lsoas$lsoa11cd, .))) %>%
  arrange(lsoa11cd) %>%
  rename(intersects_with_voronoi_polygons = 2)
