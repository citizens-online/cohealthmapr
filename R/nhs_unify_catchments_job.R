library(purrr)

gp_catchments_unified <- nhs_gp_catchments %>%
  split(.$practice_code) %>%
  purrr::map(sf::st_union) %>%
  purrr::map(sf::st_make_valid)

