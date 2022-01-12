library(purrr)
library(dplyr)
library(sf)

top_200 <- voronoi_intersects_tidy %>%
  slice_head(n = 200)

top_200_out <- top_200 %>%
  pmap_dbl( ~ calculate_overlap_pct(..1, ..3)) %>%
  bind_cols(top_200, overlap_pct = .)
