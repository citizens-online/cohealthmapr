library(purrr)
library(dplyr)

voronoi_overlap_pcts <- list(
  voronoi_intersects_tidy,    # list of 9 tibbles
  regional_lsoa_boundaries,   # list of 9 sf data frames
  voronoi_home_lsoas_sf       # list of 9 sf tibbles
) %>%
  # map(head(3)) %>%    # for testing
  pmap(.,                     # map across the list of 3 lists
       ~ calculate_overlap_pcts(..1, ..2, ..3, min_overlap_pct = 25)
  )
