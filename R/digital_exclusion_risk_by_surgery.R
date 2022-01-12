
# libraries ---------------------------------------------------------------


{
library(here)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(tidyr)
library(jogger)
library(myrmidon)
library(sf)
library(tibble)
library(tmap)
}



# import NHS data (prepared previously) and CO DE data  -------------------


# full_data created by gather_data.R

source(here("R", "gather_data.R"))

full_data <- get_full_data(build = TRUE, geo_update = FALSE, onspd_update = FALSE, onspd_download = FALSE, pomi_update = TRUE)

# full_data <- get_full_data(build = FALSE)
# full_data <- readRDS(here::here("rds_data", "full_data-2021-02-16.Rds"))


full_data_latest <- full_data[[1]]

eng_lsoa_de_data <- readRDS(here::here("rds_data", "eng_lsoa_data_202102.Rds")) %>%
  select(lsoa11cd, lad20nm, rgn20nm, popn_adult, popn_over65, eimd_decile, lsoa_total_score, de_rank, centile, centile_score, decile, regional_decile)


# How many LSOAs contain a GP Practice?
length(unique(full_data_latest$lsoa11cd))
# 5521 (Feb 2021)
# 5507 (April 2021)

length(unique(full_data_latest$lsoa11cd))/nrow(eng_lsoa_de_data)
# 16.8% - about 1 in 6 LSOAs have a GP Practice

full_data_latest %>%
  count(lsoa11cd) %>%
  count(n)  #%>%
  # filter(!n == 1) %>%
  # pull(nn) %>%
  # sum()
# 850 GP Surgeries are not the only one in their LSOA

eng_gps_digexcl_by_lsoa <- full_data_latest %>%
  count(lsoa11cd, name = "number_practices") %>%
  left_join(eng_lsoa_de_data)

eng_gps_digexcl_by_lsoa %>%
  count(decile)
eng_gps_digexcl_by_lsoa %>%
  count(decile, wt = number_practices)

eng_gps_digexcl_by_lsoa %>%
  count(eimd_decile)
eng_gps_digexcl_by_lsoa %>%
  count(eimd_decile, wt = number_practices)


regions <- eng_gps_digexcl_by_lsoa$rgn20nm %>%
  unique() %>%
  sort()

practices_by_lsoa_de_decile_summary_by_region <- eng_gps_digexcl_by_lsoa %>%
  split(.$rgn20nm) %>%
  map2(seq_along(.), ~ count(.x, decile, wt = number_practices, name = regions[[.y]])) %>%
  reduce(left_join) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()

practices_by_lsoa_eimd_decile_summary_by_region <- eng_gps_digexcl_by_lsoa %>%
  split(.$rgn20nm) %>%
  map2(seq_along(.), ~ count(.x, eimd_decile, wt = number_practices, name = regions[[.y]])) %>%
  reduce(left_join) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)))) %>%
  ungroup()


gp_practices_full <- full_data_latest %>%
  select(practice_code, practice_name, postcode, lsoa11cd, older_popn_quintile, offline_pat_pct, easting, northing) %>%
  left_join(eng_gps_digexcl_by_lsoa) %>%
  relocate(lad20nm, rgn20nm, .after = lsoa11cd) %>%
  relocate(number_practices, .after = rgn20nm)

gp_practices_summary <- gp_practices_full %>%
  select(!popn_adult:last_col())

gp_practices_sf <- gp_practices_summary %>%
  sf::st_as_sf(
    coords = c("easting", "northing"),
    crs = 27700                           # 27700 = UK Grid CRS
  ) %>%
  rename(centroid = geometry)

saveRDS(gp_practices_sf, here::here("rds_data", "gp_practices_sf.Rds"))



# calculate single centroids for practices that share an LSOA -----------
lsoa_gp_centroids <- gp_practices_sf %>%
  filter(number_practices > 1) %>%
  # select(lsoa11cd) %>%
  split(.$lsoa11cd) %>%
  map(st_combine) %>%
  map(st_centroid) %>%
  as_tibble() %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "lsoa11cd",
    values_to = "centroid"
  ) %>%
  left_join(gp_practices_summary %>%
              filter(number_practices > 1) %>%
              select(!easting:northing), .) %>%
  st_as_sf(crs = 27700)


# bind to centroids for LSOAs that only have a single practice ------------
gp_practices_centroids <- lsoa_gp_centroids %>%
  bind_rows(
    gp_practices_sf %>%
      filter(number_practices == 1)
  )

saveRDS(gp_practices_centroids, here::here("rds_data", "gp_practices_centroids.Rds"))


gp_practices_centroids %>%
  pull(lsoa11cd) %>%
  unique() %>%
  length()



# Start doing things by region ------------------------------------------


gp_practices_centroids <- readRDS(here::here("rds_data", "gp_practices_centroids.Rds"))


gp_practices_centroids_regional <- gp_practices_centroids %>%
  split(.$rgn20nm)

# run this as a job (see R/get_lsoa_bounds_job.R)
# ought to have got these with Shape_Area!! Saves calculating later.
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
    sf::st_transform(crs = 27700)
}

saveRDS(regional_lsoa_boundaries, here::here("rds_data", "regional_lsoa_boundaries.Rds"))


regional_lsoa_boundaries <- readRDS(here::here("rds_data", "regional_lsoa_boundaries.Rds"))





regional_boundaries <- readRDS(here::here("rds_data", "regional_boundaries.Rds"))

# regional_boundaries <- regional_lsoa_boundaries %>%
#   map(~ sf::st_union(., by_feature = FALSE))
# set_names(regional_boundaries, regions)

# saveRDS(regional_boundaries, here::here("rds_data", "regional_boundaries.Rds"))



# sample map
rg <- sample(seq_along(regions), 1)

tm_shape(regional_boundaries[[rg]]) +
  tm_borders("orange") +
  tm_shape(regional_lsoa_boundaries[[rg]]) +
  tm_borders("grey80", lwd = 1) +
  tm_shape(gp_practices_centroids_regional[[rg]]) +
  tm_dots("magenta", size = 0.06)





# Practices get allocated the LSOA they are actually *in*, obviously

# sw_practices <- gp_practices_by_lsoa_centroids %>%
#   filter(rgn20nm == "South West")
#
# setequal(sw_practices$lsoa11cd, gp_practices_summary %>%
#   filter(rgn20nm == "South West") %>%
#   pull(lsoa11cd) %>%
#   unique())



# create a voronoi polygon net for each regional list of GP locations by LSOA
# (centroids). Use each regions boundaries as the envelope for each net (I am
# not totally sure how well this envelope thing works.)
voronoi_nets_regional <- gp_practices_centroids_regional %>%
  map(st_union) %>%
  map2(regional_boundaries, ~ st_voronoi(.x, envelope = .y)) %>%
  map(st_cast)

saveRDS(voronoi_nets_regional, here::here("rds_data", "voronoi_nets_regional.Rds"))


# south_west_lsoas_remainder <- south_west_lsoas %>%
#   filter(!lsoa11cd %in% sw_practices$lsoa11cd)
#
#
# sw_voronoi <- sw_practices %>%
#   st_union() %>%
#   st_voronoi(envelope = south_west_region) %>%
#   st_cast()




# sample voronoi map
rg <- sample(seq_along(regions), 1)

sample_voronoi_plot <- tm_shape(regional_boundaries[[rg]]) +
  tm_borders("orange") +
  tm_shape(voronoi_nets_regional[[rg]]) +
  tm_borders("olivedrab", lwd = 1) +
  tm_shape(gp_practices_centroids_regional[[rg]]) +
  tm_dots("magenta", size = 0.06)

sample_voronoi_plot
tmap_save(sample_voronoi_plot, here("sample_voronoi_plot.png"))



# find LSOAs that each Voronoi polygon contains
# contains <- sw_voronoi %>%
#   map( ~ st_contains(., south_west_lsoas) %>%
#          unlist())



voronoi_nets_regional <- readRDS(here::here("rds_data", "voronoi_nets_regional.Rds"))

# Find the "home" LSOAs that each Voronoi polygon centroid belongs to
# Yes - we already know the list of LSOAs that have a GP surgery in them!
# But when we do st_voronoi we lose track of the associations - the voronoi
# sf object is just a single column, it doesn't keep the label of its source
# centroid. In theory each voronoi polygon contains a single GP surgery
# centroid - its "source". Unless I'm making a real mental slip here.
voronoi_home_lsoas <- voronoi_nets_regional %>%
  map(st_geometry) %>%
  map2(gp_practices_centroids_regional,
       ~ st_contains(.x, .y, sparse = TRUE) %>%
             unlist() %>%
      `[`(.y$lsoa11cd, .) %>%
        unique()
  )

saveRDS(voronoi_home_lsoas, here::here("rds_data", "voronoi_home_lsoas.Rds"))


# setequal(sw_practices$lsoa11cd, voronoi_home_lsoas)

# attach LSOA code labels to the voronoi polygons for each region as tibbles
# and convert each tibble in the list to an sf object
voronoi_home_lsoas_sf <- voronoi_home_lsoas %>%
  map( ~ tibble(voronoi_lsoa11cd = .)) %>%
  map2(voronoi_nets_regional,
       ~ bind_cols(.x, tibble::enframe(.y, name = NULL, value = "geometry")) %>%
  st_as_sf(crs = 27700)
  )


saveRDS(voronoi_home_lsoas_sf, here::here("rds_data", "voronoi_home_lsoas_sf.Rds"))


# lists of 'home' LSOAs in every region  where surgeries are actually located
# 'real' means actual LSOA polygons, not the voronoi shapes
real_home_lsoas <- regional_lsoa_boundaries %>%
  map2(voronoi_home_lsoas,
       ~ filter(.x, lsoa11cd %in% .y)) %>%
  set_names(regions)

# all the other LSOAs in each region that don't host a surgery
real_other_lsoas <- regional_lsoa_boundaries %>%
  map2(voronoi_home_lsoas,
       ~ filter(.x, !lsoa11cd %in% .y)) %>%
  set_names(regions)


# sw_home_lsoas <- south_west_lsoas %>%
#   filter(lsoa11cd %in% voronoi_home_lsoas)
# sw_remainder_lsoas <- south_west_lsoas %>%
#   filter(!lsoa11cd %in% voronoi_home_lsoas)



# tm_shape(south_west_region) +
#   tm_borders("orange") +
#   # tm_shape(sw_voronoi_lsoas) +
#   # tm_shape(sw_home_lsoas) +
#   tm_shape(sw_remainder_lsoas) +
#   tm_fill("grey80", lwd = 1) +
#   tm_shape(sw_practices) +
#   tm_dots("magenta", size = 0.06)




# Obtain a list of lists of actual non-home LSOAs (if any) that each voronoi
# polygon intersects with. Then lengthen that data. Each intersection is
# returned as a reference number - the number on the list of remainder LSOAs.
# These ref numbers then need to be converted back to the actual LSOA code.

# a couple of helper functions to stop us writing map all the time
bind_voronoi_intersects <- function(lst, vec) {
  lst %>%
    # sgbp objects need to be unlisted before being enframed
    map(unlist) %>%
    tibble::enframe(name = NULL, value = "real_lsoa_ref") %>%
    bind_cols(voronoi_code = vec, .) # voronoi_home_lsoas
}

tidy_voronoi_intersects <- function(lst, df) {
  lst %>%
    tidyr::unnest_longer(real_lsoa_ref) %>%
    filter(!is.na(real_lsoa_ref)) %>%
    mutate(across(real_lsoa_ref, ~ `[`(df$lsoa11cd, .))) %>% # real_other_lsoas
    rename(real_lsoa11cd = real_lsoa_ref)
}





# now do the magic:
voronoi_intersects_tidy <- voronoi_home_lsoas_sf %>%
  # this step needed because we want to test st_intersects for each single
  # polygon, not for each regional net as a whole (which intersects
  # everything!). st_geometry (I think) strips out the individual geometries
  # as a list.
  map(st_geometry) %>%
  map2(real_other_lsoas, ~ st_intersects(.x, .y)) %>%
  map2(voronoi_home_lsoas, ~ bind_voronoi_intersects(.x, .y)) %>%
  map2(real_other_lsoas, tidy_voronoi_intersects)





# find which LSOAs are overlapped by each voronoi polygon
# and keep only those overlaps that are 25% or more of an LSOA's area
#
# Think - how can this be sped up?
calculate_overlap_pcts <- function(df1, df2, df3, min_overlap_pct) {
  split_list <- df1 %>%
    split(.$real_lsoa11cd)

  find_intersect_sizes <- function(df1, df2, df3) {

    a <- df2 %>%
      filter(lsoa11cd == unique(df1$real_lsoa11cd))

    b <- df3 %>%
      filter(voronoi_lsoa11cd %in% df1$voronoi_code)

    # area_a <- st_area(a)
    area_a <- a %>%
      pull(shape_area)

    # intersection_area <- st_geometry(b) %>%
    intersection_area <- st_intersection(a, b) %>%
      st_area() %>%
      `*`(100) %>%
      `/`(area_a) %>%
      as.numeric() %>%
      round(digits = 1)

    bind_cols(df1, coverage_pct = intersection_area)
  }

  map(split_list, ~ find_intersect_sizes(., df2, df3)) %>%
    reduce(bind_rows) # %>%
    # filter(coverage_pct >= min_overlap_pct)
}




# run as a job - takes ages! Produces *lots* of sf assumption warnings
# see: R/calc_intersects_job.R
voronoi_overlap_pcts <- list(
  voronoi_intersects_tidy,    # list of 9 tibbles
  regional_lsoa_boundaries,   # list of 9 sf data frames
  voronoi_home_lsoas_sf       # list of 9 sf tibbles
) %>%
  # map(head(3)) %>%    # for testing
  pmap(.,                     # map across the list of 3 lists
       ~ calculate_overlap_pcts(..1, ..2, ..3, min_overlap_pct = 25)
  )

saveRDS(voronoi_overlap_pcts, here::here("rds_data", "voronoi_overlap_pcts.Rds"))


voronoi_overlap_pcts <- readRDS(here::here("rds_data", "voronoi_overlap_pcts.Rds"))

voronoi_intersects_filtered <- voronoi_overlap_pcts %>%
  map(~ filter(., coverage_pct >= 25))


# need to check that all "other" LSOAs have been included somewhere!
tibble::enframe(pmap_test_out %>%
  map_int(~ length(unique(.$real_lsoa11cd))), value = "covered") %>%
  left_join(
    tibble::enframe(voronoi_intersects_filtered %>%
      map_int(~ length(unique(.$real_lsoa11cd))), value = "filtered")) %>%
  left_join(
    tibble::enframe(real_other_lsoas %>%
      map_int(~ length(unique(.$lsoa11cd))), value = "total"))

# There are 4 LSOAs not picked up for some reason -
# possibly to do with cutting off the processing at regional boundaries


# gp_practices_sf <- readRDS(here::here("rds_data", "gp_practices_sf.Rds"))
#
# gp_practices_summary <- gp_practices_sf %>%
#   st_drop_geometry()
#
# voronoi_intersects_filtered <- readRDS(here::here("rds_data", "voronoi_overlap_pcts.Rds")) %>%
#   map(~ filter(., coverage_pct >= 25))
#
# regional_lsoa_boundaries <- readRDS(here::here("rds_data", "regional_lsoa_boundaries.Rds"))
#
# regions <- gp_practices_summary$rgn20nm %>%
#   unique() %>%
#   sort()


create_catchments <- function(x, vr_df, lsoa_df) {

  # data for surgery home LSOA
  df1 <- gp_practices_summary %>%
    # select(!easting:northing) %>%
    filter(lsoa11cd == x)

  # surgery "home" LSOA code plus voronoi-intersecting LSOA codes
  intersecting_lsoa_codes <- vr_df %>%
    filter(voronoi_code == x) %>%
    pull(real_lsoa11cd) %>%
    c(x, .) %>%
    unique()

  # export a geometry for the catchment
  shp <- lsoa_df %>%
    filter(lsoa11cd %in% intersecting_lsoa_codes) %>%
    # st_combine()
    st_union()

  # compute summary stats for the catchment area
  df2 <- lsoa_df %>%
    filter(lsoa11cd == x) %>%
    st_drop_geometry() %>%
    summarise(
      home_lsoa_decile = mean(decile, na.rm = TRUE),
      home_lsoa_reg_decile = mean(regional_decile, na.rm = TRUE)
    )

  df3 <- lsoa_df %>%
    filter(lsoa11cd %in% intersecting_lsoa_codes) %>%
    st_drop_geometry() %>%
    summarise(
      catchment_lsoas = length(intersecting_lsoa_codes),
      total_adult_popn = sum(popn_adult),
      total_over65_popn = sum(popn_over65),
      eimd_top20 = length(eimd_decile[eimd_decile < 3]),
      min_dexcl_rank = min(de_rank),
      median_dexcl_rank = median(de_rank),
      nat_decile_top20 = round(
        length(decile[decile %in% 1:2])/catchment_lsoas, 1),
      rgn_decile_top10 = round(
        length(regional_decile[regional_decile == 1])/catchment_lsoas, 1)
    )

  # create a row for each practice with the catchment summary data and geom
  bind_cols(df1, df2, df3) %>%
    split(.$practice_code) %>%
    map(~ st_set_geometry(., shp)) %>%
    reduce(bind_rows)
}



map_create_catchments <- function(df1, df2, df3) {

  df1 %>%
    pull(lsoa11cd) %>%
    map_df(~ create_catchments(x = ., vr_df = df2, lsoa_df = df3))

}

# probably better run as a job
gp_lsoa_catchments <- list(
  real_home_lsoas,
  voronoi_intersects_filtered,
  regional_lsoa_boundaries
) %>%
  # map(., head(2)) %>% # for testing
  pmap(~ map_create_catchments(..1, ..2, ..3))



saveRDS(gp_lsoa_catchments, here::here("rds_data", "gp_lsoa_catchments.Rds"))

tmap_mode("plot")
gp_lsoa_catchments %>%
  reduce(bind_rows) %>%
  tm_shape() +
  # tm_fill("median_dexcl_rank", palette = "viridis")
  # tm_fill("older_popn_quintile", palette = "-viridis", style = "cat")
  # tm_fill("min_dexcl_rank", palette = "viridis", style = "quantile")
  tm_fill("mean_centile_score", palette = "-viridis", style = "jenks")


test <- sw_home_lsoas$lsoa11cd %>%
  map(create_catchments)

test_bound <- reduce(test, bind_rows)



tm_shape(test_bound) +
  tm_borders("grey80") +
  tm_fill("median_dexcl_rank", palette = "-Blues", alpha = 0.7) +
  # tm_shape(sw_voronoi) +
  # tm_borders("green", lwd = 1) +
  tm_shape(south_west_region) +
  tm_borders("orange") +
  tm_shape(gp_practices_sf %>% filter(rgn20nm == "South West")) +
  tm_dots("older_popn_quintile", palette = "viridis", size = 0.06)

