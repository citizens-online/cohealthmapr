# From
# https://data.england.nhs.uk/dataset/gp-practice-submitted-inner-catchment-area-kml-file
# Latest data: March 2019 (but page says it will be updated monthly)

# found updated version (March 2020) via British Red Cross
# Covid Vulnerability Index data:
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/general-practice-data-collections



library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(here, quietly = TRUE)
library(tibble)
library(tidyr)
library(sf, quietly = TRUE)
library(tmap, quietly = TRUE)



# gp_catchments_kml <- "https://files.digital.nhs.uk/assets/eDEC/eDecJan-Mar2020.kml"

# nhs_gp_catchments <- sf::st_read(here("data", "GPSurgeryCatchmentAreas201903.kml")) %>%
# nhs_gp_catchments <- sf::st_read(gp_catchments_kml)
nhs_gp_catchments <- sf::st_read(here("data", "GPSurgeryCatchmentAreas202003.kml")) %>%
  # janitor::clean_names() %>%
  # dplyr::mutate(across(description, ~ dplyr::na_if(., "")))
  dplyr::select(!Description) %>%
  tidyr::separate(Name,
                  sep = "-",
                  into = c("practice_code", "practice_name"),
                  extra = "merge",
                  remove = TRUE) %>%
  dplyr::mutate(across(1:2, ~ stringr::str_trim(.))) %>%
  dplyr::mutate(across(practice_name, ~ dplyr::case_when(
    is.na(practice_name) ~ practice_code,
    practice_name == "" ~ NA_character_,
    TRUE ~ practice_name
  ))) %>%
  dplyr::mutate(across(practice_code, ~ dplyr::case_when(
    stringr::str_detect(., "^[A-Z]{1}[0-9]{5}$", negate = TRUE) ~ NA_character_,
    TRUE ~ practice_code
  )))

nhs_gp_catchments %>%
  dplyr::filter(is.na(practice_name))
  # dplyr::filter(practice_name == "")

nrow(nhs_gp_catchments)
length(unique(nhs_gp_catchments$practice_code))

# yikes
nhs_gp_catchments %>%
  dplyr::add_count(practice_code, sort = TRUE)



gp_practices_summary <- readRDS(here::here("rds_data", "full_data-2021-02-16.Rds")) %>%
  `[[`(1) %>%
  select(practice_code, practice_name, postcode, lsoa11cd, lad20nm, total_patients, older_popn_quintile, offline_pat_pct)

nhs_gp_catchments2 <- nhs_gp_catchments %>%
  filter(is.na(practice_code)) %>%
  select(!practice_code) %>%
  left_join(gp_practices_summary %>%
              select(practice_code, practice_name) %>%
              mutate(across(practice_name, stringr::str_to_upper)),
            by = "practice_name") %>%
  select(2, 1) %>%
  bind_rows(
    nhs_gp_catchments %>%
      filter(!is.na(practice_code))
  )
nhs_gp_catchments3 <- nhs_gp_catchments2 %>%
  filter(is.na(practice_name)) %>%
  select(!practice_name) %>%
  left_join(gp_practices_summary %>%
              select(practice_code, practice_name) %>%
              mutate(across(practice_name, stringr::str_to_upper)),
            by = "practice_code") %>%
  bind_rows(
    nhs_gp_catchments2 %>%
      filter(!is.na(practice_name))
  )


gp_catchments_unified2 <- nhs_gp_catchments3 %>%
  # apparently you can do this with dplyr group_by and summarise instead
  split(.$practice_code) %>%
  purrr::map(sf::st_union) %>%
  purrr::map(sf::st_make_valid) %>%
  tibble::enframe(name = "practice_code", value = "geometry") %>%

  # have to do this rowwise bcos throws an error colwise,
  # and I could find no way to avoid this, nor online advice
  #
  # ... %>%
  #   mutate(across(geometry, ~ st_sfc(.)))
  # Error: Problem with `mutate()` input `..1`.
  # x values must be length 3,
  # but FUN(X[[1]]) result is length 1
  #
  # Besides, rowwise is quite neat ;-)
  #
  # purrr::pmap() would be an alternative way to do it
  # pmap_df(~ bind_cols(practice_code = ..1, geometry = st_sfc(..2)))
  # but less neat

  rowwise() %>%
  mutate(geometry = st_sfc(c_across(geometry))) %>%
  ungroup() %>%
  left_join(gp_practices_summary, .) %>%
  st_sf()

saveRDS(gp_catchments_unified2, here::here("rds_data", "gp_catchments_unified2.Rds"))


tmap_mode("view")


gp_catchments_unified2 %>%
  tm_shape() +
  tm_fill("offline_pat_pct", palette = "viridis", alpha = 0.6)
