
# Compile to full_data ----------------------------------------------------

get_full_data <- function(build = FALSE, ...) {
  library(here)

  latest_full_data <- dir(here("rds_data"), pattern = "^full_data.*Rds$") %>%
    sort() %>%
    `[[`(1)


  if (!build) {
    readRDS(here("rds_data", latest_full_data))
  } else {
    build_full_data(...)
  }
}


build_full_data <- function(geo_update = FALSE, onspd_update = FALSE, onspd_download = FALSE, pomi_update = TRUE) {
  library(here)
  source(here("R", "functions.R"))

  load_packages()

  geo_lookup <- get_geolookup(update = geo_update)
  onspd_data <- get_postcode_data(update = onspd_update, download = onspd_download)

  # surgery_data <- readRDS(here::here("rds_data/surgery_data-2021-02-15.Rds"))
  surgery_data <- get_surgery_data(last_n_months = 6)

  saveRDS(surgery_data, here::here("rds_data", paste0("surgery_data-", lubridate::today(), ".Rds")))


  # pomi_data <- readRDS(here::here("rds_data/pomi_data-2021-02-15.Rds"))
  pomi_data <- get_pomi_data(update = pomi_update)

  saveRDS(pomi_data, here::here("rds_data", paste0("pomi_data-", lubridate::today(), ".Rds")))


  full_data <- surgery_data %>%
    left_join(onspd_data) %>%
    relocate(lsoa11cd, .after = postcode) %>%
    left_join(geo_lookup) %>%
    relocate(starts_with("lad"), .after = lsoa11cd) %>%
    left_join(pomi_data, by = c("practice_code", "extract_date" = "date_join_field")) %>%
    relocate(practice_name, .after = practice_code) %>%
    relocate(c(ccg_code, ccg_name), .after = lad20nm) %>%
    mutate(offline_patients = patient_list_size - total_pat_enbld) %>%
    mutate(offline_pat_pct = round(offline_patients * 100 / patient_list_size, 1)) %>%

    # and sort by surgery size: this helps larger circles get plotted first
    # (and hence below smaller circles)
    arrange(extract_date, lad20cd, desc(patient_list_size)) %>%

    split(.$extract_date) %>%
    rev()

  saveRDS(full_data, here("rds_data", paste0("full_data-", lubridate::today(), ".Rds")))
  readr::write_csv(full_data[[1]], here("full_data_gpsurgeries_202101.csv"))

  full_data
}
