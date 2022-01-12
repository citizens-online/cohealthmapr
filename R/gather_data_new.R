library(tidyverse, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(janitor, warn.conflicts = FALSE)

# https://github/com/francisbarton/myrmidon
library(myrmidon)
library(rvest, warn.conflicts = FALSE)


# Compile to full_data ----------------------------------------------------

get_full_data <- function(refresh = FALSE, ...) {
  library(here)

  if (!refresh) {
    dir(here::here("rds_data"), pattern = "^full_data.*Rds$") %>%
      dplyr::last() %>%
      here::here("rds_data", .) %>%
      readRDS()
  } else {
    build_full_data(...)
  }
}


build_full_data <- function(months = 6, age_data_update = TRUE, pomi_data_update = TRUE) {
  library(here)
  source(here::here("R", "functions.R"))

  practice_data <- get_practice_data(months, update = age_data_update)
  pomi_data <- get_pomi_data(update = pomi_data_update)

  full_data <- practice_data %>%
    dplyr::filter(total_patients > 99) %>%
    dplyr::left_join(pomi_data, by = c("practice_code", extract_date = "date_join_field")) %>%

    # tidy up
    dplyr::select(!pomi_report_period_end) %>%
    dplyr::relocate(c(eastings, northings), .after = postcode) %>%
    dplyr::relocate(practice_name, .after = practice_code) %>%
    dplyr::relocate(starts_with("msoa"), .after = lsoa11cd) %>%
    dplyr::relocate(starts_with("ccg"), .after = pcon21cd) %>%
    dplyr::relocate(ends_with("_patients"), .after = region) %>%

    # Patients POMI enabled %
    dplyr::mutate(patients_not_pomi_enabled = pomi_patient_list_size - pomi_total_patients_enabled) %>%
    dplyr::mutate(pct_patients_pomi_enabled = round(pomi_total_patients_enabled * 100 / pomi_patient_list_size, 1)) %>%

    # and sort by practice size: this helps larger circles get plotted first
    # (and hence below smaller circles)
    dplyr::arrange(ltla21cd, desc(pomi_patient_list_size)) %>%
    split( ~ extract_date) %>%
    rev() %>%
    test_pomi_join() %>%
    # create quintile column
    # set quintiles as factors so that they're not treated as
    # a continuous variable in the map legend
    purrr::map(~ dplyr::mutate(., age_score_quintile = forcats::as_factor(dplyr::ntile(age_score, 5))))

  full_data %>%
    saveRDS(here::here("rds_data", paste0("full_data-", lubridate::today(), ".Rds")))

  full_data %>%
    purrr::reduce(bind_rows) %>%
    readr::write_csv(here::here("full_data_practices_latest.csv"))

  # return:
  full_data
}
