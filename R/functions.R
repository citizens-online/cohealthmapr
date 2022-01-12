# Get GP practice data -----------------------------------------------------

get_practice_data <- function(last_n_months = 6, update = TRUE) {

  if (update) {
    # read CSVs from NHS Digital website
    get_practice_data_urls(last_n_months) %>%
      purrr::map_dfr(
        # col_types need to be set because empty org_type cells confuse read_csv
        ~ readr::read_csv(., col_types = "cccccccci", lazy = FALSE)) %>%

      # tidy up
      janitor::clean_names() %>%
      dplyr::filter(org_type == "GP") %>% # only show GPs (not STP/CCG data)
      dplyr::select(!c(publication, org_type, ons_code)) %>% # remove some unneeded columns
      dplyr::rename(practice_code = org_code) %>%
      dplyr::mutate(across(extract_date, lubridate::dmy)) %>%

      # sex = ALL would give us the total patients but we'll do it the long way
      # round because it'll give us the age band data we'll need further below
      dplyr::filter(!sex == "ALL") %>%
      dplyr::filter(!age_group_5 == "ALL") %>%

      # ... add FEMALE and MALE totals together for each age group
      dplyr::group_by(across(c(1:3, 5))) %>%
      dplyr::summarise(across(number_of_patients, sum), .groups = "drop") %>%

      # ... create period totals for each 5-year age bracket for each practice
      tidyr::pivot_wider(
        names_from = age_group_5,
        names_prefix = "patients_",
        values_from = number_of_patients
      ) %>%

      janitor::clean_names() %>%
      myrmidon::postcode_data_join() %>%
      dplyr::select(!country) %>%
      myrmidon::add_msoa_names(welsh = FALSE) %>%
      dplyr::relocate(msoa11hclnm, .after = msoa11cd) %>%

      saveRDS(here::here("rds_data", paste0("practice_data-", lubridate::today(), ".Rds")))
  }

  dir(here::here("rds_data"), pattern = "^practice_data.*Rds$") %>%
    dplyr::last() %>%
    here::here("rds_data", .) %>%
    readRDS() %>%

    dplyr::rowwise() %>%
    dplyr::mutate(total_patients = sum(c_across(starts_with("patients_")))) %>%
    dplyr::select(!c(patients_0_4, patients_5_9, patients_10_14)) %>%
    dplyr::mutate(over14_patients = sum(c_across(starts_with("patients_")))) %>%
    dplyr::select(!patients_15_19:patients_60_64) %>%
    dplyr::mutate(over64_patients = sum(c_across(starts_with("patients_")))) %>%
    dplyr::ungroup() %>%

    # Weightings to emphasise practices with more people
    # in older age brackets
    # 0.6*1.1*1.25*1.4 ~= 1, not that it really matters
    # but it's a kind of constraint I suppose
    dplyr::mutate(age_score =
                    (patients_65_69 +
                       (patients_70_74 * 1.2) +
                       (patients_75_79 * 1.33) +
                       (patients_80_84 * 1.5) +
                       ((patients_85_89 + patients_90_94 + patients_95) * 2)) /
                    total_patients
    ) %>%
    dplyr::select(!patients_65_69:patients_95)
}

# Helper for get_practice_data() -------------------------------------------

get_practice_data_urls <- function(n) {
  paste0(
    "https://digital.nhs.uk/",
    "data-and-information/",
    "publications/",
    "statistical/",
    "patients-registered-at-a-gp-practice/",
    get_practice_data_months(n)
  ) %>%
    purrr::map_chr( ~ rvest::read_html(.) %>%
                      rvest::html_elements("#resources div:nth-of-type(1) div:nth-of-type(5) div a") %>%
                      rvest::html_attr("href")
    )
}

# Helper for get_practice_data_urls() --------------------------------------

get_practice_data_months <- function(n) {

  seq(n) %>%
    paste0(., "m") %>%
    purrr::map_chr(~ paste0(
      tolower(
        lubridate::month(
          lubridate::today() - lubridate::period(.),
          label = TRUE, abbr = FALSE)
      ),
      "-",
      lubridate::year(
        lubridate::today() - lubridate::period(.)
      )
    ))
}




# Get NHS Digital POMI data (patient online services) ---------------------


get_pomi_data <- function(update = TRUE) {


  if (update) {
    pomi_zip <- here::here("data", "pomi_data.zip")

    # https://digital.nhs.uk/data-and-information/publications/statistical/mi-patient-online-pomi/current
    pomi_url <- get_pomi_zip_url()

    download.file(pomi_url, pomi_zip)
    unzip(pomi_zip, exdir = here::here("data"))

    pomi_file <- unzip(pomi_zip, list = TRUE) %>% pull(Name)

    cols_to_keep <- c(
      "report_period_end",
      "ccg_code",
      "ccg_name",
      "practice_code",
      "practice_name",
      "field",
      "value")


    fields_to_keep <- c(
      "patient_list_size",
      "new_pat_enbld",
      "total_pat_enbld",
      "total_use"
    )


    here::here("data", pomi_file) %>%
      readr::read_csv(col_types = "c--cccc-cd") %>%
      dplyr::select(all_of(cols_to_keep)) %>%
      dplyr::mutate(across(report_period_end, lubridate::dmy)) %>%

      dplyr::mutate(date_join_field = report_period_end + lubridate::period("1d")) %>%
      dplyr::rename(pomi_report_period_end = report_period_end) %>%
      dplyr::mutate(across(field, tolower)) %>%
      dplyr::filter(field %in% fields_to_keep) %>%
      tidyr::pivot_wider(names_from = "field", values_from = "value") %>%
      dplyr::rename(pomi_patient_list_size = patient_list_size) %>%
      dplyr::rename(pomi_new_patients_enabled = new_pat_enbld) %>%
      dplyr::rename(pomi_total_patients_enabled = total_pat_enbld) %>%
      dplyr::rename(pomi_total_usage = total_use) %>%

      # a fix for some practices where total patients enabled > list size!
      dplyr::mutate(across(
        pomi_total_patients_enabled,
        ~ if_else(pomi_total_patients_enabled > pomi_patient_list_size,
                  pomi_patient_list_size,
                  pomi_total_patients_enabled
        ))) %>%
      saveRDS(here::here("rds_data", paste0("pomi_data-", lubridate::today(), ".Rds")))
  }

  dir(here::here("rds_data"), pattern = "^pomi_data.*Rds$") %>%
    dplyr::last() %>%
    here::here("rds_data", .) %>%
    readRDS()
}


get_pomi_zip_url <- function() {
  paste0(
    "https://digital.nhs.uk/",
    "data-and-information/",
    "publications/",
    "statistical/",
    "mi-patient-online-pomi/",
    "current"
  ) %>%
    rvest::read_html() %>%
    rvest::html_elements("#resources div:nth-of-type(1) div:nth-of-type(1) div a") %>%
    rvest::html_attr("href")
}




test_pomi_join <- function(df_lst) {

  if (length(df_lst) > 0) {
    if (all(is.na(df_lst[[1]]$practice_name))) { # indicates left_join failed
      df_lst[1] <- NULL
      test_pomi_join(df_lst)
    }
    else {
      df_lst
    }
  } else {
    usethis::ui_stop("It looks like none of the practice data and POMI data have matched up successfully. Maybe the dates need checking?")
  }
}












# Poss no longer needed (using own postcodes.io function) -----------------


# get David Kane's lookup csv from github ---------------------------------

get_geolookup <- function(update = FALSE) {
  if (update) {
    drkane_url <- "https://github.com/drkane/geo-lookups/raw/master/lsoa_la.csv"

    readr::read_csv(drkane_url) %>%
      janitor::clean_names() %>%
      dplyr::select(lsoa11cd, lad20cd, lad20nm)
  } else {
    readRDS(here::here("rds_data", "geo_lookup.Rds"))
  }
}




# get ONS Postcode Directory lookup ---------------------------------------

# Latest download: August 2020
# https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-august-2020

get_postcode_data <- function(update = FALSE, download = FALSE) {


  if (!update) {
    readRDS(here("rds_data/onspd_data.Rds"))
  } else {

    onspd_zip <- here("data/tmp/", "ons_postcode_data.zip")

    if (download) {
      file.remove(onspd_zip)

      # Download and import postcode/LSOA lookup from ONS OpenGeography
      # Latest is November 2020...
      # but no need to regularly update as contains data for old postcodes
      # https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-november-2020
      onspd_url <- "https://www.arcgis.com/sharing/rest/content/items/5ec8889d7e3b4d77a9f77ab8ec27d2c2/data"


      # Important to set `mode = "wb"` here because the url does not end in
      # `.zip` so R does not know it's a binary file (normally sets "wb"
      #  automatically
      #  for URLs that end in `.zip`)
      #  Beware, this is a 210MB download (zip) and ~1.3GB file when unzipped
      download.file(onspd_url, onspd_zip, mode = "wb")
    }

    onspd_file <- paste0(
      "ONSPD_",
      get_latest_quarter(),
      "_UK.csv"
    )

    unzip(onspd_zip, exdir = here("data/tmp"), files = paste0("Data/", onspd_file), junkpaths = TRUE)

    onspd_data <- readr::read_csv(here("data/tmp", onspd_file), col_types = "ccciiccccciiiiccccicccccccccccccccccccccccddcccicc") %>%
      # filter(is.na(doterm) | doterm > 201001) %>%
      select(
        "postcode" = pcds,
        "easting" = oseast1m,
        "northing" = osnrth1m,
        # "oa11cd" = oa11,
        "lsoa11cd" = lsoa11) %>%
      filter(stringr::str_starts(lsoa11cd, "E"))

    file.remove(here("data/tmp", onspd_file))

    saveRDS(onspd_data, here::here("rds_data", "onspd_data.Rds"))

    onspd_data
  }
}



# helper function for ONSPD ----------------------------------------------


get_latest_quarter <- function() {
  this_month <- lubridate::month(lubridate::today())
  get_year <- lubridate::year(lubridate::today())

  if (this_month %in% 3:5) {
    get_month <- 2
  } else if (this_month %in% 6:8) {
    get_month <- 5
  } else if (this_month %in% 9:11) {
    get_month <- 8
  } else if (this_month == 12) {
    get_month <- 11
  } else {
    get_month <- 11
    get_year <- get_year - 1
  }

  # a <- paste0(
  #   tolower(month(get_month, label = TRUE, abbr = FALSE)),
  #   "-",
  #   get_year
  # )

  paste0(
    toupper(lubridate::month(get_month, label = TRUE, abbr = TRUE)),
    "_",
    get_year
  )
}
