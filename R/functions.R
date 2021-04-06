# functions ---------------------------------------------------------------


load_packages <- function() {
  library(dplyr, warn.conflicts = FALSE)
  library(forcats)
  library(here)
  library(janitor, warn.conflicts = FALSE)
  library(lubridate, warn.conflicts = FALSE)
  library(myrmidon)
  library(purrr, warn.conflicts = FALSE)
  library(readr)
  library(rvest, warn.conflicts = FALSE)
  library(sf)
  library(stringr)
  library(tidyr)
}



# get David Kane's lookup csv from github ---------------------------------


get_geolookup <- function(update = FALSE) {
  if (update) {
    drkane_url <- "https://github.com/drkane/geo-lookups/raw/master/lsoa_la.csv"

    readr::read_csv(drkane_url) %>%
      clean_names() %>%
      select(lsoa11cd, lad20cd, lad20nm)
  } else {
    readRDS(here("rds_data", "geo_lookup.Rds"))
  }
}



# helper for get_surgery_data_urls() --------------------------------------

get_surgery_data_months <- function(n) {

  1:6 %>%
    paste0(., "m") %>%
    purrr::map_chr(~ paste0(
      tolower(
        month(
          today() - period(.),
          label = TRUE, abbr = FALSE)
        ),
      "-",
      year(
        today() - period(.)
      )
  ))
}



# helper for get_surgery_data() -------------------------------------------

get_surgery_data_urls <- function(n) {
  paste0(
    "https://digital.nhs.uk/",
    "data-and-information/",
    "publications/",
    "statistical/",
    "patients-registered-at-a-gp-practice/",
    get_surgery_data_months(n)
  ) %>%
    purrr::map_chr( ~ xml2::read_html(.) %>%
      html_nodes("#resources ul.list li:nth-of-type(5) a") %>%
      html_attr("href")
    )
}


# get GP surgery data -----------------------------------------------------

# updated monthly and not huge d/load so may as well update by default
get_surgery_data <- function(last_n_months = 6) {


  # get NHS surgery data - 5y age brackets ----------------------------------

  # read CSVs from NHS Digital website
  get_surgery_data_urls(last_n_months) %>%
    purrr::map_dfr(
      # col_types need to be set because empty org_type cells confuse read_csv
      ~ readr::read_csv(., col_types = "cccccccci")) %>%

    # tidy up
    clean_names() %>%
    filter(org_type == "GP") %>% # only show GPs (not STP/CCG data)
    select(-c(1, 3, 5)) %>% # remove some unneeded columns
    rename(
      "practice_code" = org_code) %>%
    mutate(across(extract_date, dmy)) %>%

    # sex = ALL would give us the total patients but we'll do it the long way
    # round because it'll give us the age band data we'll need further below
    filter(!sex == "ALL") %>%
    filter(!age_group_5 == "ALL") %>%

    # ... add FEMALE and MALE totals together for each age group
    group_by(across(c(1:3, 5))) %>%
    summarise(across(number_of_patients, sum)) %>%
    ungroup() %>%

    # ... create period totals for each 5-year age bracket for each practice
    pivot_wider(
      names_from = age_group_5,
      names_prefix = "patients_",
      values_from = number_of_patients
    ) %>%
    clean_names() %>%
    rowwise() %>%
    mutate(total_patients = sum(c_across(starts_with("patients_")))) %>%
    select(!patients_0_4:patients_10_14) %>%
    mutate(over14_patients = sum(c_across(starts_with("patients_")))) %>%
    select(!patients_15_19:patients_60_64) %>%
    mutate(over64_patients = sum(c_across(starts_with("patients_")))) %>%
    ungroup() %>%

    ### OLDER AGE WEIGHTINGS
    # Fairly arbitrary weightings to emphasise surgeries with more people
    # in older age brackets
    # 0.6*1.1*1.25*1.4 ~= 1, not that it really matters
    # but it's a kind of constraint I suppose
    mutate(older_popn_wtd = round(
      (patients_65_69 * 0.6) +
        patients_70_74 +
        (patients_75_79 * 1.1) +
        (patients_80_84 * 1.25) +
        ((patients_85_89 + patients_90_94 + patients_95) * 1.4)
    )) %>%
    select(!patients_65_69:patients_95) %>%
    split(.$extract_date) %>%


    # create quintile column
    # set quintiles as factors so that they're not treated as
    # a continuous variable in the map legend
    map(~ mutate(., older_popn_quintile = as_factor(ntile(older_popn_wtd, 5)))) %>%
    reduce(bind_rows)

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



# get NHS Digital POMI data (patient online services) ---------------------


get_pomi_data <- function(update = FALSE) {

  # get patient online data (POMI) ------------------------------------------

  pomi_zip <- here("data/tmp/pomi_2021.zip")

  if (update) {
    # https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/pomi
    # this will hopefully be the right URL until ~April 2021
    pomi_url <- "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-collections/pomi/pomi_2021.zip"

    download.file(pomi_url, pomi_zip)
    unzip(pomi_zip, exdir = here("data"))
  }

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
    "new_pat_enabled",
    "total_pat_enbld",
    "total_use"
  )

  read_csv(here("data", pomi_file)) %>%
    # only keep latest month's data
    # filter(report_period_end == tail(.$report_period_end, 1)) %>%
    select(all_of(cols_to_keep)) %>%
    mutate(across(report_period_end, dmy)) %>%

    # "EMIS have identified an error in the calculation of the specified fields
    # (Total_Use, Pat_DetCodeRec_Use) for all months prior to July 2020 and are
    # unable to provide corrected data.
    # Counts for these fields from July 2020 onwards are therefore considerably
    # higher than in previous months and are
    # not comparable."
    filter(report_period_end > "2020-06-30") %>%
    mutate(date_join_field = report_period_end + period("1d")) %>%
    rename(pomi_report_period_end = report_period_end) %>%
    mutate(field = tolower(field)) %>%
    filter(field %in% fields_to_keep) %>%

    pivot_wider(names_from = "field", values_from = "value") %>%
    # clean_names() %>%

    # transform surgery names to Title Case (from ALL CAPS)
    mutate(across(practice_name, str_to_title)) %>%
    # couple of fixes to the above
    mutate(across(practice_name,
                  ~ str_replace_all(., c("GP" = "Gp", "HC" = "Hc")))) %>%
    # fix for a few surgeries where total patients enabled > list size!
    mutate(across(
      total_pat_enbld,
      ~ if_else(total_pat_enbld > patient_list_size,
                patient_list_size,
                total_pat_enbld
                )))
}


# helper function for ONSPD ----------------------------------------------


get_latest_quarter <- function() {
  this_month <- month(today())
  get_year <- year(today())

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
    toupper(month(get_month, label = TRUE, abbr = TRUE)),
    "_",
    get_year
  )
}
