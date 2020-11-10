# functions ---------------------------------------------------------------


load_packages <- function() {
  library(dplyr, warn.conflicts = FALSE)
  library(forcats)
  library(here)
  library(janitor, warn.conflicts = FALSE)
  library(lubridate, warn.conflicts = FALSE)
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


# helper for get_surgery_data() -------------------------------------------

get_surgery_data_url <- function() {
  paste0(
    "https://digital.nhs.uk/",
    "data-and-information/",
    "publications/",
    "statistical/",
    "patients-registered-at-a-gp-practice/",
    tolower(month(today() - period("1m"), label = TRUE, abbr = FALSE)),
    "-",
    year(today() - period("1m"))
  ) %>%
    xml2::read_html() %>%
    html_nodes("#resources ul li a") %>%
    `[`(5) %>%
    html_attr("href")
}


# get GP surgery data -----------------------------------------------------


get_surgery_data <- function(update = FALSE) {
  # download and import csv from NHS Digital website


  nhs_csv <- here("data/surgery_data_raw.csv")

  if (update) {
    nhs_csv_url <- get_surgery_data_url()
    download.file(nhs_csv_url, nhs_csv)
  }

  # get NHS surgery data - 5y age brackets ----------------------------------
  # col_types was needed to be set because empty org_type cells confused readr
  readr::read_csv(nhs_csv, col_types = "cccccccci") %>%
    clean_names() %>%
    filter(org_type == "GP") %>% # only show GPs (not STP/CCG data)
    select(-c(1:3, 5)) %>% # remove a few columns
    rename("practice_code" = org_code)
}


# get ONS Postcode Directory lookup ---------------------------------------

# Latest download: August 2020
# https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-august-2020

get_postcode_data <- function(update = FALSE, download = FALSE) {
  if (!update) {
    readRDS(here("rds_data/onspd_data.Rds"))
  } else {

    # get postcode data -------------------------------------------------------
    onspd_zip <- here("data/tmp/ons_postcode_data.zip")

    if (download) {
      file.remove(onspd_zip)

      onspd_latest <- paste0(
        "https://geoportal.statistics.gov.uk/",
        "datasets/",
        "ons-postcode-directory-",
        get_latest_quarter()[1]
      )

      onspd_url <- xml2::read_html(onspd_latest) %>%
        html_node("#simple_download_button") %>%
        html_attr("href")

      # download and import postcode/COA lookup from ONS OpenGeography
      # onspd_url <- "https://www.arcgis.com/sharing/rest/content/items/a644dd04d18f4592b7d36705f93270d8/data"

      # Important to set `mode = "wb"` here because the url does not end in `.zip`
      # so R does not know it's a binary file (normally sets "wb" automatically
      # for URLs that end in `.zip`)
      # Beware, this is a 210MB download (zip) and ~1.3GB file when unzipped
      download.file(onspd_url, onspd_zip, mode = "wb")
    }

    onspd_file <- paste0(
      "Data/ONSPD_",
      get_latest_quarter()[2],
      "_UK.csv"
    )
    unzip(onspd_zip, exdir = here("data/tmp"), files = onspd_file)

    readr::read_csv(here("data/tmp", onspd_file), col_types = "ccciiccccciiiiccccicccccccccccccccccccccccddcccicc") %>%
      select("postcode" = pcds, "easting" = oseast1m, "northing" = osnrth1m, "oa11cd" = oa11, "lsoa11cd" = lsoa11)
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
  read_csv(here("data", pomi_file)) %>%
    # only keep latest month's data
    filter(report_period_end == tail(.$report_period_end, 1)) %>%
    select(report_period_end, ccg_code, ccg_name, practice_code, practice_name, field, value) %>%
    pivot_wider(names_from = "field", values_from = "value") %>%
    clean_names() %>%
    # transform surgery names to Title Case (from ALL CAPS)
    mutate(across(practice_name, ~ str_to_title(.)))
}


# helper function ---------------------------------------------------------


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

  a <- paste0(
    tolower(month(get_month, label = TRUE, abbr = FALSE)),
    "-",
    get_year
  )

  b <- paste0(
    toupper(month(get_month, label = TRUE, abbr = TRUE)),
    "_",
    get_year
  )

  return(c(a, b))
}
