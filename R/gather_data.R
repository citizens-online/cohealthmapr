# geocoding GP surgery postcodes ------------------------------------------

# Compile to full_data ----------------------------------------------------

load_packages()

geo_lookup <- readRDS("C:\\Users\\Francis\\Citizens Online\\Citizens Online Team Site - Central Team\\ResearchAndStats\\CO_Heatmap\\digexcl_heatmap\\rds_data\\full_lookup.Rds") %>%
  select(lsoa11cd, lad19cd, lad19nm)

surgery_data <- get_surgery_data()
ons_nspcl_data <- get_postcode_data()

# saveRDS(ons_nspcl_data, here("data/ons_nspcl_data.Rds"))

## join postcode data to NHS data
surgery_data <- surgery_data %>%
  inner_join(ons_nspcl_data, .) %>%
  left_join(geo_lookup)

pomi_data <- get_pomi_data()


# older_age_groups <- c("65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95+")
# nicer to do this programmatically :-) >>>
older_age_groups <- surgery_data %>%
  pull(age_group_5) %>%
  unique %>%
  tail(7)


full_data <- surgery_data %>%
  # don't keep data subdivisions by sex, only keep "all"
  filter(sex == "ALL") %>%
  select(1:5, 10:11, 6, total_patients = number_of_patients) %>%

  left_join(
    surgery_data %>%
      # only keep patient numbers for patients in `older_age_groups`
      filter(age_group_5 %in% older_age_groups) %>%
      # widen into separate columns for each age group
      pivot_wider(names_from = age_group_5,
                  names_prefix = "patients_",
                  values_from = number_of_patients) %>%
      clean_names() %>%
      group_by(practice_code) %>%
      summarise_at(vars(starts_with("patients")), ~ sum(.)) %>%
      ungroup()
  ) %>%

  # could be rewritten with dplyr::c_across ?
  mutate(older_popn_unwtd = rowSums(select(.data = ., starts_with("patients")))) %>%

  # fairly arbitrary weightings to emphasise surgeries with more people in the oldest age brackets
  # 0.6*1.1*1.25*1.4 ~= 1, not that it really matters but it's a kind of constraint
  mutate(older_popn_wtd = round(
    (patients_65_69*0.6) +
      (patients_70_74) +
      (patients_75_79*1.1) +
      (patients_80_84*1.25) +
      ((patients_85_89+patients_90_94+patients_95)*1.4))) %>%

  # create quintile column
  # set quintiles as factors so that they're not treated as a continuous variable in the map legend
  mutate(older_popn_quintile = as_factor(ntile(older_popn_wtd, 5))) %>%

  left_join(pomi_data %>%
              # fix for a few surgeries where total patients enabled was higher than list size!
              mutate_at(vars(total_pat_enbld),
                        ~ if_else(total_pat_enbld > patient_list_size, patient_list_size, total_pat_enbld))) %>%

  mutate(offline_patients = patient_list_size - total_pat_enbld) %>%
  mutate(offline_pat_pct = round(offline_patients*100/patient_list_size, 2)) %>%

  # reorder columns a bit, just because
  select(8,22,1:7,20:21, everything()) %>%
  # and sort by surgery size: this helps larger circles get plotted first (and hence below smaller circles)
  arrange(desc(total_patients))

saveRDS(full_data, here("full_data.Rds"))
write_csv(full_data, here("full_data_gpsurgeries.csv"))

# shortcuts:
# full_data <- read_csv(here("full_data_gpsurgeries.csv"))
# or
# full_data <- readRDS(here("full_data.Rds"))


# convert full_data to sf -----------------------------------------------------------


# the `sf` way
full_data_sf <- full_data %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% # 27700 = UK Grid CRS
  sf::st_transform(crs = 4326) %>%                                # 4326 = lat/lon CRS
  sf::st_jitter(0.002)

saveRDS(full_data_sf, here("full_data_sf.Rds"))




# functions ---------------------------------------------------------------




load_packages <- function() {

  library(dplyr, warn.conflicts = FALSE)
  library(forcats)
  library(here, warn.conflicts = FALSE)
  library(janitor, warn.conflicts = FALSE)
  library(readr)
  library(sf)
  library(stringr)
  library(tidyr)

}

get_surgery_data <- function(download = FALSE) {


  # get NHS surgery data - 5y age brackets ----------------------------------

  nhs_csv <- here("data/surgery_data_raw.csv")

  if(download) {
    ## download and import csv from NHS Digital website (> 18MB file)

    # https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/may-2020

    nhs_csv_url = "https://files.digital.nhs.uk/7F/C526F5/gp-reg-pat-prac-quin-age.csv" # May 2020
    download.file(nhs_csv_url, nhs_csv)
  }

  # col_types was needed to be set because empty org_type cells confused readr
  readr::read_csv(nhs_csv, col_types = "cccccccci") %>%
    clean_names %>%
    filter(org_type == "GP") %>%          # only show GPs (not STP/CCG data)
    select(-c(1:3,5)) %>%                 # remove a few columns
    rename("practice_code" = org_code)

}

get_postcode_data <- function(download = FALSE) {

  # get postcode data -------------------------------------------------------

  ons_pc_zip <- here("data/tmp/ons_postcode_data.zip")

  if(download) {
    # download and import postcode/COA lookup from ONS OpenGeography
    # https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-may-2020
    ons_nspcl_url <- "https://www.arcgis.com/sharing/rest/content/items/fb894c51e72748ec8004cc582bf27e83/data"

    # Important to set `mode = "wb"` here because the url does not end in `.zip`
    # so R does not know it's a binary file (normally sets "wb" automatically for
    # URLs that end in `.zip`)

    # Beware, this is a 210MB download (zip) and ~1.3GB file when unzipped

    download.file(ons_nspcl_url, ons_pc_zip, mode = "wb")
  }

  ons_nspcl_file <- "Data/ONSPD_MAY_2020_UK.csv"
  unzip(ons_pc_zip, exdir = here("data/tmp"), files = ons_nspcl_file)


  read_csv(here("data/tmp", ons_nspcl_file), col_types = "ccciiccccciiiiccccicccccccccccccccccccccccddcccicc") %>%
    select("postcode" = pcds, "easting" = oseast1m, "northing" = osnrth1m, "oa11cd" = oa11, "lsoa11cd" = lsoa11)

  # file.remove(ons_pc_zip)
  # file.remove(here("data/tmp", ons_nspcl_file))

}


get_pomi_data <- function(download = FALSE) {

  # get patient online data (POMI) ------------------------------------------

  pomi_zip <- here("data/tmp/pomi_2021.zip")

  if(download) {
  # https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/pomi
  # this will hopefully be the right URL until ~April 2021
  pomi_url = "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-collections/pomi/pomi_2021.zip"

  download.file(pomi_url, pomi_zip)
  }

  pomi_file <- unzip(pomi_zip, list = TRUE) %>% pull(Name)
  unzip(pomi_zip, exdir = here("data"))

  pomi_data <- read_csv(here("data", pomi_file)) %>%
    # only keep latest month's data
    filter(report_period_end == tail(.$report_period_end, 1)) %>%
    select(ccg_code, ccg_name, practice_code, practice_name, field, value) %>%
    pivot_wider(names_from = "field", values_from = "value") %>%
    clean_names() %>%
    # transform surgery names to Title Case (from ALL CAPS)
    mutate_at(vars(practice_name), ~ str_to_title(.))

}


