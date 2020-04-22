# geocoding GP surgery postcodes ------------------------------------------

library(conflicted)
library(here)
library(readr)
library(dplyr)
conflict_prefer("filter", "dplyr")
library(janitor)
library(tidyr)
library(stringr)
library(forcats)
library(sf)


# get NHS surgery data - 5y age brackets ----------------------------------


## download and importcsv from NHS Digital website
## https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/march-2020

nhs_csv_url <- "https://files.digital.nhs.uk/90/E7CC92/gp-reg-pat-prac-quin-age.csv"
nhs_csv <- here("data/surgery_data_raw.csv")
# download.file(nhs_csv_url, nhs_csv)

# col_types was needed to be set because empty org_type cells confused readr
surgery_data_raw <- readr::read_csv(nhs_csv, col_types = "cccccccci")

## tidy up raw data
surgery_data2 <- surgery_data_raw %>%
  clean_names %>%
  filter(org_type == "GP") %>%          # only show GPs (not STP/CCG data)
  select(-c(1:3,5))                     # only keep some columns



# get postcode data -------------------------------------------------------

## download and import postcode/COA lookup from ONS OpenGeography
# from https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-hierarchy-with-classifications-february-2020-lookup-in-the-uk
# ons_nspcl_feb20_url <- "https://www.arcgis.com/sharing/rest/content/items/b9f02f5935be45f6ad1b6405b0d43f72/data"
ons_pc_zip <- here("data/tmp/ons_postcode_data.zip")

# important to set mode = "wb" here because the url does not end in `.zip`
# so R does not know it's a binary file (normally sets "wb" automatically for
# URLs that end in `.zip`)
# download.file(ons_nspcl_feb20_url, ons_pc_zip, mode = "wb")
ons_nspcl_file <- unzip(ons_pc_zip, list = TRUE) %>% pull(Name)
unzip(ons_pc_zip, exdir = here("data/tmp"))

ons_nspcl_data <- read_csv(here("data/tmp", ons_nspcl_file), col_types = "ccciiiiicc????????????ccc")


## join postcode data to NHS data
surgery_data <- ons_nspcl_data %>%
  select(3, "easting" = oseast1m, "northing" = osnrth1m, 22) %>%
  inner_join(surgery_data2, by = c("pcds" = "postcode")) %>%
  rename("postcode" = pcds, "practice_code" = org_code)


# get patient online data (POMI) ------------------------------------------


# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/pomi
pomi_url <- "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-collections/pomi/pomi_1920.zip"
pomi_zip <- here("data/pomi_1920.zip")

# download.file(pomi_url, pomi_zip)

pomi_file <- unzip(pomi_zip, list = TRUE) %>% pull(Name)
unzip(pomi_zip, exdir = here("data"))

pomi_data_raw <- read_csv(here("data", pomi_file))
pomi_data <- pomi_data_raw %>%
  filter(report_period_end == tail(.$report_period_end, 1)) %>%
  select(ccg_code, ccg_name, practice_code, practice_name, field, value) %>%
  pivot_wider(names_from = "field", values_from = "value") %>%
  clean_names() %>%
  mutate_at(vars(practice_name), ~ str_to_title(.))


# Compile to full_data ----------------------------------------------------

# over65_groups <- c("65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95+")

older_age_groups <- surgery_data %>%
  pull(age_group_5) %>%
  unique %>%
  tail(7)


full_data <- surgery_data %>%
  filter(sex == "ALL") %>%
  select(1:5, total_patients = 8) %>%
  left_join(
    surgery_data %>%
      filter(age_group_5 %in% older_age_groups) %>%
      pivot_wider(names_from = age_group_5,
                  names_prefix = "patients_",
                  values_from = number_of_patients) %>%
      clean_names() %>%
      group_by(practice_code) %>%
      summarise_at(vars(starts_with("patients")), ~ sum(.)) %>%
      ungroup()
  ) %>%
  mutate(older_popn_unwtd = rowSums(select(.data = ., starts_with("patients")))) %>%
  mutate(older_popn_wtd = round(
    (patients_65_69*0.6) +
    (patients_70_74) +
    (patients_75_79*1.1) +
    (patients_80_84*1.25) +
    ((patients_85_89+patients_90_94+patients_95)*1.4))) %>%
  mutate(older_popn_quintile = ntile(older_popn_wtd, 5)) %>%
  mutate_at(vars(older_popn_quintile), ~ as_factor(.)) %>%
  left_join(pomi_data %>%
              mutate_at(vars(total_pat_enbld),
                        ~ if_else(total_pat_enbld > patient_list_size, patient_list_size, total_pat_enbld))) %>%
  mutate(offline_patients = patient_list_size - total_pat_enbld) %>%
  mutate(offline_pat_pct = round(offline_patients*100/patient_list_size, 2)) %>%
  # janitor::adorn_pct_formatting("offline_pat_pct")
  # mutate_at(vars(offline_pat_pct), ~ paste0(., "%")) %>%
  # mutate(monthly_transactions = rowSums(select(.data = ., ends_with("use")))) %>%
  select(5,19,4,1:3,17:18, everything()) %>%
  arrange(desc(total_patients))

saveRDS(full_data, here("full_data.Rds"))
# write_csv(full_data, here("full_data_gpsurgeries.csv"))

# full_data <- read_csv(here("full_data_gpsurgeries.csv"))
# full_data <- readRDS(here("full_data.Rds"))


# ukgrid <- "+init=epsg:27700"
# latlon = "+init=epsg:4326"
# opensm <- "+init=epsg:3857"


# convert full_data to sf -----------------------------------------------------------


# the `sf` way
full_data_sf <- full_data %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_jitter(0.002)

saveRDS(full_data_sf, here("full_data_sf.Rds"))

