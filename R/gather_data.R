
# Compile to full_data ----------------------------------------------------

get_full_data <- function(build = FALSE, update = FALSE) {
  library(here)

  if (!build) {
    readRDS(here("rds_data", "full_data_sf.Rds"))
  } else {
    build_full_data(update = update)
  }
}


build_full_data <- function(update = FALSE) {
  library(here)
  source(here("R", "functions.R"))

  load_packages()

  geo_lookup <- get_geolookup(update = update)
  surgery_data <- get_surgery_data(update = update)
  onspd_data <- get_postcode_data(update = update, download = FALSE)
  pomi_data <- get_pomi_data(update = update) %>%
    # fix for a few surgeries where total patients enabled was higher than list size!
    mutate(across(
      total_pat_enbld,
      ~ if_else(total_pat_enbld > patient_list_size, patient_list_size, total_pat_enbld)
    ))

  # older_age_groups <- c("65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95+")
  # nicer to do this programmatically :-) >>>
  older_age_groups <- surgery_data %>%
    pull(age_group_5) %>%
    unique() %>%
    tail(7)



  full_data <- surgery_data %>%
    inner_join(onspd_data, .) %>%
    left_join(geo_lookup) %>%
    relocate(starts_with("lad"), .after = "lsoa11cd") %>%

    # don't keep data subdivisions by sex, only keep "all"
    filter(sex == "ALL") %>%
    select(1:8, 10, total_patients = number_of_patients) %>%
    left_join(
      surgery_data %>%
        # only keep patient numbers for patients in `older_age_groups`
        filter(age_group_5 %in% older_age_groups) %>%
        # widen into separate columns for each age group
        pivot_wider(
          names_from = age_group_5,
          names_prefix = "patients_",
          values_from = number_of_patients
        ) %>%
        clean_names() %>%
        group_by(practice_code) %>%
        summarise_at(vars(starts_with("patients")), ~ sum(.)) %>%
        ungroup()
    ) %>%
    rowwise() %>%
    mutate(older_popn_unwtd = sum(c_across(starts_with("patients")))) %>%
    ungroup() %>%

    # fairly arbitrary weightings to emphasise surgeries with more people
    # in the oldest age brackets
    # 0.6*1.1*1.25*1.4 ~= 1, not that it really matters
    # but it's a kind of constraint I suppose
    mutate(older_popn_wtd = round(
      (patients_65_69 * 0.6) +
        patients_70_74 +
        (patients_75_79 * 1.1) +
        (patients_80_84 * 1.25) +
        ((patients_85_89 + patients_90_94 + patients_95) * 1.4)
    )) %>%

    # create quintile column
    # set quintiles as factors so that they're not treated as
    # a continuous variable in the map legend
    mutate(older_popn_quintile = as_factor(ntile(older_popn_wtd, 5))) %>%
    left_join(pomi_data) %>%
    mutate(offline_patients = patient_list_size - total_pat_enbld) %>%
    mutate(offline_pat_pct = round(offline_patients * 100 / patient_list_size, 1)) %>%

    # reorder columns a bit
    select(8, 22, 1:7, 20:21, everything()) %>%

    # and sort by surgery size: this helps larger circles get plotted first
    # (and hence below smaller circles)
    arrange(desc(total_patients))

  saveRDS(full_data, here("rds_data", "full_data.Rds"))
  write_csv(full_data, here("full_data_gpsurgeries.csv"))

  return(full_data)
}
