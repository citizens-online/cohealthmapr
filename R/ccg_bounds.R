library(jsonlite)
library(sf)

# https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-generalised-clipped-boundaries-en

ccg_bounds_url <- "https://opendata.arcgis.com/datasets/e33a6b14379f4d0b9890f9dfa26f8a1f_1.geojson"
ccg_bounds <- st_read(ccg_bounds_url)

saveRDS(ccg_bounds, "ccg_bounds.Rds")
