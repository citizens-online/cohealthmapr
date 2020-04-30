# cohealthmapr

![Citizens Online logo](co_logo_smaller.png)

This code and resulting map were created by Francis Barton of [Citizens Online][co-www] in March-April 2020.

See:
* [this page][coh-map] for RMarkdown output and "behind the scenes"
* [our blog post][co-www-blog] launching the map, and
* [the map page and explanatory text][co-www-map].

Here's a listing of the contents of the `data` directory in my local repository.
As you can see, the file sizes are quite large so I haven't uploaded this folder to GitHub.

```{r}
library(dplyr)
tibble(files = list.files("data", recursive = TRUE)) %>% 
  mutate(size_mb = round(file.size(paste0("data/", files))/2^20))

# A tibble: 6 x 2
  files                                size_mb
  <chr>                                  <dbl>
1 gp-reg-pat-prac-sing-age-regions.csv       3
2 POMI_APR2019_to_FEB2020.csv              163
3 surgery_data_raw.csv                      18
4 tmp/NSPCL_FEB20_UK_LU.csv                799
5 tmp/ons_postcode_data.zip                 45
6 tmp/pomi_1920.zip                          6
```

Look at [`pomi_data_dictionary.txt`][] for descriptions of the Patient Online data.


This project is written in `R` using:
* [RStudio][rstudio]
* the [tidyverse][]
* [janitor][]
* [sf][] and
* [leaflet][]


[co-www]: https://www.citizensonline.org.uk/
[coh-map]: https://citizens-online.github.io/cohealthmapr/
[co-www-blog]: https://www.citizensonline.org.uk/digital-exclusion-gp-map/
[co-www-map]: https://www.citizensonline.org.uk/gp-map/
[rstudio]: https://rstudio.com/
[tidyverse]: https://www.tidyverse.org/
[janitor]: https://sfirke.github.io/janitor/
[sf]: https://r-spatial.github.io/sf/
[leaflet]: https://rstudio.github.io/leaflet/
