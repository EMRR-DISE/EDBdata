# Code to prepare HAB satellite `DATASET` data sets

library(tidyverse)
library(curl)
library(glue)
library(stars)
library(lubridate)
library(deltamapr)

# Create a subfolder in a temporary directory to temporarily store .tif files for the R session
dir.create(temp_dir_hab <- file.path(tempdir(), "hab_data"))

# Function to download and unzip harmful algal bloom (HAB) satellite data (cyanobacteria abundance)
  # from the https://fhab.sfei.org/ website
download_hab <- function(temp_path, hab_yr, hab_month) {
  hab_url <- glue("https://fhab.sfei.org/lib/download.php?request=download&dltype=month&year={hab_yr}&month={hab_month}&product=Mosaic")
  out_path <- file.path(temp_path, glue("mosaic_{hab_yr}_{hab_month}.zip"))

  curl_download(hab_url, out_path)
  unzip(out_path, exdir = temp_path)
  Sys.sleep(5)
}

# Download data for May-December 2020
hab_2020 <- c(5:12)
for (i in hab_2020) {download_hab(temp_dir_hab, 2020, i)}

# Download data for May-October 2021
hab_2021 <- c(5:10)
for (i in hab_2021) {download_hab(temp_dir_hab, 2021, i)}

# Create a vector of all file paths for the HAB satellite data
fp_hab <- dir(path = temp_dir_hab, pattern = "tif$", full.names = TRUE)

# >>>> Temporary: select a subset of files to test code - 8/1/2021 and 8/2/2021
fp_hab_temp <- fp_hab[c(338:339)]

# Create a nested data frame to store and clean the HAB satellite data
stars_hab <-
  tibble(
    fp = fp_hab_temp,
    st_obj = map(fp, read_stars)
  ) %>%
  # pull out date components and convert to date
  mutate(
    yr_chr = map_chr(fp, ~str_extract(.x, "(?<=sentinel-3a\\.)[:digit:]{4}")),
    mo_day_chr = map_chr(fp, ~str_extract(.x, "[:digit:]{4}(?=\\.L3\\.CA_mosaic)")),
    date_chr = map2_chr(yr_chr, mo_day_chr, ~str_c(.x, .y)),
    st_date = as_date(map_dbl(date_chr, ymd))
  ) %>%
  select(st_date, st_obj)

# Create crs object for crs of HAB satellite data
crs_hab <- st_crs(stars_hab$st_obj[[1]])

# Transform crs of WW_Delta shapefile from deltamapr to the crs of the HAB satellite data
WW_Delta_32611 <- st_transform(WW_Delta, crs = crs_hab)

# Start to clean HAB satellite data
stars_hab_clean1 <- stars_hab %>%
  mutate(
    st_obj_crop =
      # Crop HAB satellite data to bounding box of the SF Bay and Delta to make it easier to work with
      map(st_obj, ~st_crop(.x, st_bbox(WW_Delta_32611)) %>%
        # Transform crs of HAB satellite data to WGS 84 so it can be plotted
        st_transform(st_crs(4326)) %>%
        # Convert factor to numeric
        mutate(across(everything(), as.numeric)) %>%
        # Convert indices greater than 250 to NA values since these represent no data
        mutate(across(everything(), ~replace(.x, .x > 250, NA_real_))) %>%
        # rename list objects to be more descriptive
        set_names("cyano_index")
    )
  )

# Transform crs of WW_Delta_32611 shapefile to WGS 84
WW_Delta_4326 <- st_transform(WW_Delta_32611, st_crs(4326))

# Create temporary plot of data collected on 8/2/2021
ggplot() +
  geom_stars(data = stars_hab_clean1$st_obj_crop[[2]]) +
  scale_fill_viridis_b(name = "Cyano Index") +
  geom_sf(data = WW_Delta_4326, alpha = 0, size = 0.1) +
  coord_sf(xlim = c(-121.8, -121.3), ylim = c(37.8, 38.2))




# Delete temporary "hab_data" directory
unlink(temp_dir_hab, recursive = TRUE)

#usethis::use_data(DATASET, overwrite = TRUE)
