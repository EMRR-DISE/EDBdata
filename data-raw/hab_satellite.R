# Code to prepare HAB satellite data sets:
# 1) `hab_sat_ow_delta` - Pixel counts within each Cyano Index (CI) category for
  # 4 open water regions in the upper San Francisco Estuary: Franks Tract, Mildred
  # Island, Clifton Court Forebay, and Liberty Island. Also includes a calculated
  # average Cyano Index value for each region and date in the data set. Summary
  # statistics cover the spring through late fall (May-Dec) in 2020 and 2021.

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(glue)
library(curl)
library(stars)
library(sf)
library(exactextractr)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/hab_satellite.R")

# Set download to TRUE if need to download harmful algal bloom (HAB) satellite data
download <- FALSE

# Download HAB satellite data if necessary
if (download) {
  # Create a subfolder in data-raw to store .tif files on local computer if it doesn't already exist
  if (!dir.exists(here("data-raw/HAB_satellite_data"))) {
    dir.create(here("data-raw/HAB_satellite_data"))
  }

  # Define subfolder directory to store .tif files
  dir_hab_sat <- here("data-raw/HAB_satellite_data")

  # Function to download and unzip harmful algal bloom (HAB) satellite data (Cyano Index)
    # from the https://fhab.sfei.org/ website
  download_hab <- function(hab_yr, hab_month) {
    hab_url <- glue("https://fhab.sfei.org/lib/download.php?request=download&dltype=month&year={hab_yr}&month={hab_month}&product=Mosaic")
    out_path <- file.path(dir_hab_sat, glue("mosaic_{hab_yr}_{hab_month}.zip"))

    curl_download(hab_url, out_path)
    unzip(out_path, exdir = dir_hab_sat)
    Sys.sleep(5)
  }

  # Download data for May-December 2020
  hab_2020 <- c(5:12)
  for (i in hab_2020) {download_hab(2020, i)}

  # Download data for May-October 2021
  hab_2021 <- c(5:10)
  for (i in hab_2021) {download_hab(2021, i)}

  # Only keep sentinel-3a .tif files
  fp_remove <- str_subset(dir(dir_hab_sat, full.names = TRUE), "sentinel-3a.+tif$", negate = TRUE)
  invisible(file.remove(fp_remove))
}

# Create a vector of all file paths for the HAB satellite data
fp_hab_sat <- dir(here("data-raw/HAB_satellite_data"), pattern = "tif$", full.names = TRUE)

# Create a nested data frame to store and clean the HAB satellite data
df_hab_sat <-
  tibble(
    fp = fp_hab_sat,
    strs_prx_obj = map(fp, read_stars, proxy = TRUE)
  ) %>%
  # pull out date components and convert to date
  mutate(
    yr_chr = map_chr(fp, ~str_extract(.x, "(?<=sentinel-3a\\.)[:digit:]{4}")),
    mo_day_chr = map_chr(fp, ~str_extract(.x, "[:digit:]{4}(?=\\.L3\\.CA_mosaic)")),
    date_chr = map2_chr(yr_chr, mo_day_chr, ~str_c(.x, .y)),
    strs_date = as_date(map_dbl(date_chr, ymd))
  ) %>%
  select(strs_date, strs_prx_obj)

# Import the polygon shapefile for the four open water regions in the Delta
sf_ow_delta <- read_sf(here("data-raw/Spatial_data/Franks_Mildr_CCF_LibIsl.shp"))

# Clean up shapefile of the open water regions with a more descriptive column header
sf_ow_delta_c <- sf_ow_delta %>% rename(Region = HNAME)

# Transform crs of open water regions shapefile to the crs of the HAB satellite data
crs_hab_sat <- st_crs(df_hab_sat$strs_prx_obj[[1]])
sf_ow_delta_32611 <- st_transform(sf_ow_delta_c, crs = crs_hab_sat)

# Create a bounding box of the open water regions shapefile which will be used to
  # crop the satellite data. Add a 1 km buffer to slightly expand the bounding box
  # to ensure no desired data is removed.
bbox_ow_delta <- st_bbox(st_buffer(sf_ow_delta_32611, 1000))

# Create a vector of dates to exclude from the analysis since the imagery doesn't cover the Delta region
dates_rm <-
  ymd(
    c(
      "2020-12-22",
      "2021-07-19",
      "2021-07-22",
      "2021-08-16"
    )
  )

# Prepare HAB satellite data for zonal statistics
df_hab_sat_clean <- df_hab_sat %>%
  # Remove satellite objects that don't cover the Delta region
  filter(!strs_date %in% dates_rm) %>%
  mutate(
    rast_obj_crop =
      # Crop HAB satellite data to bounding box of the open water regions
        # shapefile to make it easier to work with
      map(strs_prx_obj, ~st_crop(.x, bbox_ow_delta) %>%
        # rename attribute to be more descriptive
        setNames("pixel_val") %>%
        # Convert factor to numeric
        mutate(pixel_val = as.numeric(as.character(pixel_val))) %>%
        # Convert to raster object
        st_as_stars() %>%
        as("Raster")
    )
  ) %>%
  select(-strs_prx_obj)

# Function to count the number of pixels within each CI category for the pixels
  # completely within the polygon (100% coverage fraction)
count_ci_cat <- function(df) {
  df %>%
    filter(coverage_fraction == 1) %>%
    left_join(EDBdata:::df_hab_sat_pixel_key, by = c("value" = "PixelValue")) %>%
    count(CICategory, name = "pixel_count") %>%
    pivot_wider(names_from = CICategory, values_from = pixel_count)
}

# Function to calculate the average Cyano Index of valid pixels completely
  # within the polygon (100% coverage fraction)
calc_avg_ci <- function(df) {
  df %>%
    filter(coverage_fraction == 1) %>%
    left_join(EDBdata:::df_hab_sat_pixel_key, by = c("value" = "PixelValue")) %>%
    summarize(AvgCI = mean(CyanoIndex, na.rm = TRUE)) %>%
    pull(AvgCI)
}

# Finish preparing the HAB satellite data for the four open water regions in the Delta
hab_sat_ow_delta <- df_hab_sat_clean %>%
  mutate(sf_fr_mil_ccf_lib = list(sf_ow_delta_32611)) %>%
  # Extract pixels from within each polygon
  mutate(
    sf_fr_mil_ccf_lib = map2(
      sf_fr_mil_ccf_lib,
      rast_obj_crop,
      ~ mutate(.x, df_rast_extract = exact_extract(.y, .x))
    )
  ) %>%
  # Convert sf object to data frame
  mutate(df_fr_mil_ccf_lib = map(sf_fr_mil_ccf_lib, st_drop_geometry)) %>%
  select(Date = strs_date, df_fr_mil_ccf_lib) %>%
  unnest(cols = df_fr_mil_ccf_lib) %>%
  # Count number of pixels in each CI category for each region and date
  mutate(df_ci_count = map(df_rast_extract, count_ci_cat)) %>%
  # Unnest CI category counts into data frame
  unnest(cols = df_ci_count) %>%
  # Replace NA values in the CI category counts with zeros
  replace_na(
    list(
      NonDetect = 0,
      Low = 0,
      Moderate = 0,
      High = 0,
      VeryHigh = 0,
      InvalidOrMissing = 0
    )
  ) %>%
  # Calculate total number of pixels counted for each row and the percent of
    # pixels with valid data (<= 250)
  mutate(
    PixelSum = rowSums(across(where(is.integer))),
    PercValid = (1 - InvalidOrMissing/PixelSum) * 100
  ) %>%
  # Only include days where there were greater than 25% valid pixels
  filter(PercValid > 25) %>%
  # Calculate the average Cyano Index value for each region and date
  mutate(AvgCI = map_dbl(df_rast_extract, calc_avg_ci)) %>%
  # Reorder and select variables
  select(
    Date,
    Region,
    AvgCI,
    NonDetect,
    Low,
    Moderate,
    High,
    VeryHigh,
    InvalidOrMissing
  )

# Save final data set containing counts and averages of Cyano Index values as csv file
  # for easier diffing
write_csv(hab_sat_ow_delta, here("data-raw/Final/hab_sat_ow_delta.csv"))

# Save final data set containing counts and averages of Cyano Index values as object
  # in the data package
usethis::use_data(hab_sat_ow_delta, overwrite = TRUE)

