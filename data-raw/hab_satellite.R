# Code to prepare HAB satellite data sets:
  # 1) `hab_sat_fr_mil` - counts of pixel values within each CI category for Franks Tract
    # and Mildred Island
  # 2) Kept placeholder code to perform same procedure for the EDB regions, but no data object
    # exists for this currently

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

  # Function to download and unzip harmful algal bloom (HAB) satellite data (cyanobacteria abundance)
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

  # Remove .zip files
  invisible(file.remove(dir(path = dir_hab_sat, pattern = "zip$", full.names = TRUE)))
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

# Import the polygon shapefiles for Franks-Mildred and the EDB regions
sf_franks_mildred <- read_sf(here("data-raw/Spatial_data/Franks_Mildred.shp"))
sf_edb_reg <- read_sf(here("data-raw/Spatial_data/EDB_Regions.shp"))

# Clean up Franks-Mildred and EDB regions shapefiles to contain just the necessary variables
sf_franks_mildred_clean <- sf_franks_mildred %>% select(Name = HNAME)
sf_edb_reg_clean <- sf_edb_reg %>% select(Region = Regions)

# Transform crs of Franks-Mildred and EDB region shapefiles to the crs of the HAB satellite data
crs_hab_sat <- st_crs(df_hab_sat$strs_prx_obj[[1]])
sf_franks_mildred_32611 <- st_transform(sf_franks_mildred_clean, crs = crs_hab_sat)
sf_edb_reg_32611 <- st_transform(sf_edb_reg_clean, crs = crs_hab_sat)

# Create a bounding box of the EDB regions shapefile which will be used to crop the satellite data
  # and extend it by 5% on each side
bbox_edb_reg <- st_bbox(sf_edb_reg_32611)
bbox_edb_reg_xrange <- bbox_edb_reg$xmax - bbox_edb_reg$xmin
bbox_edb_reg_yrange <- bbox_edb_reg$ymax - bbox_edb_reg$ymin
bbox_edb_reg[1] <- bbox_edb_reg[1] - (bbox_edb_reg_xrange * 0.05)
bbox_edb_reg[3] <- bbox_edb_reg[3] + (bbox_edb_reg_xrange * 0.05)
bbox_edb_reg[2] <- bbox_edb_reg[2] - (bbox_edb_reg_yrange * 0.05)
bbox_edb_reg[4] <- bbox_edb_reg[4] + (bbox_edb_reg_yrange * 0.05)

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
      # Crop HAB satellite data to bounding box of the EDB regions to make it easier to work with
      map(strs_prx_obj, ~st_crop(.x, bbox_edb_reg) %>%
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

# Function to count number of pixels within each CI category and the percentage of valid pixels
  # only for the pixels completely within the polygon (100% coverage fraction)
count_ci_cat <- function(df) {
  # Count pixels within each CI category
  df_count <- df %>%
    filter(coverage_fraction == 1) %>%
    mutate(
      ci_category = case_when(
        value == 0 ~ "Non_detect",
        value <= 41 ~ "Low",
        value <= 99 ~ "Moderate",
        value <= 183 ~ "High",
        value <= 250 ~ "Very_high",
        TRUE ~ "Invalid_or_missing"
      )
    ) %>%
    count(ci_category) %>%
    rename(pixel_count = n)

  # Calculate total number of pixels counted
  vec_pixel_sum <- sum(df_count$pixel_count)

  df_f <- df_count %>%
    # Restructure data frame into wide format
    pivot_wider(names_from = ci_category, values_from = pixel_count) %>%
    # Add total number of pixels counted and the percent of pixels with valid data (<= 250)
    mutate(
      pixel_sum = vec_pixel_sum,
      perc_valid = (1 - Invalid_or_missing/pixel_sum) * 100
    )

  return(df_f)
}

# Function to replace NA counts with zeros for the CI categories
repl_ci_count_na <- function(df) {
  df %>%
    replace_na(
      list(
        Non_detect = 0,
        Low = 0,
        Moderate = 0,
        High = 0,
        Very_high = 0,
        Invalid_or_missing = 0
      )
    )
}

# Calculate counts of pixel values within each CI category for Franks Tract and Mildred Island
hab_sat_fr_mil <- df_hab_sat_clean %>%
  mutate(sf_fr_mil = list(sf_franks_mildred_32611)) %>%
  # Extract pixels from within each polygon
  mutate(sf_fr_mil = map2(sf_fr_mil, rast_obj_crop, ~mutate(.x, rast_extract = exact_extract(.y, .x)))) %>%
  # Convert sf object to data frame
  mutate(df_fr_mil = map(sf_fr_mil, st_drop_geometry)) %>%
  select(Date = strs_date, df_fr_mil) %>%
  unnest(cols = df_fr_mil) %>%
  # Count number of pixels in each CI category, totals, and percent valid pixels
  mutate(rast_extract = map(rast_extract, count_ci_cat)) %>%
  # Simplify to a summary data frame
  unnest(cols = rast_extract) %>%
  # Only include days where there were greater than 25% valid pixels
  filter(perc_valid > 25) %>%
  # Reorder and select variables
  select(
    Date,
    Name,
    Non_detect,
    Low,
    Moderate,
    High,
    Very_high,
    Invalid_or_missing
  ) %>%
  # Replace NA counts with zeros
  repl_ci_count_na()

# Calculate counts of pixel values within each CI category for the EDB regions
  # Not including this for now, but keeping it as a placeholder

# Save final data set containing counts of pixel values within each CI category as csv file
  # for easier diffing
write_csv(hab_sat_fr_mil, here("data-raw/Final/hab_sat_fr_mil.csv"))

# Save final data sets containing counts of pixel values within each CI category as objects
  # in the data package
usethis::use_data(hab_sat_fr_mil, overwrite = TRUE)

