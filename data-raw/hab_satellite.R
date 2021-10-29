# Code to prepare HAB satellite data sets:
  # 1) `hab_sat_ci_fr_mil` - daily average Cyano Index pixel values within Franks Tract and Mildred Island
  # 2) `hab_sat_ci_edb_reg` - daily average Cyano Index pixel values within the EDB regions


# Load packages
library(tidyverse)
library(curl)
library(glue)
library(stars)
library(lubridate)
library(exactextractr)

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

# Create a nested data frame to store and clean the HAB satellite data
df_hab_sat <-
  tibble(
    fp = fp_hab,
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
sf_franks_mildred <- read_sf("data-raw/Spatial_data/Franks_Mildred.shp")
sf_edb_reg <- read_sf("data-raw/Spatial_data/EDB_Regions.shp")

# Clean up Franks-Mildred and EDB regions shapefiles to contain just the necessary variables
sf_franks_mildred_clean <- sf_franks_mildred %>% select(Name = HNAME)
sf_edb_reg_clean <- sf_edb_reg %>% select(-notes)

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
  tibble(
    dates = c(
      "2020-12-22",
      "2021-07-19",
      "2021-07-22",
      "2021-08-16"
    )
  ) %>%
  mutate(dates = ymd(dates)) %>%
  pull(dates)

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
        # Convert indices greater than 250 to NA values since these represent "no data"
        mutate(pixel_val = replace(pixel_val, pixel_val > 250, NA_real_)) %>%
        # Convert to raster object
        st_as_stars() %>%
        as("Raster")
    )
  ) %>%
  select(-strs_prx_obj)

# Function to calculate average pixel values within polygons - converting NaN values to
  # numeric NA values
calc_avg_pixel_val <- function(sf_obj, rast_obj) {
  sf_obj %>%
    mutate(mean_pixel_val = exact_extract(rast_obj, sf_obj, "mean")) %>%
    mutate(mean_pixel_val = if_else(!is.nan(mean_pixel_val), mean_pixel_val, NA_real_))
}

# Function to convert sf object to a tibble
conv_sf2tbl <- function(sf_obj) {
  sf_obj %>% as_tibble() %>% select(-geometry)
}

# Function to fill in any missing date-polygon combinations grouped by year
fill_miss_dates <- function(df, poly_var) {
  df %>%
    mutate(Year_tmp = year(Date)) %>%
    group_by(Year_tmp) %>%
    complete(Date = seq.Date(min(Date), max(Date), by = "1 day"), {{ poly_var }}) %>%
    ungroup() %>%
    select(-Year_tmp)
}

# Calculate daily average pixel values within Franks Tract and Mildred Island
hab_sat_ci_fr_mil <- df_hab_sat_clean %>%
  mutate(sf_fr_mil = list(sf_franks_mildred_32611)) %>%
  mutate(sf_fr_mil = map2(sf_fr_mil, rast_obj_crop, calc_avg_pixel_val)) %>%
  # Simplify to a summary data frame
  mutate(df_fr_mil = map(sf_fr_mil, conv_sf2tbl)) %>%
  select(strs_date, df_fr_mil) %>%
  unnest(cols = df_fr_mil) %>%
  rename(Date = strs_date) %>%
  # Fill in any missing date-name combinations grouped by year
  fill_miss_dates(Name)

# Calculate daily average pixel values within the EDB regions
hab_sat_ci_edb_reg <- df_hab_sat_clean %>%
  mutate(sf_edb = list(sf_edb_reg_32611)) %>%
  mutate(sf_edb = map2(sf_edb, rast_obj_crop, calc_avg_pixel_val)) %>%
  # Simplify to a summary data frame
  mutate(df_edb = map(sf_edb, conv_sf2tbl)) %>%
  select(strs_date, df_edb) %>%
  unnest(cols = df_edb) %>%
  rename(Date = strs_date) %>%
  # Fill in any missing date-region combinations grouped by year
  fill_miss_dates(Regions)

# Delete temporary directory containing HAB satellite data
unlink(temp_dir_hab, recursive = TRUE)

# Save final data sets containing daily averages as csv files for easier diffing
write_csv(hab_sat_ci_fr_mil, "data-raw/hab_sat_ci_fr_mil.csv")
write_csv(hab_sat_ci_edb_reg, "data-raw/hab_sat_ci_edb_reg.csv")

# Save final data sets containing daily averages as objects in the data package
usethis::use_data(hab_sat_ci_fr_mil, hab_sat_ci_edb_reg, overwrite = TRUE)

