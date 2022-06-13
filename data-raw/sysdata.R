# Code to prepare internal data used globally in the package:
  # `sf_edb_reg` - Shapefile containing regions for Emergency Drought Barrier
  # `sf_hab_reg` - Shapefile containing regions for the HABs/Weeds report
  # `df_hab_sat_pixel_key` - Crosswalk key for the HAB satellite pixel values

# Load packages
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/sysdata.R")

# Load polygon shapefile for the EDB regions
sf_edb_reg <- read_sf(here("data-raw/Spatial_data/EDB_Regions.shp")) %>%
  select(Region = Regions)

# Load polygon shapefile for the HABs regions which are used in the analyses of
  # the HABs/Weeds report
sf_hab_reg <- read_sf(here("data-raw/Spatial_data/HABregions.shp")) %>%
  select(Region = Stratum2)

# Load crosswalk key for the HAB satellite pixel values
df_hab_sat_pixel_key <- read_csv(here("data-raw/HAB_satellite_data/hab_sat_pixel_val_key.csv")) %>%
  mutate(
    # Clean up CyanoIndex and EstimatedMicrocystisCellConc columns
    across(
      c(CyanoIndex, EstimatedMicrocystisCellConc),
      ~ as.numeric(str_remove_all(.x, "^>|^\\?|,"))
    ),
    # Make CyanoIndex = 0 at PixelValue = 0
    CyanoIndex = if_else(PixelValue == 0, 0, CyanoIndex),
    # Create a column for Cyano Index categories
    CICategory = case_when(
      is.na(CyanoIndex) ~ "InvalidOrMissing",
      CyanoIndex == 0 ~ "NonDetect",
      CyanoIndex < 2e-04 ~ "Low",
      CyanoIndex < 1e-03 ~ "Moderate",
      CyanoIndex < 0.01 ~ "High",
      TRUE ~ "VeryHigh"
    )
  ) %>%
  select(PixelValue, CyanoIndex, CICategory, EstimatedMicrocystisCellConc, Comments)

# Save internal data set as internal object in the package
usethis::use_data(sf_edb_reg, sf_hab_reg, df_hab_sat_pixel_key, internal = TRUE, overwrite = TRUE)

