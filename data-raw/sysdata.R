# Code to prepare internal data used globally in the package:
  # `sf_edb_reg` - Shapefile containing regions for Emergency Drought Barrier

# Load packages
library(dplyr)
library(sf)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/sysdata.R")

# Load polygon shapefile for the EDB regions
sf_edb_reg <- read_sf(here("data-raw/Spatial_data/EDB_Regions.shp")) %>%
  select(Region = Regions)

# Save internal data set as internal object in the package
usethis::use_data(sf_edb_reg, internal = TRUE, overwrite = TRUE)

