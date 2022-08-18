# Code to prepare Microcystis visual index and water quality data set for the
  # Emergency Drought Barrier (HABs/Weeds) analysis:
# `mc_vis_index_wq` - Microcystis visual index and water quality measurement
  # data collected at various locations in the upper San Francisco Estuary (Delta)
  # from 2007 to 2021. Water quality parameters include water temperature and secchi
  # depth. Used in the Spring-Summer version of the 2022 HABs/Weeds report.

# Load packages
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
# Make sure we are using `discretewq` version 2.3.2
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "v2.3.2")
library(discretewq)
library(sf)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/mc_vis_index_wq.R")


# 1. Import Data ----------------------------------------------------------

# Import Microcystis visual index and water quality measurement data from the
  # discretewq package (v2.3.2)
# Select EMP, STN and FMWT since these are the only surveys in the data package
  # that have collected Microcystis visual index data that we are interested in
df_package <-
  wq(
    Sources = c("EMP", "STN", "FMWT"),
    Start_year = 2007,
    End_year = 2021
  )

# Import Microcystis visual index and water quality measurement data collected
  # by DWR_NCRO
df_ncro <-
  read_excel(
    here("data-raw/MC_visual_index_and_WQ_data/WQES HAB Observations and Field Readings.xlsx"),
    col_types = c("text", "date", "numeric", "text", "text", "numeric")
  )

# Import coordinates for NCRO stations
df_ncro_coord <- read_excel(here("data-raw/Global/NCRO_Station_Metadata_Coords.xlsx"))


# 2. Clean and Combine Data -----------------------------------------------

# discretewq data
df_package_c <- df_package %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Microcystis,
    Secchi,
    Temperature
  ) %>%
  mutate(
    # Convert Date variable to date object
    Date = date(Date),
    # Change name of Source for EMP survey
    Source = if_else(Source == "EMP", "DWR_EMP", Source)
  ) %>%
  # Only keep records with Microcystis visual index data
  filter(!is.na(Microcystis))

# DWR_NCRO station coordinates
df_ncro_coord_c <- df_ncro_coord %>%
  select(
    StationCode = `WQES Code`,
    Latitude = `Latitude (WGS84)`,
    Longitude = `Longitude (WGS84)`
  ) %>%
  drop_na()

# DWR_NCRO data
df_ncro_c <- df_ncro %>%
  # Remove dates with NA and duplicate records
  filter(!is.na(Date)) %>%
  distinct() %>%
  # Restructure to wide format
  pivot_wider(names_from = Parameter, values_from = `Field Sonde Value`) %>%
  select(
    StationCode,
    Date,
    Secchi = `Secchi (m)`,
    Microcystis,
    Temperature = Temp_C
  ) %>%
  # Only keep records with Microcystis visual index data
  filter(!is.na(Microcystis) & Microcystis != "NA") %>%
  # Add in station coordinates
  left_join(df_ncro_coord_c) %>%
  # Remove records without latitude-longitude coordinates
  filter(!if_any(c(Latitude, Longitude), is.na)) %>%
  mutate(
    # Add Source variable,
    Source = "DWR_NCRO",
    # Convert secchi depth to cm
    Secchi = Secchi * 100,
    # Convert Date variable to date object
    Date = date(Date),
    # Standardize Microcystis visual index data
    Microcystis = as.numeric(if_else(Microcystis == "Absent", "1", Microcystis))
  ) %>%
  rename(Station = StationCode)

# Combine and finish preparing Microcystis visual index and water quality
  # measurement data
mc_vis_index_wq <- bind_rows(df_package_c, df_ncro_c) %>%
  mutate(
    # Add Year and Month variables
    Year = year(Date),
    Month = month(Date),
    # Convert Microcystis to integer
    Microcystis = as.integer(Microcystis)
  ) %>%
  # Remove records without latitude-longitude coordinates
  drop_na(Latitude, Longitude) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Add Regions from sf_hab_reg and remove all stations outside area of interest
  st_join(EDBdata:::sf_hab_reg, join = st_intersects) %>%
  filter(!is.na(Region)) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Remove three water temperature values that are obviously wrong
  mutate(Temperature = replace(Temperature, Temperature < 5, NA_real_)) %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Region,
    Year,
    Month,
    Date,
    Microcystis,
    Secchi,
    Temperature
  )


# 3. Save and Export Data -------------------------------------------------

# Save final Microcystis visual index and water quality data set as csv file for
  # easier diffing
mc_vis_index_wq %>% write_csv(here("data-raw/Final/mc_vis_index_wq.csv"))

# Save final Microcystis visual index and water quality data set as object in
  # the data package
usethis::use_data(mc_vis_index_wq, overwrite = TRUE)

