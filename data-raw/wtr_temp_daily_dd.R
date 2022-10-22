# Code to prepare the continuous water temperature data for the Emergency
  # Drought Barrier (HABs/Weeds) analysis:
# `wtr_temp_daily_dd` - daily average water temperature and degree days above
  # 19C for the Central Delta (average of 6 continuous water quality stations -
  # FAL, HLT, HOL, MDM, ORQ, OSJ) for 2015-2021

# Load packages
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(pollen)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/wtr_temp_daily_dd.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# All 15-minute continuous water temperature data was collected by DWR or USGS.
  # Data from DWR was either downloaded directly from the Water Data Library
  # (https://wdl.water.ca.gov/WaterDataLibrary/) or acquired from direct data
  # requests. USGS operates the MDM station, and its data was downloaded from the
  # NWIS website. Here are more specifics for each station:

# FAL: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9504400&source=map
# HLT: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9545800&source=map
# HOL: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9512000&source=map
# MDM: https://waterdata.usgs.gov/nwis/uv?site_no=11312676
# ORQ: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9520000&source=map
# OSJ: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9510800&source=map

# Create a vector of all file paths for zip files containing the 15-minute
  # continuous water temperature data collected by DWR and USGS
fp_wt_raw <- dir(here("data-raw/Cont_WQ_data"), pattern = "\\.zip$", full.names = TRUE)

# Unzip folders containing 15-minute continuous water temperature data to a
  # temporary directory
walk(fp_wt_raw, ~ unzip(zipfile = .x, exdir = tempdir()))

# Create vectors of file paths for the continuous water temperature data
# DWR-NCRO data:
fp_wt_ncro <- dir(tempdir(), pattern = "^HYCSV.+\\.csv$", full.names = TRUE)

# USGS data (just MDM):
fp_wt_usgs <- dir(tempdir(), pattern = "^MDM.+\\.csv$", full.names = TRUE)

# Import continuous water temperature data
ndf_wt_ncro <- tibble(
  Station = str_extract(fp_wt_ncro, "(?<=HYCSV_)[:upper:]{3}"),
  df_data = map(fp_wt_ncro, ~ read_csv(.x, skip = 1, col_types = cols(.default = "c")))
)

df_wt_usgs <- read_csv(fp_wt_usgs)


# 2. Clean and Integrate Data ---------------------------------------------

# DWR-NCRO data:
df_wt_ncro_c <- ndf_wt_ncro %>%
  # Remove the first row of data
  mutate(df_data = map(df_data, ~ slice(.x, -1))) %>%
  # Bind all data together
  unnest(df_data) %>%
  # Select and rename remaining columns, drop all columns containing Qual
    # codes (start with "...")
  transmute(
    Station,
    # NCRO collects Datetime as PST
    Datetime = mdy_hm(and, tz = "Etc/GMT+8"),
    WaterTemp = as.numeric(`450`)
  )

# USGS data (just MDM):
df_wt_usgs_c <- df_wt_usgs %>%
  transmute(
    Station = "MDM",
    # Assuming that USGS collects Datetime as PST
    Datetime = force_tz(DateTime, tzone = "Etc/GMT+8"),
    WaterTemp = MDM.Temp
  )

# Combine continuous water temperature data together and prepare for calculating
  # daily averages and degree days
df_wt_all <-
  bind_rows(df_wt_ncro_c, df_wt_usgs_c) %>%
  # Only include years 2015-2021
  filter(year(Datetime) %in% 2015:2021) %>%
  # Remove records where water temperature is NA
  drop_na(WaterTemp) %>%
  # Remove two duplicated timestamps - keep the first one
  arrange(Station, Datetime) %>%
  mutate(Datetime = round_date(Datetime, unit = "15 minute")) %>%
  group_by(Station, Datetime) %>%
  mutate(RowNum = row_number()) %>%
  ungroup() %>%
  filter(RowNum == 1) %>%
  select(-RowNum)


# 3. Aggregate Values -----------------------------------------------------

# Calculate daily average water temperature and degree days of all stations
wtr_temp_daily_dd <- df_wt_all %>%
  # First, calculate daily minimums, maximums, and averages for each station
  mutate(
    Date = date(Datetime),
    Year = year(Datetime)
  ) %>%
  group_by(Station, Year, Date) %>%
  summarize(
    WaterTemp_Min = min(WaterTemp),
    WaterTemp_Max = max(WaterTemp),
    WaterTemp_Avg = mean(WaterTemp),
    .groups = "drop"
  ) %>%
  # Next, calculate the averages of the minimums, maximums, and averages across
    # all stations
  group_by(Year, Date) %>%
  summarize(across(starts_with("WaterTemp"), mean), .groups = "drop") %>%
  # Calculate degree days above 19C for each year using the averages across all
    # stations
  group_by(Year) %>%
  mutate(
    WaterTemp_DD = gdd(
      tmax = WaterTemp_Max,
      tmin = WaterTemp_Min,
      tbase = 19,
      tbase_max = 35
    )
  ) %>%
  ungroup() %>%
  select(
    Year,
    Date,
    WaterTemp_Avg,
    WaterTemp_DD
  ) %>%
  arrange(Date)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing continuous water temperature data as csv file
  # for easier diffing
wtr_temp_daily_dd %>% write_csv(here("data-raw/Final/wtr_temp_daily_dd.csv"))

# Save final data set containing continuous water temperature data as object in
  # the data package
usethis::use_data(wtr_temp_daily_dd, overwrite = TRUE)

