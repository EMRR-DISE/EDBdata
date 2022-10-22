# Code to prepare the continuous air temperature data for the Emergency Drought
  # Barrier (HABs/Weeds) analysis:
# `air_temp_daily_dd` - daily average air temperature and degree days above 19C
  # for the Delta region (average of 3 continuous air quality stations - HBP, MSD,
  # SJR) for 2015-2021

# Load packages
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(cder)
library(qs)
library(pollen)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/air_temp_daily_dd.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# Download the continuous air temperature data from CDEC using the `cder`
  # package. Save a local copy since the data is subject to change.

# Set download to TRUE if need to download and save continuous air temperature
  # data
download <- FALSE

# Download and save continuous air temperature data if necessary
if (download == TRUE) {
  # Define the site codes for the CDEC data download
  air_temp_sites <- c("RRI", "MSD", "SJR")

  # Download data
  df_air_temp_cdec <- cdec_query(
    stations = air_temp_sites,
    sensors = 4,
    durations = "E",
    start.date = ymd("2015-01-01"),
    end.date = ymd("2021-12-30")
  )

  # Save data as a compressed .qs file in the "data-raw/Cont_air_temp_data" folder
  df_air_temp_cdec %>% qsave(here("data-raw/Cont_air_temp_data/air_temp_delta_cdec.qs"))

  # Clean up
  rm(air_temp_sites, df_air_temp_cdec)
}

# Import continuous air temperature data
df_air_temp <- qread(here("data-raw/Cont_air_temp_data/air_temp_delta_cdec.qs"))


# 2. Clean Data ----------------------------------------------------------

# Prepare data for calculating daily averages and degree days
df_air_temp_c <- df_air_temp %>%
  transmute(
    Station = StationID,
    # Convert DateTime to PST so that it is consistent with the water
      # temperature data
    DateTime = with_tz(DateTime, tzone = "Etc/GMT+8"),
    Date = date(DateTime),
    Year = year(DateTime),
    # Convert air temperature to Celcius
    Value = (Value - 32) * (5/9)
  ) %>%
  # Remove records where air temperature is NA
  drop_na(Value)

# Remove duplicated records collected in winter-spring 2015 with time stamps
  # not collected at a 15 minute interval - These appear to be erroneous values
  # in the data set
df_air_temp_dups <- df_air_temp_c %>%
  mutate(DateTime_round = round_date(DateTime, unit = "15 minute")) %>%
  group_by(Station, DateTime_round) %>%
  mutate(DateTime_count = n()) %>%
  ungroup() %>%
  filter(DateTime_count > 1) %>%
  select(-c(DateTime_round, DateTime_count))

df_air_temp_dups_c <- df_air_temp_dups %>% filter(minute(DateTime) %in% c(0, 15, 30, 45))

df_air_temp_c2 <- df_air_temp_c %>%
  anti_join(df_air_temp_dups) %>%
  bind_rows(df_air_temp_dups_c)


# 3. Aggregate Values -----------------------------------------------------

# Calculate daily average air temperature and degree days of all stations
air_temp_daily_dd <- df_air_temp_c2 %>%
  # First, calculate daily minimums, maximums, and averages for each station
  group_by(Station, Year, Date) %>%
  summarize(
    AirTemp_Min = min(Value),
    AirTemp_Max = max(Value),
    AirTemp_Avg = mean(Value),
    .groups = "drop"
  ) %>%
  # Next, calculate the averages of the minimums, maximums, and averages across
    # all stations
  group_by(Year, Date) %>%
  summarize(across(starts_with("AirTemp"), mean), .groups = "drop") %>%
  # Calculate degree days above 19C for each year using the averages across all
    # stations
  group_by(Year) %>%
  mutate(
    AirTemp_DD = gdd(
      tmax = AirTemp_Max,
      tmin = AirTemp_Min,
      tbase = 19,
      tbase_max = 35
    )
  ) %>%
  ungroup() %>%
  select(
    Year,
    Date,
    AirTemp_Avg,
    AirTemp_DD
  ) %>%
  arrange(Date)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing continuous air temperature data as csv file
  # for easier diffing
air_temp_daily_dd %>% write_csv(here("data-raw/Final/air_temp_daily_dd.csv"))

# Save final data set containing continuous air temperature data as object in
  # the data package
usethis::use_data(air_temp_daily_dd, overwrite = TRUE)

