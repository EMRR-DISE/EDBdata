# Code to prepare combined HAB toxin data set for the Emergency Drought Barrier
  # (HABs/Weeds) analysis:
# `hab_toxins` - Cyanotoxin concentrations in whole-water grab samples collected
  # at various locations in the upper San Francisco Estuary (Delta). The majority
  # of the available cyanotoxin data was collected in 2021, so this data set only
  # includes data from 2021 for all stations except for Big Break Regional
  # Shoreline. Big Break has a longer monitoring history, so all data collected at
  # this location from 2015–2021 are included in this data set. Used in the
  # Spring-Summer version of the 2022 HABs/Weeds report.

# Load packages
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(purrr)
library(lubridate)
library(readxl)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/hab_toxins.R")


# 1. Import Data ----------------------------------------------------------

# Define file path with all HAB toxin data found in the data-raw folder
fp_hab_toxins <- here("data-raw/HAB_toxin_data")

# DWR's State Water Project samples
df_swp <- read_excel(path = file.path(fp_hab_toxins, "DWR_DFD_Cyanotoxin_results_2021 - JG.xlsx"))

# USGS's SPATT study, water samples only
df_usgs_spatt <- read_csv(file = file.path(fp_hab_toxins, "USGS_DWR_fixed_station_WW_cyanotoxins_Rexport.csv"))

# Water board samples from Franks Tract
df_wb_franks <- tibble(
  Station = c("FRK", "FRK", "FRK", "MI"),
  Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
  Date = ymd("2021-07-02", "2021-08-06", "2021-08-06", "2021-07-02"),
  Result = c(0, 0.63, 0, 0.6),
  Result_Sign = c("ND", "=", "ND", "=")
)

# Nautilus data - from the State Board's database, provided by Karen Atkinson
df_nautilus <- read_excel(path = file.path(fp_hab_toxins, "HAB_Monitoring.xlsx"), sheet = "Nautalis")

# Big Break data - from East Bay Regional Parks
lst_ebp <- pmap(
  list(
    rep(file.path(fp_hab_toxins, "2015-22 BB HAB Monitoring.xlsx"), 7),
    as.character(2015:2021),
    c(2, 2, 2, 1, 1, 3, 1)
  ),
  ~ read_excel(path = ..1, sheet = ..2, skip = ..3)
)

# Preece/Otten data
df_pre_ott <- read_excel(path = file.path(fp_hab_toxins, "Prop 1 data_4.1.22.xlsx"), sheet = "preecedata")

# Station information
df_stations <- read_excel(path = file.path(fp_hab_toxins, "Prop 1 data_4.1.22.xlsx"), sheet = "stations")


# 2. Clean and Combine Data -----------------------------------------------

# DWR's State Water Project samples
df_swp_c <- df_swp %>%
  # Filter for stations and analytes of interest
  filter(
    Station %in% c("Banks PP", "Clifton Court Forebay"),
    Analyte %in% c("ANTX-A", "MC")
  ) %>%
  mutate(
    # Convert Result to numeric substituting non-detect values with zero
    Result = as.numeric(if_else(str_detect(`Result (ng/mL)`, "ND"), "0", `Result (ng/mL)`)),
    # Shorten Station names
    Station = case_when(
      Station == "Banks PP" ~ "BPP",
      Station == "Clifton Court Forebay" ~ "CCF",
      TRUE ~ Station
    )
  ) %>%
  # Average replicated samples for each station, analyte, and date
  group_by(Station, Analyte, Date) %>%
  summarize(Result = mean(Result)) %>%
  ungroup() %>%
  # Add Result_Sign variable to indicate non-detect values
  mutate(Result_Sign = if_else(Result == 0, "ND", "="))

# USGS's SPATT study
df_usgs_spatt_c <- df_usgs_spatt %>%
  # Only include toxins that were detected in the samples
  group_by(toxin) %>%
  mutate(resultSum = sum(resultNum, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    resultSum != 0,
    !is.na(resultNum)
  ) %>%
  # Sum toxin values by class
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, class) %>%
  summarize(Result = sum(resultNum)) %>%
  ungroup() %>%
  # Convert Date and rename some variables
  mutate(Date = mdy(Date)) %>%
  rename(
    Analyte = class,
    Station = Site
  ) %>%
  # Add Result_Sign variable to indicate non-detect values
  mutate(Result_Sign = if_else(Result == 0, "ND", "="))

# Nautilus data
df_nautilus_c <- df_nautilus %>%
  # Rename and select columns to keep
  select(
    Station = Station_Code,
    Anatoxins = `Anatonxin-a_ELISA_Method (ug/L)`,
    Cylindrospermopsins = `Cylindrospermopsin_ELISA_Method (ug/L)`,
    Microcystins = `Microcystin_ELISA_Method (ug/L)`,
    Saxitoxins = `Saxitoxin_ELISA_Method (ug/L)`,
    Date = Sample_Date
  ) %>%
  # Restructure data to long format
  pivot_longer(
    cols = c(Microcystins, Anatoxins, Cylindrospermopsins, Saxitoxins),
    names_to = "Analyte",
    values_to = "Result"
  ) %>%
  mutate(
    # Add Result_Sign variable to indicate non-detect values
    Result_Sign = if_else(Result == "ND", "ND", "="),
    # Convert Result to numeric substituting non-detect values with zero
    Result = as.numeric(if_else(Result == "ND", "0", Result))
  ) %>%
  filter(!is.na(Result))

# Big Break data
df_ebp_c <- lst_ebp %>%
  map(
    ~ select(.x, Date, contains(c("Analyte", "Result")), starts_with("Test")) %>%
      rename(Analyte = contains(c("Analyte", "Test")), Result = contains("Result")) %>%
      drop_na(Result)
  ) %>%
  map_at(1, ~ mutate(.x, Result = as.character(Result))) %>%
  bind_rows() %>%
  mutate(
    Date = as_date(as.numeric(Date), origin = "1899-12-30"),
    Result = case_when(
      Analyte == "Ana/Cyl/Mic" ~ str_extract(Result, "(?<=/ND/).+"),
      Analyte == "Mic, Ana" ~ str_extract(Result, ".+(?=,)"),
      TRUE ~ Result
    ),
    Result = str_remove(Result, "[:space:]ppb|ppb"),
    Analyte = if_else(str_detect(Analyte, "^Mic|Mic$"), "Microcystins", Analyte)
  ) %>%
  filter(Analyte == "Microcystins") %>%
  mutate(
    # Add Result_Sign variable to indicate non-detect and above detection values
    Result_Sign = case_when(
      Result %in% c("0", "ND") ~ "ND",
      str_detect(Result, "^>") ~ ">",
      TRUE ~ "="
    ),
    # Convert Result to numeric substituting non-detect values with zero and
      # > values with their upper detection limit
    Result = as.numeric(
      case_when(
        Result == "ND" ~ "0",
        Result == "2.5-5" ~ "5",
        Result == "between 5 and 10" ~ "7.5",
        str_detect(Result, "^>") ~ str_trim(str_remove(Result, "^>")),
        TRUE ~ Result
      )
    ),
    # Add variable for Station
    Station = "BigBreak"
  )

# Preece/Otten data
df_pre_ott_c <- df_pre_ott %>%
  mutate(
    # Add variables for Analyte
    Analyte = "Microcystins",
    # Add Result_Sign variable to indicate non-detect values
    Result_Sign = if_else(`Final_conc (ug/L)` == "ND", "ND", "="),
    # Convert Result to numeric substituting non-detect values with zero
    Result = as.numeric(if_else(`Final_conc (ug/L)` == "ND", "0", `Final_conc (ug/L)`))
  ) %>%
  # Rename a few variables
  rename(
    Station = Sample_ID,
    Date = Collection_Date
  )

# Station Information
df_stations_c <- df_stations %>%
  # Correct spelling mistake for Nautilus
  mutate(Study = if_else(Study == "Nautalis", "Nautilus", Study)) %>%
  # Correct switched column names for station coordinates
  rename(
    Longitude = Latitude,
    Latitude = Longitude
  )

# Combine and finish preparing toxin data
hab_toxins <-
  bind_rows(
    df_usgs_spatt_c,
    df_swp_c,
    df_wb_franks,
    df_pre_ott_c,
    df_nautilus_c,
    df_ebp_c
  ) %>%
  # Standardize Analyte names
  mutate(
    Analyte = case_when(
      Analyte == "MC" ~ "Microcystins",
      Analyte == "ANTX-A" ~ "Anatoxins",
      TRUE ~ Analyte
    ),
    # Convert Date variable to Date class and add Year and Month variables
    Date = as_date(Date),
    Year = year(Date),
    Month = month(Date)
  ) %>%
  # Only include 2021 data, except for Big Break which we'll include 2015-2021
  filter(
    (Station != "BigBreak" & Year == 2021) | (Station == "BigBreak" & Year %in% 2015:2021)
  ) %>%
  # Join station information
  left_join(df_stations_c, by = "Station") %>%
  # Select variables to keep
  select(
    Source = Study,
    Station,
    Latitude,
    Longitude,
    Region,
    Year,
    Month,
    Date,
    Analyte,
    Result,
    Result_Sign
  )


# 3. Save and Export Data -------------------------------------------------

# Save final HAB toxin data set as csv file for easier diffing
hab_toxins %>% write_csv(here("data-raw/Final/hab_toxins.csv"))

# Save final HAB toxin data set as object in the data package
usethis::use_data(hab_toxins, overwrite = TRUE)

