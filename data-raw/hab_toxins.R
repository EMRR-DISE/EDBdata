# Code to prepare combined HAB toxin data set for the Emergency Drought Barrier
  # (HABs/Weeds) analysis:
# 1) `hab_toxins` - Cyanotoxin concentrations in whole-water grab samples
  # collected at various locations in the upper San Francisco Estuary (Delta) in
  # 2021. Used in the Spring-Summer version of the 2022 HABs/Weeds report.

# Load packages
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/hab_toxins.R")


# 1. Import Data ----------------------------------------------------------

# Create a vector of all file paths of the HAB toxin data found in the data-raw folder
fp_hab_toxins <- dir(here("data-raw/HAB_toxin_data"), full.names = TRUE)

# DWR's State Water Project samples
df_swp <- read_excel(path = str_subset(fp_hab_toxins, "DWR_DFD_Cyanotoxin"))

# USGS's SPATT study, water samples only
df_usgs_spatt <- read_csv(file = str_subset(fp_hab_toxins, "USGS_DWR_fixed"))

# Water board samples from Franks Tract
df_wb_franks <- tibble(
  Station = c("FRK", "FRK", "FRK", "MI"),
  Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
  Date = ymd("2021-07-02", "2021-08-06", "2021-08-06", "2021-07-02"),
  Result = c(0, 0.63, 0, 0.6)
)

# Nautilus data - from the State Board's database, provided by Karen Atkinson
df_nautilus <- read_excel(path = str_subset(fp_hab_toxins, "HAB_Monitoring"), sheet = "Nautalis")

# East Bay Parks data - from the State Board's database, provided by Karen Atkinson
df_ebp <- read_excel(path = str_subset(fp_hab_toxins, "HAB_Monitoring"), sheet = "East Bay")

# Preece/Otten data
df_pre_ott <- read_excel(path = str_subset(fp_hab_toxins, "Prop 1 data"), sheet = "preecedata")

# Station information
df_stations <- read_excel(path = str_subset(fp_hab_toxins, "Prop 1 data"), sheet = "stations")


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
  ungroup()

# USGS's SPATT study
df_usgs_spatt_c <- df_usgs_spatt %>%
  # Only include toxins that were detected in the samples
  group_by(toxin) %>%
  mutate(resultSum = sum(resultNum, na.rm = T)) %>%
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
  )

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
  # Convert Result to numeric substituting non-detect values with zero
  mutate(Result = as.numeric(if_else(Result == "ND", "0", Result))) %>%
  filter(!is.na(Result))

# East Bay Parks data
df_ebp_c <- df_ebp %>%
  # Only include sites within Big Break Regional Shoreline
  filter(Water_Body == "Big Break Regional Shoreline") %>%
  # Rename and select columns to keep
  select(
    Result = `Microcystin (µg/L)\r\nELISA_Method`,
    Date = Sample_Date
  ) %>%
  mutate(
    # Convert Result to numeric substituting non-detect values with zero and
      # >50 values with 50
    Result = as.numeric(
      case_when(
        Result == "ND" ~ "0",
        Result == ">50" ~ "50",
        TRUE ~ Result
      )
    ),
    # Add variables for Analyte and Station
    Analyte = "Microcystins",
    Station = "BigBreak"
  ) %>%
  filter(!is.na(Result))

# Preece/Otten data
df_pre_ott_c <- df_pre_ott %>%
  mutate(
    # Add variables for Analyte
    Analyte = "Microcystins",
    # Convert Result to numeric substituting non-detect values with zero
    Result = as.numeric(if_else(`Final_conc (ug/L)` == "ND", "0", `Final_conc (ug/L)`))
  ) %>%
  # Rename a few variables
  rename(
    Station = Sample_ID,
    Date = Collection_Date
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
  # Only include 2021 data
  filter(Year == 2021) %>%
  # Join station information
  left_join(df_stations, by = "Station") %>%
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
    Result
  )


# 3. Save and Export Data -------------------------------------------------

# Save final HAB toxin data set as csv file for easier diffing
hab_toxins %>% write_csv(here("data-raw/Final/hab_toxins.csv"))

# Save final HAB toxin data set as object in the data package
usethis::use_data(hab_toxins, overwrite = TRUE)

