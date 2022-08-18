# Code to prepare HAB incident data set for the Emergency Drought Barrier
  # (HABs/Weeds) analysis:
# `hab_incidents` - CyanoHAB incident reports in the upper San Francisco Estuary
  # (Delta) in 2021. This is data from the State Water Board's HAB Incident Report
  # Map combined with advisory level exceedences for Microcystins from the
  # `hab_toxins` data set. Used in the Spring-Summer version of the 2022
  # HABs/Weeds report.

# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(readxl)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/hab_incidents.R")

# Import HAB incident data from the State Water Board
df_incid <- read_excel("data-raw/HAB_incident_data/Legal Delta HAB incidents 2016-2021.xlsx")

# Prepare the HAB incident data to be combined with the warning levels from the
  # hab_toxins data set
df_incid_c <- df_incid %>%
  transmute(
    Date = date(`Incident date`),
    Latitude,
    Longitude,
    Advisory = `Initial Advisory Level`
  ) %>%
  filter(
    Advisory != "No Advisory",
    year(Date) == 2021
  )

# Prepare warning levels from the hab_toxins data set
df_tox_levels <- EDBdata::hab_toxins %>%
  # Only include Microcystins in 2021
  filter(
    Analyte == "Microcystins",
    Year == 2021
  ) %>%
  # Advisory levels from the CCHAB trigger levels for Microcystins
  mutate(
    Advisory = case_when(
      Result > 0.8 & Result < 6 ~ "Caution",
      Result >= 6 & Result < 20 ~ "Warning",
      Result >= 20 ~ "Danger"
    ),
    Advisory = factor(Advisory, levels = c("Caution", "Warning", "Danger"), ordered = TRUE)
  ) %>%
  filter(!is.na(Advisory)) %>%
  group_by(Station, Advisory) %>%
  # Find minimum date of occurrence of each Advisory level at each Station
  filter(Date == min(Date)) %>%
  ungroup(Advisory) %>%
  # Find maximum Advisory level in 2021 at each Station
  filter(Advisory == max(Advisory)) %>%
  ungroup() %>%
  transmute(
    Date,
    Latitude,
    Longitude,
    Advisory = as.character(Advisory)
  )

# Combine HAB incident data with the warning levels from the hab_toxins data set
hab_incidents <- bind_rows(df_incid_c, df_tox_levels) %>% arrange(Date)

# Save final HAB incident data set as csv file for easier diffing
hab_incidents %>% write_csv(here("data-raw/Final/hab_incidents.csv"))

# Save final HAB incident data set as object in the data package
usethis::use_data(hab_incidents, overwrite = TRUE)

