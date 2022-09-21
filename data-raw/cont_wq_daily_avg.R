# Code to prepare the continuous water quality data for the Emergency Drought
  # Barrier (HABs/Weeds) analysis:
# `cont_wq_daily_avg` - daily means of the continuous water quality data
  # (Dissolved Oxygen, pH, and Chlorophyll Fluorescence) from 2015-2021 for the
  # stations near Franks Tract

# Load packages
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/cont_wq_daily_avg.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# All 15-minute continuous water quality data was collected by DWR or USGS. Data
  # from DWR was either downloaded directly from the Water Data Library
  # (https://wdl.water.ca.gov/WaterDataLibrary/) or acquired from direct data
  # requests. USGS operates the MDM station, and its data was downloaded from the
  # NWIS website. Here are more specifics for each station:

# FAL: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9504400&source=map
# FRK: Direct data request from DWR-DISE-CEMP. Data from the Water Quality Portal.
# HLT: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9545800&source=map
# HOL: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9512000&source=map
# MDM: https://waterdata.usgs.gov/nwis/uv?site_no=11312676
# ORQ: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9520000&source=map
# OSJ: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9510800&source=map

# All of this 15-minute continuous water quality data collected by DWR and USGS
  # is stored on the EDB Report SharePoint site because the file sizes are too
  # large to include in this data package. As a result, we need to define the file
  # path for where this continuous data is stored on the SharePoint site.

# Function to define file path to the EDB Report SharePoint site. Uses the
  # Windows user profile for the current DWR login.
edb_abs_sp_path <- function(fp_rel = NULL) {
  fp_edb <- "California Department of Water Resources/DWR-EXT-2021 Emer Drought BarrierReport - General"

  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb, fp_rel))
  }

  return(fp_abs)
}

# DWR-EMP data (just FRK):
# Create a vector of all file paths for the continuous water quality data stored
  # on the EDB Report SharePoint site
fp_wq_emp <- dir(
  edb_abs_sp_path("Water Quality/Continuous WQ - March 2022/EMP"),
  pattern = "(DissolvedOxygen|Fluorescence|pH).csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Import continuous water quality data
df_wq_emp <- map_dfr(fp_wq_emp, ~ read_csv(.x, col_select = -c(1, 6)))

# DWR-NCRO data:
# Create a vector of all file paths for the continuous water quality data stored
  # on the EDB Report SharePoint site
fp_wq_ncro <- dir(
  edb_abs_sp_path("Water Quality/Continuous WQ - March 2022/NCRO"),
  pattern = "^HYCSV.+\\.csv$",
  full.names = TRUE
)

# Import continuous water quality data
ndf_wq_ncro <- tibble(
  Station = map_chr(fp_wq_ncro, ~ str_extract(.x, "(?<=HYCSV_)[:upper:]{3}")),
  df_data = map(fp_wq_ncro, ~ read_csv(.x, skip = 1, col_types = cols(.default = "c")))
)

# USGS data (just MDM):
# Create a vector of the file path for the continuous water quality data stored
  # on the EDB Report SharePoint site
fp_wq_usgs <- dir(
  edb_abs_sp_path("Water Quality/Continuous WQ - March 2022/NCRO"),
  pattern = "^MDM.+\\.csv$",
  full.names = TRUE
)

# Import continuous water quality data
df_wq_usgs <- read_csv(fp_wq_usgs)


# 2. Clean and Integrate Data ---------------------------------------------

# DWR-EMP data (just FRK):
df_wq_emp_c <- df_wq_emp %>%
  # Remove X-flagged data
  filter(qaqc_flag_id != "X") %>%
  transmute(
    Source = "DWR_CEMP",
    Station = station,
    # Rename parameters
    parameter = case_when(
      parameter == "DissolvedOxygen" ~ "DO",
      parameter == "Fluorescence" ~ "Chla",
      TRUE ~ parameter
    ),
    # Define timezone as PST
    Datetime = force_tz(time, tzone = "Etc/GMT+8"),
    value
  ) %>%
  # Remove duplicates
  distinct() %>%
  # Pivot data wider
  pivot_wider(names_from = parameter, values_from = value)

# DWR-NCRO data:
df_wq_ncro_c <- ndf_wq_ncro %>%
  # Remove the first row of data
  mutate(df_data = map(df_data, ~ slice(.x, -1))) %>%
  # Bind all data together
  unnest(df_data) %>%
  # Select and rename remaining columns, drop all columns containing Qual
    # codes (start with "...")
  transmute(
    Source = "DWR_NCRO",
    Station,
    Datetime = mdy_hm(and, tz = "Etc/GMT+8"),
    DO = `2351`,
    Chla = `7004`
  ) %>%
  # Convert all columns for WQ parameters to numeric
  mutate(across(c(DO, Chla), as.numeric))

# USGS data (just MDM):
df_wq_usgs_c <- df_wq_usgs %>%
  transmute(
    Source = "USGS",
    Station = "MDM",
    Datetime = force_tz(DateTime, tzone = "Etc/GMT+8"),
    Chla = MDM.Chlor
  )

# Combine continuous WQ together and prepare data for calculating daily averages
  # and maximums
df_wq_all <-
  bind_rows(df_wq_emp_c, df_wq_ncro_c, df_wq_usgs_c) %>%
  # Only include years 2015 and 2021
  filter(year(Datetime) %in% 2015:2021) %>%
  # Remove records where all WQ parameters are NA
  filter(!if_all(where(is.numeric), is.na)) %>%
  # Remove two duplicated timestamps - keep the first one
  arrange(Station, Datetime) %>%
  mutate(Datetime = round_date(Datetime, unit = "15 minute")) %>%
  group_by(Station, Datetime) %>%
  mutate(RowNum = row_number()) %>%
  ungroup() %>%
  filter(RowNum == 1) %>%
  select(-RowNum) %>%
  # Remove negative Chla values (n = 26)
  mutate(Chla = if_else(Chla < 0, NA_real_, Chla))


# 3. Aggregate Values -----------------------------------------------------

# Calculate daily averages
cont_wq_daily_avg <- df_wq_all %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(Date = date(Datetime)) %>%
  drop_na(Value) %>%
  group_by(Source, Station, Date, Parameter) %>%
  summarize(Avg = mean(Value)) %>%
  ungroup() %>%
  pivot_wider(names_from = Parameter, values_from = Avg) %>%
  select(
    Source,
    Station,
    Date,
    DO,
    pH,
    Chla
  ) %>%
  arrange(Station, Date)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing continuous water quality data as csv file for
  # easier diffing
cont_wq_daily_avg %>% write_csv(here("data-raw/Final/cont_wq_daily_avg.csv"))

# Save final data set containing continuous water quality data as object in the
  # data package
usethis::use_data(cont_wq_daily_avg, overwrite = TRUE)

