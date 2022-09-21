# Code to prepare the continuous water quality data for the Emergency Drought
  # Barrier (HABs/Weeds) analysis:
# `cont_wq_daily_max_frk` - daily maximums of the continuous water quality data
  # (Dissolved Oxygen percent saturation, pH, and Chlorophyll Fluorescence) from
  # 2015-2021 for the Franks Tract station (FRK)

# Load packages
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(rMR)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/cont_wq_daily_max_frk.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# The 15-minute continuous water quality data for the FRK station was collected
  # by DWR-DISE-CEMP. Data is stored internally on the Water Quality Portal and
  # was acquired through a direct data request. The raw continuous water quality
  # data used in this package is stored on the EDB Report SharePoint site because
  # the file sizes are too large to include in this data package. As a result, we
  # need to define the file path for where this continuous data is stored on the
  # SharePoint site.

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

# Create a vector of all file paths for the continuous water quality data for
  # the FRK station stored on the EDB Report SharePoint site
fp_wq_frk <- dir(
  edb_abs_sp_path("Water Quality/Continuous WQ - March 2022/EMP"),
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Import continuous water quality data
df_wq_frk <- map_dfr(fp_wq_frk, ~ read_csv(.x, col_select = -c(1, 6)))


# 2. Clean Data ---------------------------------------------

df_wq_frk_c <- df_wq_frk %>%
  # Remove NA values and X-flagged data
  filter(qaqc_flag_id != "X") %>%
  drop_na(value) %>%
  # Remove Turbidity data
  filter(parameter != "Turbidity") %>%
  transmute(
    # Rename parameters
    parameter = case_when(
      parameter == "DissolvedOxygen" ~ "DO",
      parameter == "Fluorescence" ~ "Chla",
      parameter == "SpC" ~ "SpCnd",
      parameter == "WaterTemperature" ~ "WaterTemp",
      TRUE ~ parameter
    ),
    # Define timezone as PST
    Datetime = force_tz(time, tzone = "Etc/GMT+8"),
    value
  ) %>%
  # Remove duplicates
  distinct() %>%
  # Pivot data wider
  pivot_wider(names_from = parameter, values_from = value) %>%
  # Calculate DO percent saturation using USGS method
  mutate(
    DO_Sat = DO.saturation(
      DO.mgl = DO,
      temp.C = WaterTemp,
      elevation.m = 0,
      salinity = SpCnd,
      salinity.units = "uS"
    ),
    DO_PerSat = DO_Sat * 100
  ) %>%
  # Only keep DO percent saturation, pH, and Chla
  select(
    Datetime,
    DO_PerSat,
    pH,
    Chla
  ) %>%
  # Remove records where all WQ parameters are NA
  filter(!if_all(where(is.numeric), is.na))


# 3. Aggregate Values -----------------------------------------------------

# Calculate daily maximums
cont_wq_daily_max_frk <- df_wq_frk_c %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(Date = date(Datetime)) %>%
  drop_na(Value) %>%
  group_by(Date, Parameter) %>%
  summarize(Max = max(Value)) %>%
  ungroup() %>%
  pivot_wider(names_from = Parameter, values_from = Max) %>%
  select(
    Date,
    DO_PerSat,
    pH,
    Chla
  ) %>%
  arrange(Date)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing continuous water quality data as csv file for
  # easier diffing
cont_wq_daily_max_frk %>% write_csv(here("data-raw/Final/cont_wq_daily_max_frk.csv"))

# Save final data set containing continuous water quality data as object in the
  # data package
usethis::use_data(cont_wq_daily_max_frk, overwrite = TRUE)

