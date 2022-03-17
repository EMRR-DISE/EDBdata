# Code to prepare the continuous chlorophyll data for the Emergency Drought Barrier analysis:
# 1) `cont_chla_daily` - daily averages and medians of the continuous chlorophyll data from
  # 2020-2021 for the stations within the designated EDB regions

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(readxl)
library(dataRetrieval)
library(sf)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/cont_chla_daily.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# All 15-minute continuous chlorophyll fluorescence data collected by DWR was
  # either downloaded directly from the Water Data Library
  # (https://wdl.water.ca.gov/WaterDataLibrary/) or acquired from direct data
  # requests. Here are more specifics for each station:

  # BLP: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9502900&source=map
  # FAL: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9504400&source=map
  # FRK: Direct data request from DWR-DISE-CEMP. Data from the Water Quality Portal.
  # HLT: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9545800&source=map
  # ORI: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9534100&source=map
  # OSJ: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9510800&source=map
  # RVB: Direct data request from DWR-DISE-CEMP. Data from the Water Quality Portal.
  # SSI: Direct data request from DWR-DISE-CEMP. Data from the Water Quality Portal.
  # TWI: Direct data request from DWR-DISE-CEMP. Data from the Water Quality Portal.
  # WCI: https://wdl.water.ca.gov/WaterDataLibrary/StationDetails.aspx?Station=B9533800&source=map

# All of this 15-minute continuous chlorophyll data collected by DWR is stored
  # on the EDB Report SharePoint site because the file sizes are too large to
  # include in this data package. As a result, we need to define the file path for
  # where this continuous chlorophyll data is stored on the SharePoint site.

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

# Create a vector of all file paths for the continuous chlorophyll data stored
  # on the EDB Report SharePoint site
fp_chla_sp_edb <- sort(dir(
  edb_abs_sp_path("Water Quality/Cont_Chla_Data"),
  pattern = "\\.csv$",
  full.names = TRUE
))

# Remove FAL (2020 and 2021), HLT (2020), and OSJ (2020) since their formats are
  # so different and need to be imported separately
fp_chla_sp_edb2 <- str_subset(fp_chla_sp_edb, "FAL_|HLT_.+2020|OSJ_.+2020", negate = TRUE)
fp_chla_diff_fmt <- str_subset(fp_chla_sp_edb, "FAL_|HLT_.+2020|OSJ_.+2020")

# Function to extract the three letter station codes from their file paths
extr_sta_code <- function(fp_str) {
  str_extract(fp_str, "(?<=Cont_Chla_Data/)[:upper:]{3}")
}

# Import continuous chlorophyll data from fp_edb_sp_chla2 into a nested dataframe
ndf_chla_orig1 <-
  tibble(
    Station = map_chr(fp_chla_sp_edb2, extr_sta_code),
    df_data = map(fp_chla_sp_edb2, ~ read_csv(.x, col_types = cols(.default = "c")))
  )

# Import continuous chlorophyll data for FAL (2020 and 2021), HLT (2020), and OSJ (2020)
ndf_chla_diff_fmt <-
  tibble(
    Station = map_chr(fp_chla_diff_fmt, extr_sta_code),
    df_data = map2(
      fp_chla_diff_fmt,
      c(9, 3, 9, 9),
      ~ read_csv(
        .x,
        col_names = c("DateTime", "value", "Quality"),
        skip = .y,
        col_types = cols(.default = "c")
      )
    )
  )

# We are also using 15-minute continuous chlorophyll fluorescence data collected
  # by USGS at a few stations (MDM, SJJ). For this data, we will download and save
  # local copies of the data using the `dataRetrieval` package since some of the
  # data is provisional and may change.

# Set download to TRUE if need to download USGS continuous chlorophyll data
download <- FALSE

# Download USGS continuous chlorophyll data if necessary
if (download == TRUE) {
  # Create vectors for parameters, start and end dates
  start_date <- "2020-01-01"
  end_date <- "2021-12-31"
  params <- "32316"  # Chlorophyll concentration estimated from reference material (ug/L)

  # Download data for each station individually since doing it all at once doesn't work correctly
  MDM <- readNWISuv("11312676", params, start_date, end_date, tz = "Etc/GMT+8")
  SJJ <- readNWISuv("11337190", params, start_date, end_date, tz = "Etc/GMT+8")

  # Export raw data as .csv files for each site
  lst(MDM, SJJ) %>%
    map(as_tibble) %>%
    map(~ mutate(.x, dateTime = as.character(dateTime))) %>%
    iwalk(
      .f = ~ write_csv(
        .x,
        file = here("data-raw/Cont_chla_data/", paste0(.y, "_Chlor_2020-2021.csv")),
        na = ""
      )
    )
}

# Create a vector of file paths for the continuous chlorophyll data collected by USGS
fp_chla_usgs <- sort(dir(here("data-raw/Cont_chla_data"), pattern = "\\.csv$", full.names = TRUE))

# Import continuous chlorophyll data from USGS into a nested dataframe
ndf_chla_usgs <-
  tibble(
    Station = map_chr(fp_chla_usgs, ~ str_extract(.x, "(?<=Cont_chla_data/)[:upper:]{3}")),
    df_data = map(fp_chla_usgs, ~ read_csv(.x, col_types = cols(.default = "c")))
  )

# Combine all nested dataframes together
ndf_chla_orig2 <-
  bind_rows(ndf_chla_orig1, ndf_chla_diff_fmt, ndf_chla_usgs) %>%
  arrange(Station)

# Import coordinates for stations
df_coords_orig <- read_excel(here("data-raw/Cont_chla_data/Cont_chla_station_coord.xlsx"))


# 2. Clean and Integrate Data ---------------------------------------------

# Create a vector of keywords for the variable names to keep in the dataframes
vec_vars_keep <-
  c(
    "Date",
    "time",
    "value",
    "32316_00000",
    "Quality",
    "qaqc",
    "Flag"
  )

# Start with some general formatting and cleaning
df_chla_clean1 <- ndf_chla_orig2 %>%
  # Standardize variable names
  mutate(
    df_data = map(
      df_data,
      ~select(.x, contains(vec_vars_keep)) %>%
        rename(DateTime = contains(c("date", "time"), ignore.case = FALSE)) %>%
        rename(Chla = matches("value|32316_00000$")) %>%
        rename(Qual = matches("Qual|Flag|qaqc|_cd$"))
    )
  ) %>%
  # Unnest nested dataframes now that all variable names are standardized
  unnest(df_data) %>%
  # Convert variable types making them new variables to check for parsing errors
  mutate(
    DateTime2 = parse_date_time(DateTime, orders = c("Ymd T", "mdY T", "mdY R"), tz = "Etc/GMT+8"),
    Chla2 = as.numeric(Chla)
  )

# Check for parsing errors in DateTime and Chla
anyNA(df_chla_clean1$DateTime)
anyNA(df_chla_clean1$DateTime2)
df_chla_test1 <- df_chla_clean1 %>% filter(is.na(Chla))
df_chla_test2 <- df_chla_clean1 %>% filter(is.na(Chla2))
identical(df_chla_test1, df_chla_test2)
# No parsing errors identified

# Finish with general formatting and cleaning
df_chla_clean2 <- df_chla_clean1 %>%
  select(-c(DateTime, Chla)) %>%
  rename(
    DateTime = DateTime2,
    Chla = Chla2
  ) %>%
  # Add Year variable
  mutate(Year = year(DateTime)) %>%
  # Only include years 2020 and 2021
  filter(Year %in% 2020:2021) %>%
  # Remove NA values, values less than zero, and records with Qual code of "X" (Bad data)
  filter(!is.na(Chla), Chla >= 0) %>%
  filter(Qual != "X" | is.na(Qual))

# Run a few quality checks on the 15-minute data before calculating daily
  # averages and medians

# Look for duplicated time stamps
# df_chla_clean2 %>%
#   mutate(DateTime = round_date(DateTime, unit = "15 minute")) %>%
#   count(Station, DateTime) %>%
#   filter(n > 1)
# No duplicated time stamps present in data set

# Look at min and max values for each station
qc_min_max <- df_chla_clean2 %>%
  group_by(Station, Year) %>%
  summarize(
    min_value = min(Chla),
    max_value = max(Chla)
  ) %>%
  ungroup()

# View(qc_min_max)
# 2 stations have some values equal to zero
# 3 stations have some values greater than 100
# A few of the stations have noisy data at times
# I tried a few outlier screening tests, but didn't perform as well as I preferred -
  # may use daily medians to visualize trends in continuous chlorophyll data
  # instead of daily means


# 4. Aggregate Values -----------------------------------------------------

# Assign EDB regions to each station
df_region_assign <- df_coords_orig %>%
  select(
    Station = `CDEC code`,
    Latitude,
    Longitude
  ) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(st_make_valid(EDBdata:::sf_edb_reg), join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Assign "Outside" to stations without region assignments
  replace_na(list(Region = "Outside"))

# Calculate daily means and medians of continuous chlorophyll data for each station
cont_chla_daily <- df_chla_clean2 %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Station, Year, Date) %>%
  summarize(
    Chla_avg = mean(Chla),
    Chla_med = median(Chla)
  ) %>%
  ungroup() %>%
  # Add regions
  left_join(df_region_assign, by = "Station") %>%
  # Remove data for stations outside of the EDB regions keeping NA values in
    # Region to catch errors during testing
  filter(Region != "Outside" | is.na(Region)) %>%
  # Reorder columns
  select(
    Station,
    Region,
    Year,
    Date,
    Chla_avg,
    Chla_med
  )

# Save final data set containing continuous chlorophyll data for the EDB analysis as csv file
  # for easier diffing
cont_chla_daily %>% write_csv(here("data-raw/Final/cont_chla_daily.csv"))

# Save final data set containing continuous chlorophyll data for the EDB analysis as object
  # in the data package
usethis::use_data(cont_chla_daily, overwrite = TRUE)

