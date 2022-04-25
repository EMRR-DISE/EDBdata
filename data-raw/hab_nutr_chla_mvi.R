# Code to prepare combined data set of discrete nutrient and chlorophyll-a
# concentrations and Microcystis visual index values for the Emergency Drought
# Barrier (HABs/Weeds) analysis:
# 1) `hab_nutr_chla_mvi` - discrete nutrient (DissAmmonia, DissNitrateNitrite,
  # and DissOrthophos) and chlorophyll-a concentrations and Microcystis visual
  # index values for 2014-2021 from the DWR-EMP, DWR-NCRO, USGS-SFBP, and
  # USGS-CAWSC surveys. Used in the Spring-Summer version of the 2022 HABs/Weeds
  # report.
# 2) `vims_nutr_chla` - discrete nutrient (DissAmmonia, DissNitrateNitrite, and
  # DissOrthophos) and chlorophyll-a concentrations for 2014-2021 from the
  # DWR-EMP, DWR-NCRO, USGS-SFBP, and USGS-CAWSC surveys. Provided as a data
  # request to VIMS to assist with their modeling effort. This data will be saved
  # as a .csv file in the data package but not exported as an .rda data object for
  # now.

# Load packages
library(dplyr)
# Make sure we are using `discretewq` version 2.3.1
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "v2.3.1")
library(discretewq)
library(dataRetrieval)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(hms)
library(readxl)
library(sf)
library(rlang)
library(glue)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/hab_nutr_chla_mvi.R")


# 1. Import Data ----------------------------------------------------------

# Import discrete nutrient, chlorophyll-a, and Microcystis visual index data
  # from the discretewq package (v2.3.1)
# Select EMP and USGS_SFBS since these are the only surveys besides
  # USGS_CAWSC that have collected discrete nutrient and chlorophyll-a data
# The USGS_CAWSC survey also has collected discrete nutrient and chlorophyll-a
  # data; however, we will use data directly imported with the dataRetrieval R
  # package since the discretewq package does not provide preliminary data and RL
  # values for the nutrients
df_package <-
  wq(
    Sources = c("EMP", "USGS_SFBS"),
    Start_year = 2014
  )

# Download and save local copies of the USGS_CAWSC discrete nutrient and
  # chlorophyll-a data using the `dataRetrieval` package since some of the data is
  # provisional and may change

# Set download to TRUE if need to download and save USGS_CAWSC discrete nutrient
  # and chlorophyll-a data
download <- FALSE

# Download and save USGS_CAWSC data if necessary
if (download == TRUE) {
  # Define the parameter codes and site numbers for the USGS data download
  # 00608 = Ammonium + Ammonia
  # 00631 = Nitrate + Nitrite
  # 00671 = Ortho-Phosphate
  # 70953 = Chlorophyll-a
  params <- c("00608", "00631", "00671", "70953")
  site_numb <-
    c(
      "USGS-11303500",
      "USGS-11304810",
      "USGS-11311300",
      "USGS-11312672",
      "USGS-11312676",
      "USGS-11312685",
      "USGS-11313315",
      "USGS-11313405",
      "USGS-11313431",
      "USGS-11313434",
      "USGS-11313440",
      "USGS-11313452",
      "USGS-11313460",
      "USGS-11336680",
      "USGS-11336685",
      "USGS-11336790",
      "USGS-11336930",
      "USGS-11337080",
      "USGS-11337190",
      "USGS-11447650",
      "USGS-11447830",
      "USGS-11447890",
      "USGS-11447903",
      "USGS-11455095",
      "USGS-11455136",
      "USGS-11455139",
      "USGS-11455140",
      "USGS-11455142",
      "USGS-11455143",
      "USGS-11455146",
      "USGS-11455166",
      "USGS-11455167",
      "USGS-11455276",
      "USGS-11455280",
      "USGS-11455315",
      "USGS-11455335",
      "USGS-11455350",
      "USGS-11455385",
      "USGS-11455420",
      "USGS-11455478",
      "USGS-11455485",
      "USGS-11455508",
      "USGS-380631122032201",
      "USGS-380833122033401",
      "USGS-381142122015801",
      "USGS-381424121405601",
      "USGS-381614121415301",
      "USGS-382006121401601",
      "USGS-382010121402301"
    )

  # Download USGS discrete nutrient and chlorophyll-a data collected by the
    # CAWSC group using the `readWQPqw` function
  df_cawsc_tmp <-
    readWQPqw(
      siteNumbers = site_numb,
      parameterCd = params,
      startDate = "2014-01-01",
      endDate = "2022-01-01"
    )

  # Download station coordinates
  df_cawsc_coord <- whatWQPsites(siteid = site_numb) %>%
    select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)

  # Add coordinates to data frame
  df_cawsc_tmp <- left_join(df_cawsc_tmp, df_cawsc_coord, by = "MonitoringLocationIdentifier")

  # Save data as a .csv file in the "data-raw/Discrete_nutr_chla_mvi_data" folder
  df_cawsc_tmp %>%
    write_csv(here("data-raw/Discrete_nutr_chla_mvi_data/USGS_CAWSC_nutr_chla_data_2014-2021.csv"))

  # Clean up
  rm(params, site_numb, df_cawsc_tmp, df_cawsc_coord)
}

# Create a vector of all file paths of the discrete nutrient, chlorophyll-a, and
  # Microcystis visual index data found in the data-raw folder
fp_nutr_chla_mvi <- dir(here("data-raw/Discrete_nutr_chla_mvi_data"), full.names = TRUE)

# Import USGS discrete nutrient and chlorophyll-a data collected by the CAWSC group
df_cawsc <-
  read_csv(
    file = str_subset(fp_nutr_chla_mvi, "USGS_CAWSC"),
    col_types = cols_only(
      ActivityStartDate = "D",
      ActivityStartTime.Time = "t",
      ActivityStartTime.TimeZoneCode = "c",
      MonitoringLocationIdentifier = "c",
      ResultDetectionConditionText = "c",
      CharacteristicName = "c",
      ResultMeasureValue = "d",
      ResultStatusIdentifier = "c",
      ResultValueTypeName = "c",
      ResultAnalyticalMethod.MethodName = "c",
      ResultLaboratoryCommentText = "c",
      DetectionQuantitationLimitMeasure.MeasureValue = "d",
      LatitudeMeasure = "d",
      LongitudeMeasure = "d"
    )
  )

# Import additional DWR_EMP discrete nutrient and chlorophyll-a data for 2021
# Provided from personal data request
df_emp_nutr_chla_2021 <-
  read_excel(
    path = str_subset(fp_nutr_chla_mvi, "EMP_water_quality"),
    range = "A2:AA288",
    col_types = "text"
  )

# Import additional DWR_EMP field data for 2021 - this includes EZ station
  # coordinates and Microcystis visual index data
# Provided from personal data request
df_emp_field_2021 <-
  read_excel(
    path = str_subset(fp_nutr_chla_mvi, "EMP_water_quality"),
    range = "A290:R576",
    col_types = "text"
  )

# Import DWR_EMP station coordinates from EDI
df_emp_coord <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")

# Import discrete nutrient, chlorophyll-a, and Microcystis visual index data
  # collected by DWR_NCRO
### Historical data:
# Create a vector of sheet names from the data spreadsheet - these will double as station names
ncro_sheets <- c("FAL", "BET", "HOL", "OSJ", "FCT", "TSL")

# Import the discrete nutrient and chlorophyll-a data as a nested data frame
  # with each element containing data for one station
ndf_ncro_nutr_chla_hist <-
  tibble(
    fp_data = rep(str_subset(fp_nutr_chla_mvi, "NCROWQES_DiscreteData_historical"), 6),
    Station = ncro_sheets
  ) %>%
  mutate(df_data = map2(fp_data, Station, ~ read_excel(.x, sheet = .y, col_types = "text"))) %>%
  select(-fp_data)

# Import all Microcystis visual index data collected by DWR_NCRO
df_ncro_mvi_hist <-
  read_excel(str_subset(fp_nutr_chla_mvi, "NCROWQES_Microcystis_Obs_2017-2022"))

### 2021 discrete nutrient and chlorophyll-a data:
# May - November
df_ncro_nutr_chla_2021a <-
  read_excel(
    str_subset(fp_nutr_chla_mvi, "NCROWQES_DiscreteData_2021_May-Nov"),
    sheet = "Cross Tab_AnalyteResults",
    col_types = "text"
  )

# December
df_ncro_nutr_chla_2021b <-
  read_excel(
    str_subset(fp_nutr_chla_mvi, "NCROWQES_DiscreteData_2021_Dec"),
    range = "A2:U6",
    col_types = "text"
  )

# Station coordinates
df_ncro_coord <-
  read_excel(
    str_subset(fp_nutr_chla_mvi, "NCROWQES_DiscreteData_historical"),
    sheet = "METADATA",
    range = "A38:E44"
  )

# Import region assignments from the Drought Synthesis project - we'll use this
  # to define which stations to include and to assign regions for QA purposes
df_regions <- read_csv(str_subset(fp_nutr_chla_mvi, "Rosies_regions")) %>%
  distinct(Region, SubRegion)

# Load Delta regions shapefile from Brian and prepare it to use for processing
  # this data set
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  select(SubRegion) %>%
  left_join(df_regions, by = "SubRegion") %>%
  # Remove SubRegions outside of Regions
  filter(!is.na(Region)) %>%
  # Remove Suisun regions
  filter(!str_detect(Region, "^Suisun")) %>%
  select(Region, SubRegion)

# Import polygon shapefile for the HABs regions used in the analysis of this
  # data set
sf_hab_reg <- read_sf(here("data-raw/Spatial_data/HABregions.shp")) %>%
  select(Region = Stratum2)


# 2. Clean and Combine Data -----------------------------------------------

# 2.1 discretewq Data -----------------------------------------------------

# Prepare data from the discretewq package to be combined with all other data
df_package_c1 <- df_package %>%
  mutate(
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    # Change name of Source for EMP survey
    Source = if_else(Source == "EMP", "DWR_EMP", Source)
  ) %>%
  # Fill in _Sign variables for all surveys
  mutate(
    # No Chla values appear to be <RL - make all records "=" in Chlorophyll_Sign
    Chlorophyll_Sign = "=",
    # Make all NA values in _Sign variables for the nutrients "="
    across(c(DissAmmonia_Sign, DissOrthophos_Sign), ~ if_else(is.na(.x), "=", .x)),
    DissNitrateNitrite_Sign = case_when(
      is.na(DissNitrateNitrite_Sign) ~ "=",
      # One DissNitrateNitrite record has an unknown RL value
      Source == "DWR_EMP" & DissNitrateNitrite_Sign == "<" & is.na(DissNitrateNitrite) ~ "< (unknown)",
      TRUE ~ DissNitrateNitrite_Sign
    )
  ) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll"),
    Microcystis
  )

# For the USGS_SFBS, if at least one of the nutrient parameters has a value reported, then we
  # will assume that the other parameters were below the reporting limit for that station and day
df_sfbs_nutr_blw_rl <- df_package_c1 %>%
  filter(Source == "USGS_SFBS") %>%
  filter(!if_all(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  filter(if_any(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  # Assume 2 DissAmmonia records were <RL and use 0.0007 for the estimated RL value -
    # RL value was provided by USGS
  mutate(
    DissAmmonia_Sign = "< (estimated)",
    DissAmmonia = 0.0007
  )

df_package_c2 <- df_package_c1 %>%
  anti_join(df_sfbs_nutr_blw_rl, by = c("Source", "Station", "Datetime")) %>%
  bind_rows(df_sfbs_nutr_blw_rl)

# 2.2 2021 DWR_EMP Data ---------------------------------------------------

# Prepare DWR_EMP station coordinates to be joined to 2021 DWR_EMP data
df_emp_coord_c <- df_emp_coord %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

# Prepare DWR_EMP EZ station coordinates for 2021 to be joined to 2021 DWR_EMP data
df_emp_coord_ez21 <- df_emp_field_2021 %>%
  select(
    SampleCode = `Sample Code`,
    Latitude_field = contains("Latitude"),
    Longitude_field = contains("Longitude")
  ) %>%
  filter(!if_any(c(Latitude_field, Longitude_field), ~ .x == "N/A")) %>%
  mutate(across(ends_with("_field"), as.numeric))

# Prepare 2021 DWR_EMP Microcystis visual index data to by joined to 2021
  # DWR_EMP discrete nutrient and chlorophyll-a data
df_emp_mvi_2021 <- df_emp_field_2021 %>%
  select(
    SampleCode = `Sample Code`,
    Microcystis = contains("Microcystis")
  ) %>%
  mutate(Microcystis = as.numeric(Microcystis))

# Prepare 2021 EMP discrete nutrient, chlorophyll-a, and Microcystis visual
  # index data to be combined with all other data
df_emp_all_2021 <- df_emp_nutr_chla_2021 %>%
  # Select and standardize variable names
  select(
    Station = `Station Name`,
    StationNumber = `Station Number`,
    SampleCode = `Sample Code`,
    Datetime = `Sample Date`,
    DissAmmonia = contains("Ammonia"),
    DissNitrateNitrite = contains("Nitrate"),
    DissOrthophos = contains("Phosphate"),
    Chlorophyll = contains("Chlorophyll")
  ) %>%
  # Parse Datetime (as PST) and create Source and Date variables
  mutate(
    Datetime = mdy_hm(Datetime, tz = "Etc/GMT+8"),
    Date = date(Datetime),
    Source = "DWR_EMP"
  ) %>%
  # Remove overlapping data
  filter(year(Date) > 2020) %>%
  # Pivot data longer to work on lab results in one column
  pivot_longer(
    cols = c(starts_with("Diss"), Chlorophyll),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  # Remove some "N/A" values
  filter(Value != "N/A") %>%
  mutate(
    # Standardize Stations
    Station = case_when(
      str_detect(Station, "^SF Estuarine") ~ StationNumber,
      str_detect(Station, "- C3A") ~ "C3A",
      str_detect(Station, "^NZ068") ~ "NZ068",
      str_detect(Station, " - ") ~ str_extract(Station, ".+(?= - )")
    ),
    # Keep the first value of a lab replicate pair
    Value = if_else(str_detect(Value, ","), str_extract(Value, ".+(?=,)"), Value),
    # Add a new variable to identify values below the reporting limit
    Sign = if_else(str_detect(Value, "^<"), "<", "="),
    # For the values below the reporting limit, make them equal to the reporting limit and convert to numeric
    Value = as.numeric(if_else(str_detect(Value, "^<"), str_remove(Value, "^<"), Value))
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = c(Value, Sign),
    names_glue = "{Parameter}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Value"), ends_with("_Value")) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  # Add station coordinates
  left_join(df_emp_coord_c, by = "Station") %>%
  left_join(df_emp_coord_ez21, by = "SampleCode") %>%
  mutate(
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  # Add Microcystis visual index data
  left_join(df_emp_mvi_2021, by = "SampleCode") %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll"),
    Microcystis
  )

# 2.3 USGS_CAWSC Data -----------------------------------------------------

# Prepare CAWSC data to be combined with all other data
df_cawsc_c <- df_cawsc %>%
  # Add source variable
  mutate(Source = "USGS_CAWSC") %>%
  # Rename variables so that they are easier to use
  select(
    Source,
    Station = MonitoringLocationIdentifier,
    Latitude = LatitudeMeasure,
    Longitude = LongitudeMeasure,
    Date = ActivityStartDate,
    Time = ActivityStartTime.Time,
    TimeZone = ActivityStartTime.TimeZoneCode,
    Parameter = CharacteristicName,
    Method = ResultAnalyticalMethod.MethodName,
    Value = ResultMeasureValue,
    ValueDetectionQual = ResultDetectionConditionText,
    DetectionLimit = DetectionQuantitationLimitMeasure.MeasureValue,
    ResultValueTypeName,
    LabComment = ResultLaboratoryCommentText,
    ResultStatusIdentifier
  ) %>%
  mutate(
    # Change Parameter names to standardized names
    Parameter = case_when(
      str_detect(Parameter, "^Amm") ~ "DissAmmonia",
      str_detect(Parameter, "^Inorg") ~ "DissNitrateNitrite",
      str_detect(Parameter, "^Ortho") ~ "DissOrthophos",
      str_detect(Parameter, "^Chloro") ~ "Chlorophyll"
    ),
    # Convert Time variable to PST (a few have tz = UTC, but they are probably PST)
    Time = if_else(TimeZone == "PDT", hms::hms(lubridate::hms(Time) - hours(1)), Time),
    # Create Datetime variable as PST
    Datetime = ymd_hms(paste(Date, Time), tz = "Etc/GMT+8"),
    # Calculate difference from noon for each data point for later filtering
    NoonDiff = abs(hms::hms(hours = 12) - Time)
  ) %>%
  # Select only 1 data point per station and date, choose data closest to noon
  group_by(Station, Date, Parameter) %>%
  filter(NoonDiff == min(NoonDiff)) %>%
  # When points are equidistant from noon, select earlier point
  filter(Time == min(Time)) %>%
  ungroup() %>%
  # Clean up data below the detection limit
  mutate(
    # Add a new variable to identify values below the detection limit - NA's in Value are <DL
    Sign = case_when(
      !is.na(Value) & ResultValueTypeName == "Actual" ~ "=",
      !is.na(Value) & ResultValueTypeName == "Estimated" ~ "= (estimated)",
      ValueDetectionQual == "Not Detected" & !is.na(DetectionLimit) ~ "<",
      ValueDetectionQual == "Not Detected" & is.na(DetectionLimit) ~ "< (estimated)"
    ),
    # For the values below the detection limit, make them equal to the detection limit
      # If no detection limit is available, use the most common detection limit for the method
    Value = case_when(
      str_detect(Sign, "=") ~ Value,
      Sign == "<" ~ DetectionLimit,
      Sign == "< (estimated)" & Method == "Ammonia, wf, DA sal/hypo (NWQL)" ~ 0.01,
      Sign == "< (estimated)" & Method == "NO3+NO2, wf, FCC,NaR, DA" ~ 0.04,
      Sign == "< (estimated)" & Method == "NO3+NO2, wf, FCC,NaR, DA, LL" ~ 0.01,
      Sign == "< (estimated)" & Method == "Ortho-PO4, wf, DA phos/mol(NWQL)" ~ 0.004
    )
  ) %>%
  # Remove a few unnecessary variables and restructure to wide format
  # Removing ResultStatusIdentifier and keeping both Accepted and Preliminary data
  select(
    -c(
      ValueDetectionQual,
      ResultValueTypeName,
      DetectionLimit,
      Method,
      LabComment,
      ResultStatusIdentifier
    )
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = c(Value, Sign),
    names_glue = "{Parameter}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Value"), ends_with("_Value")) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll")
  )

# 2.4 DWR_NCRO Data -------------------------------------------------------

# Create a vector of the variable names to keep in the data frames of historical data
vec_vars_keep <-
  c(
    "Sample Code",
    "Date",
    "Data",
    "Time",
    "Time...7",
    "Analyte",
    "Result",
    "Rpt Limit"
  )

# Prepare historical discrete nutrient and chlorophyll-a data from NCRO to be
  # combined with all other data
df_ncro_nutr_chla_hist <- ndf_ncro_nutr_chla_hist %>%
  # Standardize variable names
  mutate(
    df_data = map(
      df_data,
      ~ select(.x, any_of(vec_vars_keep)) %>%
        rename(
          SampleCode = `Sample Code`,
          Date = starts_with("Data"),
          Time = starts_with("Time...7"),
          RL = `Rpt Limit`
        )
    )
  ) %>%
  # Unnest nested data frames now that all variable names are standardized
  unnest(df_data) %>%
  # Remove one empty row
  filter(!is.na(SampleCode)) %>%
  # Filter to desired analytes
  filter(
    Analyte %in% c(
      "Chlorophyll a",
      "Dissolved Ammonia",
      "Dissolved Nitrate + Nitrite",
      "Dissolved ortho-Phosphate"
    )
  ) %>%
  mutate(
    # Date is imported as an integer, origin is two days before 1900-01-01 to account for known
      # bug in Excel and compatibility between Excel and R
    Date = as_date(as.numeric(Date), origin = "1899-12-30"),
    # Time is imported as a fraction of a day, convert it to seconds in day from midnight and
      # pass it into as_hms. There are a few times that seem wrong (collected near midnight) -
      # these were most likely collected near noon, so we'll add 12 hours to them.
    Time = if_else(
      as.numeric(Time) < 0.25,
      as_hms(as.numeric(Time) * 60 * 60 * 24 + 12 * 60 * 60),
      as_hms(as.numeric(Time) * 60 * 60 * 24)
    ),
    # Create Datetime variable in PST
    Datetime = ymd_hms(paste(Date, Time), tz = "Etc/GMT+8"),
    # Standardize analyte names
    Analyte = case_when(
      Analyte == "Chlorophyll a" ~ "Chlorophyll",
      str_detect(Analyte, "Ammonia") ~ "DissAmmonia",
      str_detect(Analyte, "Nitrate") ~ "DissNitrateNitrite",
      str_detect(Analyte, "Phosphate") ~ "DissOrthophos"
    ),
    # Add a new variable to identify values below the reporting limit
    Sign = if_else(str_detect(Result, "^<"), "<", "="),
    # For the values below the reporting limit, make them equal to the reporting limit and convert to numeric
    Result = as.numeric(if_else(str_detect(Result, "^<"), RL, Result))
  ) %>%
  # Remove data earlier than 2014
  filter(year(Date) >= 2014) %>%
  # Remove duplicate records
  distinct() %>%
  group_by(Station, SampleCode, Date, Time, Datetime, Analyte, RL) %>%
  mutate(row_numb = row_number()) %>%
  ungroup() %>%
  filter(row_numb == 1L) %>%
  # Remove a few unnecessary variables and restructure to wide format
  select(-c(RL, row_numb)) %>%
  pivot_wider(
    names_from = Analyte,
    values_from = c(Result, Sign),
    names_glue = "{Analyte}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Result"), ends_with("_Result")) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  # Select variable order
  select(
    Station,
    SampleCode,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll")
  )

# Prepare 2021 discrete nutrient and chlorophyll-a data from NCRO to be combined
  # with all other data
df_ncro_nutr_chla_2021 <- bind_rows(df_ncro_nutr_chla_2021a, df_ncro_nutr_chla_2021b) %>%
  # Select and standardize variable names
  select(
    Station = `Station Name`,
    Datetime = `Sample Date`,
    SampleCode = `Sample Code`,
    SampleType = `Sample Type`,
    DissAmmonia = contains("Ammonia"),
    DissNitrateNitrite = contains("Nitrate"),
    DissOrthophos = contains("Phosphate"),
    Chlorophyll = contains("Chlorophyll")
  ) %>%
  # Remove some unwanted rows at the end
  filter(!str_detect(Station, "\\*|Samples")) %>%
  # Only keep "Normal Samples"
  filter(SampleType == "Normal Sample") %>%
  # Pivot data longer to work on lab results in one column
  pivot_longer(
    cols = c(starts_with("Diss"), Chlorophyll),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  # Remove some "N/A" values
  filter(Value != "N/A") %>%
  mutate(
    # Standardize Stations
    Station = case_when(
      str_detect(Station, "^Bethel") ~ "BET",
      str_detect(Station, "^False") ~ "FAL",
      str_detect(Station, "^Fisherman") ~ "FCT",
      str_detect(Station, "^Holland") ~ "HOL",
      str_detect(Station, "^Old") ~ "OSJ",
      str_detect(Station, "^Three") ~ "TSL"
    ),
    # Parse Datetime and create Date variable
    Datetime = mdy_hm(Datetime, tz = "Etc/GMT+8"),
    Date = date(Datetime),
    # Keep the first value of a lab replicate pair
    Value = if_else(str_detect(Value, ","), str_extract(Value, ".+(?=,)"), Value),
    # Add a new variable to identify values below the reporting limit
    Sign = if_else(str_detect(Value, "^<"), "<", "="),
    # For the values below the reporting limit, make them equal to the reporting limit and convert to numeric
    Value = as.numeric(if_else(str_detect(Value, "^<"), str_remove(Value, "^<"), Value))
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = c(Value, Sign),
    names_glue = "{Parameter}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Value"), ends_with("_Value")) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  # Select variable order
  select(
    Station,
    SampleCode,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll")
  )

# Prepare all Microcystis visual index data collected by DWR_NCRO to be joined
  # to discrete nutrient and chlorophyll-a data
df_ncro_mvi_hist_c <- df_ncro_mvi_hist %>%
  rename(
    Station = StationCode,
    Datetime = DeploymentEnd,
    Microcystis = FldObsWaterHabs
  ) %>%
  mutate(
    # Define timezone of Datetime as PST and create Date variable
    Datetime = force_tz(Datetime, tzone = "Etc/GMT+8"),
    Date = date(Datetime),
    # Standardize Microcystis score to a number 1-5
    Microcystis = case_when(
      Microcystis %in% c("Absent", "Not Visible") ~ 1,
      Microcystis == "Low" ~ 2,
      Microcystis == "Medium" ~ 3,
      Microcystis == "High" ~ 4,
      Microcystis == "Extreme" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  # Only include stations of interest
  filter(Station %in% c("BET", "FAL", "FCT", "HOL", "OSJ", "TSL")) %>%
  # Remove records with NA values
  select(Station, Date, Microcystis) %>%
  drop_na()

# Prepare NCRO station coordinates to be joined to data
df_ncro_coord_c <- df_ncro_coord %>%
  select(
    Station = `Station Code`,
    Latitude,
    Longitude
  )

# Combine historical and 2021 DWR-NCRO data and remove a few duplicates
df_ncro_all <- bind_rows(df_ncro_nutr_chla_hist, df_ncro_nutr_chla_2021) %>%
  distinct() %>%
  # Add Source variable
  mutate(Source = "DWR_NCRO") %>%
  # Add station coordinates
  left_join(df_ncro_coord_c, by = "Station") %>%
  # Add Microcystis visual index data
  left_join(df_ncro_mvi_hist_c, by = c("Station", "Date")) %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    starts_with("Chlorophyll"),
    Microcystis
  )

# 2.5 Combine All Data ----------------------------------------------------

# Combine EMP data from 2021 and USGS-CAWSC data to discretewq data
df_nutr_chla_mvi_all <- bind_rows(df_package_c2, df_cawsc_c, df_emp_all_2021, df_ncro_all)


# 3. Clean All Raw Data ---------------------------------------------------

# 3.1 Global Cleaning Steps -----------------------------------------------

# Begin to clean all raw data
df_nutr_chla_mvi_all_c1 <- df_nutr_chla_mvi_all %>%
  # Remove records where all nutrient parameters, chlorophyll-a, and Microcystis are NA
  filter(
    !if_all(
      c(DissAmmonia, DissNitrateNitrite, DissOrthophos, Chlorophyll, Microcystis),
      is.na
    )
  ) %>%
  # Remove records without latitude-longitude coordinates
  drop_na(Latitude, Longitude) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Change to crs of sf_delta
  st_transform(crs = st_crs(sf_delta)) %>%
  # Add Delta regions
  st_join(sf_delta, join = st_intersects) %>%
  # Remove any data outside our regions of interest
  filter(!is.na(Region)) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Remove SubRegion variable
  select(-SubRegion)

# 3.2 Create Outlier Functions --------------------------------------------

# Create function to flag data points with modified z-scores greater than a
  # specified threshold
flag_modzscore <- function(df, data_var, threshold) {
  mod_zscore_sym <- sym(glue("{as_name(enquo(data_var))}_mod_zscore"))
  mod_zscore_enquo <- enquo(mod_zscore_sym)

  df %>%
    mutate(
      tmp_median = median({{ data_var }}, na.rm = TRUE),
      tmp_mad = mad({{ data_var }}, na.rm = TRUE),
      !!mod_zscore_sym := if_else(
        tmp_mad == 0,
        NA_real_,
        abs(0.6745 * ({{ data_var }} - tmp_median) / tmp_mad)
      ),
      "{{data_var}}_flag" := case_when(
        is.na(!!mod_zscore_enquo) ~ FALSE,
        !!mod_zscore_enquo > threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(!starts_with("tmp_"))
}

# Create function to flag the <RL values with high reporting limits (greater
  # than a specified percentile of the data)
flag_high_rl <- function(df, data_var, perc_thresh) {
  sign_sym <- sym(glue("{as_name(enquo(data_var))}_Sign"))
  sign_enquo <- enquo(sign_sym)
  flag_sym <- sym(glue("{as_name(enquo(data_var))}_flag"))
  flag_enquo <- enquo(flag_sym)

  threshold <- df %>%
    summarize(quant = quantile({{ data_var }}, probs = perc_thresh, na.rm = TRUE)) %>%
    pull(quant)

  if (any(names(df) == as_name(flag_sym))) {
    df %>%
      mutate(
        !!flag_sym := case_when(
          is.na({{ data_var }}) ~ FALSE,
          str_detect(!!sign_enquo, "^<") & {{ data_var }} > threshold ~ TRUE,
          TRUE ~ !!flag_enquo
        )
      )
  } else {
    df %>%
      mutate(
        !!flag_sym := case_when(
          is.na({{ data_var }}) ~ FALSE,
          str_detect(!!sign_enquo, "^<") & {{ data_var }} > threshold ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }
}

# Create function to remove flagged data points and modify value in related
  # _Sign variable to "= (unreliable)"
rm_flagged <- function(df, data_var) {
  sign_sym <- sym(glue("{as_name(enquo(data_var))}_Sign"))
  sign_enquo <- enquo(sign_sym)
  flag_sym <- sym(glue("{as_name(enquo(data_var))}_flag"))
  flag_enquo <- enquo(flag_sym)

  df %>%
    mutate(
      !!sign_sym := if_else(!!flag_enquo == TRUE, "= (unreliable)", !!sign_enquo),
      "{{data_var}}" := if_else(!!flag_enquo == TRUE, NA_real_, {{ data_var }})
    )
}

# 3.3 Prepare Data for VIMS Group -----------------------------------------

# Create function to remove values that are <RL with estimated RL values and
  # modify value in related _Sign variable to "< (unknown)"
rm_est_rl <- function(df, data_var) {
  sign_sym <- sym(glue("{as_name(enquo(data_var))}_Sign"))
  sign_enquo <- enquo(sign_sym)

  df %>%
    mutate(
      "{{data_var}}" := if_else(!!sign_enquo == "< (estimated)", NA_real_, {{ data_var }}),
      !!sign_sym := if_else(!!sign_enquo == "< (estimated)", "< (unknown)", !!sign_enquo)
    )
}

vims_nutr_chla <- df_nutr_chla_mvi_all_c1 %>%
  # Remove Microcystis data
  select(-Microcystis) %>%
  # Remove nutrient values that are <RL with estimated RL values
  rm_est_rl(DissAmmonia) %>%
  rm_est_rl(DissNitrateNitrite) %>%
  rm_est_rl(DissOrthophos) %>%
  # Remove records where all nutrient parameters and chlorophyll-a are NA
  filter(!if_all(c(DissAmmonia, DissNitrateNitrite, DissOrthophos, Chlorophyll), is.na)) %>%
  # Flag data points that have modified z-scores greater than 15 grouped by
    # Delta Region - not including chlorophyll because most of the higher values
    # are probably real
  group_by(Region) %>%
  flag_modzscore(DissAmmonia, 15) %>%
  flag_modzscore(DissNitrateNitrite, 15) %>%
  flag_modzscore(DissOrthophos, 15) %>%
  ungroup() %>%
  # Exclude flagged data points with modified z-scores greater than 15
  rm_flagged(DissAmmonia) %>%
  rm_flagged(DissNitrateNitrite) %>%
  rm_flagged(DissOrthophos) %>%
  # Flag the <RL values with high RL's (> 75th percentile) in the DWR_EMP data set
  flag_high_rl(DissAmmonia, 0.75) %>%
  flag_high_rl(DissNitrateNitrite, 0.75) %>%
  flag_high_rl(DissOrthophos, 0.75) %>%
  flag_high_rl(Chlorophyll, 0.75) %>%
  # Exclude flagged data points that are <RL values with high RL's
  rm_flagged(DissAmmonia) %>%
  rm_flagged(DissNitrateNitrite) %>%
  rm_flagged(DissOrthophos) %>%
  rm_flagged(Chlorophyll) %>%
  # Remove mod z-score and flag variables
  select(!ends_with(c("_mod_zscore", "_flag"))) %>%
  # Clean up and reorder variables
  select(-Region) %>%
  relocate(Chlorophyll_Sign, .before = Chlorophyll)

# 3.4 Prepare Data for HABs Report ----------------------------------------

# Look for and remove outliers from the data set
df_nutr_chla_mvi_all_c2 <- df_nutr_chla_mvi_all_c1 %>%
  # Flag data points that have modified z-scores greater than 15 grouped by Delta
    # Region - not including chlorophyll and ammonia because most of the higher
    # values are probably real
  group_by(Region) %>%
  flag_modzscore(DissNitrateNitrite, 15) %>%
  flag_modzscore(DissOrthophos, 15) %>%
  ungroup() %>%
  # Exclude flagged data points with modified z-scores greater than 15
  rm_flagged(DissNitrateNitrite) %>%
  rm_flagged(DissOrthophos) %>%
  # Remove the 4 DWR_EMP "EZ" stations because they don't have fixed locations -
    # We waited to remove these stations until after running the modified z-score
    # test to have the most data possible for each region
  filter(!str_detect(Station, "^EZ")) %>%
  # Remove mod z-score and Region variables
  select(-c(ends_with("_mod_zscore"), Region))

# We will only keep stations where all three nutrients and chlorophyll-a have been collected
# This doesn't require that all parameters were collected on the same day though
df_sta_keep <- df_nutr_chla_mvi_all_c2 %>%
  select(!ends_with("_Sign")) %>%
  pivot_longer(
    cols = c(starts_with("Diss"), Chlorophyll),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  count(Source, Station, Parameter) %>%
  pivot_wider(names_from = Parameter, values_from = n) %>%
  filter(!if_any(c(starts_with("Diss"), Chlorophyll), is.na)) %>%
  select(Source, Station)

# Finish preparing data for the HABs report
hab_nutr_chla_mvi <- df_nutr_chla_mvi_all_c2 %>%
  # Run inner join to only include Source-Station combinations in df_sta_keep
  inner_join(df_sta_keep, by = c("Source", "Station")) %>%
  # Flag the <RL values with high RL's (> 75th percentile) in the DWR_EMP data set
  flag_high_rl(DissAmmonia, 0.75) %>%
  flag_high_rl(DissNitrateNitrite, 0.75) %>%
  flag_high_rl(DissOrthophos, 0.75) %>%
  flag_high_rl(Chlorophyll, 0.75) %>%
  # Exclude flagged data points that are <RL values with high RL's
  rm_flagged(DissAmmonia) %>%
  rm_flagged(DissNitrateNitrite) %>%
  rm_flagged(DissOrthophos) %>%
  rm_flagged(Chlorophyll) %>%
  # Add Regions from the sf_hab_reg shapefile which were used in the analysis of
    # this data
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Add Regions from sf_hab_reg
  st_join(sf_hab_reg, join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Remove three stations not within the regions in sf_hab_reg - these are
    # located west of Chipps Island
  filter(!is.na(Region)) %>%
  # Remove and reorder variables
  select(!ends_with("_flag")) %>%
  relocate(Region, .before = Date) %>%
  relocate(Chlorophyll_Sign, .before = Chlorophyll) %>%
  # Convert Microcystis to integer
  mutate(Microcystis = as.integer(Microcystis))


# 4. Save and Export Data -------------------------------------------------

# Save final data sets of discrete nutrient and chlorophyll-a concentrations and
  # Microcystis visual index values as csv files for easier diffing
vims_nutr_chla %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv(here("data-raw/Final/vims_nutr_chla.csv"))

hab_nutr_chla_mvi %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv(here("data-raw/Final/hab_nutr_chla_mvi.csv"))

# Save final data sets of discrete nutrient and chlorophyll-a concentrations and
  # Microcystis visual index values as objects in the data package
# `vims_nutr_chla` will be saved as a .csv file in the data package but not
  # exported as an .rda data object for now. It was provided as a special data
  # request to VIMS to assist with their modeling effort.
usethis::use_data(hab_nutr_chla_mvi, overwrite = TRUE)

