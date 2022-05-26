# Code to prepare combined data set of HAB toxin data for the Emergency Drought
# Barrier (HABs/Weeds) analysis:
# 1) `hab_toxins` - . Used in the Spring-Summer version of the 2022 HABs/Weeds
# report.

# Load packages
library(dplyr)
library(stringr)
library(readr)
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
# Frk <- data.frame(
#   Station = c("FRK", "FRK", "FRK", "MI"),
#   Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
#   Date = c(as.Date("2021-07-02"), as.Date("2021-08-06"), as.Date("2021-08-06"), as.Date("2021-07-02")),
#   result = c(0, 0.63, 0, 0.6)
# )
df_wb_franks <- tibble(
  Station = c("FRK", "FRK", "FRK", "MI"),
  Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
  Date = ymd("2021-07-02", "2021-08-06", "2021-08-06", "2021-07-02"),
  result = c(0, 0.63, 0, 0.6)
)

# Nautilus data - from the State Board's database, provided by Karen Atkinson
# Nautilus = read_excel("data/datapackage/RawData/HAB_Monitoring.xlsx", sheet = "Nautalis")
df_nautilus <- read_excel(path = str_subset(fp_hab_toxins, "HAB_Monitoring"), sheet = "Nautalis")

# East Bay Parks data - from the State Board's database, provided by Karen Atkinson
# EastBay = read_excel("data/datapackage/RawData/HAB_Monitoring.xlsx", sheet = "East Bay")
df_ebp <- read_excel(path = str_subset(fp_hab_toxins, "HAB_Monitoring"), sheet = "East Bay")

# Preece/Otten data
# preece = read_excel("data/datapackage/RawData/Prop 1 data_4.1.22.xlsx", sheet = "preecedata")
df_pre_ott <- read_excel(str_subset(fp_hab_toxins, "Prop 1 data"), sheet = "preecedata")


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
  # Add year column
  mutate(Year = year(Date))

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
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, Year, Month, DOY, class) %>%
  summarize(result = sum(resultNum)) %>%
  ungroup() %>%
  # Convert Date and rename some variables
  mutate(Date = mdy(Date)) %>%
  rename(
    Analyte = class,
    Station = Site
  )

# Nautilus data
Naut = Nautilus %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Anatonxin-a_ELISA_Method (ug/L)`,
                `Cylindrospermopsin_ELISA_Method (ug/L)`, `Microcystin_ELISA_Method (ug/L)`, `Saxitoxin_ELISA_Method (ug/L)`,
                Sample_Date) %>%
  rename(Station = Station_Code, Microcystins = `Microcystin_ELISA_Method (ug/L)`,
         Anatoxins = `Anatonxin-a_ELISA_Method (ug/L)`,
         Cylindrospermopsins = `Cylindrospermopsin_ELISA_Method (ug/L)`,
         Saxitoxins = `Saxitoxin_ELISA_Method (ug/L)`,
         Date = Sample_Date) %>%
  pivot_longer(cols = c(Microcystins, Anatoxins, Cylindrospermopsins, Saxitoxins), names_to= "Analyte", values_to = "resultF")%>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF))) %>%
  dplyr::filter(!is.na(result))

# East Bay Parks data
EastBayX = dplyr::filter(EastBay, Water_Body == "Big Break Regional Shoreline") %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Microcystin (µg/L)\r\nELISA_Method`, Sample_Date) %>%
  rename(resultF = `Microcystin (µg/L)\r\nELISA_Method`, Date = Sample_Date) %>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF)),
         Analyte = "Microcystins", Station = "BigBreak") %>%
  dplyr::filter(!is.na(result))

#Preece/Otten data
preece = read_excel("data/datapackage/RawData/Prop 1 data_4.1.22.xlsx", sheet = "preecedata") %>%
  mutate(Analyte = "Microcystins", result = case_when(`Final_conc (ug/L)`=="ND"~ 0,
                                                      TRUE ~ as.numeric(`Final_conc (ug/L)`)),
         Year = year(Collection_Date), Month = as.character(month(Collection_Date))) %>%
  filter(Year == 2021) %>%
  rename(Station = Sample_ID, Date = Collection_Date)

#Put them all together
allTox = bind_rows(df_usgs_spatt_c, df_swp_c2, Frk, preece, Naut, EastBayX)  %>%
  mutate(Analyte = case_when(
    Analyte == "MC"~ "Microcystins",
    Analyte == "ANTX-A"~"Anatoxins",
    TRUE ~ Analyte
  ))


#Attatch stations
Stas =  read_excel("data/datapackage/RawData/Prop 1 data_4.1.22.xlsx", sheet = "stations")
allTox3a = left_join(allTox, Stas) %>%
  mutate(Longitude = as.numeric(Longitude))  %>%
  ungroup()


Alltox3 = dplyr::select(allTox3a, Station, Date, Year, Month, Analyte, result, Study, Region, Latitude, Longitude)



# usethis::use_data(hab_toxins, overwrite = TRUE)
