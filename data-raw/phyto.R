# Code to prepare phytoplankton data sets for the Emergency Drought Barrier analysis:
  # 1) `phyto_edb` - Phytoplankton community data collected by DWR-EMP from
    # 2014-2021 for the stations within the designated EDB regions
  # 2) `phyto_hab` - Phytoplankton community data of potentially toxic
    # cyanobacteria collected by DWR-EMP from 2014-2021. Used in the Spring-Summer
    # version of the 2022 HABs/Weeds report.

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(readxl)
library(sf)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/phyto.R")


# 1. Import Data ----------------------------------------------------------

# Create a vector of all file paths within the data-raw/Phyto_data folder
fp_phyto_data <- dir(here("data-raw/Phyto_data/"), recursive = TRUE, full.names = TRUE)

# Import earlier phytoplankton data
df_phyto_early <-
  read_csv(
    str_subset(fp_phyto_data, "EMP_phyto_data.csv$"),
    col_types = "-DTc-cccd--d-----------"
  )

# Import phytoplankton data collected from Dec 2020 - Dec 2021
lst_phyto_recent <- map(str_subset(fp_phyto_data, "202[01]\\.xlsx$"), read_excel)

# Import phytoplankton classification table (copied from the DroughtSynthesis repository)
df_phyto_taxonomy <- read_excel(str_subset(fp_phyto_data, "Phyto Classification.xlsx$"))

# Import EMP station coordinates from EDI
df_coord_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")


# 2. Clean and Combine Data -----------------------------------------------

# Earlier data:
df_phyto_early_c <- df_phyto_early %>%
  # filter to years 2014-2020, don't include Dec 2020 through 2021 since there is some overlap
  filter(SampleDate >= "2014-01-01" & SampleDate < "2020-12-01") %>%
  # Rename some variables
  rename(
    Date = SampleDate,
    Station = StationCode,
    OrganismsPerMl = Organisms_per_mL
  )

# Recent data (Dec 2020 - Dec 2021):
# Create a vector of variable names to keep in each list element
vec_vars_keep <-
  c(
    "SampleDate",
    "SampleTime",
    "Full Code",
    "StationCode",
    "Taxon",
    "Genus",
    "Species",
    "Factor",
    "Count",
    "Unit Abundance",
    "Unit Abundance (# of Natural Units)"
  )

df_phyto_recent <- lst_phyto_recent %>%
  # Select and rename variables in each element
  map(
    ~ select(.x, any_of(vec_vars_keep)) %>%
      rename(
        Count = contains("Abundance"),
        SampleCode = contains("Full Code")
      )
  ) %>%
  # Combine data now that all variable names and types are consistent
  bind_rows() %>%
  # Remove rows with all NA's
  filter(!if_all(everything(), is.na)) %>%
  # Remove Microcystis surface tow data
  filter(is.na(SampleCode) | !str_detect(SampleCode, "Tow|TOW")) %>%
  # Convert SampleDate to date and calculate Organisms/mL
  mutate(
    Date = date(SampleDate),
    OrganismsPerMl = Factor * Count
  ) %>%
  # Rename and remove some variables
  rename(Station = StationCode) %>%
  select(-c(SampleDate, Factor, SampleCode))

# Combine recent data to earlier data
df_phyto_all <- bind_rows(df_phyto_early_c, df_phyto_recent)


# 3. Clean All Data -------------------------------------------------------

# Assign EDB regions to EMP stations
df_region_emp <- df_coord_emp %>%
  select(Station, Latitude, Longitude) %>%
  filter(!if_any(c(Latitude, Longitude), is.na)) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(st_make_valid(EDBdata:::sf_edb_reg), join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Assign "Outside" to stations without region assignments
  replace_na(list(Region = "Outside"))

# Assign regions used for the HABs/Weeds report to EMP stations
df_hab_region_emp <- df_coord_emp %>%
  select(Station, Latitude, Longitude) %>%
  drop_na(Latitude, Longitude) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(EDBdata:::sf_hab_reg, join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Assign "Outside" to stations without region assignments
  replace_na(list(Region = "Outside"))

# Three genera need to be added to the taxonomy table: Mayamaea, Acanthoceras, and Lindavia
  # I looked up these three genera in AlgaeBase: https://www.algaebase.org/
  # Also adding a placeholder for unknown Genera
df_add_genera <-
  tribble(
    ~Genus, ~AlgalType,
    "Mayamaea", "Pennate Diatom",
    "Acanthoceras", "Centric Diatom",
    "Lindavia", "Centric Diatom",
    "Unknown", "Unknown"
  )

# Prepare phytoplankton classification table to be joined to data
df_phyto_taxonomy_c1 <- df_phyto_taxonomy %>%
  rename(AlgalType = `Algal Type`) %>%
  # We are not including Kingdom, Phylum, and Class in this table for now
    # since they are not up to date with recent changes in the higher taxonomy
    # of phytoplankton
  distinct(Genus, AlgalType) %>%
  filter(
    !(Genus == "Leptocylindrus" & AlgalType =="Centric diatom"),
    Genus != "Unknown"
  ) %>%
  # Add additional genera to the classification table
  bind_rows(df_add_genera)

# Finish cleaning up the phytoplankton data
phyto_hab <- df_phyto_all %>%
  mutate(
    # Fix an erroneous Date
    Date = if_else(Date == "2022-11-17", as_date("2021-11-17"), Date),
    # Create Datetime variable in PST
    Datetime = ymd_hm(
      paste0(Date, " ", hour(SampleTime), ":", minute(SampleTime)),
      tz = "Etc/GMT+8"
    ),
    # Fix a few erroneous Station names:
      # Stations C3A-Hood and C3A-HOOD represent station C3A,
      # Stations NZ328 and NZ542 are most likely typos and most likely represent NZ325 and NZS42
    Station = case_when(
      Station == "C3A-HOOD" ~ "C3A",
      Station == "C3A-Hood" ~ "C3A",
      Station == "NZ328" ~ "NZ325",
      Station == "NZ542" ~ "NZS42",
      TRUE ~ Station
    ),
    # Add variable for year
    Year = year(Date)
  ) %>%
  # Add EDB regions to all phytoplankton data
  left_join(df_hab_region_emp, by = "Station") %>%
  # Remove data for stations outside of the EDB regions keeping NA values in
    # Region to catch errors during testing
    # Also remove the EZ stations since they are not fixed
  filter(
    Region != "Outside" | is.na(Region),
    !str_detect(Station, "^EZ")
  ) %>%
  # Add taxonomic information to all phytoplankton data
  left_join(df_phyto_taxonomy_c1, by = "Genus") %>%
  # Reorder columns
  select(
    Station,
    Region,
    Year,
    Date,
    Datetime,
    Taxon,
    Genus,
    Species,
    AlgalType,
    Count,
    OrganismsPerMl
  )

phyto_hab2 <- read_csv(here("AllEMPphyto.csv")) %>%
  select(-c(...1, Stratum)) %>%
  rename(Region = Stratum2, Datetime = DateTime) %>%
  mutate(Datetime = force_tz(Datetime, tzone = "Etc/GMT+8")) %>%
  drop_na(Region) %>%
  arrange(Count) %>%
  group_by(Station, Region, Year, Date, Datetime, Taxon) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  arrange(Datetime)

phyto_hab %>% distinct(Region, Station) %>% arrange(Region, Station)
phyto_hab2 %>% distinct(Region, Station) %>% arrange(Region, Station)

phyto_hab <- phyto_hab %>%
  filter(Date <= max(phyto_hab2$Date), !Date %in% ymd("2020-12-03", "2020-12-04")) %>%
  arrange(Count) %>%
  group_by(Station, Region, Year, Date, Datetime, Taxon) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  arrange(Datetime)


setequal(phyto_hab, phyto_hab2)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing phytoplankton community data for the EDB analysis as csv file
  # for easier diffing
phyto_edb %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv(here("data-raw/Final/phyto_edb.csv"))

# Save final data set containing phytoplankton community data for the EDB analysis as objects
  # in the data package
usethis::use_data(phyto_edb, overwrite = TRUE)

