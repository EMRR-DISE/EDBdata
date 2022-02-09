# Code to prepare phytoplankton data set for the Emergency Drought Barrier analysis:
# 1) `phyto_edb` - Phytoplankton community data collected by DWR-EMP from
  # 2014-2021 for the stations within the designated EDB regions

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(sf)


# 1. Import Data ----------------------------------------------------------

# Import earlier phytoplankton data
df_phyto_early <-
  read_csv(
    "data-raw/Phyto_data/EMP_phyto_data.csv",
    col_types = "-DTcdcccd--d-----------"
  )

# Import phytoplankton data collected from Dec 2020 - Oct 2021:
# Create a vector of all file paths for the recent data
fp_phyto_recent <- dir("data-raw/Phyto_data", pattern = "202[01]\\.xlsx$", full.names = TRUE)

# Import recent phytoplankton data into a list where each element represents a file
lst_phyto_recent <- map(fp_phyto_recent, read_excel)

# Import phytoplankton classification table (copied from the DroughtSynthesis repository)
df_phyto_taxonomy <- read_excel("data-raw/Phyto_data/Phyto Classification.xlsx")

# Import the polygon shapefile for the EDB regions
sf_edb_reg <- read_sf("data-raw/Spatial_data/EDB_Regions.shp") %>% select(Region = Regions)

# Import EMP station coordinates from EDI
df_coord_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")


# 2. Clean and Combine Data -----------------------------------------------

# Earlier data:
df_phyto_early_c <- df_phyto_early %>%
  # filter to years 2014-2020, don't include Dec 2020 through 2021 since there is some overlap
  filter(SampleDate >= "2014-01-01" & SampleDate < "2020-12-01") %>%
  # Create DateTime variable in PST
  mutate(
    DateTime = ymd_hm(
      paste0(SampleDate, " ", hour(SampleTime), ":", minute(SampleTime)),
      tz = "Etc/GMT+8"
    )
  ) %>%
  # Reorder and rename variables
  select(
    Station = StationCode,
    Date = SampleDate,
    DateTime,
    Taxon,
    Genus,
    Species,
    Count,
    OrganismsPerMl = Organisms_per_mL
  )

# Recent data (Dec 2020 - Oct 2021):
# Create a vector of variable names to keep in each list element
vec_vars_keep <-
  c(
    "SampleDate",
    "SampleTime",
    "StationCode",
    "Taxon",
    "Genus",
    "Species",
    "Factor",
    "Count",
    "Unit Abundance (# of Natural Units)"
  )

df_phyto_recent <- lst_phyto_recent %>%
  # Select and rename variables in each element
  map(
    ~ select(.x, any_of(vec_vars_keep)) %>%
      rename(Count = contains("Abundance"))
  ) %>%
  # Combine data now that all variable names and types are consistent
  bind_rows() %>%
  # Remove rows with all NA's
  filter(!if_all(everything(), is.na)) %>%
  mutate(
    # Create DateTime variable in PST
    Date = date(SampleDate),
    DateTime = ymd_hm(
      paste0(Date, " ", hour(SampleTime), ":", minute(SampleTime)),
      tz = "Etc/GMT+8"
    ),
    # Calculate Organisms/mL
    OrganismsPerMl = Factor * Count
  ) %>%
  select(
    Station = StationCode,
    Date,
    DateTime,
    Taxon,
    Genus,
    Species,
    Count,
    OrganismsPerMl
  )

# Combine recent data to earlier data
df_phyto_all <- bind_rows(df_phyto_early_c, df_phyto_recent)
# To add to all data: Year, Month?, EDB Region


# 3. Clean All Data -------------------------------------------------------

# Assign EDB regions to EMP stations
df_region_emp <- df_coord_emp %>%
  select(Station, Latitude, Longitude) %>%
  filter(!if_any(c(Latitude, Longitude), is.na)) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(st_make_valid(sf_edb_reg), join = st_intersects) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Assign "Outside" to stations without region assignments
  replace_na(list(Region = "Outside"))

# Add EDB regions to all phytoplankton data
df_phyto_all_c1 <- df_phyto_all %>% left_join(df_region_emp, by = "Station")

# Look for any Stations without region assignments
df_phyto_all_c1 %>% filter(is.na(Region)) %>% count(Station)
# The EZ stations are all without regions because they are not fixed stations -
  # We won't include the data for the EZ stations.
# Stations C3A-Hood and C3A-HOOD are without regions but represent station C3A -
  # C3A is located outside of the area of interest, so we won't include the data for these
  # two stations.
# Additionally, two other stations (NZ328, NZ542) are also without regions -
  # These are most likely typos. The two stations these most likely represent (NZ325, NZS42)
  # are located outside of the area of interest. Therefore, we won't include the data
  # for these two stations as well.

# Remove data for stations outside of the EDB regions
df_phyto_all_c2 <- df_phyto_all_c1 %>% filter(Region != "Outside")

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
  add_row(
    Genus = "Unknown",
    AlgalType = "Unknown"
  )

# Add taxonomic information to all phytoplankton data
df_phyto_all_c3 <- df_phyto_all_c2 %>% left_join(df_phyto_taxonomy_c1, by = "Genus")

# Look for any genera without taxonomic information
df_phyto_all_c3 %>% filter(is.na(AlgalType)) %>% distinct(Genus)
# Three genera need to be added to the taxonomy table: Mayamaea, Acanthoceras, and Lindavia
  # I looked up these three genera in AlgaeBase: https://www.algaebase.org/
df_add_genera <-
  tribble(
    ~Genus, ~AlgalType,
    "Mayamaea", "Pennate Diatom",
    "Acanthoceras", "Centric Diatom",
    "Lindavia", "Centric Diatom"
  )

# Add two info for additional genera to the phytoplankton classification table
df_phyto_taxonomy_c2 <- bind_rows(df_phyto_taxonomy_c1, df_add_genera)

# Add updated taxonomic information to all phytoplankton data
df_phyto_all_c4 <- df_phyto_all_c2 %>% left_join(df_phyto_taxonomy_c2, by = "Genus")

# Look for any genera without taxonomic information
df_phyto_all_c4 %>% filter(is.na(AlgalType)) %>% distinct(Genus)
# All genera are accounted for now

# Add variable for year to all phytoplankton data and reorder columns
phyto_edb <- df_phyto_all_c4 %>%
  mutate(Year = year(Date)) %>%
  select(
    Station,
    Region,
    Year,
    Date,
    DateTime,
    Taxon,
    Genus,
    Species,
    AlgalType,
    Count,
    OrganismsPerMl
  )

# Look for NA values in each variable
for (n in names(phyto_edb)) {
  print(n)
  phyto_edb %>%
    summarize(num_na = sum(is.na(.data[[n]]))) %>%
    pull(num_na) %>%
    print()
}

# Save final data set containing phytoplankton community data for the EDB analysis as csv file
  # for easier diffing
write_csv(phyto_edb, "data-raw/Final/phyto_edb.csv")

# Save final data set containing phytoplankton community data for the EDB analysis as objects
  # in the data package
usethis::use_data(phyto_edb, overwrite = TRUE)

