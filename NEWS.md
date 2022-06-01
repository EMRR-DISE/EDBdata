# EDBdata (development version)

* Renamed columns for the daily mean and median values in `cont_chla_daily` to `AvgChla` and `MedianChla`, respectively, to have consistent naming convention across all drought-related data sets
* Updated regions in `hab_nutr_chla_mvi` to be consistent with the regions used in the analysis. Changes to the regions includes:
  * Removed regions and stations west of Chipps Island (this removes the "Suisun Bay" region and the three stations within it)
  * Merged the "SDWSC" region with "Cache/Liberty"
  * Divided the "South Delta" region into Franks Tract (Franks), Old/Middle River (OMR), and South Delta
  * Using the new regions also added two USGS stations located in the Toe Drain to the data set: 11455139 and 11455140
* Added `hab_toxins` - cyanotoxin concentrations in whole-water grab samples collected at various locations in the upper San Francisco Estuary (Delta) in 2021

# EDBdata 0.4.0

* Updates to `phyto_edb` data set:
  * Added November - December 2021 data so it contains all data collected in 2021
  * Removed *Microcystis* surface tow data collected in September 2021
  * Renamed `DateTime` column to `Datetime` to have a consistent naming convention across all drought-related data sets
* Added a `Source` column to the `cont_chla_daily` data set
* Added regions to `hab_nutr_chla_mvi` which were used in the analysis of this data set
* Added two regions (Clifton Court Forebay and Liberty Island) to the `hab_sat_fr_mil` data set. This data set is now called `hab_sat_ow_delta` to better describe its contents.
* Calculated and added an average Cyano Index value (`AvgCI`) for each region and date in the `hab_sat_ow_delta` data set
* Renamed columns in `cont_chla_daily` and `hab_sat_ow_delta` to upper camel case (UpperCamelCase) to have consistent naming convention across all drought-related data sets

# EDBdata 0.3.0

* Added `hab_nutr_chla_mvi` - discrete nutrient concentrations (Dissolved Ammonia, Dissolved Nitrate + Nitrite, and Dissolved Ortho-phosphate), chlorophyll-a concentrations, and *Microcystis* visual index values in the San Francisco Estuary east of Suisun Marsh for 2014-2021

# EDBdata 0.2.1

* Fixed a timezone issue with the continuous chlorophyll data downloaded from USGS to make it PST instead of UTC. This changed the daily mean and median values in `cont_chla_daily` for the MDM and SJJ stations. While the plots of the daily median chlorophyll data in the current EDB report are from the USGS data in UTC, these updated values should be used in any further plots and analysis of this data.

# EDBdata 0.2.0

* Added `phyto_edb` - phytoplankton community data collected by DWR-EMP from 2014-2021 for the stations within the designated EDB regions
* Added `cont_chla_daily` - daily average and median values of continuous chlorophyll fluorescence data collected by DWR and USGS from 2020-2021 for the stations within the designated EDB regions
* Renamed the `sat_ci_count_fr_mil` data set to `hab_sat_fr_mil` to be more descriptive
* Added packages used for cleaning data to Suggests
* Added unit tests for all data sets in package
* Added the polygon shapefile for the EDB regions to internal data

# EDBdata 0.1.0

* Finished processing HAB satellite data for Franks Tract and Mildred Island which is in the `sat_ci_count_fr_mil` data object. This is the data used in the November draft of the EDB report.
* Added a `NEWS.md` file to track changes to the package.
