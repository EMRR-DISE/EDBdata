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
