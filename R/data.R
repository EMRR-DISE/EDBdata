#' @title Counts of pixel values within Cyano Index categories for four open
#'   water regions in the Delta
#' @description A data set containing counts of pixel values derived from HAB
#'   satellite data within 4 Cyano Index categories for four open water regions
#'   in the upper San Francisco Estuary (Delta): Franks Tract, Mildred Island,
#'   Clifton Court Forebay, and Liberty Island. Summary statistics cover the
#'   summer through fall (June-Oct) in 2019-2021. The Cyano Index categories
#'   (Low, Moderate, High, and Very High) were based on WHO recreational
#'   guidance level thresholds. The data set also includes counts of pixels that
#'   were below the detection limit for the imagery processing method and counts
#'   of pixels that were either invalid or missing.
#'
#'   Counts only include pixels that were completely within the polygons for the
#'   four regions. In addition, the data set only includes date-region
#'   combinations for when there were greater than 25% valid pixels within the
#'   region. Zonal statistics were calculated from satellite data downloaded
#'   from the Harmful Algal Blooms Analysis Tool (see Source section below).
#'   Used in the analyses for the Emergency Drought Barrier (EDB).
#'
#' @format data frame with 800 rows and 8 columns
#' \describe{
#'   \item{Date}{Date of satellite imagery in yyyy-mm-dd}
#'   \item{Region}{Name of the polygon region. Is one of the following: Franks
#'     Tract, Mildred Island, Clifton Court Forebay, or Liberty Island.}
#'   \item{NonDetect}{Count of the number of pixels below the detection limit
#'     for the method which is less than or equal to a Cyano Index of 0.0000631}
#'   \item{Low}{Count of the number of pixels in the Low Cyano Index category
#'     defined as having a Cyano Index of greater than 0.00006310 but less than
#'     0.0002}
#'   \item{Moderate}{Count of the number of pixels in the Moderate Cyano Index
#'     category defined as having a Cyano Index greater than or equal to 0.0002
#'     but less than 0.001}
#'   \item{High}{Count of the number of pixels in the High Cyano Index category
#'     defined as having a Cyano Index greater than or equal to 0.001 but less
#'     than 0.01}
#'   \item{VeryHigh}{Count of the number of pixels in the Very High Cyano Index
#'     category defined as having a Cyano Index greater than or equal to 0.01. The
#'     maximum detectable level for the Cyano Index is 0.06327.}
#'   \item{InvalidOrMissing}{Count of the number of pixels that were either
#'     invalid (land, adjacency) or missing (clouds, no data, or some other
#'     reason)}
#' }
#'
#' @source San Francisco Estuary Institute (SFEI). Harmful Algal Blooms Analysis
#'   Tool. <https://fhab.sfei.org/>
"hab_sat_ow_delta"

#' @title Phytoplankton community data for stations within the EDB regions
#' @description A data set containing phytoplankton community data collected by
#'   DWR's Environmental Monitoring Program from 2014-2021 for the stations
#'   within the designated Emergency Drought Barrier (EDB) regions. Used in the
#'   analyses for the EDB.
#'
#' @format data frame with 4,353 rows and 11 columns
#' \describe{
#'   \item{Station}{Location where sample was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: Central Delta, Sacramento, or
#'     San Joaquin.}
#'   \item{Year}{Calendar year of the sample}
#'   \item{Date}{Calendar date of the sample in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the sample in PST}
#'   \item{Taxon}{Taxonomic designation of the organism as "Genus species"}
#'   \item{Genus}{Genus designation of the organism}
#'   \item{Species}{Species designation of the organism}
#'   \item{AlgalType}{General Algal Type classification of the organism}
#'   \item{Count}{Number of organisms found in the sample}
#'   \item{OrganismsPerMl}{Estimated number of organisms per milliliter of
#'     sample}
#' }
#'
#' @source Acquired from DWR's Environmental Monitoring Program through direct
#'   data requests
"phyto_edb"

#' @title Phytoplankton community data of potentially toxic Cyanobacteria in the
#'   upper San Francisco Estuary
#' @description A data set containing phytoplankton community data of
#'   potentially toxic Cyanobacteria collected by DWR's Environmental Monitoring
#'   Program at various locations in the upper San Francisco Estuary (Delta)
#'   from 2014-2021. Used in the analyses for the Spring-Summer version of the
#'   2022 HABs/Weeds report for the Emergency Drought Barrier.
#'
#' @format data frame with 228 rows and 11 columns
#' \describe{
#'   \item{Station}{Location where sample was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: East Delta, Franks, Lower Sac,
#'     Lower SJ, OMR, South Delta, or Upper Sac.}
#'   \item{Year}{Calendar year of the sample}
#'   \item{Date}{Calendar date of the sample in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the sample in PST}
#'   \item{Taxon}{Taxonomic designation of the organism as "Genus species"}
#'   \item{Genus}{Genus designation of the organism. The genus *Anabaena* is
#'     grouped with *Dolichospermum*.}
#'   \item{Species}{Species designation of the organism}
#'   \item{AlgalType}{General Algal Type classification of the organism}
#'   \item{Count}{Number of organisms found in the sample}
#'   \item{OrganismsPerMl}{Estimated number of organisms per milliliter of
#'     sample}
#' }
#'
#' @source Acquired from DWR's Environmental Monitoring Program through direct
#'   data requests
"phyto_hab"

#' @title Daily values of continuous chlorophyll fluorescence data for stations
#'   within the EDB regions
#' @description A data set containing daily average and median values of
#'   continuous chlorophyll fluorescence measured in the water in-situ at the
#'   stations within the designated Emergency Drought Barrier (EDB) regions.
#'   Data was collected by DWR and USGS from 2020-2021. Used in the analyses for
#'   the EDB.
#'
#'   All chlorophyll data collected by DWR_NCRO (DWR's North Central Region
#'   Office) was downloaded from the Water Data Library (WDL). Data collected by
#'   DWR_CEMP (DWR's Continuous Environmental Monitoring Program) was acquired
#'   through direct data requests. And data from USGS were downloaded directly
#'   from the National Water Information System (NWIS) using the `dataRetrieval`
#'   R package. Some of the data from USGS is considered provisional.
#'
#' @format data frame with 7,612 rows and 7 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Is one of the following: DWR_CEMP
#'     (DWR's Continuous Environmental Monitoring Program), DWR_NCRO (DWR's North
#'     Central Region Office), or USGS}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: Central Delta, Sacramento, or
#'     San Joaquin.}
#'   \item{Year}{Calendar year of the measurement}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{AvgChla}{Daily average chlorophyll fluorescence value in micrograms
#'     per liter}
#'   \item{MedianChla}{Daily median chlorophyll fluorescence value in micrograms
#'     per liter}
#' }
#'
#' @source
#' * California Department of Water Resources (DWR) Water Data Library (WDL):
#'   <https://wdl.water.ca.gov/WaterDataLibrary/>
#' * USGS National Water Information System (NWIS):
#'   <https://nwis.waterdata.usgs.gov/nwis>
"cont_chla_daily"

#' @title Discrete nutrient and chlorophyll-a water concentrations in the upper
#'   San Francisco Estuary
#' @description A data set containing concentrations of chlorophyll-a and three
#'   nutrient parameters (Dissolved Ammonia, Dissolved Nitrate + Nitrite, and
#'   Dissolved Ortho-phosphate) in water samples collected at various locations
#'   in the San Francisco Estuary (Delta) east of Chipps Island for 2014-2021.
#'   Used in the analyses for the Spring-Summer version of the 2022 HABs/Weeds
#'   report for the Emergency Drought Barrier.
#'
#'   Most of the nutrient and chlorophyll-a data from DWR_EMP (DWR's
#'   Environmental Monitoring Program) and all the data from USGS_SFBS (USGS's
#'   San Francisco Bay Water Quality Survey) are from the `discretewq` data
#'   package. DWR_EMP data collected in 2021 was acquired through a direct data
#'   request and should be considered provisional. All data collected by
#'   DWR_NCRO (DWR's North Central Region Office) was also acquired through a
#'   direct data request; however, most of this data is available for download
#'   from the Water Data Library (WDL). Some of the most recent DWR_NCRO data
#'   collected in 2021 should be considered provisional. Data from USGS_CAWSC
#'   (USGS's California Water Science Center) were downloaded directly from the
#'   Water Quality Portal using the `dataRetrieval` R package. Some of the data
#'   from USGS_CAWSC is also considered provisional.
#'
#'   This data set contains only one sample or measurement per `Station` and
#'   `Date` and only includes stations where all three nutrient parameters and
#'   chlorophyll-a have been collected during their periods of record from
#'   2014-2021. This data set excludes any nitrate + nitrite and ortho-phosphate
#'   data that have modified Z-scores greater than 15 grouped by `Region`.
#'   Outlier removal was not performed on the ammonia and chlorophyll-a data
#'   because all the higher values appeared to be real based on best
#'   professional judgment.
#'
#'   There were a few instances where nutrient values were below the reporting
#'   limit (RL) but the RL values were greater than the 75th quantile of the
#'   overall data. These values were excluded from this data set. In addition,
#'   some of the nutrient values that were below the RL in the USGS_CAWSC data
#'   set did not have RL values provided from the data source. In these
#'   instances, this data set uses the most common RL for each nutrient
#'   parameter and laboratory method.
#'
#' @format data frame with 3,133 rows and 15 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Is one of the following: DWR_EMP
#'     (DWR's Environmental Monitoring Program), DWR_NCRO (DWR's North Central
#'     Region Office), USGS_SFBS (USGS's San Francisco Bay Water Quality Survey),
#'     or USGS_CAWSC (USGS's California Water Science Center)}
#'   \item{Station}{Location where sample was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: Cache/Liberty, East Delta,
#'     Franks, Lower Sac, Lower SJ, OMR, South Delta, or Upper Sac.}
#'   \item{Date}{Calendar date of the sample in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the sample in PST}
#'   \item{DissAmmonia_Sign}{A symbol representing whether the Dissolved Ammonia
#'     value is below the reporting limit or above it. Contains one of four
#'     symbols: "<", "< (estimated)", "=", or "= (unreliable)". See the section
#'     below for more information about what these symbols represent.}
#'   \item{DissAmmonia}{Dissolved Ammonia concentration measured in the water
#'     sample in milligrams per liter as N}
#'   \item{DissNitrateNitrite_Sign}{A symbol representing whether the Dissolved
#'     Nitrate + Nitrite value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate + Nitrite concentration
#'     measured in the water sample in milligrams per liter as N}
#'   \item{DissOrthophos_Sign}{A symbol representing whether the Dissolved
#'     Ortho-phosphate value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate concentration measured in
#'     the water sample in milligrams per liter as P}
#'   \item{Chlorophyll_Sign}{A symbol representing whether the chlorophyll-a
#'     value is below the reporting limit or above it. Contains one of three
#'     symbols: "<", "= (estimated)", or "=". See the section below for more
#'     information about what these symbols represent.}
#'   \item{Chlorophyll}{Chlorophyll-a concentration in micrograms per liter}
#' }
#'
#' @section More information about the `_Sign` columns:
#' For the columns that have the _Sign suffix, the symbols in these columns
#' represent five conditions of the value contained in its corresponding result
#' column.
#' * "<" - The value is below the Reporting Limit (RL) with the value in the
#' corresponding result column equal to the RL.
#' * "< (estimated)" - The value is below the RL, but the RL is unknown. The
#' value in the corresponding result column is the estimated RL value.
#' * "=" - The value is above the RL with the value in the corresponding result
#' column equal to the actual value measured by the laboratory. An `NA` value
#' in the corresponding result column indicates that the value is missing or
#' wasn't collected.
#' * "= (estimated)" - The value in the corresponding result column is estimated
#' (extrapolated at low end)
#' * "= (unreliable)" - The value in the corresponding result column was
#' determined to be an outlier and is excluded from the data set.
#'
#' @source
#' * `discretewq` data package (version 2.3.2):
#'   <https://github.com/sbashevkin/discretewq/tree/v2.3.2>
#' * California Department of Water Resources (DWR) Water Data Library (WDL):
#'   <https://wdl.water.ca.gov/WaterDataLibrary/>
#' * Water Quality Portal hosted by the National Water Quality Monitoring
#'   Council: <https://www.waterqualitydata.us/>
"disc_nutr_chla"

#' @title Cyanotoxin water concentrations in the upper San Francisco Estuary
#' @description A data set containing Cyanotoxin concentrations in whole-water
#'   grab samples collected at various locations in the upper San Francisco
#'   Estuary (Delta). The majority of the available cyanotoxin data was
#'   collected in 2021, so this data set only includes data from 2021 for all
#'   stations except for Big Break Regional Shoreline. Big Break has a longer
#'   monitoring history, so all data collected at this location from 2015–2021
#'   are included in this data set. Used in the analyses for the Spring-Summer
#'   version of the 2022 HABs/Weeds report for the Emergency Drought Barrier.
#'
#' @format data frame with 719 rows and 11 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Is one of the following: CVRWQCB
#'     (Central Valley Regional Water Quality Control Board), DWR (DWR's State
#'     Water Project), EastBay (East Bay Regional Park District), Nautilus
#'     (Nautilus Data Technologies), Preece (Preliminary results from Preece et
#'     al.), or USGS (Special study by USGS and DWR).}
#'   \item{Station}{Location where sample was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: Cache Slough/Liberty Island,
#'     Clifton Court, Lower Sacramento, Lower San Joaquin, San Joaquin, SDWSC,
#'     South Delta, Upper Sacramento, or Vernalis.}
#'   \item{Year}{Calendar year of the sample}
#'   \item{Month}{Numeric calendar month of the sample where 1 represents
#'     January and 12 represents December}
#'   \item{Date}{Calendar date of the sample in yyyy-mm-dd}
#'   \item{Analyte}{Name of Cyanotoxin measured in the water sample. Is one of
#'     the following: Anabaenopeptins, Anatoxins, Microcystins, or Saxitoxins.}
#'   \item{Result}{Concentration of `Analyte` measured in the water sample in
#'     micrograms per liter}
#'   \item{Result_Sign}{A symbol representing whether the Cyanotoxin value in
#'     `Result` is below a lower detection limit, equal to the reported value, or
#'     above an upper detection limit. Contains one of three symbols: "ND", "=",
#'     or ">". "ND" indicates that the value was below a lower detection limit.
#'     All values below a lower detection limit in this data set were replaced
#'     with zeros. "=" indicates that the value was detected by the laboratory
#'     with the value in the corresponding `Result` column equal to the actual
#'     value measured by the laboratory. ">" indicates that the value was above an
#'     upper detection limit with the with the value in the corresponding `Result`
#'     column equal to the upper detection limit.}
#' }
#'
#' @source Acquired through direct data requests
"hab_toxins"

#' @title *Microcystis* visual index values and water quality measurements in
#'   the upper San Francisco Estuary
#' @description A data set containing *Microcystis* visual index, secchi depth,
#'   and water temperature data collected at various locations in the upper San
#'   Francisco Estuary (Delta) from 2007-2021. Used in the analyses for the
#'   Spring-Summer version of the 2022 HABs/Weeds report for the Emergency
#'   Drought Barrier.
#'
#'   All *Microcystis* visual index and water quality measurement data from
#'   DWR_EMP (DWR's Environmental Monitoring Program), FMWT (CDFW's Fall
#'   Midwater Trawl survey), and STN (CDFW's Summer Townet survey)  are from the
#'   `discretewq` data package. All data collected by DWR_NCRO (DWR's North
#'   Central Region Office) was acquired through a direct data request; however,
#'   most of this data is available for download from the Water Data Library
#'   (WDL).
#'
#' @format data frame with 7,339 rows and 11 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Is one of the following: DWR_EMP
#'     (DWR's Environmental Monitoring Program), DWR_NCRO (DWR's North Central
#'     Region Office), FMWT (CDFW's Fall Midwater Trawl survey), or STN (CDFW's
#'     Summer Townet survey)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Is one of the following: Cache/Liberty, East Delta,
#'     Franks, Lower Sac, Lower SJ, OMR, South Delta, or Upper Sac.}
#'   \item{Year}{Calendar year of the measurement}
#'   \item{Month}{Numeric calendar month of the measurement where 1 represents
#'     January and 12 represents December}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{Microcystis}{*Microcystis* visual index value on a qualitative scale
#'     from 1 to 5, where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very
#'     high}
#'   \item{Secchi}{Secchi depth in centimeters}
#'   \item{Temperature}{Water temperature in degrees Celsius}
#' }
#'
#' @source `discretewq` data package (version 2.3.2):
#'   <https://github.com/sbashevkin/discretewq/tree/v2.3.2>
"mc_vis_index_wq"

#' @title CyanoHAB incident reports in the upper San Francisco Estuary
#' @description A data set containing CyanoHAB incident reports in the upper San
#'   Francisco Estuary (Delta) in 2021. Most of this data is from the State
#'   Water Board's HAB Incident Report Map. This is combined with the maximum
#'   advisory levels for Microcystins that occurred at each station in 2021 from
#'   the cyanotoxin concentration data in [`hab_toxins`]. The [advisory
#'   levels](https://mywaterquality.ca.gov/monitoring_council/cyanohab_network/docs/2016/appendix_a_2016_1.pdf)
#'    were developed by the California Cyanobacteria and Harmful Algal Bloom
#'   Network (CCHAB). Used in the analyses for the Spring-Summer version of the
#'   2022 HABs/Weeds report for the Emergency Drought Barrier.
#'
#' @format data frame with 50 rows and 4 columns
#' \describe{
#'   \item{Date}{Calendar date of the incident in yyyy-mm-dd}
#'   \item{Latitude}{Latitude of incident location in Decimal Degrees (WGS 84
#'     Datum)}
#'   \item{Longitude}{Longitude of incident location in Decimal Degrees (WGS 84
#'     Datum)}
#'   \item{Advisory}{CyanoHAB advisory level for the incident as [defined by
#'     CCHAB](https://mywaterquality.ca.gov/monitoring_council/cyanohab_network/docs/2016/appendix_a_2016_1.pdf).
#'     Is one of the following: Caution, Warning, or Danger.}
#' }
#'
#' @source HAB Incident Reports Map maintained by the State Water Resources
#'   Control Board:
#'   <https://mywaterquality.ca.gov/habs/where/freshwater_events.html>
"hab_incidents"

#' @title Daily averages of continuous water quality data for stations near
#'   Franks Tract
#' @description A data set containing daily averages of various continuous water
#'   quality parameters measured in the water in-situ at the stations near
#'   Franks Tract located in the upper San Francisco Estuary (Delta). Water
#'   quality parameters include Dissolved Oxygen, pH, and Chlorophyll
#'   Fluorescence. Data was collected by DWR and USGS from 2015-2021. Used in
#'   the analyses for the Spring-Summer version of the 2022 HABs/Weeds report
#'   for the Emergency Drought Barrier.
#'
#'   All water quality data collected by DWR_NCRO (DWR's North Central Region
#'   Office) was downloaded from the Water Data Library (WDL). Data collected by
#'   DWR_CEMP (DWR's Continuous Environmental Monitoring Program) was acquired
#'   through direct data requests. And data from USGS were downloaded from the
#'   National Water Information System (NWIS).
#'
#' @format data frame with 13,273 rows and 6 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Is one of the following: DWR_CEMP
#'     (DWR's Continuous Environmental Monitoring Program), DWR_NCRO (DWR's North
#'     Central Region Office), or USGS}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{DO}{Daily average dissolved oxygen in milligrams per liter}
#'   \item{pH}{Daily average pH in pH units}
#'   \item{Chla}{Daily average chlorophyll fluorescence in micrograms per liter}
#' }
#'
#' @source
#' * California Department of Water Resources (DWR) Water Data Library (WDL):
#'   <https://wdl.water.ca.gov/WaterDataLibrary/>
#' * USGS National Water Information System (NWIS):
#'   <https://nwis.waterdata.usgs.gov/nwis>
"cont_wq_daily_avg"

#' @title Daily maximums of continuous water quality data for the Franks Tract
#'   station (FRK)
#' @description A data set containing daily maximum values of various continuous
#'   water quality parameters measured in the water in-situ at the the Franks
#'   Tract station (FRK) located in the upper San Francisco Estuary (Delta).
#'   Water quality parameters include Dissolved Oxygen (as percent saturation),
#'   pH, and Chlorophyll Fluorescence. Data was collected from 2015-2021. Used
#'   in the analyses for the Spring-Summer version of the 2022 HABs/Weeds report
#'   for the Emergency Drought Barrier.
#'
#'   Water quality data was collected by DWR_CEMP (DWR's Continuous
#'   Environmental Monitoring Program) and was acquired through direct data
#'   requests.
#'
#' @format data frame with 2,376 rows and 4 columns
#' \describe{
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{DO_PerSat}{Daily maximum dissolved oxygen in percent saturation.
#'     Values were converted from the recorded dissolved oxygen concentration in
#'     mg/L to percent saturation using the USGS method implemented from the
#'     [`rMR`](https://cran.r-project.org/web/packages/rMR/index.html) package.}
#'   \item{pH}{Daily maximum pH in pH units}
#'   \item{Chla}{Daily maximum chlorophyll fluorescence in micrograms per liter}
#' }
#'
#' @source Acquired through direct data requests
"cont_wq_daily_max_frk"
