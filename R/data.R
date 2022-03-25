#' @title Summary of Cyano Index values derived from HAB satellite data for
#'   Franks Tract and Mildred Island
#' @description A data set containing counts and averages of Cyano Index values
#'   from HAB satellite data downloaded from the Harmful Algal Blooms Analysis
#'   Tool (see Source section below). Summary statistics cover the spring
#'   through late fall (May-Dec) in 2020 and 2021 for two open water regions in
#'   the upper San Francisco Estuary: Franks Tract and Mildred Island. Used in
#'   the analyses for the Emergency Drought Barrier (EDB).
#'
#'   The data set contains averages of Cyano Index for each region and date in
#'   addition to pixel counts within 4 Cyano Index categories. The Cyano Index
#'   categories (Low, Moderate, High, and Very High) were based on WHO
#'   recreational guidance level thresholds. The data set also includes counts
#'   of pixels that were below the detection limit for the imagery processing
#'   method and counts of pixels that were either invalid or missing. Zeros were
#'   substituted for the values below the detection limit and invalid or missing
#'   pixels were excluded when calculating the average.
#'
#'   Counts and averages only include pixels that were completely within the
#'   polygons for the two regions. In addition, the data set only includes
#'   date-region combinations for when there were greater than 25% valid pixels
#'   within the region.
#'
#' @format data frame with 329 rows and 9 columns
#' \describe{
#'   \item{Date}{Date of satellite imagery}
#'   \item{Name}{Name of the polygon region. Either Franks Tract or Mildred
#'     Island.}
#'   \item{AvgCI}{Average Cyano Index value of all valid pixels within the
#'     region. The Cyano Index is a exponential and unitless metric.}
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
"hab_sat_fr_mil"

#' @title Phytoplankton community data from 2014-2021 for stations within the
#'   EDB regions
#' @description A data set containing phytoplankton community data collected by
#'   DWR's Environmental Monitoring Program from 2014-2021 for the stations
#'   within the designated Emergency Drought Barrier (EDB) regions. Used in the
#'   analyses for the EDB.
#'
#' @format data frame with 4,353 rows and 11 columns
#' \describe{
#'   \item{Station}{Location where sample was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Either Central Delta, Sacramento, or San Joaquin.}
#'   \item{Year}{Calendar year of the sample}
#'   \item{Date}{Calendar date of the sample}
#'   \item{Datetime}{Date and time of the sample in PST}
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

#' @title Daily values of continuous chlorophyll fluorescence data from
#'   2020-2021
#' @description A data set containing daily average and median values of
#'   continuous chlorophyll fluorescence data collected by DWR and USGS from
#'   2020-2021 for the stations within the designated Emergency Drought Barrier
#'   (EDB) regions. Used in the analyses for the EDB.
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
#'   \item{Source}{Name of the source dataset. Either DWR_CEMP (DWR's Continuous
#'     Environmental Monitoring Program), DWR_NCRO (DWR's North Central Region
#'     Office), or USGS}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Either Central Delta, Sacramento, or San Joaquin.}
#'   \item{Year}{Calendar year of the value}
#'   \item{Date}{Calendar date of the value}
#'   \item{Chla_avg}{Daily average chlorophyll fluorescence value in micrograms
#'     per liter}
#'   \item{Chla_med}{Daily median chlorophyll fluorescence value in micrograms
#'     per liter}
#' }
#'
#' @source
#' * California Department of Water Resources (DWR) Water Data Library (WDL):
#'   <https://wdl.water.ca.gov/WaterDataLibrary/>
#' * USGS National Water Information System (NWIS):
#'   <https://nwis.waterdata.usgs.gov/nwis>
"cont_chla_daily"

#' @title Discrete nutrient and chlorophyll-a concentrations and *Microcystis*
#'   visual index values for 2014-2021
#' @description A data set containing concentrations of chlorophyll-a and three
#'   nutrient parameters (Dissolved Ammonia, Dissolved Nitrate + Nitrite, and
#'   Dissolved Ortho-phosphate) in the San Francisco Estuary (Delta) east of
#'   Suisun Marsh for 2014-2021. This data set also contains Microcystis visual
#'   index values for the surveys (DWR_EMP and DWR_NCRO) that have collected
#'   this data. Used in the analyses for the Spring-Summer version of the 2022
#'   HABs/Weeds report for the Emergency Drought Barrier.
#'
#'   Most of the nutrient, chlorophyll-a, and Microcystis visual index data from
#'   DWR_EMP (DWR's Environmental Monitoring Program) and all the data from
#'   USGS_SFBS (USGS's San Francisco Bay Water Quality Survey) are from the
#'   `discretewq` data package. DWR_EMP data collected in 2021 was acquired
#'   through a direct data request and should be considered provisional. All
#'   data collected by DWR_NCRO (DWR's North Central Region Office) was also
#'   acquired through a direct data request; however, most of this data is
#'   available for download from the Water Data Library (WDL). Some of the most
#'   recent DWR_NCRO data collected in 2021 should be considered provisional.
#'   Data from USGS_CAWSC (USGS's California Water Science Center) were
#'   downloaded directly from the Water Quality Portal using the `dataRetrieval`
#'   R package. Some of the data from USGS_CAWSC is also considered provisional.
#'
#'   This data set contains only one sample or measurement per `Station` and
#'   `Date` and only includes stations where all three nutrient parameters and
#'   chlorophyll-a have been collected during their periods of record from
#'   2014-2021. This data set excludes any nitrate + nitrite and ortho-phosphate
#'   data that have modified Z-scores greater than 15 grouped by three broad
#'   regions in the Delta. Outlier removal was not performed on the ammonia and
#'   chlorophyll-a data because all the higher values appeared to be real based
#'   on best professional judgment.
#'
#'   There were a few instances where nutrient values were below the reporting
#'   limit (RL) but the RL values were greater than the 75th quantile of the
#'   overall data. These values were excluded from this data set. In addition,
#'   some of the nutrient values that were below the RL in the USGS_CAWSC data
#'   set did not have RL values provided from the data source. In these
#'   instances, this data set uses the most common RL for each nutrient
#'   parameter and laboratory method to fill in the missing RL values.
#'
#' @format data frame with 3,273 rows and 15 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either DWR_EMP (DWR's
#'     Environmental Monitoring Program), DWR_NCRO (DWR's North Central Region
#'     Office), USGS_SFBS (USGS's San Francisco Bay Water Quality Survey), or
#'     USGS_CAWSC (USGS's California Water Science Center)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Date}{Calendar date of the measurement}
#'   \item{Datetime}{Date and time of the measurement in PST}
#'   \item{DissAmmonia_Sign}{A symbol representing whether the Dissolved Ammonia
#'     value is below the reporting limit or above it. Contains one of four
#'     symbols: "<", "< (estimated)", "=", or "= (unreliable)". See the section
#'     below for more information about what these symbols represent.}
#'   \item{DissAmmonia}{Dissolved Ammonia concentration in milligrams per liter
#'     as N}
#'   \item{DissNitrateNitrite_Sign}{A symbol representing whether the Dissolved
#'     Nitrate + Nitrite value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate + Nitrite concentration in
#'     milligrams per liter as N}
#'   \item{DissOrthophos_Sign}{A symbol representing whether the Dissolved
#'     Ortho-phosphate value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate concentration in milligrams
#'     per liter as P}
#'   \item{Chlorophyll_Sign}{A symbol representing whether the chlorophyll-a
#'     value is below the reporting limit or above it. Contains one of three
#'     symbols: "<", "= (estimated)", or "=". See the section below for more
#'     information about what these symbols represent.}
#'   \item{Chlorophyll}{Chlorophyll-a concentration in micrograms per liter}
#'   \item{Microcystis}{*Microcystis* visual index value on a qualitative scale
#'     from 1 to 5, where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very
#'     high}
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
#' * `discretewq` data package (version 2.3.1):
#'   <https://github.com/sbashevkin/discretewq/tree/258dd2591a695710067fb29b4e8cbd9ffab54aa9>
#' * California Department of Water Resources (DWR) Water Data Library (WDL):
#'   <https://wdl.water.ca.gov/WaterDataLibrary/>
#' * Water Quality Portal hosted by the National Water Quality Monitoring
#'   Council: <https://www.waterqualitydata.us/>
"hab_nutr_chla_mvi"

