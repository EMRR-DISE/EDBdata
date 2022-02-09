#' @title Counts of pixel values within Cyano Index categories for Franks Tract
#'   and Mildred Island
#' @description A data set containing counts of pixel values within 4 Cyano
#'   Index categories for Franks Tract and Mildred Island for the spring through
#'   late fall (May-Dec) in 2020 and 2021. The Cyano Index categories (Low,
#'   Moderate, High, and Very High) were based on WHO recreational guidance
#'   level thresholds. The data set also includes counts of pixels that were
#'   below the detection limit for the imagery processing method and counts of
#'   pixels that were either invalid or missing.
#'
#'   Counts only include pixels that were completely within the polygons for the
#'   two regions. The data set only includes date-region combinations where
#'   there were greater than 25% valid pixels within the region. Zonal
#'   statistics were calculated from satellite data downloaded from the Harmful
#'   Algal Blooms Analysis Tool (see Source section below). Used in the analyses
#'   for the Emergency Drought Barrier (EDB).
#'
#' @format data frame with 329 rows and 3 columns
#' \describe{
#'   \item{Date}{Date of satellite imagery}
#'   \item{Name}{Name of the polygon region. Either Franks Tract or Mildred
#'     Island.}
#'   \item{Non_detect}{Count of the number of pixels below the detection limit
#'     for the method which is less than or equal to 6,310 cells/mL}
#'   \item{Low}{Count of the number of pixels in the Low Cyano Index category
#'     defined as having greater than 6,310 cells/mL but less than 19,999
#'     cells/mL}
#'   \item{Moderate}{Count of the number of pixels in the Moderate Cyano Index
#'     category defined as having between 19,999 and 99,999 cells/mL}
#'   \item{High}{Count of the number of pixels in the High Cyano Index category
#'     defined as having between 99,999 and 999,999 cells/mL}
#'   \item{Very_high}{Count of the number of pixels in the Very High Cyano Index
#'     category defined as having greater than 999,999 cells/mL}
#'   \item{Invalid_or_missing}{Count of the number of pixels that were either
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
#'   DWR's Environmental Monitoring Program from 2014 - October 2021 for the
#'   stations within the designated Emergency Drought Barrier (EDB) regions.
#'   Used in the analyses for the EDB.
#'
#' @format data frame with 4,254 rows and 11 columns
#' \describe{
#'   \item{Station}{Location where sample was collected}
#'   \item{Region}{Region designation of `Station` for the Emergency Drought
#'     Barrier analysis. Either Central Delta, Sacramento, or San Joaquin.}
#'   \item{Year}{Calendar year of the sample}
#'   \item{Date}{Calendar date of the sample}
#'   \item{DateTime}{Date and time of the sample in PST}
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
