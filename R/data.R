#' @title Daily average Cyano Index values for Franks Tract and Mildred Island
#' @description A data set containing daily average Cyano Index pixel values for
#'   Franks Tract and Mildred Island for the spring through late fall (May-Dec)
#'   in 2020 and 2021. Zonal statistics were calculated from satellite data
#'   downloaded from the Harmful Algal Blooms Analysis Tool (see Source section
#'   below). Used in the analyses for the Emergency Drought Barrier (EDB).
#'
#' @format data frame with 852 rows and 3 columns
#' \describe{
#'   \item{Date}{Date of satellite imagery}
#'   \item{Name}{Name of the polygon area. Either Franks Tract or Mildred
#'     Island.}
#'   \item{mean_pixel_val}{Daily average Cyano Index pixel value for the area in `Name`}
#' }
#'
#' @source San Francisco Estuary Institute (SFEI). Harmful Algal Blooms Analysis
#'   Tool. <https://fhab.sfei.org/>
"hab_sat_ci_fr_mil"

#' @title Daily average Cyano Index values for the EDB regions
#' @description A data set containing daily average Cyano Index pixel values for
#'   the Emergency Drought Barrier (EDB) regions for the spring through late
#'   fall (May-Dec) in 2020 and 2021. Zonal statistics were calculated from
#'   satellite data downloaded from the Harmful Algal Blooms Analysis Tool (see
#'   Source section below). Used in the analyses for the EDB.
#'
#' @format data frame with 1278 rows and 3 columns
#' \describe{
#'   \item{Date}{Date of satellite imagery}
#'   \item{Region}{Name of the EDB region. Either Sacramento, San Joaquin, or
#'     Central Delta.}
#'   \item{mean_pixel_val}{Daily average Cyano Index pixel value for the area in `Region`}
#' }
#'
#' @inherit hab_sat_ci_fr_mil return source
"hab_sat_ci_edb_reg"

