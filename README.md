
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EDBdata

<!-- badges: start -->

[![R-CMD-check](https://github.com/mountaindboz/EDBdata/workflows/R-CMD-check/badge.svg)](https://github.com/mountaindboz/EDBdata/actions)
<!-- badges: end -->

An R data package containing data sets used in the Emergency Drought
Barrier (EDB) analysis. This data package also contains useful metadata
describing the data sets and the processing scripts used to derive the
data.

This package provides the following data tables:

-   `hab_sat_fr_mil`: Counts of pixel values within 4 Cyano Index
    categories for Franks Tract and Mildred Island for the spring
    through late fall (May-Dec) in 2020 and 2021
-   `phyto_edb`: Phytoplankton community data collected by DWR’s
    Environmental Monitoring Program from 2014-2021 for stations within
    the Emergency Drought Barrier regions

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("mountaindboz/EDBdata")
```

## Data Sources

San Francisco Estuary Institute (SFEI). Harmful Algal Blooms Analysis
Tool. <https://fhab.sfei.org/>

Phytoplankton data was acquired from DWR’s Environmental Monitoring
Program through direct data requests
