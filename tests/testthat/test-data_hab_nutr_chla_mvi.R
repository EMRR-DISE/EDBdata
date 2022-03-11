
test_that("No expected variables contain `NA` values", {
  count_na <- function(test_var) {
    hab_nutr_chla_mvi %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for those containing results
  vars_no_NA <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Date",
    "Datetime",
    "DissAmmonia_Sign",
    "DissNitrateNitrite_Sign",
    "DissOrthophos_Sign",
    "Chlorophyll_Sign"
  )

  purrr::map(vars_no_NA, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(hab_nutr_chla_mvi), 3273)
  expect_equal(ncol(hab_nutr_chla_mvi), 15)

  name_check <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Date",
    "Datetime",
    "DissAmmonia_Sign",
    "DissAmmonia",
    "DissNitrateNitrite_Sign",
    "DissNitrateNitrite",
    "DissOrthophos_Sign",
    "DissOrthophos",
    "Chlorophyll_Sign",
    "Chlorophyll",
    "Microcystis"
  )

  expect_equal(names(hab_nutr_chla_mvi), name_check)
})

test_that("There are no duplicate records", {
  hab_nutr_chla_mvi_t <- hab_nutr_chla_mvi %>%
    dplyr::mutate(ID = paste(Date, Source, Station, sep = "_"))

  select_var <- function(test_var) {
    hab_nutr_chla_mvi_t %>%
      dplyr::filter(!is.na(.data[[test_var]])) %>%
      dplyr::pull(ID)
  }

  expect_no_dups <- function(test_var2) {
    eval(bquote(expect_equal(
      length(unique(select_var(.(test_var2)))),
      length(select_var(.(test_var2)))
    )))
  }

  # Test all variables containing results
  vars_results <- c(
    "DissAmmonia",
    "DissNitrateNitrite",
    "DissOrthophos",
    "Chlorophyll",
    "Microcystis"
  )

  purrr::map(vars_results, expect_no_dups)
})

test_that("All variables are correct class", {
  expect_equal(class(hab_nutr_chla_mvi$Source), "character")
  expect_equal(class(hab_nutr_chla_mvi$Station), "character")
  expect_equal(class(hab_nutr_chla_mvi$Latitude), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$Longitude), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$Date), "Date")
  expect_equal(class(hab_nutr_chla_mvi$Datetime), c("POSIXct", "POSIXt"))
  expect_equal(class(hab_nutr_chla_mvi$DissAmmonia_Sign), "character")
  expect_equal(class(hab_nutr_chla_mvi$DissAmmonia), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$DissNitrateNitrite_Sign), "character")
  expect_equal(class(hab_nutr_chla_mvi$DissNitrateNitrite), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$DissOrthophos_Sign), "character")
  expect_equal(class(hab_nutr_chla_mvi$DissOrthophos), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$Chlorophyll_Sign), "character")
  expect_equal(class(hab_nutr_chla_mvi$Chlorophyll), "numeric")
  expect_equal(class(hab_nutr_chla_mvi$Microcystis), "integer")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_EMP", "DWR_NCRO", "USGS_CAWSC", "USGS_SFBS")
  expect_equal(sort(unique(hab_nutr_chla_mvi$Source)), sources_check)
})

test_that("All Stations are as expected", {
  stations_check <- c(
    "3",
    "649",
    "657",
    "BET",
    "C3A",
    "C9",
    "D10",
    "D12",
    "D16",
    "D19",
    "D22",
    "D24",
    "D26",
    "D28A",
    "D4",
    "FAL",
    "FCT",
    "HOL",
    "MD10A",
    "NZ068",
    "OSJ",
    "P8",
    "TSL",
    "USGS-11311300",
    "USGS-11312672",
    "USGS-11312676",
    "USGS-11312685",
    "USGS-11313405",
    "USGS-11313460",
    "USGS-11336680",
    "USGS-11336685",
    "USGS-11336790",
    "USGS-11336930",
    "USGS-11337190",
    "USGS-11447650",
    "USGS-11447890",
    "USGS-11455095",
    "USGS-11455136",
    "USGS-11455142",
    "USGS-11455143",
    "USGS-11455146",
    "USGS-11455166",
    "USGS-11455167",
    "USGS-11455276",
    "USGS-11455280",
    "USGS-11455315",
    "USGS-11455335",
    "USGS-11455350",
    "USGS-11455385",
    "USGS-11455420",
    "USGS-11455478",
    "USGS-11455485",
    "USGS-11455508",
    "USGS-381424121405601",
    "USGS-381614121415301",
    "USGS-382006121401601",
    "USGS-382010121402301"
  )

  expect_equal(sort(unique(hab_nutr_chla_mvi$Station)), stations_check)
})

test_that("All Latitude and Longitude values are within expected ranges", {
  expect_gte(min(hab_nutr_chla_mvi$Latitude), 37)
  expect_lte(max(hab_nutr_chla_mvi$Latitude), 39)
  expect_gte(min(hab_nutr_chla_mvi$Longitude), -122)
  expect_lte(max(hab_nutr_chla_mvi$Longitude), -121)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_nutr_chla_mvi$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Datetime is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_nutr_chla_mvi$Datetime),
    "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  )))
})

test_that("The Date and Datetime variables are in alignment", {
  hab_nutr_chla_mvi_t <- hab_nutr_chla_mvi %>% dplyr::mutate(Date_t = lubridate::date(Datetime))
  expect_equal(hab_nutr_chla_mvi_t$Date, hab_nutr_chla_mvi_t$Date_t)
})

test_that("The time zone of Datetime is PST", {
  expect_equal(lubridate::tz(hab_nutr_chla_mvi$Datetime), "Etc/GMT+8")
})

test_that("Data is present for years 2014 through 2021", {
  expect_equal(sort(unique(lubridate::year(hab_nutr_chla_mvi$Date))), c(2014:2021))
})

test_that("All values in _Sign variables are as expected", {
  signs_check <- c("<", "< (estimated)", "=", "= (unreliable)")
  expect_equal(sort(unique(hab_nutr_chla_mvi$DissAmmonia_Sign)), signs_check)
  expect_equal(sort(unique(hab_nutr_chla_mvi$DissNitrateNitrite_Sign)), signs_check)
  expect_equal(sort(unique(hab_nutr_chla_mvi$DissOrthophos_Sign)), signs_check)

  sign_chla_check <- c("<", "=", "= (estimated)")
  expect_equal(sort(unique(hab_nutr_chla_mvi$Chlorophyll_Sign)), sign_chla_check)
})

test_that("Nutrient and chlorophyll-a values are greater than zero", {
  calc_min <- function(test_var) {
    hab_nutr_chla_mvi %>%
      dplyr::summarize(min_val = min(.data[[test_var]], na.rm = TRUE)) %>%
      dplyr::pull(min_val)
  }

  expect_gt_zero <- function(test_var2) {
    eval(bquote(expect_gt(calc_min(.(test_var2)), 0)))
  }

  # Test all variables containing results
  vars_results <- c(
    "DissAmmonia",
    "DissNitrateNitrite",
    "DissOrthophos",
    "Chlorophyll"
  )

  purrr::map(vars_results, expect_gt_zero)
})

test_that("Microcystis visual index values are between 1 and 5", {
  expect_gte(min(hab_nutr_chla_mvi$Microcystis, na.rm = TRUE), 1)
  expect_lte(max(hab_nutr_chla_mvi$Microcystis, na.rm = TRUE), 5)
})

