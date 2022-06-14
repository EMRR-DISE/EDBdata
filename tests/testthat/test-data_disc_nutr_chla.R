
test_that("No expected variables contain `NA` values", {
  count_na <- function(test_var) {
    disc_nutr_chla %>%
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
    "Region",
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
  expect_equal(nrow(disc_nutr_chla), 3133)
  expect_equal(ncol(disc_nutr_chla), 15)

  name_check <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Region",
    "Date",
    "Datetime",
    "DissAmmonia_Sign",
    "DissAmmonia",
    "DissNitrateNitrite_Sign",
    "DissNitrateNitrite",
    "DissOrthophos_Sign",
    "DissOrthophos",
    "Chlorophyll_Sign",
    "Chlorophyll"
  )

  expect_equal(names(disc_nutr_chla), name_check)
})

test_that("There are no duplicate records", {
  disc_nutr_chla_t <- disc_nutr_chla %>%
    dplyr::mutate(ID = paste(Date, Source, Station, sep = "_"))

  select_var <- function(test_var) {
    disc_nutr_chla_t %>%
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
    "Chlorophyll"
  )

  purrr::map(vars_results, expect_no_dups)
})

test_that("All variables are correct class", {
  expect_equal(class(disc_nutr_chla$Source), "character")
  expect_equal(class(disc_nutr_chla$Station), "character")
  expect_equal(class(disc_nutr_chla$Latitude), "numeric")
  expect_equal(class(disc_nutr_chla$Longitude), "numeric")
  expect_equal(class(disc_nutr_chla$Region), "character")
  expect_equal(class(disc_nutr_chla$Date), "Date")
  expect_equal(class(disc_nutr_chla$Datetime), c("POSIXct", "POSIXt"))
  expect_equal(class(disc_nutr_chla$DissAmmonia_Sign), "character")
  expect_equal(class(disc_nutr_chla$DissAmmonia), "numeric")
  expect_equal(class(disc_nutr_chla$DissNitrateNitrite_Sign), "character")
  expect_equal(class(disc_nutr_chla$DissNitrateNitrite), "numeric")
  expect_equal(class(disc_nutr_chla$DissOrthophos_Sign), "character")
  expect_equal(class(disc_nutr_chla$DissOrthophos), "numeric")
  expect_equal(class(disc_nutr_chla$Chlorophyll_Sign), "character")
  expect_equal(class(disc_nutr_chla$Chlorophyll), "numeric")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_EMP", "DWR_NCRO", "USGS_CAWSC", "USGS_SFBS")
  expect_equal(sort(unique(disc_nutr_chla$Source)), sources_check)
})

test_that("All Stations are as expected", {
  stations_check <- c(
    "649",
    "657",
    "BET",
    "C3A",
    "C9",
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
    "USGS-11455139",
    "USGS-11455140",
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
    "USGS-381424121405601",
    "USGS-381614121415301",
    "USGS-382006121401601",
    "USGS-382010121402301"
  )

  expect_equal(sort(unique(disc_nutr_chla$Station)), stations_check)
})

test_that("All Latitude and Longitude values are within expected ranges", {
  expect_gte(min(disc_nutr_chla$Latitude), 37)
  expect_lte(max(disc_nutr_chla$Latitude), 39)
  expect_gte(min(disc_nutr_chla$Longitude), -122)
  expect_lte(max(disc_nutr_chla$Longitude), -121)
})

test_that("All Region names are as expected", {
  regions_check <- c(
    "Cache/Liberty",
    "East Delta",
    "Franks",
    "Lower SJ",
    "Lower Sac",
    "OMR",
    "South Delta",
    "Upper Sac"
  )

  expect_equal(sort(unique(disc_nutr_chla$Region)), regions_check)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(disc_nutr_chla$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Datetime is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(disc_nutr_chla$Datetime),
    "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  )))
})

test_that("The Date and Datetime variables are in alignment", {
  disc_nutr_chla_t <- disc_nutr_chla %>% dplyr::mutate(Date_t = lubridate::date(Datetime))
  expect_equal(disc_nutr_chla_t$Date, disc_nutr_chla_t$Date_t)
})

test_that("The time zone of Datetime is PST", {
  expect_equal(lubridate::tz(disc_nutr_chla$Datetime), "Etc/GMT+8")
})

test_that("Data is present for years 2014 through 2021", {
  expect_equal(sort(unique(lubridate::year(disc_nutr_chla$Date))), c(2014:2021))
})

test_that("All values in _Sign variables are as expected", {
  signs_check <- c("<", "< (estimated)", "=", "= (unreliable)")
  expect_equal(sort(unique(disc_nutr_chla$DissAmmonia_Sign)), signs_check)
  expect_equal(sort(unique(disc_nutr_chla$DissNitrateNitrite_Sign)), signs_check)
  expect_equal(sort(unique(disc_nutr_chla$DissOrthophos_Sign)), signs_check)

  sign_chla_check <- c("<", "=", "= (estimated)")
  expect_equal(sort(unique(disc_nutr_chla$Chlorophyll_Sign)), sign_chla_check)
})

test_that("Nutrient and chlorophyll-a values are greater than zero", {
  calc_min <- function(test_var) {
    disc_nutr_chla %>%
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

