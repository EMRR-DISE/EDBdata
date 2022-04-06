
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    cont_chla_daily %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(cont_chla_daily), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(cont_chla_daily), 7612)
  expect_equal(ncol(cont_chla_daily), 7)

  name_check <- c(
    "Source",
    "Station",
    "Region",
    "Year",
    "Date",
    "ChlaAvg",
    "ChlaMed"
  )

  expect_equal(names(cont_chla_daily), name_check)
})

test_that("There are no duplicate records", {
  cont_chla_daily_t <- cont_chla_daily %>% dplyr::mutate(ID = paste(Date, Station, sep = "_"))
  expect_equal(length(unique(cont_chla_daily_t$ID)), nrow(cont_chla_daily_t))
})

test_that("All variables are correct class", {
  expect_equal(class(cont_chla_daily$Source), "character")
  expect_equal(class(cont_chla_daily$Station), "character")
  expect_equal(class(cont_chla_daily$Region), "character")
  expect_equal(class(cont_chla_daily$Year), "numeric")
  expect_equal(class(cont_chla_daily$Date), "Date")
  expect_equal(class(cont_chla_daily$ChlaAvg), "numeric")
  expect_equal(class(cont_chla_daily$ChlaMed), "numeric")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_CEMP", "DWR_NCRO", "USGS")
  expect_equal(sort(unique(cont_chla_daily$Source)), sources_check)
})

# Provide a vector of stations for the two following tests
stations_check <- c(
  "BLP",
  "FAL",
  "FRK",
  "HLT",
  "MDM",
  "ORI",
  "OSJ",
  "RVB",
  "SJJ",
  "SSI",
  "TWI",
  "WCI"
)

test_that("All Stations are as expected", {
  expect_equal(sort(unique(cont_chla_daily$Station)), stations_check)
})

test_that("Data is present for years 2020 and 2021 for each station", {
  cont_chla_daily_summ <- cont_chla_daily %>% dplyr::distinct(Station, Year)
  year_por <- function(sta_name) {
    cont_chla_daily_summ %>% dplyr::filter(Station == sta_name) %>% dplyr::pull(Year)
  }

  expect_all_yrs <- function(sta_name2, yrs_expect) {
    eval(bquote(expect_equal(year_por(.(sta_name2)), .(yrs_expect))))
  }

  # Define expected years for data
  yrs_check <- c(2020:2021)

  # Test all stations
  purrr::map(stations_check, expect_all_yrs, yrs_expect = yrs_check)
})

test_that("All Region names are as expected", {
  regions_check <- c("Central Delta", "Sacramento", "San Joaquin")
  expect_equal(sort(unique(cont_chla_daily$Region)), regions_check)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(cont_chla_daily$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Daily averages and medians are greater than or equal to zero", {
  expect_gte(min(cont_chla_daily$ChlaAvg), 0)
  expect_gte(min(cont_chla_daily$ChlaMed), 0)
})

