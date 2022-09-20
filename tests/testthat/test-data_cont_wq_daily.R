
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    cont_wq_daily %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for DO, pH, and Chla
  vars_no_NAs <- stringr::str_subset(names(cont_wq_daily), "^DO|^pH|^Chla", negate = TRUE)
  purrr::map(vars_no_NAs, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(cont_wq_daily), 17552)
  expect_equal(ncol(cont_wq_daily), 11)

  name_check <- c(
    "Source",
    "Station",
    "Date",
    "WaterTemp_Mean",
    "WaterTemp_Max",
    "DO_Mean",
    "DO_Max",
    "pH_Mean",
    "pH_Max",
    "Chla_Mean",
    "Chla_Max"
  )

  expect_equal(names(cont_wq_daily), name_check)
})

test_that("There are no duplicate records", {
  cont_wq_daily_t <- cont_wq_daily %>% dplyr::mutate(ID = paste(Date, Station, sep = "_"))
  expect_equal(length(unique(cont_wq_daily_t$ID)), nrow(cont_wq_daily_t))
})

test_that("All variables are correct class", {
  expect_equal(class(cont_wq_daily$Source), "character")
  expect_equal(class(cont_wq_daily$Station), "character")
  expect_equal(class(cont_wq_daily$Date), "Date")
  expect_equal(class(cont_wq_daily$WaterTemp_Mean), "numeric")
  expect_equal(class(cont_wq_daily$WaterTemp_Max), "numeric")
  expect_equal(class(cont_wq_daily$DO_Mean), "numeric")
  expect_equal(class(cont_wq_daily$DO_Max), "numeric")
  expect_equal(class(cont_wq_daily$pH_Mean), "numeric")
  expect_equal(class(cont_wq_daily$pH_Max), "numeric")
  expect_equal(class(cont_wq_daily$Chla_Mean), "numeric")
  expect_equal(class(cont_wq_daily$Chla_Max), "numeric")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_CEMP", "DWR_NCRO", "USGS")
  expect_equal(sort(unique(cont_wq_daily$Source)), sources_check)
})

# Provide a vector of stations for the two following tests
stations_check <- c(
  "FAL",
  "FRK",
  "HLT",
  "HOL",
  "MDM",
  "ORQ",
  "OSJ"
)

test_that("All Stations are as expected", {
  expect_equal(sort(unique(cont_wq_daily$Station)), stations_check)
})

test_that("Data is present for years 2015-2021 for each station", {
  cont_wq_daily_summ <- cont_wq_daily %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::distinct(Station, Year)

  year_por <- function(sta_name) {
    cont_wq_daily_summ %>% dplyr::filter(Station == sta_name) %>% dplyr::pull(Year)
  }

  expect_all_yrs <- function(sta_name2, yrs_expect) {
    eval(bquote(expect_equal(year_por(.(sta_name2)), .(yrs_expect))))
  }

  # Define expected years for data
  yrs_check <- c(2015:2021)

  # Test all stations
  purrr::map(stations_check, expect_all_yrs, yrs_expect = yrs_check)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(cont_wq_daily$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Daily averages and maximums are greater than or equal to zero", {
  calc_min <- function(test_var) {
    cont_wq_daily %>%
      dplyr::summarize(min_var = min(.data[[test_var]], na.rm = TRUE)) %>%
      dplyr::pull(min_var)
  }

  expect_gte_zero <- function(test_var2) {
    eval(bquote(expect_gte(calc_min(.(test_var2)), 0)))
  }

  # Test all variables with daily means and maximums
  vars_daily_val <- stringr::str_subset(names(cont_wq_daily), "^WaterTemp|^DO|^pH|^Chla")
  purrr::map(vars_daily_val, expect_gte_zero)
})

