
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    cont_wq_daily_avg %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for DO, pH, and Chla
  vars_no_NAs <- stringr::str_subset(names(cont_wq_daily_avg), "^DO|^pH|^Chla", negate = TRUE)
  purrr::map(vars_no_NAs, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(cont_wq_daily_avg), 13273)
  expect_equal(ncol(cont_wq_daily_avg), 6)

  name_check <- c(
    "Source",
    "Station",
    "Date",
    "DO",
    "pH",
    "Chla"
  )

  expect_equal(names(cont_wq_daily_avg), name_check)
})

test_that("There are no duplicate records", {
  cont_wq_daily_avg_t <- cont_wq_daily_avg %>% dplyr::mutate(ID = paste(Date, Station, sep = "_"))
  expect_equal(length(unique(cont_wq_daily_avg_t$ID)), nrow(cont_wq_daily_avg_t))
})

test_that("All variables are correct class", {
  expect_equal(class(cont_wq_daily_avg$Source), "character")
  expect_equal(class(cont_wq_daily_avg$Station), "character")
  expect_equal(class(cont_wq_daily_avg$Date), "Date")
  expect_equal(class(cont_wq_daily_avg$DO), "numeric")
  expect_equal(class(cont_wq_daily_avg$pH), "numeric")
  expect_equal(class(cont_wq_daily_avg$Chla), "numeric")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_CEMP", "DWR_NCRO", "USGS")
  expect_equal(sort(unique(cont_wq_daily_avg$Source)), sources_check)
})

# Provide a vector of stations for the two following tests
stations_check <- c(
  "FAL",
  "FRK",
  "HLT",
  "HOL",
  "MDM",
  "OSJ"
)

test_that("All Stations are as expected", {
  expect_equal(sort(unique(cont_wq_daily_avg$Station)), stations_check)
})

test_that("Data is present for years 2015-2021 for each station, except for MDM (2019-2021)", {
  cont_wq_daily_avg_summ <- cont_wq_daily_avg %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::distinct(Station, Year)

  year_por <- function(sta_name) {
    cont_wq_daily_avg_summ %>% dplyr::filter(Station == sta_name) %>% dplyr::pull(Year)
  }

  expect_all_yrs <- function(sta_name2, yrs_expect) {
    eval(bquote(expect_equal(year_por(.(sta_name2)), .(yrs_expect))))
  }

  # Define expected years for data
  yrs_check <- c(2015:2021)

  # Test all stations except for MDM
  purrr::map(
    stringr::str_subset(stations_check, "MDM", negate = TRUE),
    expect_all_yrs,
    yrs_expect = yrs_check
  )

  # Test MDM
  expect_all_yrs("MDM", c(2019:2021))
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(cont_wq_daily_avg$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Dissolved oxygen values are within expected ranges", {
  expect_gte(min(cont_wq_daily_avg$DO, na.rm = TRUE), 0)
  expect_lte(max(cont_wq_daily_avg$DO, na.rm = TRUE), 20)
})

test_that("pH values are within expected ranges", {
  expect_gte(min(cont_wq_daily_avg$pH, na.rm = TRUE), 5)
  expect_lte(max(cont_wq_daily_avg$pH, na.rm = TRUE), 11)
})

test_that("Chlorophyll values are within expected ranges", {
  expect_gte(min(cont_wq_daily_avg$Chla, na.rm = TRUE), 0)
  expect_lte(max(cont_wq_daily_avg$Chla, na.rm = TRUE), 200)
})

