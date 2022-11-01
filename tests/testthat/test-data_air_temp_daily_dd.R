
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    air_temp_daily_dd %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(air_temp_daily_dd), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(air_temp_daily_dd), 2556)
  expect_equal(ncol(air_temp_daily_dd), 4)

  name_check <- c(
    "Year",
    "Date",
    "AirTemp_Avg",
    "AirTemp_DD"
  )

  expect_equal(names(air_temp_daily_dd), name_check)
})

test_that("There are no duplicate records", {
  expect_equal(length(unique(air_temp_daily_dd$Date)), nrow(air_temp_daily_dd))
})

test_that("All variables are correct class", {
  expect_equal(class(air_temp_daily_dd$Year), "numeric")
  expect_equal(class(air_temp_daily_dd$Date), "Date")
  expect_equal(class(air_temp_daily_dd$AirTemp_Avg), "numeric")
  expect_equal(class(air_temp_daily_dd$AirTemp_DD), "numeric")
})

test_that("Data is present for years 2015-2021", {
  expect_equal(sort(unique(air_temp_daily_dd$Year)), 2015:2021)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(air_temp_daily_dd$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("The Date and Year variables are in alignment", {
  air_temp_daily_dd_t <- air_temp_daily_dd %>% dplyr::mutate(Year_t = lubridate::year(Date))
  expect_equal(air_temp_daily_dd_t$Year, air_temp_daily_dd_t$Year_t)
})

test_that("Daily average air temperatures are within expected range", {
  expect_gte(min(air_temp_daily_dd$AirTemp_Avg), 0)
  expect_lt(max(air_temp_daily_dd$AirTemp_Avg), 35)
})

test_that("Degree days are within expected range", {
  expect_gte(min(air_temp_daily_dd$AirTemp_DD), 0)
})

