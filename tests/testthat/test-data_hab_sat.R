
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    hab_sat_ow_delta %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(hab_sat_ow_delta), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(hab_sat_ow_delta), 708)
  expect_equal(ncol(hab_sat_ow_delta), 9)

  name_check <- c(
    "Date",
    "Region",
    "AvgCI",
    "NonDetect",
    "Low",
    "Moderate",
    "High",
    "VeryHigh",
    "InvalidOrMissing"
  )

  expect_equal(names(hab_sat_ow_delta), name_check)
})

test_that("There are no duplicate records", {
  hab_sat_ow_delta_t <- hab_sat_ow_delta %>% dplyr::mutate(ID = paste(Date, Region, sep = "_"))
  expect_equal(length(unique(hab_sat_ow_delta_t$ID)), nrow(hab_sat_ow_delta_t))
})

test_that("All variables are correct class", {
  expect_equal(class(hab_sat_ow_delta$Date), "Date")
  expect_equal(class(hab_sat_ow_delta$Region), "character")
  expect_equal(class(hab_sat_ow_delta$AvgCI), "numeric")
  expect_equal(class(hab_sat_ow_delta$NonDetect), "integer")
  expect_equal(class(hab_sat_ow_delta$Low), "integer")
  expect_equal(class(hab_sat_ow_delta$Moderate), "integer")
  expect_equal(class(hab_sat_ow_delta$High), "integer")
  expect_equal(class(hab_sat_ow_delta$VeryHigh), "integer")
  expect_equal(class(hab_sat_ow_delta$InvalidOrMissing), "integer")
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_sat_ow_delta$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

# Provide a vector of region names for the following tests
region_names_check <- c(
  "Clifton Court Forebay",
  "Franks Tract",
  "Liberty Island",
  "Mildred Island"
)

test_that("All region names are as expected", {
  expect_equal(sort(unique(hab_sat_ow_delta$Region)), region_names_check)
})

test_that("Periods of record for each region are as expected", {
  hab_sat_ow_delta_summ <- hab_sat_ow_delta %>%
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date)
    ) %>%
    dplyr::distinct(Region, Year, Month)

  month_por <- function(region_name, yr_val) {
    hab_sat_ow_delta_summ %>%
      dplyr::filter(Region == region_name, Year == yr_val) %>%
      dplyr::pull(Month)
  }

  expect_all_months <- function(region_name2, yr_val2, months_expect) {
    eval(bquote(expect_equal(month_por(.(region_name2), .(yr_val2)), .(months_expect))))
  }

  # Test both polygons for year 2020
  # Define expected months for data in 2020
  months2020_check <- c(5:12)
  purrr::map(region_names_check, expect_all_months, yr_val2 = 2020, months_expect = months2020_check)

  # Test both polygons for year 2021
  # Define expected months for data in 2021
  months2021_check <- c(5:10)
  purrr::map(region_names_check, expect_all_months, yr_val2 = 2021, months_expect = months2021_check)
})

test_that("All average Cyano Index values are in their expected range", {
  expect_gte(min(hab_sat_ow_delta$AvgCI), 0)
  expect_lte(max(hab_sat_ow_delta$AvgCI), 0.06327)
})

test_that("All pixel count values are in their expected range", {
  count_min_max <- function(region_name, test_var, min_max = c("minimum", "maximum")) {
    hab_sat_ow_delta %>%
      dplyr::filter(Region == region_name) %>%
      {if (min_max == "minimum") {
        dplyr::summarize(., stat_val = min(.data[[test_var]]))
      } else if (min_max == "maximum") {
        dplyr::summarize(., stat_val = max(.data[[test_var]]))
      }} %>%
      dplyr::pull(stat_val)
  }

  # Define column names with pixel counts
  pixel_vars <- c(
    "NonDetect",
    "Low",
    "Moderate",
    "High",
    "VeryHigh",
    "InvalidOrMissing"
  )

  # Create a tibble of all Region-Pixel column name combinations with the
    # expected maximum number of pixels for each Region - this will be used to run
    # the tests on
  count_test_combo <-
    tibble::tibble(
      Region = region_names_check,
      MaxPixels = c(109, 166, 144, 42)
    ) %>%
    tidyr::expand_grid(PixelVars = pixel_vars)

  # Test pixel count columns - minimum is greater than or equal to zero
  expect_min <- function(region_name2, test_var2) {
    eval(bquote(expect_gte(count_min_max(.(region_name2), .(test_var2), "minimum"), 0)))
  }
  purrr::map2(count_test_combo$Region, count_test_combo$PixelVars, expect_min)

  # Test pixel count columns - maximum is less than or equal to:
    # 109 for Clifton Court Forebay
    # 166 for Franks Tract
    # 144 for Liberty Island
    # 42 for Mildred Island
  expect_max <- function(region_name2, test_var2, test_val) {
    eval(bquote(expect_lte(count_min_max(.(region_name2), .(test_var2), "maximum"), .(test_val))))
  }
  purrr::pmap(
    list(
      count_test_combo$Region,
      count_test_combo$PixelVars,
      count_test_combo$MaxPixels
    ),
    expect_max
  )
})

