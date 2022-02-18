
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    hab_sat_fr_mil %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(hab_sat_fr_mil), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(hab_sat_fr_mil), 329)
  expect_equal(ncol(hab_sat_fr_mil), 8)

  name_check <- c(
    "Date",
    "Name",
    "Non_detect",
    "Low",
    "Moderate",
    "High",
    "Very_high",
    "Invalid_or_missing"
  )

  expect_equal(names(hab_sat_fr_mil), name_check)
})

test_that("There are no duplicate records", {
  hab_sat_fr_mil_t <- hab_sat_fr_mil %>% dplyr::mutate(ID = paste(Date, Name, sep = "_"))
  expect_equal(length(unique(hab_sat_fr_mil_t$ID)), nrow(hab_sat_fr_mil_t))
})

test_that("All variables are correct class", {
  expect_equal(class(hab_sat_fr_mil$Date), "Date")
  expect_equal(class(hab_sat_fr_mil$Name), "character")
  expect_equal(class(hab_sat_fr_mil$Non_detect), "numeric")
  expect_equal(class(hab_sat_fr_mil$Low), "numeric")
  expect_equal(class(hab_sat_fr_mil$Moderate), "numeric")
  expect_equal(class(hab_sat_fr_mil$High), "numeric")
  expect_equal(class(hab_sat_fr_mil$Very_high), "numeric")
  expect_equal(class(hab_sat_fr_mil$Invalid_or_missing), "numeric")
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_sat_fr_mil$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

# Provide a vector of polygon names for the two following tests
polygon_names_check <- c("Franks Tract", "Mildred Island")

test_that("All polygon Names are as expected", {
  expect_equal(sort(unique(hab_sat_fr_mil$Name)), polygon_names_check)
})

test_that("Periods of record for each polygon are as expected", {
  hab_sat_fr_mil_summ <- hab_sat_fr_mil %>%
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date)
    ) %>%
    dplyr::distinct(Name, Year, Month)

  month_por <- function(polygon_name, yr_val) {
    hab_sat_fr_mil_summ %>%
      dplyr::filter(Name == polygon_name, Year == yr_val) %>%
      dplyr::pull(Month)
  }

  expect_all_months <- function(polygon_name2, yr_val2, months_expect) {
    eval(bquote(expect_equal(month_por(.(polygon_name2), .(yr_val2)), .(months_expect))))
  }

  # Test both polygons for year 2020
  # Define expected months for data in 2020
  months2020_check <- c(5:12)
  purrr::map(polygon_names_check, expect_all_months, yr_val2 = 2020, months_expect = months2020_check)

  # Test both polygons for year 2021
  # Define expected months for data in 2021
  months2021_check <- c(5:10)
  purrr::map(polygon_names_check, expect_all_months, yr_val2 = 2021, months_expect = months2021_check)
})

test_that("All pixel count values are in their expected range", {
  count_min_max <- function(polygon_name, test_var, min_max = c("minimum", "maximum")) {
    hab_sat_fr_mil %>%
      dplyr::filter(Name == polygon_name) %>%
      {if (min_max == "minimum") {
        dplyr::summarize(., stat_val = min(.data[[test_var]]))
      } else if (min_max == "maximum") {
        dplyr::summarize(., stat_val = max(.data[[test_var]]))
      }} %>%
      dplyr::pull(stat_val)
  }

  pixel_vars <- c(
    "Non_detect",
    "Low",
    "Moderate",
    "High",
    "Very_high",
    "Invalid_or_missing"
  )

  # Test pixel count variables - minimum is greater than or equal to zero
  expect_min <- function(polygon_name2, test_var2) {
    eval(bquote(expect_gte(count_min_max(.(polygon_name2), .(test_var2), "minimum"), 0)))
  }
  expect_min_fr <- purrr::partial(expect_min, polygon_name2 = "Franks Tract")
  purrr::map(pixel_vars, expect_min_fr)
  expect_min_mil <- purrr::partial(expect_min, polygon_name2 = "Mildred Island")
  purrr::map(pixel_vars, expect_min_mil)

  # Test pixel count variables - maximum is less than or equal to 166 (Franks) or 42 (Mildred)
  expect_max <- function(polygon_name2, test_var2, test_val) {
    eval(bquote(expect_lte(count_min_max(.(polygon_name2), .(test_var2), "maximum"), .(test_val))))
  }
  expect_max_fr <- purrr::partial(expect_max, polygon_name2 = "Franks Tract", test_val = 166)
  purrr::map(pixel_vars, expect_max_fr)
  expect_max_mil <- purrr::partial(expect_max, polygon_name2 = "Mildred Island", test_val = 42)
  purrr::map(pixel_vars, expect_max_mil)
})

