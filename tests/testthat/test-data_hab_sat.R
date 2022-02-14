
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

test_that("Data is present for years 2020 and 2021", {
  hab_sat_fr_mil_t <- hab_sat_fr_mil %>% dplyr::mutate(Year = lubridate::year(Date))
  expect_equal(sort(unique(hab_sat_fr_mil_t$Year)), c(2020:2021))
})

test_that("Data is present for months May through December", {
  hab_sat_fr_mil_t <- hab_sat_fr_mil %>% dplyr::mutate(Month = lubridate::month(Date))
  expect_equal(sort(unique(hab_sat_fr_mil_t$Month)), c(5:12))
})

test_that("All Names are as expected", {
  names_check <- c("Franks Tract", "Mildred Island")
  expect_equal(sort(unique(hab_sat_fr_mil$Name)), names_check)
})

test_that("All pixel count values are in their expected range", {
  count_min_max <- function(name, test_var, min_max = c("minimum", "maximum")) {
    hab_sat_fr_mil %>%
      dplyr::filter(Name == name) %>%
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
  expect_min <- function(name2, test_var2) {
    eval(bquote(expect_gte(count_min_max(.(name2), .(test_var2), "mininum"), 0)))
  }
  expect_min_fr <- purrr::partial(expect_min, name2 = "Franks Tract")
  purrr::map(pixel_vars, expect_min_fr)
  expect_min_mil <- purrr::partial(expect_min, name2 = "Mildred Island")
  purrr::map(pixel_vars, expect_min_mil)

  # Test pixel count variables - maximum is less than or equal to 166 (Franks) or 42 (Mildred)
  expect_max <- function(name2, test_var2, test_val) {
    eval(bquote(expect_lte(count_min_max(.(name2), .(test_var2), "maximum"), .(test_val))))
  }
  expect_max_fr <- purrr::partial(expect_max, name2 = "Franks Tract", test_val = 166)
  purrr::map(pixel_vars, expect_max_fr)
  expect_max_mil <- purrr::partial(expect_max, name2 = "Mildred Island", test_val = 42)
  purrr::map(pixel_vars, expect_max_mil)
})

