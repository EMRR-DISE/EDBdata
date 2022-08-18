
test_that("No expected variables contain `NA` values", {
  count_na <- function(test_var) {
    hab_incidents %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(hab_incidents), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(hab_incidents), 50)
  expect_equal(ncol(hab_incidents), 4)

  name_check <- c(
    "Date"    ,
    "Latitude",
    "Longitude",
    "Advisory"
  )

  expect_equal(names(hab_incidents), name_check)
})

test_that("There are no duplicate records", {
  hab_incidents_t <- hab_incidents %>% dplyr::mutate(ID = paste(Date, Latitude, Longitude, sep = "_"))
  expect_equal(length(unique(hab_incidents_t$ID)), nrow(hab_incidents_t))
})

test_that("All variables are correct class", {
  expect_equal(class(hab_incidents$Date), "Date")
  expect_equal(class(hab_incidents$Latitude), "numeric")
  expect_equal(class(hab_incidents$Longitude), "numeric")
  expect_equal(class(hab_incidents$Advisory), "character")
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_incidents$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Data is present only for year 2021", {
  expect_equal(sort(unique(lubridate::year(hab_incidents$Date))), 2021)
})

test_that("All Latitude and Longitude values are within expected ranges", {
  expect_gte(min(hab_incidents$Latitude), 37)
  expect_lte(max(hab_incidents$Latitude), 39)
  expect_gte(min(hab_incidents$Longitude), -122)
  expect_lte(max(hab_incidents$Longitude), -121)
})

test_that("All Advisory names are as expected", {
  advisory_check <- c("Caution", "Danger")
  expect_equal(sort(unique(hab_incidents$Advisory)), advisory_check)
})

