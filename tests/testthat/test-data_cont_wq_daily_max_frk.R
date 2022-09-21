
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    cont_wq_daily_max_frk %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  # Only test Date variable since all others have NA values
  expect_equal(count_na("Date"), 0)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(cont_wq_daily_max_frk), 2376)
  expect_equal(ncol(cont_wq_daily_max_frk), 4)

  name_check <- c(
    "Date",
    "DO_PerSat",
    "pH",
    "Chla"
  )

  expect_equal(names(cont_wq_daily_max_frk), name_check)
})

test_that("There are no duplicate records", {
  expect_equal(length(unique(cont_wq_daily_max_frk$Date)), nrow(cont_wq_daily_max_frk))
})

test_that("All variables are correct class", {
  expect_equal(class(cont_wq_daily_max_frk$Date), "Date")
  expect_equal(class(cont_wq_daily_max_frk$DO_PerSat), "numeric")
  expect_equal(class(cont_wq_daily_max_frk$pH), "numeric")
  expect_equal(class(cont_wq_daily_max_frk$Chla), "numeric")
})

test_that("Data is present for years 2015-2021", {
  expect_equal(sort(unique(lubridate::year(cont_wq_daily_max_frk$Date))), 2015:2021)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(cont_wq_daily_max_frk$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Daily maximums are greater than or equal to zero", {
  expect_gte(min(cont_wq_daily_max_frk$DO_PerSat, na.rm = TRUE), 0)
  expect_gte(min(cont_wq_daily_max_frk$pH, na.rm = TRUE), 0)
  expect_gte(min(cont_wq_daily_max_frk$Chla, na.rm = TRUE), 0)
})

