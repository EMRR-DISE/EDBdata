
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    phyto_hab %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for Species
  vars_no_NAs <- stringr::str_subset(names(phyto_hab), "Species", negate = TRUE)
  purrr::map(vars_no_NAs, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(phyto_hab), 228)
  expect_equal(ncol(phyto_hab), 11)

  name_check <- c(
    "Station",
    "Region",
    "Year",
    "Date",
    "Datetime",
    "Taxon",
    "Genus",
    "Species",
    "AlgalType",
    "Count",
    "OrganismsPerMl"
  )

  expect_equal(names(phyto_hab), name_check)
})

test_that("There are no duplicate records", {
  phyto_hab_t <- phyto_hab %>% dplyr::mutate(ID = paste(Date, Station, Taxon, sep = "_"))
  expect_equal(length(unique(phyto_hab_t$ID)), nrow(phyto_hab_t))
})

test_that("All variables are correct class", {
  expect_equal(class(phyto_hab$Station), "character")
  expect_equal(class(phyto_hab$Region), "character")
  expect_equal(class(phyto_hab$Year), "numeric")
  expect_equal(class(phyto_hab$Date), "Date")
  expect_equal(class(phyto_hab$Datetime), c("POSIXct", "POSIXt"))
  expect_equal(class(phyto_hab$Taxon), "character")
  expect_equal(class(phyto_hab$Genus), "character")
  expect_equal(class(phyto_hab$Species), "character")
  expect_equal(class(phyto_hab$AlgalType), "character")
  expect_equal(class(phyto_hab$Count), "numeric")
  expect_equal(class(phyto_hab$OrganismsPerMl), "numeric")
})

test_that("All Stations are as expected", {
  stations_check <- c(
    "C3A",
    "C9",
    "D12",
    "D16",
    "D19",
    "D22",
    "D26",
    "D28A",
    "D4",
    "MD10A",
    "NZ068",
    "P8"
  )

  expect_equal(sort(unique(phyto_hab$Station)), stations_check)
})

test_that("All Region names are as expected", {
  regions_check <- c(
    "East Delta",
    "Franks",
    "Lower SJ",
    "Lower Sac",
    "OMR",
    "South Delta",
    "Upper Sac"
  )

  expect_equal(sort(unique(phyto_hab$Region)), regions_check)
})

test_that("Data is present for all years between 2014 and 2021", {
  expect_equal(sort(unique(phyto_hab$Year)), c(2014:2021))
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(phyto_hab$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Datetime is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(phyto_hab$Datetime),
    "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  )))
})

test_that("The Date and Year variables are in alignment", {
  phyto_hab_t <- phyto_hab %>% dplyr::mutate(Year_t = lubridate::year(Date))
  expect_equal(phyto_hab_t$Year, phyto_hab_t$Year_t)
})

test_that("The Date and Datetime variables are in alignment", {
  phyto_hab_t <- phyto_hab %>% dplyr::mutate(Date_t = lubridate::date(Datetime))
  expect_equal(phyto_hab_t$Date, phyto_hab_t$Date_t)
})

test_that("The time zone of Datetime is PST", {
  expect_equal(lubridate::tz(phyto_hab$Datetime), "Etc/GMT+8")
})

test_that("All Genus names are as expected", {
  genus_check <- c(
    "Anabaenopsis",
    "Aphanizomenon",
    "Cylindrospermopsis",
    "Dolichospermum",
    "Microcystis",
    "Oscillatoria",
    "Planktothrix"
  )

  expect_equal(sort(unique(phyto_hab$Genus)), genus_check)
})

test_that("All AlgalType names are as expected", {
  AlgalTypes_check <- c("Cyanobacterium")
  expect_equal(sort(unique(phyto_hab$AlgalType)), AlgalTypes_check)
})

test_that("Count is at least 1", {
  expect_gte(min(phyto_hab$Count), 1)
})

test_that("OrganismsPerMl is greater than zero", {
  expect_gt(min(phyto_hab$OrganismsPerMl), 0)
})

