
test_that("No expected variables contain `NA` values", {
  count_na <- function(test_var) {
    hab_toxins %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables
  purrr::map(names(hab_toxins), expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(hab_toxins), 643)
  expect_equal(ncol(hab_toxins), 11)

  name_check <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Region",
    "Year",
    "Month",
    "Date"    ,
    "Analyte" ,
    "Result"   ,
    "Result_Sign"
  )

  expect_equal(names(hab_toxins), name_check)
})

# There are known duplicates in the Preece and USGS data sets. We decided to
  # keep them in there for now.
test_that("There are no duplicate records, excluding Preece and USGS", {
  hab_toxins_t <- hab_toxins %>%
    dplyr::filter(!Source %in% c("Preece", "USGS")) %>%
    dplyr::mutate(ID = paste(Date, Source, Station, Analyte, sep = "_"))
  expect_equal(length(unique(hab_toxins_t$ID)), nrow(hab_toxins_t))
})

test_that("All variables are correct class", {
  expect_equal(class(hab_toxins$Source), "character")
  expect_equal(class(hab_toxins$Station), "character")
  expect_equal(class(hab_toxins$Latitude), "numeric")
  expect_equal(class(hab_toxins$Longitude), "numeric")
  expect_equal(class(hab_toxins$Region), "character")
  expect_equal(class(hab_toxins$Year), "numeric")
  expect_equal(class(hab_toxins$Month), "numeric")
  expect_equal(class(hab_toxins$Date), "Date")
  expect_equal(class(hab_toxins$Analyte), "character")
  expect_equal(class(hab_toxins$Result), "numeric")
  expect_equal(class(hab_toxins$Result_Sign), "character")
})

test_that("All Sources are as expected", {
  sources_check <- c("CVRWQCB", "DWR", "EastBay", "Nautilus", "Preece", "USGS")
  expect_equal(sort(unique(hab_toxins$Source)), sources_check)
})

test_that("All Stations are as expected", {
  stations_check <- c(
    "ALG-001",
    "ALG-002",
    "ALG-003",
    "ALG-004",
    "ALG-005",
    "ALG-006",
    "BPP",
    "BigBreak",
    "CCF",
    "DEC/TOL",
    "DHAB001",
    "DHAB002",
    "DHAB003",
    "DHAB004",
    "DHAB005",
    "DHAB006",
    "DHAB007",
    "DHAB008",
    "DHAB009",
    "DHAB010",
    "FRK",
    "JPT",
    "LIB",
    "MDM",
    "MI",
    "RRI",
    "VER"
  )

  expect_equal(sort(unique(hab_toxins$Station)), stations_check)
})

test_that("All Latitude and Longitude values are within expected ranges", {
  expect_gte(min(hab_toxins$Latitude), 37)
  expect_lte(max(hab_toxins$Latitude), 39)
  expect_gte(min(hab_toxins$Longitude), -122)
  expect_lte(max(hab_toxins$Longitude), -121)
})

test_that("All Region names are as expected", {
  regions_check <- c(
    "Cache Slough/Liberty Island",
    "Clifton Court",
    "Lower Sacramento",
    "Lower San Joaquin",
    "SDWSC",
    "San Joaquin",
    "South Delta",
    "Upper Sacramento",
    "Vernalis"
  )

  expect_equal(sort(unique(hab_toxins$Region)), regions_check)
})

test_that("Data is present for year 2021", {
  expect_equal(sort(unique(hab_toxins$Year)), 2021)
})

test_that("Values in Month are between 1 and 12", {
  expect_gte(min(hab_toxins$Month), 1)
  expect_lte(max(hab_toxins$Month), 12)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(hab_toxins$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("The Date and Year variables are in alignment", {
  hab_toxins_t <- hab_toxins %>% dplyr::mutate(Year_t = lubridate::year(Date))
  expect_equal(hab_toxins_t$Year, hab_toxins_t$Year_t)
})

test_that("The Date and Month variables are in alignment", {
  hab_toxins_t <- hab_toxins %>% dplyr::mutate(Month_t = lubridate::month(Date))
  expect_equal(hab_toxins_t$Month, hab_toxins_t$Month_t)
})

test_that("All Analyte names are as expected", {
  analytes_check <- c("Anabaenopeptins", "Anatoxins", "Microcystins", "Saxitoxins")
  expect_equal(sort(unique(hab_toxins$Analyte)), analytes_check)
})

test_that("Cyanotoxin results are greater than or equal to zero", {
  expect_gte(min(hab_toxins$Result), 0)
})

test_that("All values in Result_Sign are as expected", {
  signs_check <- c("=", ">", "ND")
  expect_equal(sort(unique(hab_toxins$Result_Sign)), signs_check)
})

