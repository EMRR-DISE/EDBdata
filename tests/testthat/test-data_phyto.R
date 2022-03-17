
test_that("No variables contain `NA` values", {
  count_na <- function(test_var) {
    phyto_edb %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for Species
  vars_no_NAs <- stringr::str_subset(names(phyto_edb), "Species", negate = TRUE)
  purrr::map(vars_no_NAs, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(phyto_edb), 4353)
  expect_equal(ncol(phyto_edb), 11)

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

  expect_equal(names(phyto_edb), name_check)
})

test_that("All variables are correct class", {
  expect_equal(class(phyto_edb$Station), "character")
  expect_equal(class(phyto_edb$Region), "character")
  expect_equal(class(phyto_edb$Year), "numeric")
  expect_equal(class(phyto_edb$Date), "Date")
  expect_equal(class(phyto_edb$Datetime), c("POSIXct", "POSIXt"))
  expect_equal(class(phyto_edb$Taxon), "character")
  expect_equal(class(phyto_edb$Genus), "character")
  expect_equal(class(phyto_edb$Species), "character")
  expect_equal(class(phyto_edb$AlgalType), "character")
  expect_equal(class(phyto_edb$Count), "numeric")
  expect_equal(class(phyto_edb$OrganismsPerMl), "numeric")
})

test_that("All Stations are as expected", {
  stations_check <- c("C9", "D12", "D16", "D19", "D22", "D24", "D26", "D28A", "D4", "NZ068")
  expect_equal(sort(unique(phyto_edb$Station)), stations_check)
})

test_that("All Region names are as expected", {
  regions_check <- c("Central Delta", "Sacramento", "San Joaquin")
  expect_equal(sort(unique(phyto_edb$Region)), regions_check)
})

test_that("Data is present for all years between 2014 and 2021", {
  expect_equal(sort(unique(phyto_edb$Year)), c(2014:2021))
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(phyto_edb$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("Datetime is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(phyto_edb$Datetime),
    "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  )))
})

test_that("The Date and Datetime variables are in alignment", {
  phyto_edb_t <- phyto_edb %>% dplyr::mutate(Date_t = lubridate::date(Datetime))
  expect_true(all(phyto_edb_t$Date == phyto_edb_t$Date_t))
})

test_that("The time zone of Datetime is PST", {
  expect_equal(lubridate::tz(phyto_edb$Datetime), "Etc/GMT+8")
})

test_that("All AlgalType names are as expected", {
  AlgalTypes_check <- c(
    "Centric Diatom",
    "Chrysophyte",
    "Ciliate",
    "Cryptophyte",
    "Cyanobacterium",
    "Dinoflagellate",
    "Euglenoid",
    "Green Alga",
    "Haptophyte",
    "Pennate Diatom",
    "Synurophyte"
  )

  expect_equal(sort(unique(phyto_edb$AlgalType)), AlgalTypes_check)
})

test_that("Count is at least 1", {
  expect_gte(min(phyto_edb$Count), 1)
})

test_that("OrganismsPerMl is greater than zero", {
  expect_gt(min(phyto_edb$OrganismsPerMl), 0)
})

