
test_that("No variables contain `NA` values", {
  count_na <- function(var) {
    phyto_edb %>%
      dplyr::summarize(n_na = sum(is.na(.data[[var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var) {
    eval(bquote(expect_equal(count_na(.(test_var)), 0)))
  }

  # Test all variables except for Species
  expect_no_NAs("Station")
  expect_no_NAs("Region")
  expect_no_NAs("Year")
  expect_no_NAs("Date")
  expect_no_NAs("DateTime")
  expect_no_NAs("Taxon")
  expect_no_NAs("Genus")
  expect_no_NAs("AlgalType")
  expect_no_NAs("Count")
  expect_no_NAs("OrganismsPerMl")
})

test_that("All variables are correct class", {
  expect_equal(class(phyto_edb$Station), "character")
  expect_equal(class(phyto_edb$Region), "character")
  expect_equal(class(phyto_edb$Year), "numeric")
  expect_equal(class(phyto_edb$Date), "Date")
  expect_equal(class(phyto_edb$DateTime), c("POSIXct", "POSIXt"))
  expect_equal(class(phyto_edb$Taxon), "character")
  expect_equal(class(phyto_edb$Genus), "character")
  expect_equal(class(phyto_edb$Species), "character")
  expect_equal(class(phyto_edb$AlgalType), "character")
  expect_equal(class(phyto_edb$Count), "numeric")
  expect_equal(class(phyto_edb$OrganismsPerMl), "numeric")
})

test_that("The time zone of DateTime is PST", {
  expect_equal(lubridate::tz(phyto_edb$DateTime), "Etc/GMT+8")
})

test_that("There are no zeros in Count and OrganismsPerMl", {
  expect_true(!any(phyto_edb$Count == 0))
  expect_true(!any(phyto_edb$OrganismsPerMl == 0))
})

test_that("All Region names are as expected", {
  Regions <- c("Central Delta", "Sacramento", "San Joaquin")
  expect_equal(sort(unique(phyto_edb$Region)), Regions)
})

test_that("All AlgalType names are as expected", {
  AlgalTypes <- c(
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

  expect_equal(sort(unique(phyto_edb$AlgalType)), AlgalTypes)
})

test_that("All Stations are as expected", {
  Stations <- c("C9", "D12", "D16", "D19", "D22", "D24", "D26", "D28A", "D4", "NZ068")
  expect_equal(sort(unique(phyto_edb$Station)), Stations)
})

test_that("The Date and DateTime variables are in alignment", {
  phyto_edb_t <- phyto_edb %>% dplyr::mutate(Date_t = lubridate::date(DateTime))
  expect_true(all(phyto_edb_t$Date == phyto_edb_t$Date_t))
})

test_that("Data is present for all years between 2014 and 2021", {
  expect_equal(sort(unique(phyto_edb$Year)), c(2014:2021))
})
