
test_that("No expected variables contain `NA` values", {
  count_na <- function(test_var) {
    mc_vis_index_wq %>%
      dplyr::summarize(n_na = sum(is.na(.data[[test_var]]))) %>%
      dplyr::pull(n_na)
  }

  expect_no_NAs <- function(test_var2) {
    eval(bquote(expect_equal(count_na(.(test_var2)), 0)))
  }

  # Test all variables except for Secchi and Temperature
  vars_no_NA <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Region",
    "Year",
    "Month",
    "Date",
    "Microcystis"
  )

  purrr::map(vars_no_NA, expect_no_NAs)
})

test_that("Data dimensions are correct", {
  expect_equal(nrow(mc_vis_index_wq), 7339)
  expect_equal(ncol(mc_vis_index_wq), 11)

  name_check <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Region",
    "Year",
    "Month",
    "Date",
    "Microcystis",
    "Secchi",
    "Temperature"
  )

  expect_equal(names(mc_vis_index_wq), name_check)
})

test_that("There are no duplicate records", {
  mc_vis_index_wq_t <- mc_vis_index_wq %>% dplyr::mutate(ID = paste(Date, Source, Station, sep = "_"))
  expect_equal(length(unique(mc_vis_index_wq_t$ID)), nrow(mc_vis_index_wq_t))
})

test_that("All variables are correct class", {
  expect_equal(class(mc_vis_index_wq$Source), "character")
  expect_equal(class(mc_vis_index_wq$Station), "character")
  expect_equal(class(mc_vis_index_wq$Latitude), "numeric")
  expect_equal(class(mc_vis_index_wq$Longitude), "numeric")
  expect_equal(class(mc_vis_index_wq$Region), "character")
  expect_equal(class(mc_vis_index_wq$Year), "numeric")
  expect_equal(class(mc_vis_index_wq$Month), "numeric")
  expect_equal(class(mc_vis_index_wq$Date), "Date")
  expect_equal(class(mc_vis_index_wq$Microcystis), "integer")
  expect_equal(class(mc_vis_index_wq$Secchi), "numeric")
  expect_equal(class(mc_vis_index_wq$Temperature), "numeric")
})

test_that("All Sources are as expected", {
  sources_check <- c("DWR_EMP", "DWR_NCRO", "FMWT", "STN")
  expect_equal(sort(unique(mc_vis_index_wq$Source)), sources_check)
})

test_that("All Stations are as expected", {
  stations_check <- c(
    "072",
    "073",
    "701",
    "703",
    "704",
    "705",
    "706",
    "707",
    "708",
    "709",
    "710",
    "711",
    "712",
    "713",
    "715",
    "716",
    "717",
    "719",
    "721",
    "722",
    "723",
    "724",
    "735",
    "736",
    "794",
    "795",
    "796",
    "797",
    "799",
    "804",
    "806",
    "807",
    "808",
    "809",
    "810",
    "811",
    "812",
    "813",
    "814",
    "815",
    "902",
    "903",
    "904",
    "905",
    "906",
    "908",
    "909",
    "910",
    "911",
    "912",
    "913",
    "914",
    "915",
    "918",
    "919",
    "920",
    "921",
    "922",
    "923",
    "BET",
    "BLP",
    "C3A",
    "C9",
    "D12",
    "D16",
    "D19",
    "D22",
    "D24",
    "D26",
    "D28A",
    "D4",
    "DGL",
    "DRB",
    "EZ2",
    "EZ2-SJR",
    "EZ6",
    "EZ6-SJR",
    "FAL",
    "FCT",
    "GCT",
    "GLC",
    "GLE",
    "HCHM",
    "HLT",
    "HOL",
    "MD10A",
    "MHO",
    "MIR",
    "MOK",
    "MRU",
    "MRX",
    "MUP",
    "NZ068",
    "OAD",
    "OBI",
    "ODM",
    "OH1",
    "ORI",
    "ORM",
    "ORQ",
    "ORX",
    "OSJ",
    "P8",
    "PDC",
    "RSCC",
    "SJD",
    "SOI",
    "SXS",
    "TRN",
    "TSL",
    "TWA",
    "VCU",
    "WCI",
    "WDC"
  )

  expect_equal(sort(unique(mc_vis_index_wq$Station)), stations_check)
})

test_that("All Latitude and Longitude values are within expected ranges", {
  expect_gte(min(mc_vis_index_wq$Latitude), 37)
  expect_lte(max(mc_vis_index_wq$Latitude), 39)
  expect_gte(min(mc_vis_index_wq$Longitude), -122)
  expect_lte(max(mc_vis_index_wq$Longitude), -121)
})

test_that("All Region names are as expected", {
  regions_check <- c(
    "Cache/Liberty",
    "East Delta",
    "Franks",
    "Lower SJ",
    "Lower Sac",
    "OMR",
    "South Delta",
    "Upper Sac"
  )

  expect_equal(sort(unique(mc_vis_index_wq$Region)), regions_check)
})

test_that("Data is present for years 2007 through 2021", {
  expect_equal(sort(unique(mc_vis_index_wq$Year)), c(2007:2021))
})

test_that("Values in Month are between 1 and 12", {
  expect_gte(min(mc_vis_index_wq$Month), 1)
  expect_lte(max(mc_vis_index_wq$Month), 12)
})

test_that("Date is formatted correctly", {
  expect_true(all(stringr::str_detect(
    as.character(mc_vis_index_wq$Date),
    "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  )))
})

test_that("The Date and Year variables are in alignment", {
  mc_vis_index_wq_t <- mc_vis_index_wq %>% dplyr::mutate(Year_t = lubridate::year(Date))
  expect_equal(mc_vis_index_wq_t$Year, mc_vis_index_wq_t$Year_t)
})

test_that("The Date and Month variables are in alignment", {
  mc_vis_index_wq_t <- mc_vis_index_wq %>% dplyr::mutate(Month_t = lubridate::month(Date))
  expect_equal(mc_vis_index_wq_t$Month, mc_vis_index_wq_t$Month_t)
})

test_that("Microcystis visual index values are between 1 and 5", {
  expect_gte(min(mc_vis_index_wq$Microcystis), 1)
  expect_lte(max(mc_vis_index_wq$Microcystis), 5)
})

test_that("Secchi depth values are within expected ranges", {
  expect_gte(min(mc_vis_index_wq$Secchi, na.rm = TRUE), 1)
  expect_lte(max(mc_vis_index_wq$Secchi, na.rm = TRUE), 1000)
})

test_that("Water temperature values are within expected ranges", {
  expect_gte(min(mc_vis_index_wq$Temperature, na.rm = TRUE), 5)
  expect_lte(max(mc_vis_index_wq$Temperature, na.rm = TRUE), 30)
})

