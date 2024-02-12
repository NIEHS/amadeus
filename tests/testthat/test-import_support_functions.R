# Tests for data import support functions

testthat::test_that("import support functions return expected.", {
  path <- list.files(
    "../testdata/geos/aqc_tavg_1hr_g1440x721_v1/",
    full.names = TRUE
  )
  expect_error(
    geos_strsplit(
      path = path,
      collection = TRUE,
      date = TRUE,
      datetime = TRUE
    )
  )
  path_split_d <- geos_strsplit(
    path = path,
    date = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- geos_strsplit(
    path = path,
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})
