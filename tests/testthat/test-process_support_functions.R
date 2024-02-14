# Tests for data import support functions

testthat::test_that("proccess support functions return expected.", {
  path <- list.files(
    "../testdata/geos/aqc_tavg_1hr_g1440x721_v1/",
    full.names = TRUE
  )
  expect_error(
    process_geos_collection(
      path = path,
      collection = TRUE,
      date = TRUE,
      datetime = TRUE
    )
  )
  path_split_d <- process_geos_collection(
    path = path,
    date = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- process_geos_collection(
    path = path,
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})
