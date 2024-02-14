# Tests for covariate generation support functions

testthat::test_that(
  "procces_locs_vector handles data type and missing columns.", {
  withr::local_package("terra")
  locs <- readRDS("../testdata/sites_nc.RDS")
  narr <-
    process_narr(
      date_start = "2018-01-01",
      date_end = "2018-01-01",
      variable = "weasd",
      path = "../testdata/narr/weasd/"
    )
  # expect error when missing `lat` or `lon`
  expect_error(
    calc_narr(
      from = narr,
      locs = subset(
        locs,
        select = "lon"
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are SpatVector
  expect_error(
    calc_narr(
      from = narr,
      locs = terra::vect(
        locs,
        geom = c("lon", "lat"),
        crs = "EPSG:4326"
      ),
      locs_id = "site_id"
    )
  )
})
