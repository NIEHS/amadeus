# Tests for covariate generation support functions

testthat::test_that("sites_vector handles data type and missing columns.", {
  withr::local_package("terra")
  sites <- readRDS("../testdata/sites_sample.RDS")
  narr <-
    import_narr(
      date_start = "2018-01-01",
      date_end = "2018-01-01",
      variable = "weasd",
      directory_with_data = "../testdata/narr/weasd/"
    )
  # expect error when missing `lat` or `lon`
  expect_error(
    covar_narr(
      data = narr,
      sites = subset(
        sites,
        select = "lon"
      ),
      identifier = "site_id"
    )
  )
  # expect error when sites are SpatVector
  expect_error(
    covar_narr(
      data = narr,
      sites = terra::vect(
        sites,
        geom = c("lon", "lat"),
        crs = "EPSG:4326"
      ),
      identifier = "site_id"
    )
  )
})
