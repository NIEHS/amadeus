
# test OpenLandMap ####
# nolint start
testthat::test_that("process_olm", {
  withr::local_package("terra")
  tmwm <- testthat::test_path("..", "testdata", "openlandmap",
    "no2_s5p.l3.trop.tmwm.p50_p90_2km_a_20180501_20221130_go_epsg.4326_v20221219_test.tif")
  testthat::expect_no_error(
    olm <- process_olm(path = tmwm)
  )
  testthat::expect_s4_class(olm, "SpatRaster")
  testthat::expect_error(
    process_olm(path = 1L)
  )

  # test with cropping extent
  testthat::expect_no_error(
    olm_ext <- process_olm(path = tmwm, extent = terra::ext(olm))
  )
})
# nolint end
