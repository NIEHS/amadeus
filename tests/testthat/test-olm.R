################################################################################
# Offline tests for process_olm() using bundled fixture raster.
#
# The OLM download path and STAC listing path are exercised in
# tests/testthat/test-olm-live.R (live, scheduled CI only).
################################################################################

testthat::test_that(
  "process_olm(path=<bundled tif>): returns a SpatRaster",
  {
    withr::local_package("terra")
    tmwm <- testthat::test_path(
      "..", "testdata", "openlandmap",
      paste0(
        "no2_s5p.l3.trop.tmwm.p50_p90_2km_a_20180501_",
        "20221130_go_epsg.4326_v20221219_test.tif"
      )
    )
    olm <- amadeus:::process_olm(path = tmwm)
    testthat::expect_s4_class(olm, "SpatRaster")
    testthat::expect_gt(terra::ncell(olm), 0)
  }
)

testthat::test_that(
  "process_olm(path=1L): errors on non-character path",
  {
    testthat::expect_error(amadeus:::process_olm(path = 1L))
  }
)

testthat::test_that(
  "process_olm(path, extent=<ext>): respects cropping extent",
  {
    withr::local_package("terra")
    tmwm <- testthat::test_path(
      "..", "testdata", "openlandmap",
      paste0(
        "no2_s5p.l3.trop.tmwm.p50_p90_2km_a_20180501_",
        "20221130_go_epsg.4326_v20221219_test.tif"
      )
    )
    olm <- amadeus:::process_olm(path = tmwm)
    olm_ext <- amadeus:::process_olm(path = tmwm, extent = terra::ext(olm))
    testthat::expect_s4_class(olm_ext, "SpatRaster")
  }
)
