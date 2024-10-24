################################################################################
##### unit and integration tests for Sum of Exponential Decay functions

################################################################################
##### sum_edc
testthat::test_that("sum_edc", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"),
                keepgeom = TRUE, crs = "EPSG:4326")
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  tri_r <- terra::project(tri_r, terra::crs(ncpt))

  targcols <- grep("FUGITIVE_", names(tri_r), value = TRUE)
  testthat::expect_no_error(
    tri_sedc <-
      sum_edc(
        locs = ncpt,
        from = tri_r,
        locs_id = "site_id",
        sedc_bandwidth = 30000,
        target_fields = targcols
      )
  )
  testthat::expect_s3_class(tri_sedc, "data.frame")

  testthat::expect_no_error(
    sum_edc(
      locs = sf::st_as_sf(ncpt),
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      sedc_bandwidth = 30000,
      target_fields = targcols
    )
  )

  # with geometry
  testthat::expect_no_error(
    tri_sedc_geom <- sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      sedc_bandwidth = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )
  testthat::expect_s4_class(tri_sedc_geom, "SpatVector")

  # warning case: duplicate field names between locs and from
  ncpta <- ncpt
  ncpta$YEAR <- 2018
  testthat::expect_warning(
    sum_edc(
      locs = ncpta,
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      sedc_bandwidth = 30000,
      target_fields = targcols
    )
  )

})
