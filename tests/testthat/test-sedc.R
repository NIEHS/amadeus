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
    terra::vect(ncp, geom = c("lon", "lat"), keepgeom = TRUE, crs = "EPSG:4326")
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  tri_r <- terra::project(tri_r, terra::crs(ncpt))

  targcols <- grep("STACK_AIR_", names(tri_r), value = TRUE)
  testthat::expect_no_error(
    tri_sedc <-
      sum_edc(
        locs = ncpt,
        from = tri_r,
        locs_id = "site_id",
        decay_range = 30000,
        target_fields = targcols
      )
  )
  testthat::expect_s3_class(tri_sedc, "data.frame")
  testthat::expect_equal(attr(tri_sedc, "sedc_threshold"), 150000)

  testthat::expect_no_error(
    sum_edc(
      locs = sf::st_as_sf(ncpt),
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols
    )
  )

  # with geometry terra
  testthat::expect_no_error(
    tri_sedc_terra <- sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols,
      geom = "terra"
    )
  )
  testthat::expect_s4_class(tri_sedc_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    tri_sedc_sf <- sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols,
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(tri_sedc_sf))

  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )

  # warning case: duplicate field names between locs and from
  ncpta <- ncpt
  ncpta$YEAR <- 2018
  testthat::expect_warning(
    sum_edc(
      locs = ncpta,
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols
    )
  )

  testthat::expect_no_error(
    tri_sedc_c0 <- sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = targcols[1]
    )
  )
  testthat::expect_s3_class(tri_sedc_c0, "data.frame")
  testthat::expect_no_error(
    tri_sedc_c0_df <- sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = data.frame(c0 = tri_r[[targcols[1]]][[1]])
    )
  )
  testthat::expect_s3_class(tri_sedc_c0_df, "data.frame")

  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = "NOT_A_COLUMN"
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = ""
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = rep(1, nrow(tri_r) - 1)
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = 1
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols[1],
      use_threshold = NA
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = "site_id",
      decay_range = -10,
      target_fields = targcols[1]
    )
  )

  far_locs_df <- data.frame(
    lon = -10,
    lat = -10,
    site_id = "far-away-site",
    time = 2018L
  )
  far_locs <- terra::vect(
    far_locs_df,
    geom = c("lon", "lat"),
    keepgeom = TRUE,
    crs = "EPSG:4326"
  )
  far_locs <- terra::project(far_locs, terra::crs(tri_r))
  testthat::expect_no_error(
    tri_sedc_empty <- sum_edc(
      locs = far_locs,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols
    )
  )
  sedc_cols <- setdiff(names(tri_sedc_empty), "site_id")
  testthat::expect_true(all(tri_sedc_empty[, sedc_cols] == 0))
  testthat::expect_no_error(
    tri_sedc_empty_sf <- sum_edc(
      locs = far_locs,
      from = tri_r,
      locs_id = "site_id",
      decay_range = 30000,
      target_fields = targcols,
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(tri_sedc_empty_sf))

  outside_locs_df <- data.frame(
    lon = -76.5,
    lat = 35.95013,
    site_id = "outside-threshold-site",
    time = 2018L
  )
  outside_locs <- terra::vect(
    outside_locs_df,
    geom = c("lon", "lat"),
    keepgeom = TRUE,
    crs = "EPSG:4326"
  )
  outside_locs <- terra::project(outside_locs, terra::crs(tri_r))
  tri_sedc_threshold <- sum_edc(
    locs = outside_locs,
    from = tri_r,
    locs_id = "site_id",
    decay_range = 30000,
    target_fields = targcols,
    use_threshold = TRUE
  )
  tri_sedc_all_sources <- sum_edc(
    locs = outside_locs,
    from = tri_r,
    locs_id = "site_id",
    decay_range = 30000,
    target_fields = targcols,
    use_threshold = FALSE
  )
  sedc_cols_cmp <- setdiff(names(tri_sedc_threshold), "site_id")
  testthat::expect_true(
    any(
      as.numeric(tri_sedc_all_sources[1, sedc_cols_cmp]) >
        as.numeric(tri_sedc_threshold[1, sedc_cols_cmp])
    )
  )
})
