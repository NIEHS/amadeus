################################################################################
##### unit and integration tests for NASA GEOS-CF functions
# nolint start

################################################################################
##### download_geos
testthat::test_that("download_geos", {
  skip_if_offline()
  skip_if(
    Sys.getenv("NASA_EARTHDATA_TOKEN") == "",
    message = "No NASA token available"
  )

  withr::with_tempdir({
    # function parameters
    date_start <- "2019-09-09"
    date_end <- "2019-09-09"
    collections <- c("aqc_tavg_1hr_g1440x721_v1", "chm_inst_1hr_g1440x721_p23")

    result <- suppressWarnings(
      download_geos(
        date = c(date_start, date_end),
        collection = collections,
        nasa_earth_data_token = Sys.getenv("NASA_EARTHDATA_TOKEN"),
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )

    # Check return structure (httr2 download_run_method pattern)
    testthat::expect_type(result, "list")
    testthat::expect_true(all(c("success", "failed", "skipped") %in% names(result)))

    # Check that some files were processed
    total <- result$success + result$failed + result$skipped
    testthat::expect_gt(total, 0)
  })
})

testthat::test_that("download_geos (single date)", {
  skip_if_offline()
  skip_if(
    Sys.getenv("NASA_EARTHDATA_TOKEN") == "",
    message = "No NASA token available"
  )

  withr::with_tempdir({
    # function parameters
    date <- "2019-09-09"
    collections <- c("aqc_tavg_1hr_g1440x721_v1", "chm_inst_1hr_g1440x721_p23")

    result <- suppressWarnings(
      download_geos(
        date = date,
        collection = collections,
        nasa_earth_data_token = Sys.getenv("NASA_EARTHDATA_TOKEN"),
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )

    # Check return structure (httr2 download_run_method pattern)
    testthat::expect_type(result, "list")
    testthat::expect_true(all(c("success", "failed", "skipped") %in% names(result)))

    # Check that some files were processed
    total <- result$success + result$failed + result$skipped
    testthat::expect_gt(total, 0)
  })
})

testthat::test_that("download_geos remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
        download_geos(
          date = "2019-09-09",
          collection = "aqc_tavg_1hr_g1440x721_v1",
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_geos mock download with hash", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_geos(
          date = "2019-09-09",
          collection = "aqc_tavg_1hr_g1440x721_v1",
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### process_geos
testthat::test_that("process_geos (no errors)", {
  withr::local_package("terra")
  collections <- c(
    "a",
    "c"
  )
  # expect function
  testthat::expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date = c("2018-01-01", "2018-01-01"),
        variable = "O3",
        path = testthat::test_path(
          "..",
          "testdata",
          "geos",
          collection
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collection
    if (collection == "a") {
      testthat::expect_true(
        dim(geos)[3] == 1
      )
    } else if (collection == "c") {
      testthat::expect_true(
        dim(geos)[3] == 5
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    geos_ext <- process_geos(
      date = c("2018-01-01", "2018-01-01"),
      variable = "O3",
      path = testthat::test_path(
        "..",
        "testdata",
        "geos",
        "c"
      ),
      extent = terra::ext(geos)
    )
  )
})

testthat::test_that("process_geos (single date)", {
  withr::local_package("terra")
  collections <- c(
    "a",
    "c"
  )
  # expect function
  testthat::expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date = "2018-01-01",
        variable = "O3",
        path = testthat::test_path(
          "..",
          "testdata",
          "geos",
          collection
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collection
    if (collection == "a") {
      testthat::expect_true(
        dim(geos)[3] == 1
      )
    } else if (collection == "c") {
      testthat::expect_true(
        dim(geos)[3] == 5
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    geos_ext <- process_geos(
      date = "2018-01-01",
      variable = "O3",
      path = testthat::test_path(
        "..",
        "testdata",
        "geos",
        "c"
      ),
      extent = terra::ext(geos)
    )
  )
})

testthat::test_that("process_geos (expected errors)", {
  # expect error without variable
  testthat::expect_error(
    process_geos()
  )
  # expect error on directory without data
  testthat::expect_error(
    process_geos(
      variable = "O3",
      path = "./empty/directory"
    )
  )
})

################################################################################
##### process_geos daily_agg
testthat::test_that("process_geos daily_agg=FALSE default is unchanged", {
  withr::local_package("terra")
  # Default (daily_agg=FALSE) must return same result as before
  geos_default <- process_geos(
    date = "2018-01-01",
    variable = "O3",
    path = testthat::test_path("..", "testdata", "geos", "c")
  )
  geos_explicit_false <- process_geos(
    date = "2018-01-01",
    variable = "O3",
    path = testthat::test_path("..", "testdata", "geos", "c"),
    daily_agg = FALSE
  )
  testthat::expect_equal(terra::nlyr(geos_default), terra::nlyr(geos_explicit_false))
  testthat::expect_equal(terra::values(geos_default), terra::values(geos_explicit_false))
})

testthat::test_that("process_geos daily_agg collapses sub-daily layers", {
  withr::local_package("terra")
  src_file <- testthat::test_path(
    "..", "testdata", "geos", "c",
    "GEOS-CF.v01.rpl.chm_inst_1hr_g1440x721_p23.20180101_0000z.nc4"
  )
  # Make src_file absolute before entering withr::with_tempdir
  src_file <- normalizePath(src_file, mustWork = TRUE)

  withr::with_tempdir({
    tmpdir <- getwd()
    # Two files: 0000z and 0100z for the same day
    file.copy(src_file, file.path(tmpdir, "GEOS-CF.v01.rpl.chm_inst_1hr_g1440x721_p23.20180101_0000z.nc4"))
    file.copy(src_file, file.path(tmpdir, "GEOS-CF.v01.rpl.chm_inst_1hr_g1440x721_p23.20180101_0100z.nc4"))

    geos_sub <- suppressMessages(
      process_geos(date = "2018-01-01", variable = "O3", path = tmpdir)
    )
    geos_daily_mean <- suppressMessages(
      process_geos(date = "2018-01-01", variable = "O3", path = tmpdir,
                   daily_agg = TRUE, fun = "mean")
    )
    geos_daily_max <- suppressMessages(
      process_geos(date = "2018-01-01", variable = "O3", path = tmpdir,
                   daily_agg = TRUE, fun = "max")
    )

    # Two files × 5 pressure levels = 10 sub-daily layers
    testthat::expect_equal(terra::nlyr(geos_sub), 10)
    # Daily agg preserves pressure level structure: 5 output layers
    testthat::expect_equal(terra::nlyr(geos_daily_mean), 5)
    # CRS is preserved
    testthat::expect_false(terra::crs(geos_daily_mean) == "")
    testthat::expect_match(terra::crs(geos_daily_mean, describe = TRUE)$code, "4326")
    # Time is set to midnight UTC of the aggregated date
    testthat::expect_true("POSIXt" %in% class(terra::time(geos_daily_mean)))
    testthat::expect_true(all(
      format(as.Date(terra::time(geos_daily_mean)), "%Y%m%d") == "20180101"
    ))
    # max >= mean (both files are identical, so max == mean == values)
    testthat::expect_equal(terra::nlyr(geos_daily_max), 5)
    testthat::expect_true(all(
      terra::values(geos_daily_max) >= terra::values(geos_daily_mean),
      na.rm = TRUE
    ))
  })
})


testthat::test_that("calculate_geos", {
  withr::local_package("terra")
  withr::local_package("data.table")
  collections <- c(
    "a",
    "c"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (r in seq_along(radii)) {
      geos <-
        process_geos(
          date = c("2018-01-01", "2018-01-01"),
          variable = "O3",
          path = testthat::test_path(
            "..",
            "testdata",
            "geos",
            collection
          )
        )
      geos_covariate <-
        calculate_geos(
          from = geos,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      geos_covariate <- calc_setcolumns(
        from = geos_covariate,
        lag = 0,
        dataset = "geos",
        locs_id = "site_id"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(geos_covariate) == "data.frame"
      )
      # expect 4 columns
      testthat::expect_true(
        ncol(geos_covariate) == 4
      )
      # expect numeric value
      testthat::expect_true(
        class(geos_covariate[, 4]) == "numeric"
      )
      # expect $time is class POSIXt
      testthat::expect_true(
        "POSIXt" %in% class(geos_covariate$time)
      )
    }
  }
  # with included geometry terra
  testthat::expect_no_error(
    geos_covariate_terra <- calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(geos_covariate_terra),
    4
  )
  testthat::expect_true(
    "SpatVector" %in% class(geos_covariate_terra)
  )

  # with included geometry sf
  testthat::expect_no_error(
    geos_covariate_sf <- calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(geos_covariate_sf),
    5
  )
  testthat::expect_true(
    "sf" %in% class(geos_covariate_sf)
  )

  testthat::expect_error(
    calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})
# nolint end

################################################################################
##### calculate_geos .by interface

testthat::test_that("calculate_geos .by wiring aggregates rows", {
  withr::local_package("terra")
  from_rast <- terra::rast(nrows = 2, ncols = 2, vals = 5)
  terra::ext(from_rast) <- c(-80, -78, 34, 36)
  terra::crs(from_rast) <- "EPSG:4326"
  names(from_rast) <- "pm25_850_20200101_000000"
  locs_df <- data.frame(site_id = "A", lon = -79, lat = 35)
  fake_extracted <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXlt(
      c("2020-01-01 00:00:00", "2020-01-01 06:00:00"),
      tz = "UTC"
    ),
    level = c("850", "850"),
    pm25_0 = c(10.0, 20.0)
  )
  testthat::local_mocked_bindings(
    calc_prepare_locs = function(from, locs, locs_id, radius, geom) {
      sv <- terra::vect(locs_df, geom = c("lon", "lat"), crs = "EPSG:4326")
      list(sv, data.frame(site_id = "A"))
    },
    calc_worker = function(...) fake_extracted,
    .package = "amadeus"
  )
  result_null <- suppressMessages(
    calculate_geos(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(result_null), 2L)
  result_mean <- suppressMessages(
    calculate_geos(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      .by = "day",
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(result_mean), 1L)
  testthat::expect_equal(result_mean$pm25_0, 15)
  testthat::expect_s3_class(result_mean$time, "POSIXct")
})

testthat::test_that("download_geos mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_geos(
          date = "2019-09-09",
          collection = "aqc_tavg_1hr_g1440x721_v1",
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})

################################################################################
##### calculate_geos backward compatibility

testthat::test_that("calculate_geos default .by NULL is backward-compatible", {
  withr::local_package("terra")
  from_rast <- terra::rast(nrows = 2, ncols = 2, vals = 5)
  terra::ext(from_rast) <- c(-80, -78, 34, 36)
  terra::crs(from_rast) <- "EPSG:4326"
  names(from_rast) <- "pm25_850_20200101_000000"
  locs_df <- data.frame(site_id = "A", lon = -79, lat = 35)
  fake_extracted <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXlt(
      c("2020-01-01 00:00:00", "2020-01-01 06:00:00"),
      tz = "UTC"
    ),
    level = c("850", "850"),
    pm25_0 = c(10.0, 20.0)
  )
  testthat::local_mocked_bindings(
    calc_prepare_locs = function(from, locs, locs_id, radius, geom) {
      sv <- terra::vect(locs_df, geom = c("lon", "lat"), crs = "EPSG:4326")
      list(sv, data.frame(site_id = "A"))
    },
    calc_worker = function(...) fake_extracted,
    .package = "amadeus"
  )
  # Default (.by = NULL) returns all rows unchanged
  result_default <- suppressMessages(
    calculate_geos(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(result_default), 2L)
})
