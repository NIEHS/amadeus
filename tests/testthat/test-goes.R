################################################################################
##### unit and integration tests for NOAA GOES ADP functions
# nolint start

################################################################################
##### goes_parse_start_datetime
testthat::test_that("goes_parse_start_datetime parses valid filename", {
  fname <- "OR_ADP-C3C02_G16_s20180010000000_e20180010001000_c20180010002000.nc"
  dt <- goes_parse_start_datetime(fname)
  testthat::expect_s3_class(dt, "POSIXct")
  testthat::expect_equal(format(dt, "%Y-%m-%d"), "2018-01-01")
  testthat::expect_equal(format(dt, "%H:%M:%S"), "00:00:00")
  testthat::expect_equal(attr(dt, "tzone"), "UTC")
})

testthat::test_that("goes_parse_start_datetime parses DOY correctly", {
  # DOY 032 of 2018 = February 1
  fname <- "OR_ADP-C3C02_G16_s20180320000000_e20180320001000_c20180320002000.nc"
  dt <- goes_parse_start_datetime(fname)
  testthat::expect_equal(format(dt, "%Y-%m-%d"), "2018-02-01")
})

testthat::test_that("goes_parse_start_datetime errors on bad filename", {
  testthat::expect_error(
    goes_parse_start_datetime("bad_filename.nc"),
    regexp = "Cannot parse"
  )
})

################################################################################
##### download_goes
testthat::test_that("download_goes errors without acknowledgement", {
  testthat::expect_error(
    download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "16",
      product = "ADP-C",
      directory_to_save = tempdir(),
      acknowledgement = FALSE
    )
  )
})

testthat::test_that("download_goes errors on invalid satellite", {
  testthat::expect_error(
    download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "99",
      product = "ADP-C",
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "satellite must be"
  )
})

testthat::test_that("download_goes errors on invalid product", {
  testthat::expect_error(
    download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "16",
      product = "BADPROD",
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "product must be one of"
  )
})

testthat::test_that("download_goes remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 0, failed = 0, skipped = 1),
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_goes(
          date = c("2018-01-01", "2018-01-01"),
          satellite = "16",
          product = "ADP-C",
          directory_to_save = ".",
          acknowledgement = TRUE,
          remove_command = TRUE
        )
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_goes download=FALSE deprecation warning", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 0, failed = 0, skipped = 1),
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_goes(
          date = c("2018-01-01", "2018-01-01"),
          satellite = "16",
          product = "ADP-C",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      ),
      regexp = "download=FALSE.*deprecated"
    )
  })
})

testthat::test_that("download_goes mock: hash=TRUE returns hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  # Mock httr2 request/response to return a fake XML with one file key
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(
          status_code = 200L,
          body = charToRaw(
            paste0(
              "<ListBucketResult>",
              "<Key>ADP-C/2018/001/OR_ADP-C3C02_G16_",
              "s20180010000000_e20180010001000_c20180010002000.nc</Key>",
              "</ListBucketResult>"
            )
          )
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      download_goes(
        date = c("2018-01-01", "2018-01-01"),
        satellite = "16",
        product = "ADP-C",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        hash = TRUE
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_goes dispatch via download_data", {
  for (alias in c("goes", "goes_adp", "GOES")) {
    testthat::expect_error(
      download_data(
        dataset_name = alias,
        date = c("2024-01-01", "2024-01-01"),
        satellite = "16",
        product = "ADP-C",
        directory_to_save = tempdir(),
        acknowledgement = FALSE
      )
    )
  }
})

testthat::test_that("download_goes single-date + download=FALSE returns listing", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(
          status_code = 200L,
          body = charToRaw(
            paste0(
              "<ListBucketResult>",
              "<Key>ADP-C/2018/001/OR_ADP-C3C02_G16_",
              "s20180010000000_e20180010001000_c20180010002000.nc</Key>",
              "</ListBucketResult>"
            )
          )
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      out <- suppressMessages(
        download_goes(
          date = "2018-01-01",
          satellite = "16",
          product = "ADP-C",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        )
      ),
      regexp = "download=FALSE.*deprecated"
    )
    testthat::expect_true(is.list(out))
    testthat::expect_equal(out$n_files, 1L)
    testthat::expect_true(length(out$urls) == 1L)
  })
})

testthat::test_that("download_goes warns when listing fails for one day", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      stop("synthetic listing failure")
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      out <- suppressMessages(
        download_goes(
          date = "2018-01-01",
          satellite = "16",
          product = "ADP-C",
          directory_to_save = ".",
          acknowledgement = TRUE
        )
      ),
      regexp = "Failed to list GOES files"
    )
    testthat::expect_true(is.list(out))
  })
})

testthat::test_that("download_goes hash=FALSE returns download result", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(
          status_code = 200L,
          body = charToRaw(
            paste0(
              "<ListBucketResult>",
              "<Key>ADP-C/2018/001/OR_ADP-C3C02_G16_",
              "s20180010000000_e20180010001000_c20180010002000.nc</Key>",
              "</ListBucketResult>"
            )
          )
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )
  testthat::local_mocked_bindings(
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    out <- suppressMessages(
      download_goes(
        date = "2018-01-01",
        satellite = "16",
        product = "ADP-C",
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = FALSE
      )
    )
    testthat::expect_equal(out$success, 1)
    testthat::expect_equal(out$failed, 0)
  })
})

################################################################################
##### process_goes
testthat::test_that("process_goes errors with no matching files", {
  withr::with_tempdir({
    testthat::expect_error(
      process_goes(
        date = c("2018-01-01", "2018-01-01"),
        variable = "Smoke",
        path = "."
      ),
      regexp = "No GOES ADP NetCDF files found"
    )
  })
})

testthat::test_that("process_goes errors when date range has no matches", {
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  testthat::expect_error(
    process_goes(
      date = c("2020-01-01", "2020-01-02"),
      variable = "Smoke",
      path = goes_dir
    ),
    regexp = "No GOES ADP files matching"
  )
})

testthat::test_that("process_goes handles unparseable filenames in date parser", {
  src_dir <- testthat::test_path("..", "testdata", "goes")
  withr::with_tempdir({
    ok <- list.files(src_dir, pattern = "OR_ADP.*\\.nc$", full.names = TRUE)
    file.copy(ok[1], ".", overwrite = TRUE)
    file.create("OR_ADP-C3C02_G16_badstamp.nc")
    testthat::expect_error(
      process_goes(
        date = c("2018-01-01", "2018-01-01"),
        variable = "Smoke",
        path = "."
      ),
      regexp = "matching the requested date range"
    )
  })
})

testthat::test_that("process_goes returns SpatRaster for Smoke", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  result <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-02"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  testthat::expect_s4_class(result, "SpatRaster")
  testthat::expect_gte(terra::nlyr(result), 1L)
  # layer names follow pattern {variable}_{YYYYMMDD}_{HHMMSS}
  testthat::expect_true(
    all(grepl("^Smoke_[0-9]{8}_[0-9]{6}$", names(result)))
  )
  # CRS should be EPSG:4326
  crs_str <- terra::crs(result, describe = TRUE)
  testthat::expect_true(
    grepl("4326", crs_str$authority) ||
      grepl("WGS", terra::crs(result))
  )
  # time should be set
  testthat::expect_false(any(is.na(terra::time(result))))
  testthat::expect_s3_class(terra::time(result)[1], "POSIXct")
})

testthat::test_that("process_goes returns SpatRaster for Dust", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  result <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Dust",
      path = goes_dir
    )
  )
  testthat::expect_s4_class(result, "SpatRaster")
  testthat::expect_true(
    all(grepl("^Dust_", names(result)))
  )
})

testthat::test_that("process_goes errors on missing variable", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  testthat::expect_error(
    suppressMessages(
      process_goes(
        date = c("2018-01-01", "2018-01-01"),
        variable = "NON_EXISTENT_VAR",
        path = goes_dir
      )
    ),
    regexp = "was not found"
  )
})

testthat::test_that("process_goes single date works", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  result <- suppressMessages(
    process_goes(
      date = "2018-01-01",
      variable = "Smoke",
      path = goes_dir
    )
  )
  testthat::expect_s4_class(result, "SpatRaster")
  testthat::expect_gte(terra::nlyr(result), 1L)
})

testthat::test_that("process_goes extent crops result", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  full <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ext_crop <- terra::ext(-98, -95, 31, 35)
  cropped <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir,
      extent = ext_crop
    )
  )
  testthat::expect_s4_class(cropped, "SpatRaster")
  # Cropped raster should have smaller or equal extent
  testthat::expect_lte(
    terra::ext(cropped)$xmax,
    terra::ext(full)$xmax
  )
})

testthat::test_that("process_goes via process_covariates dispatch", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  for (alias in c("goes", "goes_adp", "GOES")) {
    result <- suppressMessages(
      process_covariates(
        covariate = alias,
        date = c("2018-01-01", "2018-01-01"),
        variable = "Smoke",
        path = goes_dir
      )
    )
    testthat::expect_s4_class(result, "SpatRaster")
  }
})

################################################################################
##### calculate_goes
testthat::test_that("calculate_goes returns data.frame with expected structure", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-02"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = c("site_A", "site_B"),
    lon = c(-97.0, -95.5),
    lat = c(32.0, 34.0)
  )
  result <- suppressMessages(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = FALSE
    )
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true("time" %in% names(result))
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("calculate_goes with radius", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  testthat::expect_no_error(
    suppressMessages(
      calculate_goes(
        from = goes_r,
        locs = ncp,
        locs_id = "site_id",
        radius = 50000L,
        fun = "mean",
        geom = FALSE
      )
    )
  )
})

testthat::test_that("calculate_goes geom='terra' returns SpatVector", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  result <- suppressMessages(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      geom = "terra"
    )
  )
  testthat::expect_s4_class(result, "SpatVector")
})

testthat::test_that("calculate_goes geom='sf' returns sf", {
  withr::local_package("terra")
  withr::local_package("sf")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  result <- suppressMessages(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(result))
})

testthat::test_that("calculate_goes fun_temporal aggregates rows", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  # 3 files: 2 on 2018-01-01 and 1 on 2018-01-02
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-02"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  result_daily <- suppressMessages(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun_temporal = "mean",
      time_bucket = "day",
      geom = FALSE
    )
  )
  testthat::expect_s3_class(result_daily, "data.frame")
  # Should have 2 rows: one for 2018-01-01, one for 2018-01-02
  testthat::expect_equal(nrow(result_daily), 2L)
  testthat::expect_s3_class(result_daily$time, "POSIXct")
})

testthat::test_that("calculate_goes fun_temporal=NULL backward compatible", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  result <- suppressMessages(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun_temporal = NULL,
      geom = FALSE
    )
  )
  testthat::expect_s3_class(result, "data.frame")
  # Should return all time steps (2 files for 2018-01-01)
  testthat::expect_gte(nrow(result), 2L)
})

testthat::test_that("calculate_goes invalid fun_temporal errors", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(site_id = "site_A", lon = -97.0, lat = 32.0)
  testthat::expect_error(
    calculate_goes(
      from = goes_r,
      locs = ncp,
      locs_id = "site_id",
      fun_temporal = "variance"
    ),
    regexp = "fun_temporal"
  )
})

testthat::test_that("calculate_goes dispatch via calculate_covariates", {
  withr::local_package("terra")
  goes_dir <- testthat::test_path("..", "testdata", "goes")
  goes_r <- suppressMessages(
    process_goes(
      date = c("2018-01-01", "2018-01-01"),
      variable = "Smoke",
      path = goes_dir
    )
  )
  ncp <- data.frame(
    site_id = "site_A",
    lon = -97.0,
    lat = 32.0
  )
  for (alias in c("goes", "goes_adp", "GOES")) {
    result <- suppressMessages(
      calculate_covariates(
        covariate = alias,
        from = goes_r,
        locs = ncp,
        locs_id = "site_id",
        radius = 0
      )
    )
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_true("site_id" %in% names(result))
  }
})

# nolint end

testthat::test_that("download_goes empty S3 listing returns early", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(
          status_code = 200L,
          body = charToRaw("<ListBucketResult></ListBucketResult>")
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    out <- suppressMessages(
      download_goes(
        date = "2018-01-01",
        satellite = "16",
        product = "ADP-C",
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_true(is.list(out))
    testthat::expect_equal(out$success, 0)
    testthat::expect_equal(out$skipped, 0)
  })
})
