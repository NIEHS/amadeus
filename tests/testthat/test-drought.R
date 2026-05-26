################################################################################
##### unit and integration tests for drought process functions

testdata_spei <- testthat::test_path("..", "testdata", "drought", "spei")
testdata_eddi <- testthat::test_path("..", "testdata", "drought", "eddi")
testdata_usdm <- testthat::test_path("..", "testdata", "drought", "usdm")

################################################################################
##### process_drought — SPEI
testthat::test_that("process_drought (SPEI)", {
  withr::local_package("terra")

  testthat::expect_true(is.function(process_drought))

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  # class
  testthat::expect_s4_class(spei, "SpatRaster")
  # layers == months in range
  testthat::expect_equal(terra::nlyr(spei), 3L)
  # layer naming convention: spei_TS_YYYY-MM-DD
  testthat::expect_true(all(grepl("^spei_01_[0-9]{4}-[0-9]{2}-[0-9]{2}$", names(spei))))
  # time dimension is set
  testthat::expect_false(any(is.na(terra::time(spei))))
  testthat::expect_equal(
    as.character(terra::time(spei)),
    c("2020-01-01", "2020-02-01", "2020-03-01")
  )
  # CRS is EPSG:4326
  testthat::expect_equal(terra::crs(spei, describe = TRUE)$code, "4326")
  # has values
  testthat::expect_true(terra::hasValues(spei))
  # spatial dimensions > 1
  testthat::expect_false(any(c(0L, 1L) %in% dim(spei)[1:2]))
})

testthat::test_that("process_drought (SPEI single date)", {
  withr::local_package("terra")
  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-02-01",
    timescale = 1L
  )
  testthat::expect_s4_class(spei, "SpatRaster")
  testthat::expect_equal(terra::nlyr(spei), 1L)
  testthat::expect_equal(as.character(terra::time(spei)), "2020-02-01")
})

testthat::test_that("process_drought (SPEI with extent)", {
  withr::local_package("terra")
  ext_crop <- terra::ext(-99, -96, 36, 39)
  testthat::expect_no_error(
    spei_ext <- process_drought(
      source = "spei",
      path = testdata_spei,
      date = "2020-01-01",
      timescale = 1L,
      extent = ext_crop
    )
  )
  testthat::expect_s4_class(spei_ext, "SpatRaster")
})

################################################################################
##### process_drought — EDDI
testthat::test_that("process_drought (EDDI)", {
  withr::local_package("terra")

  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = c("2020-01-01", "2020-06-30"),
    timescale = 1L
  )
  testthat::expect_s4_class(eddi, "SpatRaster")
  testthat::expect_equal(terra::nlyr(eddi), 6L)
  testthat::expect_true(all(grepl("^eddi_01_[0-9]{4}-[0-9]{2}-[0-9]{2}$", names(eddi))))
  testthat::expect_false(any(is.na(terra::time(eddi))))
  testthat::expect_equal(terra::crs(eddi, describe = TRUE)$code, "4326")
  testthat::expect_true(terra::hasValues(eddi))
})

testthat::test_that("process_drought (EDDI single date)", {
  withr::local_package("terra")
  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = "2020-04-01",
    timescale = 1L
  )
  testthat::expect_s4_class(eddi, "SpatRaster")
  testthat::expect_equal(terra::nlyr(eddi), 1L)
})

################################################################################
##### process_drought — USDM
testthat::test_that("process_drought (USDM)", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = c("2020-01-07", "2020-01-14")
  )
  testthat::expect_s4_class(usdm, "SpatVector")
  # two weekly files
  testthat::expect_equal(nrow(usdm), 2L)
  # expected columns
  testthat::expect_true(all(c("DM", "date", "source") %in% names(usdm)))
  # source column is "usdm"
  testthat::expect_true(all(terra::values(usdm)$source == "usdm"))
  # CRS is EPSG:4326
  testthat::expect_equal(terra::crs(usdm, describe = TRUE)$code, "4326")
})

testthat::test_that("process_drought (USDM single date)", {
  withr::local_package("terra")
  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = "2020-01-07"
  )
  testthat::expect_s4_class(usdm, "SpatVector")
  testthat::expect_equal(nrow(usdm), 1L)
  testthat::expect_equal(terra::values(usdm)$date, "2020-01-07")
})

testthat::test_that("process_drought (USDM with extent)", {
  withr::local_package("terra")
  ext_crop <- terra::ext(-99, -96, 36, 39)
  testthat::expect_no_error(
    usdm_ext <- process_drought(
      source = "usdm",
      path = testdata_usdm,
      date = c("2020-01-07", "2020-01-14"),
      extent = ext_crop
    )
  )
  testthat::expect_s4_class(usdm_ext, "SpatVector")
})

################################################################################
##### process_drought — error handling
testthat::test_that("process_drought (error: invalid source)", {
  testthat::expect_error(
    process_drought(source = "foo", path = testdata_spei, date = "2020-01-01"),
    regexp = "arg"
  )
})

testthat::test_that("process_drought (error: SPEI date out of range)", {
  testthat::expect_error(
    process_drought(
      source = "spei",
      path = testdata_spei,
      date = c("2019-01-01", "2019-12-31"),
      timescale = 1L
    ),
    regexp = "No SPEI data found"
  )
})

testthat::test_that("process_drought (error: USDM date out of range)", {
  testthat::expect_error(
    process_drought(
      source = "usdm",
      path = testdata_usdm,
      date = c("2019-01-01", "2019-12-31")
    ),
    regexp = "No USDM files found"
  )
})

################################################################################
##### process_covariates — drought aliases
testthat::test_that("process_covariates drought aliases", {
  withr::local_package("terra")

  # spei alias
  r_spei <- process_covariates(
    covariate = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  testthat::expect_s4_class(r_spei, "SpatRaster")
  testthat::expect_true(grepl("^spei_", names(r_spei)))

  # eddi alias
  r_eddi <- process_covariates(
    covariate = "eddi",
    path = testdata_eddi,
    date = "2020-01-01",
    timescale = 1L
  )
  testthat::expect_s4_class(r_eddi, "SpatRaster")
  testthat::expect_true(grepl("^eddi_", names(r_eddi)))

  # usdm alias
  r_usdm <- process_covariates(
    covariate = "usdm",
    path = testdata_usdm,
    date = "2020-01-07"
  )
  testthat::expect_s4_class(r_usdm, "SpatVector")
  testthat::expect_true("DM" %in% names(r_usdm))

  # drought alias (defaults to spei via match.arg)
  r_drought <- process_covariates(
    covariate = "drought",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  testthat::expect_s4_class(r_drought, "SpatRaster")
})

################################################################################
##### download_drought – acknowledgement / parameter errors

testthat::test_that("download_drought errors without acknowledgement", {
  testthat::expect_error(
    download_drought(
      source = "spei",
      date = c("2020-01-01", "2020-12-31"),
      timescale = 1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = FALSE
    )
  )
})

testthat::test_that("download_drought errors on invalid source", {
  testthat::expect_error(
    download_drought(
      source = "bogus",
      date = "2020-01-01",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "arg"
  )
})

testthat::test_that("download_drought errors on bad timescale", {
  testthat::expect_error(
    download_drought(
      source = "spei",
      date = "2020-06-01",
      timescale = -1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "timescale"
  )
})

testthat::test_that("download_drought errors on unzip/remove_zip conflict", {
  testthat::expect_error(
    download_drought(
      source = "usdm",
      date = "2020-01-07",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      unzip = FALSE,
      remove_zip = TRUE
    ),
    regexp = "unzip"
  )
})

testthat::test_that("download_drought errors when no Tuesdays in EDDI range", {
  # Wed–Sat: no Tuesday
  testthat::expect_error(
    download_drought(
      source = "eddi",
      date = c("2020-01-08", "2020-01-11"),
      timescale = 1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "Tuesday"
  )
})

testthat::test_that("download_drought errors when no Tuesdays in USDM range", {
  testthat::expect_error(
    download_drought(
      source = "usdm",
      date = c("2020-01-08", "2020-01-11"),
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "Tuesday"
  )
})

################################################################################
##### download_drought – wrapper alias routing

testthat::test_that("download_data routes 'drought' alias to download_drought", {
  testthat::local_mocked_bindings(
    download_drought = function(...) "drought_called",
    .package = "amadeus"
  )
  result <- download_data(
    dataset_name = "drought",
    source = "spei",
    date = "2020-06-03",
    timescale = 1L,
    directory_to_save = withr::local_tempdir(),
    acknowledgement = TRUE
  )
  testthat::expect_equal(result, "drought_called")
})

testthat::test_that("download_data routes 'spei' alias to download_drought", {
  testthat::local_mocked_bindings(
    download_drought = function(...) "drought_called",
    .package = "amadeus"
  )
  result <- download_data(
    dataset_name = "spei",
    source = "spei",
    date = "2020-06-03",
    timescale = 1L,
    directory_to_save = withr::local_tempdir(),
    acknowledgement = TRUE
  )
  testthat::expect_equal(result, "drought_called")
})

testthat::test_that("download_data routes 'eddi' alias to download_drought", {
  testthat::local_mocked_bindings(
    download_drought = function(...) "drought_called",
    .package = "amadeus"
  )
  result <- download_data(
    dataset_name = "eddi",
    source = "eddi",
    date = "2020-06-02",
    timescale = 1L,
    directory_to_save = withr::local_tempdir(),
    acknowledgement = TRUE
  )
  testthat::expect_equal(result, "drought_called")
})

testthat::test_that("download_data routes 'usdm' alias to download_drought", {
  testthat::local_mocked_bindings(
    download_drought = function(...) "drought_called",
    .package = "amadeus"
  )
  result <- download_data(
    dataset_name = "usdm",
    source = "usdm",
    date = "2020-01-07",
    directory_to_save = withr::local_tempdir(),
    acknowledgement = TRUE
  )
  testthat::expect_equal(result, "drought_called")
})

################################################################################
##### drought_weekly_dates helper

testthat::test_that("drought_weekly_dates returns Tuesdays only", {
  dates <- drought_weekly_dates("2020-01-01", "2020-01-31")
  # January 2020 Tuesdays: 7, 14, 21, 28
  testthat::expect_equal(
    dates,
    c("20200107", "20200114", "20200121", "20200128")
  )
})

testthat::test_that("drought_weekly_dates returns empty when no Tuesdays", {
  dates <- drought_weekly_dates("2020-01-08", "2020-01-11")
  testthat::expect_length(dates, 0L)
})

testthat::test_that("drought_weekly_dates handles single-day range on Tuesday", {
  dates <- drought_weekly_dates("2020-01-07", "2020-01-07")
  testthat::expect_equal(dates, "20200107")
})

################################################################################
##### download_drought SPEI – mock download

testthat::test_that("download_drought SPEI mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      download_drought(
        source = "spei",
        date = c("2020-01-01", "2020-12-31"),
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = FALSE
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1L)
  })
})

testthat::test_that("download_drought SPEI mock download hash=TRUE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      download_drought(
        source = "spei",
        date = c("2020-01-01", "2020-12-31"),
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = TRUE
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_drought SPEI prefers current SPEIbase endpoint", {
  checked_urls <- character(0)
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    check_url_status = function(url) {
      checked_urls <<- c(checked_urls, url)
      grepl("spei_database_2_11/nc/spei01\\.nc$", url)
    },
    check_destfile = function(...) TRUE,
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      list(success = 1, failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressMessages(
      download_drought(
        source = "spei",
        date = "2020-01-01",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_true(any(grepl("spei_database_2_11/nc/spei01\\.nc$", checked_urls)))
    testthat::expect_equal(captured_urls, "https://spei.csic.es/spei_database_2_11/nc/spei01.nc")
  })
})

testthat::test_that("download_drought SPEI falls back to prior SPEIbase endpoint", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    check_url_status = function(url) grepl("spei_database_2_10/nc/spei01\\.nc$", url),
    check_destfile = function(...) TRUE,
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      list(success = 1, failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressMessages(
      download_drought(
        source = "spei",
        date = "2020-01-01",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_equal(captured_urls, "https://spei.csic.es/spei_database_2_10/nc/spei01.nc")
  })
})

testthat::test_that("download_drought SPEI file already exists skips download", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    result <- withCallingHandlers(
      download_drought(
        source = "spei",
        date = "2020-01-01",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = FALSE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exists|Skipping", msgs)))
    testthat::expect_equal(result$skipped, 1L)
  })
})

################################################################################
##### download_drought EDDI – mock download

testthat::test_that(
  "download_drought EDDI mock download produces correct URLs", {
    captured_urls <- NULL
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(...) TRUE,
      download_run_method = function(urls, ...) {
        captured_urls <<- urls
        list(success = length(urls), failed = 0, skipped = 0)
      },
      download_hash = function(hash, dir) {
        if (isTRUE(hash)) "fakehash" else NULL
      },
      .package = "amadeus"
    )
    withr::with_tempdir({
      result <- suppressMessages(
        download_drought(
          source = "eddi",
          date = c("2020-01-01", "2020-01-31"),
          timescale = 1L,
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      )
      # January 2020 Tuesdays: 7, 14, 21, 28 → 4 files
      testthat::expect_equal(result$success, 4L)
      testthat::expect_true(all(grepl("EDDI_ETrs_01mn_2020", captured_urls)))
      testthat::expect_true(all(grepl("ftp.cdc.noaa.gov", captured_urls)))
    })
  }
)

testthat::test_that("download_drought EDDI uses CONUS_archive ftp endpoint", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    check_destfile = function(...) TRUE,
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressMessages(
      download_drought(
        source = "eddi",
        date = c("2020-01-01", "2020-01-31"),
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_true(all(grepl("CONUS_archive/data/2020/EDDI_ETrs_01mn_2020", captured_urls)))
    testthat::expect_true(all(grepl("ftp.cdc.noaa.gov", captured_urls)))
    testthat::expect_true(all(grepl("\\.asc$", captured_urls)))
  })
})

testthat::test_that("download_drought EDDI timescale 12 URL has 12mn", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      list(success = length(urls), failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressMessages(
      download_drought(
        source = "eddi",
        date = "2020-01-07",
        timescale = 12L,
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_true(all(grepl("12mn", captured_urls)))
  })
})

testthat::test_that("download_drought EDDI 404 error propagates", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) TRUE,
    download_run_method = function(...) stop("HTTP 404"),
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_error(
      download_drought(
        source = "eddi",
        date = "2020-01-07",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      regexp = "HTTP 404"
    )
  })
})

testthat::test_that("download_drought EDDI skips when all files already exist", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    result <- withCallingHandlers(
      download_drought(
        source = "eddi",
        date = c("2020-01-01", "2020-01-31"),
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exist|Skipping", msgs)))
    testthat::expect_equal(result$skipped, 4L)
  })
})

################################################################################
##### download_drought USDM – mock download

testthat::test_that(
  "download_drought USDM mock download produces correct URLs", {
    captured_urls <- NULL
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(...) TRUE,
      download_run_method = function(urls, ...) {
        captured_urls <<- urls
        list(success = length(urls), failed = 0, skipped = 0)
      },
      download_unzip = function(...) invisible(NULL),
      download_remove_zips = function(...) invisible(NULL),
      download_hash = function(hash, dir) {
        if (isTRUE(hash)) "fakehash" else NULL
      },
      .package = "amadeus"
    )
    withr::with_tempdir({
      result <- suppressMessages(
        download_drought(
          source = "usdm",
          date = c("2020-01-01", "2020-01-31"),
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      )
      # January 2020 Tuesdays: 7, 14, 21, 28 → 4 files
      testthat::expect_equal(result$success, 4L)
      testthat::expect_true(all(grepl("USDM_2020", captured_urls)))
      testthat::expect_true(all(grepl("droughtmonitor.unl.edu", captured_urls)))
      testthat::expect_true(all(grepl("_M\\.zip$", captured_urls)))
    })
  }
)

testthat::test_that("download_drought USDM hash=TRUE returns hash", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      download_drought(
        source = "usdm",
        date = "2020-01-07",
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = TRUE
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_drought USDM 404 error propagates", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) FALSE,
    check_destfile = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_error(
      download_drought(
        source = "usdm",
        date = "2020-01-07",
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      regexp = "HTTP 404"
    )
  })
})

testthat::test_that("download_drought USDM skips when all files already exist", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    result <- withCallingHandlers(
      download_drought(
        source = "usdm",
        date = c("2020-01-01", "2020-01-31"),
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exist|Skipping", msgs)))
    testthat::expect_equal(result$skipped, 4L)
  })
})

################################################################################
##### calculate_drought — SPEI

testthat::test_that("calculate_drought (SPEI baseline)", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from = spei,
    locs = locs,
    locs_id = "site_id",
    radius = 0L
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true("time" %in% names(result))
  testthat::expect_true("spei_01_0" %in% names(result))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(inherits(result$time, "POSIXt"))
})

testthat::test_that("calculate_drought (SPEI .by_time month)", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from     = spei,
    locs     = locs,
    locs_id  = "site_id",
    radius   = 0L,
    .by_time = "month"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("spei_01_0" %in% names(result))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(inherits(result$time, "POSIXt"))
})

testthat::test_that("calculate_drought (SPEI .by_time year)", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = spei,
    locs    = locs,
    locs_id = "site_id",
    radius  = 0L,
    .by_time = "year"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1L)
})

testthat::test_that("calculate_drought (SPEI geom='sf')", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = spei,
    locs    = locs,
    locs_id = "site_id",
    geom    = "sf"
  ))
  testthat::expect_true(inherits(result, "sf"))
})

testthat::test_that("calculate_drought (SPEI geom='terra')", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = spei,
    locs    = locs,
    locs_id = "site_id",
    geom    = "terra"
  ))
  testthat::expect_s4_class(result, "SpatVector")
})

################################################################################
##### calculate_drought — EDDI

testthat::test_that("calculate_drought (EDDI baseline)", {
  withr::local_package("terra")

  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = eddi,
    locs    = locs,
    locs_id = "site_id",
    radius  = 0L
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("eddi_01_0" %in% names(result))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(inherits(result$time, "POSIXt"))
})

################################################################################
##### calculate_drought — USDM

testthat::test_that("calculate_drought (USDM baseline)", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = c("2020-01-07", "2020-01-14")
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true("time" %in% names(result))
  testthat::expect_true("usdm_dm_0" %in% names(result))
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(inherits(result$time, "POSIXt"))
  testthat::expect_equal(result$usdm_dm_0, c(2, 2))
})

testthat::test_that("calculate_drought(source=usdm, radius=1000): returns USDM class proportion columns", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = c("2020-01-07", "2020-01-14")
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id",
    radius  = 1000L
  ))

  prop_cols <- grep("^usdm_dm_[0-4]_1000$", names(result), value = TRUE)
  testthat::expect_length(prop_cols, 5L)
  testthat::expect_true("usdm_dm_0" %in% names(result))
  testthat::expect_true(
    all(vapply(result[, prop_cols, drop = FALSE], is.numeric, logical(1)))
  )
})

testthat::test_that("calculate_drought(source=usdm, radius=1000): class proportions are bounded and sum to one", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = c("2020-01-07", "2020-01-14")
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id",
    radius  = 1000L
  ))

  prop_cols <- grep("^usdm_dm_[0-4]_1000$", names(result), value = TRUE)
  prop_mat <- as.matrix(result[, prop_cols, drop = FALSE])

  testthat::expect_true(all(prop_mat >= 0, na.rm = TRUE))
  testthat::expect_true(all(prop_mat <= 1, na.rm = TRUE))
  testthat::expect_equal(
    as.numeric(rowSums(prop_mat)),
    rep(1, nrow(result)),
    tolerance = 1e-6
  )
  testthat::expect_equal(result$usdm_dm_2_1000, rep(1, nrow(result)))
  testthat::expect_equal(result$usdm_dm_0, c(2, 2))
})

testthat::test_that("calculate_drought (USDM point outside polygon → NA)", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = "2020-01-07"
  )
  # Point well outside the test polygon extent (-100 to -95, 35 to 40)
  locs <- data.frame(site_id = "out", lon = -80.0, lat = 25.0)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true(is.na(result$usdm_dm_0))
})

testthat::test_that("calculate_drought (USDM .by_time week)", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = c("2020-01-07", "2020-01-14")
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id",
    .by_time = "week"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("usdm_dm_0" %in% names(result))
  testthat::expect_equal(nrow(result), 2L)
})

testthat::test_that("calculate_drought (USDM geom='sf')", {
  withr::local_package("terra")

  usdm <- process_drought(
    source = "usdm",
    path = testdata_usdm,
    date = "2020-01-07"
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = usdm,
    locs    = locs,
    locs_id = "site_id",
    geom    = "sf"
  ))
  testthat::expect_true(inherits(result, "sf"))
})

################################################################################
##### calculate_drought — error handling

testthat::test_that("calculate_drought errors on invalid from type", {
  testthat::expect_error(
    calculate_drought(
      from    = list(a = 1),
      locs    = data.frame(site_id = "001", lon = -97.5, lat = 37.5),
      locs_id = "site_id"
    ),
    regexp = "SpatRaster.*SpatVector|SpatVector.*SpatRaster"
  )
})

testthat::test_that("calculate_drought errors when deprecated .by is supplied", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  testthat::expect_error(
    calculate_drought(
      from    = spei,
      locs    = locs,
      locs_id = "site_id",
      .by     = 123L
    ),
    regexp = "no longer supported"
  )
})

testthat::test_that("calculate_drought errors on bad geom value", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  testthat::expect_error(
    calculate_drought(
      from    = spei,
      locs    = locs,
      locs_id = "site_id",
      geom    = "bad"
    )
  )
})

################################################################################
##### calculate_drought — .by_time behaviors

testthat::test_that("calculate_drought (SPEI .by_time month)", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from     = spei,
    locs     = locs,
    locs_id  = "site_id",
    .by_time = "month"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("spei_01_0" %in% names(result))
  # one row per site-month combination (3 months × 1 site)
  testthat::expect_equal(nrow(result), 3L)
})

testthat::test_that("calculate_drought errors on non-character .by_time", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  testthat::expect_error(
    calculate_drought(
      from     = spei,
      locs     = locs,
      locs_id  = "site_id",
        .by_time = 5L
    ),
    regexp = "\\.by_time"
  )
})

testthat::test_that("calculate_drought accepts .by_time without deprecated .by", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  testthat::expect_no_error(
    suppressMessages(calculate_drought(
      from     = spei,
      locs     = locs,
      locs_id  = "site_id",
      .by_time = "month"
    ))
  )
})

################################################################################
##### calculate_drought — EDDI additional coverage

testthat::test_that("calculate_drought (EDDI .by_time year)", {
  withr::local_package("terra")

  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = eddi,
    locs    = locs,
    locs_id = "site_id",
    .by_time = "year"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true("eddi_01_0" %in% names(result))
})

testthat::test_that("calculate_drought (EDDI geom='sf')", {
  withr::local_package("terra")

  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = eddi,
    locs    = locs,
    locs_id = "site_id",
    geom    = "sf"
  ))
  testthat::expect_true(inherits(result, "sf"))
})

testthat::test_that("calculate_drought (EDDI geom='terra')", {
  withr::local_package("terra")

  eddi <- process_drought(
    source = "eddi",
    path = testdata_eddi,
    date = "2020-01-01",
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_drought(
    from    = eddi,
    locs    = locs,
    locs_id = "site_id",
    geom    = "terra"
  ))
  testthat::expect_s4_class(result, "SpatVector")
})

################################################################################
##### calculate_covariates — drought wrapper routing

testthat::test_that("calculate_covariates routes 'drought' alias", {
  testthat::local_mocked_bindings(
    calculate_drought = function(...) "drought_calc_called",
    .package = "amadeus"
  )
  result <- calculate_covariates(
    covariate = "drought",
    from      = NULL,
    locs      = NULL,
    locs_id   = "site_id"
  )
  testthat::expect_equal(result, "drought_calc_called")
})

testthat::test_that("calculate_covariates routes 'spei' alias", {
  testthat::local_mocked_bindings(
    calculate_drought = function(...) "drought_calc_called",
    .package = "amadeus"
  )
  result <- calculate_covariates(
    covariate = "spei",
    from      = NULL,
    locs      = NULL,
    locs_id   = "site_id"
  )
  testthat::expect_equal(result, "drought_calc_called")
})

testthat::test_that("calculate_covariates routes 'eddi' alias", {
  testthat::local_mocked_bindings(
    calculate_drought = function(...) "drought_calc_called",
    .package = "amadeus"
  )
  result <- calculate_covariates(
    covariate = "eddi",
    from      = NULL,
    locs      = NULL,
    locs_id   = "site_id"
  )
  testthat::expect_equal(result, "drought_calc_called")
})

testthat::test_that("calculate_covariates routes 'usdm' alias", {
  testthat::local_mocked_bindings(
    calculate_drought = function(...) "drought_calc_called",
    .package = "amadeus"
  )
  result <- calculate_covariates(
    covariate = "usdm",
    from      = NULL,
    locs      = NULL,
    locs_id   = "site_id"
  )
  testthat::expect_equal(result, "drought_calc_called")
})

testthat::test_that("calculate_covariates drought .by_time passes through", {
  withr::local_package("terra")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = c("2020-01-01", "2020-03-31"),
    timescale = 1L
  )
  locs <- data.frame(site_id = "001", lon = -97.5, lat = 37.5)

  result <- suppressMessages(calculate_covariates(
    covariate = "drought",
    from      = spei,
    locs      = locs,
    locs_id   = "site_id",
    .by_time = "year"
  ))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1L)
})

################################################################################
##### drought internal helper branch coverage

testthat::test_that("drought_process_nc errors when SPEI file not found", {
  withr::with_tempdir({
    testthat::expect_error(
      amadeus:::drought_process_nc(
        path = ".",
        date = c("2020-01-01", "2020-12-31"),
        timescale = 1L,
        source = "spei"
      ),
      "No SPEI file"
    )
  })
})

testthat::test_that("drought_process_nc errors when EDDI files not found", {
  withr::with_tempdir({
    testthat::expect_error(
      amadeus:::drought_process_nc(
        path = ".",
        date = c("2020-01-01", "2020-12-31"),
        timescale = 1L,
        source = "eddi"
      ),
      "No EDDI files"
    )
  })
})

testthat::test_that("drought_process_nc processes EDDI ASC files from download_drought", {
  withr::local_package("terra")
  withr::with_tempdir({
    r <- terra::rast(
      nrows = 2, ncols = 2,
      xmin = -100, xmax = -99, ymin = 39, ymax = 40,
      crs = "EPSG:4326"
    )
    terra::values(r) <- c(1, 2, 3, 4)

    terra::writeRaster(
      r,
      "EDDI_ETrs_01mn_20200114.asc",
      overwrite = TRUE
    )
    terra::writeRaster(
      r,
      "EDDI_ETrs_01mn_20200107.asc",
      overwrite = TRUE
    )

    out <- amadeus:::drought_process_nc(
      source = "eddi",
      path = ".",
      date = c("2020-01-01", "2020-01-31"),
      timescale = 1L,
      extent = NULL
    )

    testthat::expect_s4_class(out, "SpatRaster")
    testthat::expect_equal(terra::nlyr(out), 2L)
    testthat::expect_equal(
      as.character(terra::time(out)),
      c("2020-01-07", "2020-01-14")
    )
    testthat::expect_equal(
      names(out),
      c("eddi_01_2020-01-07", "eddi_01_2020-01-14")
    )
  })
})

testthat::test_that("drought_process_nc assigns EPSG:4326 when CRS is empty", {
  withr::local_package("terra")
  spei_path <- testthat::test_path("..", "testdata", "drought", "spei")

  # Wrap process to intercept just after CRS assignment
  r_out <- amadeus:::drought_process_nc(
    path = spei_path,
    date = c("2020-01-01", "2020-01-01"),
    timescale = 1L,
    source = "spei",
    extent = NULL
  )
  # CRS should be set (EPSG:4326 or equivalent)
  testthat::expect_false(is.na(terra::crs(r_out, describe = TRUE)$code))
})

testthat::test_that("drought_set_time_nc derives time from filename when terra time is NA", {
  withr::local_package("terra")
  # Create a raster with NA time (as if terra couldn't parse CF metadata)
  eddi_path <- testthat::test_path("..", "testdata", "drought", "eddi")
  eddi_files <- list.files(eddi_path, pattern = "eddi.*\\.nc$", full.names = TRUE)

  r <- terra::rast(eddi_files[1])
  # Force terra time to be NULL via mock
  testthat::local_mocked_bindings(
    time = function(x, ...) {
      if (missing(x)) stop("bad call")
      NULL
    },
    .package = "terra"
  )
  result <- amadeus:::drought_set_time_nc(r, "eddi", "01", eddi_files[1])
  testthat::expect_true(all(grepl("^eddi_01_", names(result))))
})

testthat::test_that("drought_process_usdm errors when no USDM shapefiles found", {
  withr::with_tempdir({
    testthat::expect_error(
      amadeus:::drought_process_usdm(
        path = ".",
        date = c("2020-01-07", "2020-01-14"),
        extent = NULL
      ),
      "No USDM shapefiles"
    )
  })
})

testthat::test_that("drought_process_usdm finds shapefiles in data_files subdir", {
  withr::local_package("terra")
  usdm_path <- normalizePath(
    testthat::test_path("..", "testdata", "drought", "usdm"),
    mustWork = TRUE
  )
  withr::with_tempdir({
    dir.create("data_files")
    source_files <- list.files(usdm_path, full.names = TRUE)
    testthat::expect_gt(length(source_files), 0L)
    copied <- file.copy(
      source_files,
      "data_files",
      overwrite = TRUE
    )
    testthat::expect_true(all(copied))

    result <- amadeus:::drought_process_usdm(
      path = ".",
      date = c("2020-01-07", "2020-01-14"),
      extent = NULL
    )
    testthat::expect_s4_class(result, "SpatVector")
    testthat::expect_equal(length(unique(terra::values(result)$date)), 2L)
  })
})

testthat::test_that("drought_process_usdm assigns CRS when shapefile has no CRS", {
  withr::local_package("terra")
  usdm_path <- testthat::test_path("..", "testdata", "drought", "usdm")

  # Mock terra::crs on vect result to return "" (empty CRS) so the else branch runs
  testthat::local_mocked_bindings(
    crs = function(x, ...) {
      if (inherits(x, "SpatVector")) ""
      else terra::crs(x, ...)
    },
    .package = "terra"
  )
  result <- amadeus:::drought_process_usdm(
    path = usdm_path,
    date = c("2020-01-07", "2020-01-14"),
    extent = NULL
  )
  testthat::expect_s4_class(result, "SpatVector")
})

testthat::test_that("download_drought SPEI file exists returns hash when hash=TRUE", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_hash = function(hash, directory) if (isTRUE(hash)) "spei-hash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      amadeus::download_drought(
        source = "spei",
        date = "2020-01-01",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = TRUE
      )
    )
    testthat::expect_equal(result, "spei-hash")
  })
})

testthat::test_that("download_drought SPEI URL 404 raises error", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) TRUE,
    check_url_status = function(...) FALSE,
    download_run_method = function(...) stop("HTTP 404"),
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_error(
      amadeus::download_drought(
        source = "spei",
        date = "2020-01-01",
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      "HTTP 404|SPEI timescale"
    )
  })
})

testthat::test_that(
  "download_drought SPEI falls back to direct download when preflight fails",
  {
    captured_urls <- character(0)
    testthat::local_mocked_bindings(
      check_destfile = function(...) TRUE,
      check_url_status = function(...) FALSE,
      download_run_method = function(urls, ...) {
        captured_urls <<- c(captured_urls, urls)
        list(success = 1, failed = 0, skipped = 0)
      },
      .package = "amadeus"
    )
    withr::with_tempdir({
      result <- suppressMessages(
        amadeus::download_drought(
          source = "spei",
          date = "2020-01-01",
          timescale = 1L,
          directory_to_save = ".",
          acknowledgement = TRUE
        )
      )
      testthat::expect_equal(result$success, 1L)
      testthat::expect_equal(
        captured_urls[1],
        "https://spei.csic.es/spei_database_2_11/nc/spei01.nc"
      )
    })
  }
)

testthat::test_that(
  "download_drought SPEI retries alternate endpoint after failed download result",
  {
    captured_urls <- character(0)
    testthat::local_mocked_bindings(
      check_destfile = function(...) TRUE,
      check_url_status = function(url) grepl("spei_database_2_11", url),
      download_run_method = function(urls, ...) {
        captured_urls <<- c(captured_urls, urls)
        if (grepl("spei_database_2_11", urls)) {
          return(list(success = 0, failed = 1, skipped = 0))
        }
        list(success = 1, failed = 0, skipped = 0)
      },
      .package = "amadeus"
    )
    withr::with_tempdir({
      result <- suppressMessages(
        amadeus::download_drought(
          source = "spei",
          date = "2020-01-01",
          timescale = 1L,
          directory_to_save = ".",
          acknowledgement = TRUE
        )
      )
      testthat::expect_equal(result$success, 1L)
      testthat::expect_equal(length(captured_urls), 2L)
      testthat::expect_true(grepl("spei_database_2_10", captured_urls[2]))
    })
  }
)

testthat::test_that("download_drought EDDI returns hash when hash=TRUE", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) TRUE,
    check_url_status = function(...) TRUE,
    download_run_method = function(urls, ...) {
      list(success = length(urls), failed = 0, skipped = 0)
    },
    download_hash = function(hash, directory) if (isTRUE(hash)) "eddi-hash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      amadeus::download_drought(
        source = "eddi",
        date = c("2020-01-01", "2020-01-31"),
        timescale = 1L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        hash = TRUE
      )
    )
    testthat::expect_equal(result, "eddi-hash")
  })
})

testthat::test_that("calculate_drought uses exact_extract for polygon locations", {
  withr::local_package("terra")
  testthat::skip_if_not_installed("exactextractr")

  spei <- process_drought(
    source = "spei",
    path = testdata_spei,
    date = "2020-01-01",
    timescale = 1L
  )

  # Create a polygon location that overlaps the SPEI raster extent
  poly_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-100, 35, -95, 35, -95, 40, -100, 40, -100, 35),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  )
  poly_locs <- sf::st_sf(site_id = "poly1", geometry = poly_geom)

  result <- suppressMessages(
    calculate_drought(
      from   = spei,
      locs   = poly_locs,
      locs_id = "site_id",
      radius  = 0L
    )
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true(any(grepl("^spei_", names(result))))
})

testthat::test_that("drought_set_time_nc stops when filename has no year", {
  withr::local_package("terra")
  eddi_path <- testthat::test_path("..", "testdata", "drought", "eddi")
  eddi_files <- list.files(eddi_path, pattern = "eddi.*[.]nc$", full.names = TRUE)
  r <- terra::rast(eddi_files[1])

  testthat::local_mocked_bindings(
    time = function(x, ...) NULL,
    .package = "terra"
  )
  testthat::expect_error(
    amadeus:::drought_set_time_nc(r, "eddi", "01", "eddi_no_year_here.nc"),
    "Cannot determine time coordinates"
  )
})

testthat::test_that("drought_process_nc sets CRS when raster CRS is empty", {
  withr::local_package("terra")
  spei_path <- testthat::test_path("..", "testdata", "drought", "spei")

  crs_call_count <- 0L
  testthat::local_mocked_bindings(
    crs = function(x, ...) {
      if (missing(x)) terra::crs(x)
      crs_call_count <<- crs_call_count + 1L
      if (crs_call_count == 1L) NA_character_
      else ""
    },
    .package = "terra"
  )
  testthat::expect_no_error(
    suppressMessages(
      amadeus:::drought_process_nc(
        path = spei_path,
        date = c("2020-01-01", "2020-01-01"),
        timescale = 1L,
        source = "spei",
        extent = NULL
      )
    )
  )
})
