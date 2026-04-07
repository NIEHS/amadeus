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
      testthat::expect_true(all(grepl("psl.noaa.gov", captured_urls)))
    })
  }
)

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
    check_url_status = function(...) FALSE,
    check_destfile = function(...) TRUE,
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
