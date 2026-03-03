################################################################################
##### unit and integration tests for U.S. EPA AQS functions

################################################################################
##### download_aqs
testthat::test_that("download_aqs returns proper URL list", {
  withr::with_tempdir({
    year_start <- 2018
    year_end <- 2022

    # Suppress deprecation warning for download=FALSE
    result <- suppressWarnings(
      download_aqs(
        year = c(year_start, year_end),
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check return structure
    testthat::expect_type(result, "list")
    testthat::expect_named(result, c("urls", "destfiles", "n_files"))
    testthat::expect_equal(length(result$urls), length(result$destfiles))
    testthat::expect_equal(result$n_files, length(result$urls))

    # Check URLs are valid format
    testthat::expect_true(all(grepl("^https?://", result$urls)))

    # Check destfiles have proper extension
    testthat::expect_true(all(grepl("\\.zip$", result$destfiles)))

    # Check expected number of files (5 years)
    testthat::expect_equal(result$n_files, 5)
  })
})

testthat::test_that("download_aqs (single year)", {
  withr::with_tempdir({
    year <- 2018

    # Suppress deprecation warning
    result <- suppressWarnings(
      download_aqs(
        year = year,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check return structure
    testthat::expect_type(result, "list")
    testthat::expect_named(result, c("urls", "destfiles", "n_files"))

    # Check single year returns single file
    testthat::expect_equal(result$n_files, 1)

    # Check URL is valid
    testthat::expect_true(grepl("^https?://", result$urls))
    testthat::expect_true(grepl("2018", result$urls))
    testthat::expect_true(grepl("\\.zip$", result$destfiles))
  })
})

testthat::test_that("download_aqs validates URLs", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Get URLs for a recent year
    result <- suppressWarnings(
      download_aqs(
        year = 2022,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check first URL is accessible
    testthat::expect_true(check_url_status(result$urls[1]))
  })
})

testthat::test_that("download_aqs creates proper directory structure", {
  withr::with_tempdir({
    suppressWarnings(
      download_aqs(
        year = 2020,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check directories were created
    testthat::expect_true(dir.exists("zip_files"))
    testthat::expect_true(dir.exists("data_files"))
  })
})

testthat::test_that("download_aqs handles parameter_code correctly", {
  withr::with_tempdir({
    # Test with specific parameter code
    result <- suppressWarnings(
      download_aqs(
        year = 2020,
        parameter_code = 88502, # Different parameter
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check parameter code is in URLs
    testthat::expect_true(any(grepl("88502", result$urls)))
  })
})

testthat::test_that("download_aqs handles temporal resolution", {
  withr::with_tempdir({
    # Test with hourly data
    result <- suppressWarnings(
      download_aqs(
        year = 2020,
        resolution_temporal = "hourly",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    testthat::expect_type(result, "list")
    testthat::expect_true(result$n_files > 0)
  })
})

testthat::test_that("download_aqs validates year range", {
  withr::with_tempdir({
    # Test that invalid years are rejected
    testthat::expect_error(
      download_aqs(
        year = c(1900, 1901),
        directory_to_save = ".",
        acknowledgement = TRUE
      ),
      "year"
    )
  })
})

testthat::test_that("download_aqs (LIVE - small download)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Download one recent year
    result <- download_aqs(
      year = 2022,
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = FALSE
    )

    # Check files were downloaded
    zip_files <- list.files("zip_files", pattern = "\\.zip$")
    testthat::expect_true(length(zip_files) > 0)

    # Check file sizes are reasonable
    zip_paths <- list.files("zip_files", pattern = "\\.zip$", full.names = TRUE)
    testthat::expect_true(all(file.size(zip_paths) > 1000))
  })
})

################################################################################
##### process_aqs
testthat::test_that("process_aqs", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  aqssub <- testthat::test_path(
    "..",
    "testdata",
    "aqs",
    "aqs_daily_88101_triangle.csv"
  )
  testd <- testthat::test_path(
    "..",
    "testdata",
    "aqs"
  )

  # main test
  testthat::expect_no_error(
    aqsft <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqsst <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqslt <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "terra"
    )
  )

  # expect
  testthat::expect_s4_class(aqsft, "SpatVector")
  testthat::expect_s4_class(aqsst, "SpatVector")
  testthat::expect_s4_class(aqslt, "SpatVector")

  testthat::expect_no_error(
    aqsfs <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsss <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsls <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )
  testthat::expect_s3_class(aqsfs, "sf")
  testthat::expect_s3_class(aqsss, "sf")
  testthat::expect_s3_class(aqsls, "sf")

  testthat::expect_no_error(
    aqsfd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssdd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsld <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsldd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqslddsd <- process_aqs(
      path = aqssub,
      date = "2022-02-04",
      mode = "location",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_s3_class(aqsfd, "data.table")
  testthat::expect_s3_class(aqssd, "data.table")
  testthat::expect_s3_class(aqssdd, "data.table")
  testthat::expect_s3_class(aqsld, "data.table")
  testthat::expect_s3_class(aqsldd, "data.table")
  testthat::expect_s3_class(aqslddsd, "data.table")

  testthat::expect_no_error(
    aqssf <- process_aqs(
      path = testd,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )

  tempd <- tempdir()
  testthat::expect_error(
    process_aqs(
      path = tempd,
      date = c("2022-02-04", "2022-02-28"),
      return_format = "sf"
    )
  )

  # expect
  testthat::expect_s3_class(aqssf, "sf")

  # error cases
  testthat::expect_error(
    process_aqs(testthat::test_path("../testdata", "modis"))
  )
  testthat::expect_error(
    process_aqs(path = 1L)
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = c("January", "Januar"))
  )
  testthat::expect_error(
    process_aqs(
      path = aqssub,
      date = c("2021-08-15", "2021-08-16", "2021-08-17")
    )
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = NULL)
  )
  testthat::expect_no_error(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf",
      extent = c(-79, 33, -78, 36)
    )
  )
  testthat::expect_no_error(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf",
      extent = c(-79, 33, -78, 36)
    )
  )
  testthat::expect_warning(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "data.table",
      extent = c(-79, -78, 33, 36)
    ),
    "Extent is not applicable for data.table. Returning data.table..."
  )
})

testthat::test_that("download_aqs remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      download_aqs(
        year = 2022,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_aqs all files exist branch", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_aqs(
          year = 2022,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$skipped, 1)
  })
})

testthat::test_that("download_aqs hash = TRUE path", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_aqs(
          year = 2022,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_aqs -> process_aqs integration (basic)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Download one recent year
    result <- download_aqs(
      year = 2022,
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = TRUE
    )

    # Check that download succeeded
    data_dir <- "./data_files"
    testthat::expect_true(dir.exists(data_dir))

    csv_files <- list.files(
      data_dir,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    testthat::expect_true(
      length(csv_files) > 0,
      info = "At least one CSV file should be downloaded"
    )

    # Verify files have content
    if (length(csv_files) > 0) {
      file_sizes <- file.size(csv_files)
      testthat::expect_true(
        all(file_sizes > 100),
        info = "Downloaded CSV files should have content"
      )
    }
  })
})
