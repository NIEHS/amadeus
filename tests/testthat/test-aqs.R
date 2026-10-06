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

testthat::test_that("download_normalize_aqs_unzip flattens nested AQS output", {
  withr::with_tempdir({
    data_dir <- file.path(".", "data_files")
    nested_dir <- file.path(data_dir, "daily_88101_2022")
    dir.create(nested_dir, recursive = TRUE, showWarnings = FALSE)
    nested_csv <- file.path(nested_dir, "daily_88101_2022.csv")
    writeLines("x,y\n1,2", nested_csv)

    amadeus:::download_normalize_aqs_unzip(
      directory_to_unzip = data_dir,
      resolution_temporal = "daily",
      parameter_code = 88101,
      year = 2022
    )

    testthat::expect_false(dir.exists(nested_dir))
    testthat::expect_true(
      file.exists(file.path(data_dir, "daily_88101_2022.csv"))
    )
  })
})

testthat::test_that("download_normalize_aqs_unzip no-ops when nested dir is absent", {
  withr::with_tempdir({
    data_dir <- file.path(".", "data_files")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    testthat::expect_invisible(
      amadeus:::download_normalize_aqs_unzip(
        directory_to_unzip = data_dir,
        resolution_temporal = "daily",
        parameter_code = 88101,
        year = 2022
      )
    )
    testthat::expect_false(
      dir.exists(file.path(data_dir, "daily_88101_2022"))
    )
  })
})

testthat::test_that("download_normalize_aqs_unzip no-ops without nested files", {
  withr::with_tempdir({
    data_dir <- file.path(".", "data_files")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    nested_dir <- file.path(data_dir, "daily_88101_2022")
    dir.create(file.path(nested_dir, "subdir"), recursive = TRUE, showWarnings = FALSE)

    testthat::expect_invisible(
      amadeus:::download_normalize_aqs_unzip(
        directory_to_unzip = data_dir,
        resolution_temporal = "daily",
        parameter_code = 88101,
        year = 2022
      )
    )

    testthat::expect_true(dir.exists(nested_dir))
    testthat::expect_true(dir.exists(file.path(nested_dir, "subdir")))
  })
})

testthat::test_that("download_normalize_aqs_unzip skips existing target files", {
  withr::with_tempdir({
    data_dir <- file.path(".", "data_files")
    nested_dir <- file.path(data_dir, "daily_88101_2022")
    dir.create(nested_dir, recursive = TRUE, showWarnings = FALSE)
    target_csv <- file.path(data_dir, "daily_88101_2022.csv")
    nested_csv <- file.path(nested_dir, "daily_88101_2022.csv")
    writeLines("existing", target_csv)
    writeLines("nested", nested_csv)

    testthat::expect_invisible(
      amadeus:::download_normalize_aqs_unzip(
        directory_to_unzip = data_dir,
        resolution_temporal = "daily",
        parameter_code = 88101,
        year = 2022
      )
    )

    testthat::expect_true(file.exists(target_csv))
    testthat::expect_false(file.exists(nested_csv))
    testthat::expect_false(dir.exists(nested_dir))
    testthat::expect_equal(readLines(target_csv), "existing")
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
    process_aqs(path = aqssub, date = c("January", "Januar")),
    "date has invalid format"
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

testthat::test_that("process_aqs handles mixed AQS date and duration formats", {
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::with_tempdir({
    aqs_path <- file.path(".", "aqs_mixed_formats.csv")
    mixed_aqs <- data.frame(
      State.Code = c(37, 37),
      County.Code = c(63, 63),
      Site.Num = c(15, 15),
      Parameter.Code = c(88101, 42602),
      POC = c(1, 1),
      Latitude = c(36.032955, 35.7796),
      Longitude = c(-78.904037, -78.6382),
      Datum = c("WGS84", "WGS84"),
      Parameter.Name = c("PM2.5 - Local Conditions", "Nitrogen dioxide (NO2)"),
      Sample.Duration = c("1 HOUR", "24 HOUR"),
      Pollutant.Standard = c("", ""),
      Date.Local = c("1/2/2022", "2022-01-03"),
      Units.of.Measure = c("Micrograms/cubic meter (LC)", "Parts per billion"),
      Event.Type = c("None", "None"),
      Observation.Count = c(24, 1),
      Observation.Percent = c(100, 100),
      Arithmetic.Mean = c(10.5, 12.1),
      X1st.Max.Value = c(23, 13),
      X1st.Max.Hour = c(23, 15),
      AQI = c(NA, NA),
      Method.Code = c(170, 600),
      Method.Name = c("Method A", "Method B"),
      Local.Site.Name = c("Durham Armory", "Raleigh Site"),
      Address = c("801 STADIUM DRIVE", "123 MAIN ST"),
      State.Name = c("North Carolina", "North Carolina"),
      County.Name = c("Durham", "Wake"),
      City.Name = c("Durham", "Raleigh"),
      CBSA.Name = c("Durham-Chapel Hill, NC", "Raleigh-Cary, NC"),
      Date.of.Last.Change = c("2022-09-26", "2022-09-26")
    )
    utils::write.csv(mixed_aqs, aqs_path, row.names = FALSE)

    aqs_processed <- process_aqs(
      path = aqs_path,
      date = c("2022-01-01", "2022-01-03"),
      mode = "available-data",
      return_format = "data.table"
    )

    testthat::expect_equal(nrow(aqs_processed), 2)
    testthat::expect_true(all(aqs_processed$time %in% c("2022-01-02", "2022-01-03")))
  })
})

testthat::test_that("process_aqs handles WGS84-only input", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    aqs_wgs84 <- data.frame(
      State.Code = c(37, 37),
      County.Code = c(63, 63),
      Site.Num = c(1, 2),
      Parameter.Code = c(88101, 88101),
      Date.Local = c("2022-02-10", "2022-02-11"),
      Sample.Duration = c("24-HR BLK AVG", "24-HR BLK AVG"),
      POC = c(1, 1),
      Longitude = c(-78.9040, -78.8803),
      Latitude = c(36.0330, 36.1702),
      Datum = c("WGS84", "WGS84")
    )
    csv_path <- file.path(".", "aqs_wgs84_only.csv")
    utils::write.csv(aqs_wgs84, csv_path, row.names = FALSE)

    testthat::expect_no_error(
      out_sf <- process_aqs(
        path = csv_path,
        date = c("2022-02-01", "2022-02-28"),
        mode = "location",
        return_format = "sf"
      )
    )
    testthat::expect_s3_class(out_sf, "sf")
    testthat::expect_equal(nrow(out_sf), 2)
  })
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

    testthat::expect_false(
      dir.exists(file.path(data_dir, "daily_88101_2022"))
    )
    testthat::expect_true(
      file.exists(file.path(data_dir, "daily_88101_2022.csv"))
    )
  })
})

################################################################################
##### download_aqs download_run_method branch (files need downloading)

testthat::test_that("download_aqs mock download with download_run_method", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_aqs(
          year = c(2018, 2018),
          resolution_temporal = "daily",
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

testthat::test_that("download_aqs all files exist path", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    withCallingHandlers(
      suppressWarnings(
        download_aqs(
          year = c(2018, 2018),
          resolution_temporal = "daily",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exist", msgs)))
  })
})

testthat::test_that(
  "download_data(aqs, resolution_temporal='hourly'): uses hourly archives",
  {
    captured <- NULL
    local_download_mocks(download_run_method = function(urls, destfiles, ...) {
      captured <<- list(urls = urls, destfiles = destfiles)
      list(success = length(urls), failed = 0L)
    })
    out <- download_data(
      dataset_name = "aqs", resolution_temporal = "hourly",
      year = c(2021, 2022), parameter_code = 88101,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE, unzip = FALSE
    )
    testthat::expect_equal(out$success, 2)
    testthat::expect_equal(captured$urls, paste0(
      "https://aqs.epa.gov/aqsweb/airdata/hourly_88101_", 2021:2022, ".zip"
    ))
    testthat::expect_equal(basename(captured$destfiles), paste0(
      "aqs_hourly_88101_", 2021:2022, ".zip"
    ))
  }
)

testthat::test_that(
  "process_covariates(aqs, resolution_temporal='hourly'): preserves samples",
  {
    path <- testthat::test_path(
      "..", "testdata", "aqs", "aqs_hourly_88101_sample.csv"
    )
    out <- process_covariates(
      covariate = "aqs", path = path, date = "2022-02-04",
      resolution_temporal = "hourly", mode = "available-data",
      return_format = "data.table"
    )
    testthat::expect_s3_class(out, "data.table")
    testthat::expect_equal(out$time, paste("2022-02-04", c(
      "00:00:00", "01:00:00", "23:00:00"
    )))
    testthat::expect_equal(out$Sample.Measurement, c(10, 20, 30))
    testthat::expect_equal(unique(out$site_id), "37063001588101")
    testthat::expect_false("Event.Type" %in% names(out))
    custom <- process_aqs(
      path, "2022-02-04", mode = "available-data", data_field = "MDL",
      resolution_temporal = "hourly", return_format = "data.table"
    )
    testthat::expect_equal(custom$MDL, rep(0.5, 3))
  }
)

testthat::test_that(
  "process_aqs(resolution_temporal='hourly'): supports all modes and formats",
  {
    path <- testthat::test_path(
      "..", "testdata", "aqs", "aqs_hourly_88101_sample.csv"
    )
    for (mode in c("available-data", "date-location", "location")) {
      for (fmt in c("data.table", "sf", "terra")) {
        out <- process_aqs(
          path, "2022-02-04", mode = mode, return_format = fmt,
          resolution_temporal = "hourly"
        )
        if (fmt == "terra") {
          testthat::expect_s4_class(out, "SpatVector")
        } else {
          testthat::expect_s3_class(out, fmt)
        }
        testthat::expect_equal(nrow(out), switch(
          mode, "available-data" = 3L, "date-location" = 24L, "location" = 1L
        ))
        if (mode == "date-location") {
          testthat::expect_equal(out$time, paste(
            "2022-02-04", sprintf("%02d:00:00", 0:23)
          ))
        }
      }
    }
  }
)

testthat::test_that(
  "process_aqs(resolution_temporal='daily'): preserves daily results",
  {
    daily <- testthat::test_path(
      "..", "testdata", "aqs", "aqs_daily_88101_triangle.csv"
    )
    hourly <- testthat::test_path(
      "..", "testdata", "aqs", "aqs_hourly_88101_sample.csv"
    )
    for (mode in c("available-data", "date-location", "location")) {
      default <- process_aqs(
        daily, "2022-02-04", mode = mode, return_format = "data.table"
      )
      mixed <- process_aqs(
        c(daily, hourly), "2022-02-04", mode = mode,
        return_format = "data.table", resolution_temporal = "daily"
      )
      testthat::expect_equal(mixed, default)
    }
    testthat::expect_error(
      process_aqs(daily, resolution_temporal = "hourly"),
      "No hourly AQS CSV files"
    )
    testthat::expect_error(
      process_aqs(hourly, resolution_temporal = "monthly"), "arg.*should be"
    )
  }
)

testthat::test_that(
  "process_aqs(resolution_temporal='hourly', path=<mixed>): selects hourly data",
  {
    dir <- withr::local_tempdir()
    files <- testthat::test_path("..", "testdata", "aqs", c(
      "aqs_daily_88101_triangle.csv", "aqs_hourly_88101_sample.csv"
    ))
    file.copy(files, dir)
    out <- process_aqs(
      dir, c("2022-02-04", "2022-02-05"), mode = "available-data",
      return_format = "data.table", resolution_temporal = "hourly"
    )
    testthat::expect_equal(out$Sample.Measurement, c(10, 20, 30, 40))
    grid <- process_aqs(
      dir, c("2022-02-04", "2022-02-05"), mode = "date-location",
      return_format = "data.table", resolution_temporal = "hourly"
    )
    testthat::expect_equal(nrow(grid), 48L)
    testthat::expect_equal(tail(grid$time, 1), "2022-02-05 23:00:00")
  }
)

testthat::test_that(
  "download_normalize_aqs_unzip(resolution_temporal='hourly'): flattens archive",
  {
    dir <- withr::local_tempdir()
    nested <- file.path(dir, "hourly_88101_2022")
    dir.create(nested)
    writeLines("hourly fixture", file.path(nested, "hourly_88101_2022.csv"))
    download_normalize_aqs_unzip(dir, "hourly", 88101, 2022)
    testthat::expect_equal(
      readLines(file.path(dir, "hourly_88101_2022.csv")), "hourly fixture"
    )
    testthat::expect_false(dir.exists(nested))
  }
)
