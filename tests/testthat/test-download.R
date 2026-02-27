################################################################################
##### unit and integration tests for download_data and auxiliary functions

################################################################################
##### download_data
testthat::test_that("download_data (expected errors - acknowledgement)", {
  download_datasets <- c(
    "aqs",
    "ecoregion",
    "geos",
    "gmted",
    "koppen",
    "koppengeiger",
    "merra2",
    "merra",
    "narr",
    "nlcd",
    "noaa",
    "sedac_groads",
    "sedac_population",
    "groads",
    "population",
    "hms",
    "smoke",
    "gridmet",
    "terraclimate",
    "huc",
    "cropscape",
    "cdl",
    "prism"
  )
  for (d in seq_along(download_datasets)) {
    testthat::expect_error(
      download_data(
        dataset_name = download_datasets[d],
        acknowledgement = FALSE
      )
    )
  }
})

testthat::test_that("download_data (expected errors - directory)", {
  download_datasets <- c(
    "aqs",
    "ecoregion",
    "geos",
    "gmted",
    "koppen",
    "koppengeiger",
    "merra2",
    "merra",
    "narr",
    "nlcd",
    "noaa",
    "sedac_groads",
    "sedac_population",
    "groads",
    "population",
    "hms",
    "smoke",
    "gridmet",
    "terraclimate",
    "huc",
    "cropscape",
    "cdl",
    "prism"
  )
  for (d in seq_along(download_datasets)) {
    testthat::expect_error(
      download_data(
        dataset_name = download_datasets[d],
        acknowledgement = TRUE,
        directory_to_save = NULL
      )
    )
  }
})

testthat::test_that("download_data (expected errors - temporal range)", {
  withr::with_tempdir({
    testthat::expect_error(
      download_geos(
        date = c("1900-01-01", "2018-01-01"),
        collection = "aqc_tavg_1hr_g1440x721_v1",
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_aqs(
        year = c(1900, 2022),
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_narr(
        year = c(1900, 2022),
        variables = "air.sfc",
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_merra2(
        date = c("1900-01-01", "2023-09-01"),
        collection = "inst1_2d_asm_Nx",
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_error(
      download_hms(
        date = c("1900-01-01", "2018-01-01"),
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_error(
      download_gridmet(
        year = c(1900, 2022),
        variables = "Precipitation",
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_terraclimate(
        year = c(1900, 2022),
        variables = "Wind Speed",
        acknowledgement = TRUE,
        directory_to_save = ".",
      )
    )
  })
})

################################################################################
##### httr2-specific tests
testthat::test_that("download_run_method validates inputs", {
  testthat::expect_error(
    download_run_method(urls = NULL),
    "No URLs provided"
  )

  testthat::expect_error(
    download_run_method(
      urls = c("http://example.com"),
      destfiles = c("file1.nc", "file2.nc") # length mismatch
    ),
    "same length"
  )

  testthat::expect_error(
    download_run_method(
      urls = character(0),
      destfiles = character(0)
    ),
    "No URLs provided"
  )
})

testthat::test_that("download_run_method has proper parameters", {
  params <- names(formals(download_run_method))

  testthat::expect_true("urls" %in% params)
  testthat::expect_true("destfiles" %in% params)
  testthat::expect_true("token" %in% params)
  testthat::expect_true("show_progress" %in% params)
  testthat::expect_true("max_tries" %in% params)
  testthat::expect_true("rate_limit" %in% params)
  testthat::expect_true("timeout" %in% params)
})

testthat::test_that("download_run_method skips existing files", {
  withr::with_tempdir({
    # Create existing file with content
    writeLines("existing data", "existing.txt")

    result <- download_run_method(
      urls = "http://httpbin.org/status/200",
      destfiles = "existing.txt",
      show_progress = FALSE
    )

    testthat::expect_equal(result$skipped, 1)
    testthat::expect_equal(result$success, 0)
  })
})

testthat::test_that("get_token retrieves from environment variable", {
  withr::local_envvar(TEST_TOKEN = "env_token_value")

  token <- get_token(token = NULL, env_var = "TEST_TOKEN")
  testthat::expect_equal(token, "env_token_value")
})

testthat::test_that("get_token reads from file", {
  withr::with_tempdir({
    writeLines("file_token_value", "token.txt")

    token <- get_token(token = "token.txt", env_var = "NONEXISTENT")
    testthat::expect_equal(token, "file_token_value")
  })
})

testthat::test_that("get_token uses direct token string", {
  withr::local_envvar(TEST_TOKEN = "")

  token <- get_token(token = "direct_token", env_var = "TEST_TOKEN")
  testthat::expect_equal(token, "direct_token")
})

testthat::test_that("get_token errors when no token found", {
  withr::local_envvar(NONEXISTENT_TOKEN = "")

  testthat::expect_error(
    get_token(token = NULL, env_var = "NONEXISTENT_TOKEN"),
    "No authentication token found"
  )
})

testthat::test_that("get_token handles empty file", {
  withr::with_tempdir({
    file.create("empty_token.txt")

    testthat::expect_error(
      get_token(token = "empty_token.txt", env_var = "NONEXISTENT"),
      "empty"
    )
  })
})

################################################################################
##### Deprecated parameter warnings
testthat::test_that("download functions warn about deprecated parameters", {
  withr::with_tempdir({
    # Test remove_command deprecation - DON'T suppress the warning we're testing
    testthat::expect_warning(
      download_narr(
        year = 2020,
        variables = "air.sfc",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      "deprecated and ignored"
    )

    # Test download=FALSE deprecation message specifically
    testthat::expect_warning(
      download_aqs(
        year = 2020,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      ),
      "deprecated"
    )
  })
})

################################################################################
##### Return value structure tests
testthat::test_that("download functions return proper structure with download=FALSE", {
  withr::with_tempdir({
    # Suppress the deprecation warning for cleaner test output
    result <- suppressWarnings(
      download_narr(
        year = 2020,
        variables = "air.sfc",
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
  })
})

testthat::test_that("download functions return proper structure with download=TRUE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Use a small, fast download for testing
    result <- download_koppen_geiger(
      data_resolution = "0.5",
      time_period = "Present",
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = FALSE
    )

    # Should return NULL (when hash=FALSE)
    testthat::expect_true(
      is.null(result) || is.list(result)
    )

    # Check file was created
    zip_files <- list.files("zip_files", pattern = "\\.zip$")
    testthat::expect_true(length(zip_files) > 0)
  })
})

################################################################################
##### check_url_status
testthat::test_that("check_url_status with valid URL", {
  skip_on_cran()
  skip_if_offline()

  urls <- "https://google.com"
  url_status <- check_url_status(url = urls)

  testthat::expect_length(url_status, 1)
  testthat::expect_true(url_status)
})

testthat::test_that("check_url_status with valid NASA file endpoint", {
  skip_on_cran()
  skip_if_offline()

  # Use a known valid URL
  urls <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
  url_status <- check_url_status(url = urls)

  testthat::expect_length(url_status, 1)
})

testthat::test_that("check_url_status with invalid URL returns FALSE", {
  skip_on_cran()
  skip_if_offline()

  # Use a URL that will definitely fail
  urls <- "https://httpbin.org/status/404"
  url_status <- check_url_status(url = urls)

  testthat::expect_length(url_status, 1)
  testthat::expect_false(url_status)
})

testthat::test_that("check_url_status handles DNS failures gracefully", {
  skip_on_cran()

  # This should return FALSE, not error
  urls <- "https://invalid.domain.that.does.not.exist.12345/file.nc"
  url_status <- check_url_status(url = urls)

  testthat::expect_false(url_status)
})

################################################################################
##### check_destfile
testthat::test_that("check_destfile returns TRUE for non-existent file", {
  testthat::expect_true(check_destfile("nonexistent.nc"))
})

testthat::test_that("check_destfile returns FALSE for existing file with content", {
  withr::with_tempdir({
    file_with_content <- "hasdata.nc"
    writeLines("data", file_with_content)
    testthat::expect_false(check_destfile(file_with_content))
  })
})

testthat::test_that("check_destfile returns TRUE for zero-byte file", {
  withr::with_tempdir({
    zero_byte <- "empty.nc"
    file.create(zero_byte)
    testthat::expect_true(check_destfile(zero_byte))
  })
})

testthat::test_that("check_destfile works in download context", {
  withr::with_tempdir({
    # Suppress deprecation warnings for cleaner output
    result <- suppressWarnings(
      download_data(
        dataset_name = "narr",
        year = c(2010, 2011),
        variables = "weasd",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        hash = FALSE
      )
    )

    # Should find 2 files to download when files don't exist
    testthat::expect_equal(result$n_files, 2)

    # Create files with content
    years <- seq(2013, 2015, 1)
    files <- paste0("soilm/soilm.", years, ".nc")
    dir.create("soilm")
    lapply(files, function(f) {
      writeLines("data", f)
    })

    result2 <- suppressWarnings(
      download_data(
        dataset_name = "narr",
        year = c(2013, 2015),
        variables = "soilm",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        hash = FALSE
      )
    )

    # Should skip existing files with content
    testthat::expect_equal(result2$n_files, 0)

    # Create zero-byte files
    dir.create("air.sfc")
    file.create("air.sfc/air.sfc.2020.nc")

    result3 <- suppressWarnings(
      download_data(
        dataset_name = "narr",
        year = 2020,
        variables = "air.sfc",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        hash = FALSE
      )
    )

    # Should re-download zero-byte files
    testthat::expect_equal(result3$n_files, 1)
  })
})

################################################################################
##### DEPRECATED: check_urls (kept for backward compatibility testing)
testthat::test_that("check_urls returns NULL with undefined size", {
  urls <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
  )
  url_status <- check_urls(urls = urls)
  testthat::expect_true(
    is.null(url_status)
  )
})

testthat::test_that("check_urls handles size > length(urls)", {
  skip_on_cran()
  skip_if_offline()

  urls <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
  )
  testthat::expect_no_error(
    url_status <- check_urls(urls = urls, size = 10)
  )
  testthat::expect_length(url_status, 1)
})

testthat::test_that("check_urls returns TRUE for valid URL", {
  skip_on_cran()
  skip_if_offline()

  urls <- "https://google.com"
  testthat::expect_no_error(
    url_status <- check_urls(urls = urls, size = 1)
  )
  testthat::expect_length(url_status, 1)
  testthat::expect_true(url_status)
})

################################################################################
##### DEPRECATED: extract_urls (kept for backward compatibility)
testthat::test_that("extract_url fails with NULL position", {
  withr::with_tempdir({
    # This test is now less relevant but kept for compatibility
    # Generate a sample command-like string
    commands <- c("wget http://example.com/file.nc -O output.nc")

    testthat::expect_message(
      extract_urls(commands, position = NULL),
      "URL position in command is not defined."
    )
  })
})

################################################################################
##### download_sink (DEPRECATED - but test still valid)
testthat::test_that("download_sink", {
  dir <- paste0(tempdir(), "/sink/")
  dir.create(dir, recursive = TRUE)
  testfile <- paste0(dir, "sink_test.txt")
  file.create(testfile)
  testthat::expect_no_error(
    download_sink(testfile)
  )
  sink()
  Sys.sleep(1.5)
  file.remove(testfile)
  unlink(dir, recursive = TRUE)
})

################################################################################
##### download_remove_zips
testthat::test_that("download_remove_zips", {
  dir <- paste0(tempdir(), "/yellowstone/")
  testfile1 <- paste0(dir, "barren/coyote.zip")
  dir.create(dirname(testfile1), recursive = TRUE)
  file.create(testfile1, recursive = TRUE)
  testfile2 <- paste0(dir, "retain/retain.txt")
  dir.create(dirname(testfile2), recursive = TRUE)
  file.create(testfile2, recursive = TRUE)
  testthat::expect_no_error(
    download_remove_zips(remove = TRUE, testfile1)
  )
  # expect only the testfile1 directory to be removed
  testthat::expect_equal(
    length(
      list.files(
        dir,
        recursive = TRUE,
        include.dirs = TRUE
      )
    ),
    2
  )
  unlink(paste0(dir, "/yellowstone"))
})

################################################################################
##### download_hash
testthat::test_that("download_hash", {
  dir <- paste0(tempdir(), "/hash/")
  dir.create(dir, recursive = TRUE)
  hashfile <- paste0(dir, "hash_test.txt")
  file.create(hashfile)
  testthat::expect_true(
    is.character(download_hash(TRUE, dir))
  )
  testthat::expect_true(
    is.null(download_hash(FALSE, dir))
  )
  Sys.sleep(1.5)
  file.remove(hashfile)
  unlink(dir, recursive = TRUE)
})

testthat::test_that("download_hash (LIVE)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    h_first <- download_narr(
      year = 2010,
      variables = "air.sfc",
      directory_to_save = "./first",
      acknowledgement = TRUE,
      download = TRUE,
      hash = TRUE
    )
    testthat::expect_true(
      is.character(h_first)
    )
    h_second <- download_data(
      dataset_name = "narr",
      year = 2010,
      variables = "air.sfc",
      directory_to_save = "./second",
      acknowledgement = TRUE,
      download = TRUE,
      hash = TRUE
    )
    testthat::expect_true(
      is.character(h_second)
    )
    testthat::expect_identical(h_first, h_second)
    h_third <- download_narr(
      year = 2011,
      variables = "air.sfc",
      directory_to_save = "./third",
      acknowledgement = TRUE,
      download = TRUE,
      hash = TRUE
    )
    testthat::expect_true(
      is.character(h_third)
    )
    testthat::expect_false(
      identical(h_first, h_third)
    )
  })
})

################################################################################
##### download_run (DEPRECATED - but test for backward compatibility)
testthat::test_that("download_run shows deprecation warning", {
  withr::with_tempdir({
    # Create a dummy commands file
    writeLines("wget http://example.com", "commands.txt")

    # Reset the warning flag for this test
    options(amadeus.download_run.warned = NULL)

    testthat::expect_warning(
      download_run(
        download = FALSE,
        commands_txt = "commands.txt",
        remove = FALSE
      ),
      "deprecated"
    )
  })
})

################################################################################
##### Integration tests with actual downloads
testthat::test_that("download workflow with hash (LIVE)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Download small dataset
    result <- download_koppen_geiger(
      data_resolution = "0.5",
      time_period = "Present",
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = FALSE,
      hash = TRUE
    )

    # Should return hash
    testthat::expect_true(is.character(result))
    testthat::expect_true(nchar(result) > 0)
  })
})

testthat::test_that("download with show_progress = FALSE and hash = FALSE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Test that downloads work without progress display
    # Should return invisible NULL when hash = FALSE
    result <- suppressMessages(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        show_progress = FALSE,
        hash = FALSE
      )
    )

    # Should return NULL
    testthat::expect_null(result)

    # Check file was created in the correct location
    testthat::expect_true(dir.exists("zip_files"))
    zip_files <- list.files("zip_files", pattern = "\\.zip$", full.names = TRUE)
    testthat::expect_true(length(zip_files) > 0)
    testthat::expect_true(all(file.size(zip_files) > 0))
  })
})

testthat::test_that("download with show_progress = FALSE and hash = TRUE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Test that downloads work and return hash
    result <- suppressMessages(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        show_progress = FALSE,
        hash = TRUE
      )
    )

    # Should return a hash string
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
    testthat::expect_true(grepl("^[a-f0-9]+$", result)) # MD5 hash format

    # Check file was created
    testthat::expect_true(dir.exists("zip_files"))
    zip_files <- list.files("zip_files", pattern = "\\.zip$", full.names = TRUE)
    testthat::expect_true(length(zip_files) > 0)
  })
})

testthat::test_that("download with show_progress = TRUE works", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Test that downloads work with progress display
    result <- suppressMessages(
      download_koppen_geiger(
        data_resolution = "0.5",
        time_period = "Present",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        show_progress = TRUE,
        hash = FALSE
      )
    )

    # Should return NULL
    testthat::expect_null(result)

    # Check file was created
    testthat::expect_true(dir.exists("zip_files"))
    zip_files <- list.files("zip_files", pattern = "\\.zip$", full.names = TRUE)
    testthat::expect_true(length(zip_files) > 0)
  })
})

testthat::test_that("download handles network errors gracefully", {
  skip_on_cran()

  withr::with_tempdir({
    # download_run_method should return a result with failures, not throw error
    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = "https://invalid.domain.that.does.not.exist.12345/file.nc",
          destfiles = "output.nc",
          show_progress = FALSE,
          max_tries = 2
        )
      )
    )

    # Should return a list with failure information
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$failed, 1)
    testthat::expect_true("failed_urls" %in% names(result))
  })
})

testthat::test_that("download respects max_tries parameter", {
  # Check that max_tries parameter is respected in function signature
  testthat::expect_true(
    "max_tries" %in% names(formals(download_run_method))
  )

  # Check default value
  testthat::expect_equal(
    formals(download_run_method)$max_tries,
    20
  )
})

testthat::test_that("download respects rate_limit parameter", {
  # Check that rate_limit parameter exists
  testthat::expect_true(
    "rate_limit" %in% names(formals(download_run_method))
  )

  # Check default value
  testthat::expect_equal(
    formals(download_run_method)$rate_limit,
    2
  )
})

################################################################################
##### Test download_run_method comprehensively
testthat::test_that("download_run_method with show_progress = TRUE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    url <- paste0(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
      "us_eco_l3_state_boundaries.zip"
    )
    destfile <- "test_progress_true.zip"

    result <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = TRUE,
        max_tries = 2
      )
    )

    # Check result structure
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 0)

    # Check file exists
    testthat::expect_true(file.exists(destfile))
    testthat::expect_gt(file.size(destfile), 0)
  })
})

testthat::test_that("download_run_method with show_progress = FALSE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    url <- paste0(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
      "us_eco_l3_state_boundaries.zip"
    )
    destfile <- "test_progress_false.zip"

    result <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = FALSE,
        max_tries = 2
      )
    )

    # Check result structure
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 0)

    # Check file exists
    testthat::expect_true(file.exists(destfile))
    testthat::expect_gt(file.size(destfile), 0)
  })
})

testthat::test_that("download_run_method handles failure with show_progress = TRUE", {
  skip_on_cran()

  withr::with_tempdir({
    url <- "https://invalid.domain.that.does.not.exist.12345/file.zip"
    destfile <- "invalid_progress_true.zip"

    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = url,
          destfiles = destfile,
          show_progress = TRUE,
          max_tries = 2
        )
      )
    )

    # Check result structure
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$failed, 1)
    testthat::expect_length(result$failed_urls, 1)

    # Check file does not exist
    testthat::expect_false(file.exists(destfile))
  })
})

testthat::test_that("download_run_method handles failure with show_progress = FALSE", {
  skip_on_cran()

  withr::with_tempdir({
    url <- "https://invalid.domain.that.does.not.exist.12345/file.zip"
    destfile <- "invalid_progress_false.zip"

    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = url,
          destfiles = destfile,
          show_progress = FALSE,
          max_tries = 2
        )
      )
    )

    # Check result structure
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$failed, 1)
    testthat::expect_length(result$failed_urls, 1)

    # Check file does not exist
    testthat::expect_false(file.exists(destfile))
  })
})

testthat::test_that("download_run_method handles mixed success/failure with show_progress = TRUE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    urls <- c(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip",
      "https://invalid.domain.12345/file.zip"
    )
    destfiles <- c("valid_progress_true.zip", "invalid_progress_true.zip")

    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = urls,
          destfiles = destfiles,
          show_progress = TRUE,
          max_tries = 2
        )
      )
    )

    # Check result structure
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 1)

    # Check files
    testthat::expect_true(file.exists("valid_progress_true.zip"))
    testthat::expect_false(file.exists("invalid_progress_true.zip"))
  })
})

testthat::test_that("download_run_method handles mixed success/failure with show_progress = FALSE", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    urls <- c(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip",
      "https://invalid.domain.12345/file.zip"
    )
    destfiles <- c("valid_progress_false.zip", "invalid_progress_false.zip")

    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = urls,
          destfiles = destfiles,
          show_progress = FALSE,
          max_tries = 2
        )
      )
    )

    # Check result structure
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 1)

    # Check files
    testthat::expect_true(file.exists("valid_progress_false.zip"))
    testthat::expect_false(file.exists("invalid_progress_false.zip"))
  })
})

testthat::test_that("download_run_method skips existing files with both progress modes", {
  skip_on_cran()

  withr::with_tempdir({
    destfile <- "existing.zip"
    writeLines("existing data", destfile)

    url <- "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"

    # Test with show_progress = TRUE
    result_true <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = TRUE,
        max_tries = 2
      )
    )

    testthat::expect_equal(result_true$success, 0)
    testthat::expect_equal(result_true$failed, 0)
    testthat::expect_equal(result_true$skipped, 1)

    # Test with show_progress = FALSE
    result_false <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = FALSE,
        max_tries = 2
      )
    )

    testthat::expect_equal(result_false$success, 0)
    testthat::expect_equal(result_false$failed, 0)
    testthat::expect_equal(result_false$skipped, 1)

    # File should still contain original content
    content <- readLines(destfile)
    testthat::expect_equal(content, "existing data")
  })
})

testthat::test_that("download_run_method respects rate limiting", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    url <- "https://s3-eu-west-1.amazonaws.com/pfigshare-u-files/12407516/Beck_KG_V1.zip"
    urls <- c(url, url)
    destfiles <- c("file1.zip", "file2.zip")

    start_time <- Sys.time()
    result <- suppressMessages(
      download_run_method(
        urls = urls,
        destfiles = destfiles,
        show_progress = FALSE,
        max_tries = 2,
        rate_limit = 2
      )
    )
    end_time <- Sys.time()

    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Should take at least 1.5 seconds due to rate limiting
    testthat::expect_gte(elapsed, 1.5)
    testthat::expect_equal(result$success, 2)
    testthat::expect_equal(result$failed, 0)
  })
})


testthat::test_that("format_file_size formats correctly", {
  testthat::expect_equal(format_file_size(500), "500 B")
  testthat::expect_equal(format_file_size(1024), "1.0 KB")
  testthat::expect_equal(format_file_size(1024 * 1024), "1.0 MB")
  testthat::expect_equal(format_file_size(1024 * 1024 * 1024), "1.0 GB")
  testthat::expect_equal(format_file_size(2560), "2.5 KB")
})


################################################################################
##### Test download_run_method with actual requests

# Simple baseline test
testthat::test_that("download_run_method handles http success", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    url <- paste0(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
      "us_eco_l3_state_boundaries.zip"
    )
    destfile <- "test.zip"

    result <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = FALSE,
        max_tries = 2
      )
    )

    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 0)
    testthat::expect_true(file.exists(destfile))
    testthat::expect_gt(file.size(destfile), 0)
  })
})

testthat::test_that("download_run_method handles multiple files", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Use Köppen-Geiger URL from download_koppen_geiger (known to work)
    url <- "https://s3-eu-west-1.amazonaws.com/pfigshare-u-files/12407516/Beck_KG_V1.zip"
    destfile <- "koppen_test.zip"

    result <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = FALSE,
        max_tries = 2,
        rate_limit = 1
      )
    )

    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 0)
    testthat::expect_equal(result$skipped, 0)
    testthat::expect_true(file.exists(destfile))
    testthat::expect_gt(file.size(destfile), 1000) # Should be a reasonable size
  })
})

testthat::test_that("download_run_method handles failures correctly", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Mix of valid URL (from ecoregion) and invalid URL
    urls <- c(
      "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip",
      "https://invalid.domain.that.does.not.exist.12345/file.nc"
    )
    destfiles <- c("valid.zip", "invalid.nc")

    result <- suppressWarnings(
      suppressMessages(
        download_run_method(
          urls = urls,
          destfiles = destfiles,
          show_progress = FALSE,
          max_tries = 2,
          rate_limit = 1
        )
      )
    )

    testthat::expect_type(result, "list")
    # Should have 1 success and 1 failure
    testthat::expect_equal(result$success, 1)
    testthat::expect_equal(result$failed, 1)
    testthat::expect_true(file.exists("valid.zip"))
    testthat::expect_false(file.exists("invalid.nc"))
  })
})

testthat::test_that("download_run_method respects rate limiting", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Download the same file twice with different names to test rate limiting
    # Use small Köppen-Geiger file
    url <- "https://s3-eu-west-1.amazonaws.com/pfigshare-u-files/12407516/Beck_KG_V1.zip"
    urls <- c(url, url)
    destfiles <- c("file1.zip", "file2.zip")

    # Time the download with rate limiting
    start_time <- Sys.time()
    result <- suppressMessages(
      download_run_method(
        urls = urls,
        destfiles = destfiles,
        show_progress = FALSE,
        max_tries = 2,
        rate_limit = 2 # 2 seconds between requests
      )
    )
    end_time <- Sys.time()

    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Should take at least 2 seconds due to rate limiting
    # (allowing tolerance for download time variability)
    testthat::expect_gte(elapsed, 1.5)

    # Both should succeed
    testthat::expect_equal(result$success, 2)
    testthat::expect_equal(result$failed, 0)
  })
})

testthat::test_that("download_run_method skips existing files correctly", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Create an existing file with content
    destfile <- "existing.zip"
    writeLines("existing data", destfile)

    url <- "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"

    result <- suppressMessages(
      download_run_method(
        urls = url,
        destfiles = destfile,
        show_progress = FALSE,
        max_tries = 2
      )
    )

    # Should skip the existing file
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$failed, 0)
    testthat::expect_equal(result$skipped, 1)

    # File should still contain original content
    testthat::expect_true(file.exists(destfile))
    content <- readLines(destfile)
    testthat::expect_equal(content, "existing data")
  })
})

testthat::test_that("download functions create proper directory structure", {
  withr::with_tempdir({
    # Test that directories are created properly
    suppressWarnings(
      download_narr(
        year = 2020,
        variables = "air.sfc",
        directory_to_save = "./data",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    testthat::expect_true(dir.exists("./data"))
    testthat::expect_true(dir.exists("./data/air.sfc"))
  })
})

testthat::test_that("download functions handle nested directories", {
  withr::with_tempdir({
    # Test deeply nested directory creation
    suppressWarnings(
      download_geos(
        collection = "aqc_tavg_1hr_g1440x721_v1",
        date = "2024-01-01",
        directory_to_save = "./deep/nested/path",
        acknowledgement = TRUE,
        download = FALSE,
        nasa_earth_data_token = "dummy_token"
      )
    )

    testthat::expect_true(dir.exists("./deep/nested/path"))
  })
})

################################################################################
##### Coverage for edge cases
testthat::test_that("download handles empty URL lists", {
  withr::with_tempdir({
    # Create a scenario where all files exist
    dir.create("air.sfc")
    writeLines("data", "air.sfc/air.sfc.2020.nc")

    result <- suppressWarnings(
      download_narr(
        year = 2020,
        variables = "air.sfc",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    testthat::expect_equal(result$n_files, 0)
  })
})

testthat::test_that("download sanitizes paths correctly", {
  withr::with_tempdir({
    # Test path without trailing slash
    result <- suppressWarnings(
      download_narr(
        year = 2020,
        variables = "air.sfc",
        directory_to_save = "./data", # no trailing slash
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Should still work
    testthat::expect_type(result, "list")

    # Test path with trailing slash
    result2 <- suppressWarnings(
      download_narr(
        year = 2020,
        variables = "air.sfc",
        directory_to_save = "./data/", # with trailing slash
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    testthat::expect_type(result2, "list")
  })
})

testthat::test_that("download handles special characters in paths", {
  skip_on_os("windows") # Windows has different path rules

  withr::with_tempdir({
    dir.create("path with spaces")

    testthat::expect_no_error(
      suppressWarnings(
        download_narr(
          year = 2020,
          variables = "air.sfc",
          directory_to_save = "./path with spaces",
          acknowledgement = TRUE,
          download = FALSE
        )
      )
    )
  })
})

################################################################################
##### Test setup_nasa_token function
testthat::test_that("setup_nasa_token validates inputs", {
  skip_on_cran()

  # Empty token should error
  testthat::expect_error(
    setup_nasa_token(method = "session", token = ""),
    "cannot be empty"
  )
})

testthat::test_that("setup_nasa_token sets session variable", {
  skip_on_cran()

  withr::local_envvar(NASA_EARTHDATA_TOKEN = "")

  testthat::expect_no_error(
    suppressMessages(
      setup_nasa_token(method = "session", token = "test_token_123")
    )
  )

  testthat::expect_equal(
    Sys.getenv("NASA_EARTHDATA_TOKEN"),
    "test_token_123"
  )
})

################################################################################
##### Test download_setup_dir
testthat::test_that("download_setup_dir creates directories", {
  withr::with_tempdir({
    dir_path <- "./test_dir"

    testthat::expect_no_error(
      download_setup_dir(dir_path)
    )

    testthat::expect_true(dir.exists(dir_path))
  })
})

testthat::test_that("download_setup_dir creates zip subdirectories", {
  withr::with_tempdir({
    dir_path <- "./test_dir"

    dirs <- download_setup_dir(dir_path, zip = TRUE)

    testthat::expect_length(dirs, 2)
    testthat::expect_true(dir.exists(paste0(dir_path, "/zip_files")))
    testthat::expect_true(dir.exists(paste0(dir_path, "/data_files")))
  })
})

################################################################################
##### Test download_sanitize_path
testthat::test_that("download_sanitize_path adds trailing slash", {
  result <- download_sanitize_path("/path/without/slash")
  testthat::expect_equal(result, "/path/without/slash/")
})

testthat::test_that("download_sanitize_path preserves trailing slash", {
  result <- download_sanitize_path("/path/with/slash/")
  testthat::expect_equal(result, "/path/with/slash/")
})

################################################################################
##### Test download_permit
testthat::test_that("download_permit errors when FALSE", {
  testthat::expect_error(
    download_permit(acknowledgement = FALSE),
    "acknowledgement"
  )
})

testthat::test_that("download_permit succeeds when TRUE", {
  testthat::expect_no_error(
    download_permit(acknowledgement = TRUE)
  )
})

################################################################################
##### Test download_unzip
testthat::test_that("download_unzip works correctly", {
  skip_on_cran()

  withr::with_tempdir({
    # Create a test zip file
    test_file <- "test.txt"
    writeLines("test content", test_file)
    zip_file <- "test.zip"
    utils::zip(zip_file, test_file)
    file.remove(test_file)

    unzip_dir <- "./unzipped"
    dir.create(unzip_dir)

    testthat::expect_no_error(
      suppressMessages(
        download_unzip(
          file_name = zip_file,
          directory_to_unzip = unzip_dir,
          unzip = TRUE
        )
      )
    )

    testthat::expect_true(file.exists(file.path(unzip_dir, test_file)))
  })
})

testthat::test_that("download_unzip respects unzip=FALSE", {
  withr::with_tempdir({
    zip_file <- "test.zip"
    file.create(zip_file)
    unzip_dir <- "./unzipped"
    dir.create(unzip_dir)

    testthat::expect_message(
      download_unzip(
        file_name = zip_file,
        directory_to_unzip = unzip_dir,
        unzip = FALSE
      ),
      "not be unzipped"
    )
  })
})

################################################################################
##### Test check_for_null_parameters
testthat::test_that("check_for_null_parameters detects nulls", {
  params <- list(a = 1, b = NULL, c = 3)

  testthat::expect_error(
    check_for_null_parameters(params),
    "NULL"
  )
})

testthat::test_that("check_for_null_parameters allows extent to be NULL", {
  params <- list(a = 1, extent = NULL, c = 3)

  testthat::expect_no_error(
    check_for_null_parameters(params)
  )
})

testthat::test_that("check_for_null_parameters passes with no nulls", {
  params <- list(a = 1, b = 2, c = 3)

  testthat::expect_no_error(
    check_for_null_parameters(params)
  )
})
