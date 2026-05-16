################################################################################
# Live network tests for download_aqs().
#
# Exercises the real EPA AQS endpoint. Gated by `skip_if_no_live_tests()`.
# Run via the scheduled workflow `.github/workflows/test-live.yaml` or
# locally with `AMADEUS_LIVE_TESTS=true devtools::test(filter = "aqs-live")`.
#
# The mocked counterpart lives in tests/testthat/test-aqs.R.
################################################################################

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='88101'): downloads a non-empty zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "88101",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='44201'): downloads ozone zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "44201",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='42101'): downloads carbon monoxide zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "42101",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='42401'): downloads sulfur dioxide zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "42401",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='42602'): downloads nitrogen dioxide zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "42602",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2022, ",
    "parameter_code='81102'): downloads PM10 zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "81102",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='hourly', year=2022, ",
    "parameter_code='88101'): downloads hourly criteria pollutant zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "hourly",
      parameter_code = "88101",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)

testthat::test_that(
  paste0(
    "download_aqs(resolution_temporal='daily', year=2020, ",
    "parameter_code='88101'): downloads historical PM2.5 zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "88101",
      year = 2020,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE,
                       full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)
