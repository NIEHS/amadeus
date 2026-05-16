################################################################################
# Live network tests for download_cropscape(). Mocked tests: test-cropscape.R.
################################################################################

testthat::test_that(
  paste0(
    "download_cropscape(year=2022, source='USDA'): ",
    "downloads USDA current zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_cropscape(
      year = 2022,
      source = "USDA",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_cropscape(year=2018, source='USDA'): ",
    "downloads USDA historical zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_cropscape(
      year = 2018,
      source = "USDA",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_cropscape(year=2020, source='USDA'): ",
    "downloads USDA intermediate zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_cropscape(
      year = 2020,
      source = "USDA",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_cropscape(year=2018, source='GMU'): ",
    "downloads GMU tarball"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_cropscape(
      year = 2018,
      source = "GMU",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
