################################################################################
# Live network tests for download_koppen_geiger(). Mocked tests live in test-koppen-geiger.R.
################################################################################

testthat::test_that(
  paste0(
    "download_koppen_geiger(time_period='Present', data_resolution='0.5'): ",
    "downloads present coarse zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_koppen_geiger(
      time_period = "Present",
      data_resolution = "0.5",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_koppen_geiger(time_period='Present', data_resolution='0.083'): ",
    "downloads present medium zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_koppen_geiger(
      time_period = "Present",
      data_resolution = "0.083",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_koppen_geiger(time_period='Future', data_resolution='0.5'): ",
    "downloads future coarse zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_koppen_geiger(
      time_period = "Future",
      data_resolution = "0.5",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
