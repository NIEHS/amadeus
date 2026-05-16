################################################################################
# Live network tests for download_gmted(). Mocked tests: test-gmted.R.
################################################################################

testthat::test_that(
  paste0(
    "download_gmted(statistic='Mean Statistic', resolution='30 arc-seconds'): ",
    "downloads mean archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_gmted(
      statistic = "Mean Statistic",
      resolution = "30 arc-seconds",
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
    "download_gmted(statistic='Minimum Statistic', resolution='15 arc-seconds'): ",
    "downloads minimum archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_gmted(
      statistic = "Minimum Statistic",
      resolution = "15 arc-seconds",
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
    "download_gmted(statistic='Maximum Statistic', resolution='7.5 arc-seconds'): ",
    "downloads maximum archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_gmted(
      statistic = "Maximum Statistic",
      resolution = "7.5 arc-seconds",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
