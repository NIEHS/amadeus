################################################################################
# Live network tests for download_gmted(). Mocked tests: test-gmted.R.
################################################################################

testthat::test_that(
  "download_gmted(statistic='Mean Statistic', resolution='30 arc-seconds'): downloads non-empty file",
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
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
