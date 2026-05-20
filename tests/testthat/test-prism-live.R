################################################################################
# Live network tests for download_prism(). Mocked tests: test-prism.R.
################################################################################

testthat::test_that(
  paste0(
    "download_prism(time='202201', element='ppt', data_type='ts'): ",
    "downloads monthly precipitation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_prism(
      time = "202201",
      element = "ppt",
      data_type = "ts",
      format = "nc",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_prism(time='20220101', element='tmean', data_type='ts'): ",
    "downloads daily mean temperature file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_prism(
      time = "20220101",
      element = "tmean",
      data_type = "ts",
      format = "nc",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
