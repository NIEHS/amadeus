################################################################################
# Live network tests for download_improve(). Mocked tests: test-improve.R.
################################################################################

testthat::test_that(
  paste0(
    "download_improve(year=c(2022,2022), product='raw'): ",
    "downloads raw file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_improve(
      year = c(2022, 2022),
      product = "raw",
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
    "download_improve(year=c(2022,2022), product='rhr2'): ",
    "downloads RHR2 file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_improve(
      year = c(2022, 2022),
      product = "rhr2",
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
    "download_improve(year=c(2022,2022), product='rhr3'): ",
    "downloads RHR3 file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_improve(
      year = c(2022, 2022),
      product = "rhr3",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
