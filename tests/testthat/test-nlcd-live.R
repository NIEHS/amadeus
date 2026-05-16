################################################################################
# Live network tests for download_nlcd(). Mocked tests: test-nlcd.R.
################################################################################

testthat::test_that(
  paste0(
    "download_nlcd(product='Land Cover', year=2021): ",
    "downloads land cover zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_nlcd(
      product = "Land Cover",
      year = 2021,
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
    "download_nlcd(product='Land Cover Change', year=2020): ",
    "downloads change zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_nlcd(
      product = "Land Cover Change",
      year = 2020,
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
    "download_nlcd(product='Fractional Impervious Surface', year=2022): ",
    "downloads impervious zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_nlcd(
      product = "Fractional Impervious Surface",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
