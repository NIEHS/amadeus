################################################################################
# Live network tests for download_hrrr(). Mocked tests: test-hrrr.R.
################################################################################

testthat::test_that(
  paste0(
    "download_hrrr(product = 'surface', date = '2021-04-05')): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hrrr(
      product = "surface",
      date = "2021-04-05",
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
    "download_hrrr(product = 'pressure', date = '2021-04-05')): ",
    "downloads monolevel snow water equivalent file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hrrr(
      product = "pressure",
      date = "2021-04-05",
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
    "download_hrrr(product = 'native', date = '2021-04-05')): ",
    "downloads pressure-level files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hrrr(
      product = "native",
      date = "2021-04-05",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
