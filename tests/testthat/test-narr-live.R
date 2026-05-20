################################################################################
# Live network tests for download_narr(). Mocked tests: test-narr.R.
################################################################################

testthat::test_that(
  paste0(
    "download_narr(variables='air.sfc', year=c(2022,2022)): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_narr(
      variables = "air.sfc",
      year = c(2022, 2022),
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
    "download_narr(variables='weasd', year=c(2022,2022)): ",
    "downloads monolevel snow water equivalent file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_narr(
      variables = "weasd",
      year = c(2022, 2022),
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
    "download_narr(variables='omega', year=c(2022,2022)): ",
    "downloads pressure-level files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_narr(
      variables = "omega",
      year = c(2022, 2022),
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
    "download_narr(variables='air.2m', year=c(2022,2022)): ",
    "downloads second monolevel variable"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_narr(
      variables = "air.2m",
      year = c(2022, 2022),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
