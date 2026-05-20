################################################################################
# Live network tests for download_gridmet(). Mocked tests: test-gridmet.R.
################################################################################

testthat::test_that(
  paste0(
    "download_gridmet(variables='pr', year=c(2022,2022)): ",
    "downloads precipitation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_gridmet(
      variables = "pr",
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
    "download_gridmet(variables='tmmx', year=c(2022,2022)): ",
    "downloads maximum temperature file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_gridmet(
      variables = "tmmx",
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
    "download_gridmet(variables='vs', year=c(2022,2022)): ",
    "downloads wind speed file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_gridmet(
      variables = "vs",
      year = c(2022, 2022),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
