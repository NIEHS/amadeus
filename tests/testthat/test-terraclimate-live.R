################################################################################
# Live network tests for download_terraclimate(). Mocked tests: test-terraclimate.R.
################################################################################

testthat::test_that(
  paste0(
    "download_terraclimate(variables='ppt', year=c(2022,2022)): ",
    "downloads precipitation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_terraclimate(
      variables = "ppt",
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
    "download_terraclimate(variables='tmax', year=c(2022,2022)): ",
    "downloads maximum temperature file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_terraclimate(
      variables = "tmax",
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
    "download_terraclimate(variables='vpd', year=c(2022,2022)): ",
    "downloads vapor pressure deficit file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_terraclimate(
      variables = "vpd",
      year = c(2022, 2022),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
