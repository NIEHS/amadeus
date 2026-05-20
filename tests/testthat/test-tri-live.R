################################################################################
# Live network tests for download_tri(). Mocked tests: test-tri.R.
################################################################################

testthat::test_that(
  paste0(
    "download_tri(year=c(2022,2022), jurisdiction='US'): ",
    "downloads national file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_tri(
      year = c(2022L, 2022L),
      directory_to_save = dir,
      acknowledgement = TRUE,
      jurisdiction = "US"
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_tri(year=c(2022,2022), jurisdiction='NC'): ",
    "downloads state file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_tri(
      year = c(2022L, 2022L),
      directory_to_save = dir,
      acknowledgement = TRUE,
      jurisdiction = "NC"
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_tri(year=c(2022,2022), jurisdiction='tbl'): ",
    "downloads tribal file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_tri(
      year = c(2022L, 2022L),
      directory_to_save = dir,
      acknowledgement = TRUE,
      jurisdiction = "tbl"
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
