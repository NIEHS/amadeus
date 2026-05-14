################################################################################
# Live network tests for download_gridmet(). Mocked tests: test-gridmet.R.
################################################################################

testthat::test_that(
  "download_gridmet(variables='Precipitation', year=c(2022,2022)): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_gridmet(
      variables = "Precipitation",
      year = c(2022, 2022),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
