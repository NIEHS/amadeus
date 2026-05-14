################################################################################
# Live network tests for download_edgar(). Mocked tests: test-edgar.R.
################################################################################

testthat::test_that(
  "download_edgar(species='CO', version='8.1', sector_yearly=<one>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    testthat::expect_no_error(
      amadeus::download_edgar(
        species = "CO",
        version = "8.1",
        format = "nc",
        output = "emi",
        directory_to_save = dir,
        acknowledgement = TRUE
      )
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
  }
)
