################################################################################
# Live network tests for download_ecoregion(). Mocked tests: test-ecoregion.R.
################################################################################

testthat::test_that(
  "download_ecoregion(acknowledgement=TRUE): downloads non-empty zip",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_ecoregion(
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
