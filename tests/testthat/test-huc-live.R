################################################################################
# Live network tests for download_huc(). Mocked tests: test-huc.R.
################################################################################

testthat::test_that(
  "download_huc(region='Lower48', type='Seamless'): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_huc(
      region = "Lower48",
      type = "Seamless",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
