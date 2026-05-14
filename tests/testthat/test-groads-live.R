################################################################################
# Live network tests for download_groads(). Mocked tests: test-groads.R.
################################################################################

testthat::test_that(
  "download_groads(data_region='Americas', data_format='Shapefile'): downloads non-empty file",
  {
    skip_if_no_live_tests()
    skip_if_no_credentials("NASA_EARTHDATA_TOKEN")
    dir <- withr::local_tempdir()
    amadeus::download_groads(
      data_region = "Americas",
      data_format = "Shapefile",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
