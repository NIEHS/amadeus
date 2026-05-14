################################################################################
# Live network tests for download_population(). Mocked tests: test-population.R.
################################################################################

testthat::test_that(
  "download_population(data_resolution='60 minute', data_format='GeoTIFF', year=2020): downloads non-empty file",
  {
    skip_if_no_live_tests()
    skip_if_no_credentials("NASA_EARTHDATA_TOKEN")
    dir <- withr::local_tempdir()
    amadeus::download_population(
      data_resolution = "60 minute",
      data_format = "GeoTIFF",
      year = "2020",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
