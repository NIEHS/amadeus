################################################################################
# Live network tests for download_modis(). Mocked tests: test-modis.R.
################################################################################

testthat::test_that(
  "download_modis(product='MOD09GA', date=<single day>, tile=<single>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    skip_if_no_credentials("NASA_EARTHDATA_TOKEN")
    dir <- withr::local_tempdir()
    amadeus::download_modis(
      product = "MOD09GA",
      version = "61",
      horizontal_tiles = c(8, 8),
      vertical_tiles = c(4, 4),
      date = c("2024-01-01", "2024-01-01"),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
