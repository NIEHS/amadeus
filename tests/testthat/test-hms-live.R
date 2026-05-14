################################################################################
# Live network tests for download_hms(). Mocked tests: test-hms.R.
################################################################################

testthat::test_that(
  "download_hms(data_format='Shapefile', date=<single day>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hms(
      data_format = "Shapefile",
      date = c("2024-01-01", "2024-01-01"),
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
