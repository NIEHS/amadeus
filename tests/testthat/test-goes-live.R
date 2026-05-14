################################################################################
# Live network tests for download_goes(). Mocked tests: test-goes.R.
################################################################################

testthat::test_that(
  "download_goes(satellite='16', product='ADP-C', date=<single>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "16",
      product = "ADP-C",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
