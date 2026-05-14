################################################################################
# Live network tests for download_koppen_geiger().
#
# Gated by `skip_if_no_live_tests()`. Mocked tests live in
# tests/testthat/test-koppen-geiger.R.
################################################################################

testthat::test_that(
  "download_koppen_geiger(time_period='Present', data_resolution='0.5'): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_koppen_geiger(
      time_period = "Present",
      data_resolution = "0.5",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = FALSE
    )

    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
