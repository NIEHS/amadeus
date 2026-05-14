################################################################################
# Live network tests for download_merra2(). Mocked tests: test-merra2.R.
################################################################################

testthat::test_that(
  "download_merra2(collection='inst1_2d_asm_Nx', date=<single>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    skip_if_no_credentials("NASA_EARTHDATA_TOKEN")
    dir <- withr::local_tempdir()
    amadeus::download_merra2(
      collection = "inst1_2d_asm_Nx",
      date = c("2024-01-01", "2024-01-01"),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
