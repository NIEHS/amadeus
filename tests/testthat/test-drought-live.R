################################################################################
# Live network tests for download_drought(). Mocked tests: test-drought.R.
################################################################################

testthat::test_that(
  "download_drought(source='spei', timescale=1, date=<single>): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_drought(
      source = "spei",
      date = c("2022-01-01", "2022-01-31"),
      timescale = 1L,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
