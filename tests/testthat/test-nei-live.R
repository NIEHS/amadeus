################################################################################
# Live network tests for download_nei(). Mocked tests: test-nei.R.
#
# NOTE: download_nei() requires an EPA SSL certificate path. Adjust the
# certificate handling for live runs (the function fetches it from a URL).
################################################################################

testthat::test_that(
  "download_nei(year=c(2020,2020)): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_nei(
      year = c(2020L, 2020L),
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
