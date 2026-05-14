################################################################################
# Live network tests for download_aqs().
#
# Exercises the real EPA AQS endpoint. Gated by `skip_if_no_live_tests()`.
# Run via the scheduled workflow `.github/workflows/test-live.yaml` or
# locally with `AMADEUS_LIVE_TESTS=true devtools::test(filter = "aqs-live")`.
#
# The mocked counterpart lives in tests/testthat/test-aqs.R.
################################################################################

testthat::test_that(
  "download_aqs(resolution_temporal='daily', year=2022, parameter_code='88101'): downloads a non-empty zip",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()

    amadeus::download_aqs(
      resolution_temporal = "daily",
      parameter_code = "88101",
      year = 2022,
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )

    zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(zips), 0)
    testthat::expect_true(all(file.info(zips)$size > 0))
  }
)
