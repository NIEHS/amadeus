################################################################################
# Live network tests for download_prism(). Mocked tests: test-prism.R.
################################################################################

testthat::test_that(
  "download_prism(element='ppt', data_type='normals_800'): downloads non-empty file",
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_prism(
      time = "202201",
      element = "ppt",
      data_type = "ts",
      format = "nc",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_true(any(file.info(files)$size > 0))
  }
)
