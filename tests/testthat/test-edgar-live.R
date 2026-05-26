################################################################################
# Live network tests for download_edgar(). Mocked tests: test-edgar.R.
################################################################################

testthat::test_that(
  paste0(
    "download_edgar(species='CO', temp_res='yearly', year_range=2022): ",
    "downloads yearly totals file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_edgar(
      species = "CO",
      version = "8.1",
      temp_res = "yearly",
      year_range = 2022,
      format = "nc",
      output = "emi",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_edgar(species='SO2', temp_res='monthly'): ",
    "downloads monthly file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_edgar(
      species = "SO2",
      version = "8.1",
      temp_res = "monthly",
      format = "nc",
      output = "emi",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_edgar(version='8.1_voc', voc='01'): ",
    "downloads VOC speciation file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_edgar(
      version = "8.1_voc",
      voc = "1",
      format = "nc",
      output = "emi",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
