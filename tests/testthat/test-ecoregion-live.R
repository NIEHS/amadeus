################################################################################
# Live network tests for download_ecoregion(). Mocked tests: test-ecoregion.R.
################################################################################

testthat::test_that(
  paste0(
    "download_ecoregion(unzip=FALSE): ",
    "downloads non-empty zip"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_ecoregion(
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
    "download_ecoregion(unzip=TRUE, remove_zip=FALSE): ",
    "downloads and extracts files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_ecoregion(
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_ecoregion(unzip=TRUE, remove_zip=TRUE): ",
    "keeps extracted files after cleanup"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_ecoregion(
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = TRUE,
      remove_zip = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
