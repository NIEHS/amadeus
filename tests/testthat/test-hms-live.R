################################################################################
# Live network tests for download_hms(). Mocked tests: test-hms.R.
################################################################################

# download_hms() supports smoke polygons only; there is no product argument for
# fire. KML covers the nearest valid alternate output branch.

testthat::test_that(
  paste0(
    "download_hms(data_format='Shapefile', date='2022-07-01'): ",
    "downloads smoke shapefile"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hms(
      data_format = "Shapefile",
      date = c("2022-07-01", "2022-07-01"),
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
    "download_hms(data_format='KML', date='2022-07-01'): ",
    "downloads smoke KML"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hms(
      data_format = "KML",
      date = c("2022-07-01", "2022-07-01"),
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
    "download_hms(data_format='Shapefile', date='2023-06-15'): ",
    "downloads smoke shapefile for second date"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_hms(
      data_format = "Shapefile",
      date = c("2023-06-15", "2023-06-15"),
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
