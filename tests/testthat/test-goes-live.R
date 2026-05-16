################################################################################
# Live network tests for download_goes(). Mocked tests: test-goes.R.
################################################################################

testthat::test_that(
  paste0(
    "download_goes(satellite='16', product='ADP-C', date=<single>): ",
    "downloads GOES-16 ADP-C files"
  ),
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
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_goes(satellite='16', product='ADP-F', date=<single>): ",
    "downloads GOES-16 ADP-F files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "16",
      product = "ADP-F",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)

testthat::test_that(
  paste0(
    "download_goes(satellite='18', product='ADP-C', date=<single>): ",
    "downloads GOES-18 ADP-C files"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_goes(
      date = c("2024-01-01", "2024-01-01"),
      satellite = "18",
      product = "ADP-C",
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
