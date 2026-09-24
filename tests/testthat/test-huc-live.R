################################################################################
# Live network tests for HUC workflows. Mocked tests: test-huc.R.
################################################################################

testthat::test_that(
  paste0(
    "process_huc(id='030202011003'): ",
    "retrieves remote HUC data with hydrogeofetch"
  ),
  {
    skip_if_no_live_tests()
    skip_if_pkg_missing("hydrogeofetch")

    result <- amadeus::process_huc(id = "030202011003")

    testthat::expect_s4_class(result, "SpatVector")
    testthat::expect_gt(nrow(result), 0)
  }
)

testthat::test_that(
  paste0(
    "download_huc(region='Lower48', type='Seamless'): ",
    "downloads lower-48 seamless archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_huc(
      region = "Lower48",
      type = "Seamless",
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
    "download_huc(region='Lower48', type='OceanCatchment'): ",
    "downloads lower-48 ocean archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_huc(
      region = "Lower48",
      type = "OceanCatchment",
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
    "download_huc(region='Islands', type='Seamless'): ",
    "downloads islands seamless archive"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
amadeus::download_huc(
      region = "Islands",
      type = "Seamless",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
