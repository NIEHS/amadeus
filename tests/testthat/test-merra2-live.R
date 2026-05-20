################################################################################
# Live network tests for download_merra2(). Mocked tests: test-merra2.R.
################################################################################

# Short collection names such as M2T1NXSLV are ESDT names, not valid
# download_merra2() collection arguments; use the matching collection ids.

testthat::test_that(
  paste0(
    "download_merra2(collection='inst1_2d_asm_Nx', date=<single>): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_merra2(
      collection = "inst1_2d_asm_Nx",
      date = c("2024-01-01", "2024-01-01"),
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
    "download_merra2(collection='tavg1_2d_slv_Nx', date=<single>): ",
    "downloads single-level meteorology file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_merra2(
      collection = "tavg1_2d_slv_Nx",
      date = c("2022-01-01", "2022-01-01"),
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
    "download_merra2(collection='tavg1_2d_aer_Nx', date=<single>): ",
    "downloads aerosol file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_merra2(
      collection = "tavg1_2d_aer_Nx",
      date = c("2022-01-01", "2022-01-01"),
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
    "download_merra2(collection='fwi', date=<single>): ",
    "downloads corrected fire weather index file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    amadeus::download_merra2(
      collection = "fwi",
      date = c("2022-01-01", "2022-01-01"),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
