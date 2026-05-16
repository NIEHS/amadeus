################################################################################
# Live network tests for download_geos(). Mocked tests: test-geos.R.
################################################################################

# download_geos() downloads whole collections and has no variable or FWI argument.

testthat::test_that(
  paste0(
    "download_geos(collection='aqc_tavg_1hr_g1440x721_v1', ",
    "date=<single day>): downloads aerosol collection files"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_geos(
      collection = "aqc_tavg_1hr_g1440x721_v1",
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
    "download_geos(collection='chm_tavg_1hr_g1440x721_v1', ",
    "date=<single day>): downloads chemistry collection files"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_geos(
      collection = "chm_tavg_1hr_g1440x721_v1",
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
    "download_geos(collection='met_tavg_1hr_g1440x721_x1', ",
    "date=<single day>): downloads meteorology collection files"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_geos(
      collection = "met_tavg_1hr_g1440x721_x1",
      date = c("2022-01-01", "2022-01-01"),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
