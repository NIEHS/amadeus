################################################################################
# Live network tests for download_population(). Mocked tests: test-population.R.
################################################################################

testthat::test_that(
  paste0(
    "download_population(data_resolution='60 minute', data_format='GeoTIFF', year=2020): ",
    "downloads GeoTIFF zip"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_population(
      data_resolution = "60 minute",
      data_format = "GeoTIFF",
      year = "2020",
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
    "download_population(data_resolution='2.5 minute', data_format='ASCII', year=2015): ",
    "downloads ASCII zip"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_population(
      data_resolution = "2.5 minute",
      data_format = "ASCII",
      year = "2015",
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
    "download_population(data_resolution='30 second', data_format='GeoTIFF', year='all'): ",
    "downloads all-year netCDF fallback"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_population(
      data_resolution = "30 second",
      data_format = "GeoTIFF",
      year = "all",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
