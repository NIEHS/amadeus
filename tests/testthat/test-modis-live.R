################################################################################
# Live network tests for download_modis(). Mocked tests: test-modis.R.
################################################################################

testthat::test_that(
  paste0(
    "download_modis(product='MOD09GA', date=<single day>, extent=<NC>): ",
    "downloads non-empty file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("EARTHDATA_TOKEN")),
                      "no Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_modis(
      product = "MOD09GA",
      version = "061",
      nasa_earth_data_token = Sys.getenv("EARTHDATA_TOKEN"),
      date = c("2024-01-01", "2024-01-01"),
      extent = c(-79, 35, -78, 36),
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
    "download_modis(product='MOD11A1', date=<single day>, extent=<NC>): ",
    "downloads daily LST file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("EARTHDATA_TOKEN")),
                      "no Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_modis(
      product = "MOD11A1",
      version = "061",
      nasa_earth_data_token = Sys.getenv("EARTHDATA_TOKEN"),
      date = c("2022-01-01", "2022-01-01"),
      extent = c(-79, 35, -78, 36),
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
    "download_modis(product='MCD19A2', date=<single day>, extent=<NC>): ",
    "downloads MAIAC aerosol file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("EARTHDATA_TOKEN")),
                      "no Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_modis(
      product = "MCD19A2",
      version = "061",
      nasa_earth_data_token = Sys.getenv("EARTHDATA_TOKEN"),
      date = c("2022-01-01", "2022-01-01"),
      extent = c(-79, 35, -78, 36),
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
    "download_modis(product='MOD06_L2', date=<single day>, extent=<NC>): ",
    "downloads cloud product file"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("EARTHDATA_TOKEN")),
                      "no Earthdata token")
    dir <- withr::local_tempdir()
    amadeus::download_modis(
      product = "MOD06_L2",
      nasa_earth_data_token = Sys.getenv("EARTHDATA_TOKEN"),
      date = c("2022-01-01", "2022-01-01"),
      extent = c(-79, 35, -78, 36),
      directory_to_save = dir,
      acknowledgement = TRUE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
