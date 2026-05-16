################################################################################
# Live network tests for download_groads(). Mocked tests: test-groads.R.
################################################################################

testthat::test_that(
  paste0(
    "download_groads(data_region='Americas', data_format='Shapefile'): ",
    "downloads regional shapefile"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_groads(
      data_region = "Americas",
      data_format = "Shapefile",
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
    "download_groads(data_region='Global', data_format='Shapefile'): ",
    "downloads global geodatabase fallback"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_groads(
      data_region = "Global",
      data_format = "Shapefile",
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
    "download_groads(data_region='Europe', data_format='Geodatabase'): ",
    "downloads regional geodatabase"
  ),
  {
    skip_if_no_live_tests()
    testthat::skip_if(!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN")),
                      "no NASA Earthdata token")
    dir <- withr::local_tempdir()
amadeus::download_groads(
      data_region = "Europe",
      data_format = "Geodatabase",
      directory_to_save = dir,
      acknowledgement = TRUE,
      unzip = FALSE
    )
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    testthat::expect_gt(length(files), 0)
    testthat::expect_gt(sum(file.info(files)$size > 0), 0)
  }
)
