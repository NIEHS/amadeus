################################################################################
##### unit and integration tests for download_data and auxiliary functions
# nolint start

################################################################################
##### download_data
testthat::test_that("download_data (expected errors - acknowledgement)", {
  download_datasets <- c("aqs", "ecoregion", "geos", "gmted", "koppen",
                         "koppengeiger", "merra2", "merra", "narr",
                         "nlcd", "noaa", "sedac_groads",
                         "sedac_population", "groads", "population",
                         "hms", "smoke", "gridmet",
                         "terraclimate", "huc", "cropscape", "cdl", "prism")
  for (d in seq_along(download_datasets)) {
    testthat::expect_error(
      download_data(dataset_name = download_datasets[d],
                    acknowledgement = FALSE)
    )
  }
})

testthat::test_that("download_data (expected errors - directory)", {
  download_datasets <- c("aqs", "ecoregion", "geos", "gmted", "koppen",
                         "koppengeiger", "merra2", "merra", "narr",
                         "nlcd", "noaa", "sedac_groads",
                         "sedac_population", "groads", "population",
                         "hms", "smoke", "gridmet",
                         "terraclimate", "huc", "cropscape", "cdl", "prism")
  for (d in seq_along(download_datasets)) {
    testthat::expect_error(
      download_data(dataset_name = download_datasets[d],
                    acknowledgement = TRUE,
                    directory_to_save = NULL)
    )
  }
})

testthat::test_that("download_data (expected errors - temporal range)", {
  withr::with_tempdir({
    testthat::expect_error(
      download_geos(
      date = c("1900-01-01", "2018-01-01"),
      collection = "aqc_tavg_1hr_g1440x721_v1",
      acknowledgement = TRUE,
      directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_aqs(
      year = c(1900, 2022),
      acknowledgement = TRUE,
      directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_narr(
        year = c(1900, 2022),
        variables = "air.sfc",
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_merra2(
        date = c("1900-01-01", "2023-09-01"),
        collection = "inst1_2d_asm_Nx",
        directory_to_save = ".",
        acknowledgement = TRUE,
        remove_command = TRUE
      )
    )
    sink()
    testthat::expect_error(
      download_hms(
        date = c("1900-01-01", "2018-01-01"),
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )
    testthat::expect_error(
      download_gridmet(
        year = c(1900, 2022),
        variables = "Precipitation",
        acknowledgement = TRUE,
        directory_to_save = "."
      )
    )
    testthat::expect_error(
      download_terraclimate(
        year = c(1900, 2022),
        variables = "Wind Speed",
        acknowledgement = TRUE,
        directory_to_save = ".",
      )
    )
  })
})

################################################################################
##### download_epa_certificate
testthat::test_that("download_epa_certificate", {
  testthat::expect_error(
    download_epa_certificate("file.txt")
  )
  testthat::expect_no_error(
    download_epa_certificate(file.path(tempdir(), "file.pem"))
  )
  testthat::expect_no_error(
    download_epa_certificate(
      system.file("extdata/cacert_gaftp_epa.pem", package = "amadeus")
    )
  )
})

################################################################################
##### extract_urls
testthat::test_that("extract_urls", {
  commands <- paste0(
    "curl -s -o ",
    "/PATH/hms_smoke_Shapefile_20230901.zip --url ",
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
    )
  urls <- extract_urls(commands = commands)
  testthat::expect_true(
    is.null(urls)
  )
})

################################################################################
##### check_urls
testthat::test_that("check_urls returns NULL undefined size.", {
  urls <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
  )
  url_status <- check_urls(urls = urls, method = "HEAD")
  testthat::expect_true(
    is.null(url_status)
  )
})

testthat::test_that("check_urls handles size > length(urls)", {
  urls <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    "Shapefile/2023/09/hms_smoke20230901.zip"
  )
  testthat::expect_no_error(
    url_status <- check_urls(urls = urls, size = 10, method = "HEAD")
  )
  testthat::expect_length(url_status, 1)
})

################################################################################
##### download_sink
testthat::test_that("download_sink", {
  dir <- paste0(tempdir(), "/sink/")
  dir.create(dir, recursive = TRUE)
  testfile <- paste0(dir, "sink_test.txt")
  file.create(testfile)
  testthat::expect_no_error(
    download_sink(testfile)
  )
  sink()
  Sys.sleep(1.5)
  file.remove(testfile)
  unlink(dir, recursive = TRUE)
})

################################################################################
##### download_remove_zips
testthat::test_that("download_remove_zips", {
  dir <- paste0(tempdir(), "/yellowstone/")
  testfile1 <- paste0(dir, "barren/coyote.zip")
  dir.create(dirname(testfile1), recursive = TRUE)
  file.create(testfile1, recursive = TRUE)
  testfile2 <- paste0(dir, "retain/retain.txt")
  dir.create(dirname(testfile2), recursive = TRUE)
  file.create(testfile2, recursive = TRUE)
  testthat::expect_no_error(
    download_remove_zips(remove = TRUE, testfile1)
  )
  # expect only the testfile1 directory to be removed
  testthat::expect_equal(
    length(
      list.files(
        dir,
        recursive = TRUE,
        include.dirs = TRUE
      )
    ),
    2
  )
  unlink(paste0(dir, "/yellowstone"))
})
# nolint end
