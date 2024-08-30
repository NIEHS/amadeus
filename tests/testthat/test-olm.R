################################################################################
##### unit and integration tests for OpenLandMap functions

################################################################################
##### download_olm
testthat::test_that("download_olm", {
  withr::local_package("rstac")
  links <-
    readRDS(
      system.file("extdata", "openlandmap_assets.rds", package = "amadeus")
    )
  product <- "no2_s5p.l3.trop.tmwm"
  format <- "p50_p90_2km*.*tif"
  directory_to_save <- paste0(tempdir(), "/olm")
  acknowledgement <- TRUE
  download <- FALSE

  testthat::expect_no_error(
    download_olm(
      product = product,
      format = format,
      directory_to_save = directory_to_save,
      acknowledgement = acknowledgement,
      download = download,
      remove_command = FALSE
    )
  )

  commands_path <- paste0(
    directory_to_save,
    "/OLM_queried_",
    product,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 5)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_olm
testthat::test_that("process_olm", {
  withr::local_package("terra")
  tmwm <- testthat::test_path("..", "testdata", "openlandmap",
    paste0(
      "no2_s5p.l3.trop.tmwm.p50_p90_2km_a_20180501_",
      "20221130_go_epsg.4326_v20221219_test.tif"
    )
  )
  testthat::expect_no_error(
    olm <- process_olm(path = tmwm)
  )
  testthat::expect_s4_class(olm, "SpatRaster")
  testthat::expect_error(
    process_olm(path = 1L)
  )

  # test with cropping extent
  testthat::expect_no_error(
    olm_ext <- process_olm(path = tmwm, extent = terra::ext(olm))
  )
})
