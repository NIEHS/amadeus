################################################################################
##### unit and integration tests for USGS NHD functions

################################################################################
##### download_huc
testthat::test_that("download_huc", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/huc/")
  allregions <- c("Lower48", "Islands")
  alltypes <- c("Seamless", "OceanCatchment")

  for (region in allregions) {
    for (type in alltypes) {
      testthat::expect_no_error(
        download_huc(
          region,
          type,
          directory_to_save,
          acknowledgement = TRUE,
          download = FALSE,
          unzip = FALSE
        )
      )
      commands_path <- paste0(
        directory_to_save,
        "USGS_NHD_",
        region,
        "_",
        type,
        "_",
        Sys.Date(),
        "_wget_commands.txt"
      )
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 5)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L)
      # implement unit tests
      test_download_functions(
        directory_to_save = directory_to_save,
        commands_path = commands_path,
        url_status = url_status
      )
      # remove file with commands after test
      file.remove(commands_path)
    }
  }
  testthat::expect_error(
    download_huc(
      "Lower48",
      "OceanCatchment",
      tempdir(),
      acknowledgement = TRUE,
      download = TRUE,
      unzip = TRUE
    )
  )
  unlink(directory_to_save, recursive = TRUE)
})


################################################################################
##### process_huc
testthat::test_that("process_huc", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("nhdplusTools")
  withr::local_options(list(sf_use_s2 = FALSE))
  # Set up test data
  path <- testthat::test_path(
    "..",
    "testdata",
    "huc12",
    "NHDPlus_test.gpkg"
  )

  # Call the function
  testthat::expect_error(process_huc(path))
  testthat::expect_no_error(
    result <-
      process_huc(
        path,
        layer_name = "NHDPlus_test",
        huc_level = "HUC_12",
        huc_header = "030202"
      )
  )
  testthat::expect_true(inherits(result, "SpatVector"))

  # query case
  testthat::expect_no_error(
    result <-
      process_huc(
        path,
        layer_name = "NHDPlus_test",
        huc_level = "HUC_12",
        huc_header = "030202"
      )
  )
  testthat::expect_true(inherits(result, "SpatVector"))

  testthat::expect_error(
    process_huc(
      path,
      layer_name = "HUc",
      huc_level = "HUC_12",
      huc_header = "030202"
    )
  )

  # Set up test data
  path2 <- testthat::test_path(
    "..",
    "testdata",
    "huc12"
  )

  # Call the function and expect an error
  testthat::expect_error(process_huc(path2))

  # test with cropping extent
  testthat::expect_no_error(
    huc_ext <- process_huc(
      path,
      layer_name = "NHDPlus_test",
      huc_level = "HUC_12",
      huc_header = "030202",
      extent = terra::ext(result)
    )
  )

})


################################################################################
##### calculate_huc
testthat::test_that("calculate_huc", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("nhdplusTools")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Set up test data
  path <- testthat::test_path(
    "..",
    "testdata",
    "huc12",
    "NHDPlus_test.gpkg"
  )
  huc_vect <- process_huc(
    path,
    layer_name = "NHDPlus_test",
    huc_level = "HUC_12",
    huc_header = "030202"
  )

  # faux loc
  locs_v <- data.frame(
    site_id = c("loc1", "loc2", "loc3"),
    lon = c(-77.0365, -77.0434, -77.0283),
    lat = c(38.8977, 38.9097, 38.8895)
  )
  locs_v <-
    terra::vect(locs_v, geom = c("lon", "lat"), crs = "epsg:4326")

  # runs ok
  testthat::expect_message(
    huc_df <- calculate_huc(
      huc_vect,
      locs = locs_v,
      locs_id = "site_id"
    ),
    "Calculating HUC covariates..."
  )

  # error 1 - invalid from object
  testthat::expect_error(
    calculate_huc(from = 0, locs = locs_v, locs_id = "site_id"),
    "`from` must be the output of process_huc()."
  )

})
