################################################################################
##### unit and integration tests for CropScape functions

################################################################################
##### download_cropscape
testthat::test_that("download_cropscape (no errors - GMU)", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  # Set up test data
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  # Call the function
  testthat::expect_no_error(
    download_cropscape(
      year = year,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "CropScape_CDL_",
    "GMU",
    "_",
    year,
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
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (no errors - USDA)", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  # Set up test data
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  # Call the function
  testthat::expect_no_error(
    download_cropscape(
      year = year,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )
  commands_path <- paste0(
    directory_to_save,
    "CropScape_CDL_",
    "USDA",
    "_",
    year,
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
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (expected errors)", {
  # expected errors due to invalid years
  # Set up test data
  invalid_year <- 1996
  testthat::expect_error(download_cropscape(year = 2020, source = "CMU"))
  # Call the function and expect an error
  testthat::expect_error(
    download_cropscape(year = invalid_year, source = "GMU")
  )
  testthat::expect_error(
    download_cropscape(year = 2000, source = "USDA")
  )
})


################################################################################
##### process_cropscape
testthat::test_that("process_cropscape", {
  # Set up test data
  withr::local_package("terra")
  filepath <-
    testthat::test_path("..", "testdata/cropscape/cdl_30m_r_nc_2019_sub.tif")
  dirpath <- testthat::test_path("..", "testdata/cropscape")
  year <- 2019

  # Call the function
  testthat::expect_no_error(result <- process_cropscape(filepath, year))
  testthat::expect_no_error(process_cropscape(dirpath, year))

  # test with cropping extent
  testthat::expect_no_error(
    result_ext <- process_cropscape(
      filepath,
      year,
      extent = terra::ext(result)
    )
  )

  # Check the return type
  testthat::expect_true(inherits(result, "SpatRaster"))

  # Check the metadata
  testthat::expect_equal(
    unname(terra::metags(result)[2, 2]), # manual index to 'year' metags
    as.character(year)
  )

  # error cases
  testthat::expect_error(process_cropscape(path = 0, year = "MILLENNIUM"))
  testthat::expect_error(
    process_cropscape(path = "/home/some/path", year = "MILLENNIUM")
  )
})

## calculate_cropscape
testthat::test_that("calculate_cropscape", {
  # Set up test data
  withr::local_package("terra")
  filepath <-
    testthat::test_path("..", "testdata/cropscape/cdl_30m_r_nc_2019_sub.tif")
  dirpath <- testthat::test_path("..", "testdata/cropscape")
  year <- 2019

  # Call the function
  testthat::expect_no_error(crop_rast <- process_cropscape(filepath, year))

  # Check the return type
  testthat::expect_true(inherits(crop_rast, "SpatRaster"))

  # Calculation
  # make a faux location
  locs <- data.frame(site_id = "001", lon = -78.90, lat = 35.97)
  locs_v <- terra::vect(locs, geom = c("lon", "lat"), crs = "epsg:4326")
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_v,
      locs_id = "site_id",
      radius = 300
    )
  )

  # Check the return type
  testthat::expect_true(inherits(crop_df, "data.frame"))

  # error cases
  testthat::expect_error(
    calculate_cropscape(locs_v, locs = locs_v, locs_id = "site_id"),
    "`from` must be a SpatRaster object."
  )
})
