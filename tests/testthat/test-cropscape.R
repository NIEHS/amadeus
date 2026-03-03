################################################################################
##### unit and integration tests for CropScape functions

################################################################################
##### download_cropscape
testthat::test_that("download_cropscape (no errors - GMU)", {
  withr::local_package("httr2")
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  result <- suppressWarnings(
    download_cropscape(
      year = year,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  testthat::expect_true(grepl("^https://", result$urls))
  testthat::expect_true(grepl(as.character(year), result$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (no errors - USDA)", {
  withr::local_package("httr2")
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  result <- suppressWarnings(
    download_cropscape(
      year = year,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  testthat::expect_true(grepl("^https://", result$urls))
  testthat::expect_true(grepl(as.character(year), result$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape deprecation warnings", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/cps_dep/")

  testthat::expect_warning(
    download_cropscape(
      year = 2010,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    download_cropscape(
      year = 2010,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    ),
    regexp = "remove_command.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (expected errors)", {
  # invalid source
  testthat::expect_error(download_cropscape(year = 2020, source = "CMU"))
  # GMU year too early
  testthat::expect_error(
    download_cropscape(year = 1996, source = "GMU")
  )
  # USDA year too early
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
  withr::local_package("sf")

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

  # zero radius
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_v,
      locs_id = "site_id",
      radius = 0
    )
  )

  # sf input
  locs_s <- sf::st_as_sf(locs_v)
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_s,
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
