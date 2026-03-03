################################################################################
##### unit and integration tests for PRISM functions

################################################################################
##### download_prism
testthat::test_that("download_prism (url discovery, no download)", {
  directory_to_save <- paste0(tempdir(), "/prism/")
  time <- "201005"
  element <- "ppt"
  data_type <- "ts"
  format <- "nc"

  result <- suppressWarnings(download_prism(
    time = time,
    element = element,
    data_type = data_type,
    format = format,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE
  ))
  testthat::expect_true(is.list(result))
  testthat::expect_true(grepl("^https://", result$urls))
  testthat::expect_equal(result$n_files, 1)

  # normals path (format is ignored, message expected)
  suppressWarnings(
    testthat::expect_message(
      result2 <- download_prism(
        time = "0228",
        element = "ppt",
        data_type = "normals",
        format = "asc",
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
      )
    )
  )
  testthat::expect_true(grepl("^https://", result2$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_prism deprecation warnings", {
  directory_to_save <- paste0(tempdir(), "/prism_dep/")

  testthat::expect_warning(
    download_prism(
      time = "201005",
      element = "ppt",
      data_type = "ts",
      format = "nc",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    download_prism(
      time = "201005",
      element = "ppt",
      data_type = "ts",
      format = "nc",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    ),
    regexp = "remove_command.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_prism (expected errors)", {
  directory_to_save <- paste0(tempdir(), "/prism/")

  # sol* elements not valid for ts data_type
  testthat::expect_error(
    download_prism(
      time = "202105",
      element = "soltotal",
      data_type = "ts",
      format = "nc",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_prism
testthat::test_that("process_prism", {
  # Set up test data
  withr::local_package("terra")
  path <- testthat::test_path(
    "..",
    "testdata",
    "prism",
    "PRISM_tmin_30yr_normal_4kmD1_0228_bil_test.nc"
  )
  path_dir <- testthat::test_path(
    "..",
    "testdata",
    "prism"
  )
  element <- "tmin"
  time <- "0228"

  # Call the function
  testthat::expect_no_error(result <- process_prism(path, element, time))
  testthat::expect_no_error(result2 <- process_prism(path_dir, element, time))

  # Check the return type
  testthat::expect_true(inherits(result, "SpatRaster"))
  testthat::expect_true(inherits(result2, "SpatRaster"))

  # Check the metadata
  testthat::expect_equal(
    unname(terra::metags(result)[terra::metags(result)$name == "time", 2]),
    time
  )
  testthat::expect_equal(
    unname(terra::metags(result)[terra::metags(result)$name == "element", 2]),
    element
  )

  # Set up test data
  path_bad <- "/path/to/nonexistent/folder"
  element_bad <- "invalid_element"
  time_bad <- "invalid_time"

  # Call the function and expect an error
  testthat::expect_error(process_prism(NULL, element, time))
  testthat::expect_error(
    testthat::expect_warning(
      process_prism(path_bad, element, time)
    )
  )
  testthat::expect_error(process_prism(path_dir, element_bad, time))
  testthat::expect_error(process_prism(path_dir, element, time_bad))

  # test with cropping extent
  testthat::expect_no_error(
    result_ext <- process_prism(
      path,
      element,
      time,
      extent = terra::ext(result)
    )
  )
})

testthat::test_that("calculate_prism", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")

  path <- testthat::test_path(
    "..",
    "testdata",
    "prism",
    "PRISM_tmin_30yr_normal_4kmD1_0228_bil_test.nc"
  )
  path_dir <- testthat::test_path(
    "..",
    "testdata",
    "prism"
  )
  element <- "tmin"
  time <- "0228"

  proc <- process_prism(path, element, time)
  locs <- data.frame(
    site_id = "001",
    lon = -78.90,
    lat = 35.97
  )
  locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "epsg:4326")

  testthat::expect_message(
    {
      result <- calculate_prism(proc, locs)
    },
    "Calculating PRISM covariates with 0 meters radius..."
  )
  testthat::expect_true(inherits(result, "data.frame"))
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(ncol(result), 2)
  testthat::expect_equal(result$site_id, "001")
  testthat::expect_equal(result[, 2], 0.8952, tolerance = 0.00005)

  testthat::expect_message(
    {
      result_r <- calculate_prism(proc, locs, radius = 1000)
    },
    "Calculating PRISM covariates with 1000 meters radius..."
  )

  locs_sf <- sf::st_as_sf(locs)
  testthat::expect_message(
    {
      result_r_sf <- calculate_prism(proc, locs_sf, radius = 1000)
    },
    "Calculating PRISM covariates with 1000 meters radius..."
  )


  # error cases
  testthat::expect_error(
    calculate_prism(list(), locs),
    "`from` must be a SpatRaster object."
  )
})
