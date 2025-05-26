################################################################################
##### unit and integration tests for PRISM functions

################################################################################
##### download_prism
testthat::test_that("download_prism", {
  # Set up test data
  time <- seq(201005, 201012, by = 1)
  element <- c("ppt", "tmin", "tmax", "tmean", "tdmean",
               "vpdmin", "vpdmax")
  # in case of multiple test runs
  # note that PRISM download for the same data element
  # is allowed up to twice a day. IP address could be blocked
  # if the limit is exceeded
  time <- sample(time, 1)
  element <- sample(element, 1)
  data_type <- "ts"
  format <- "nc"
  directory_to_save <- paste0(tempdir(), "/prism/")
  acknowledgement <- TRUE
  download <- FALSE
  remove_command <- FALSE

  # Call the function
  download_prism(
    time = time,
    element = element,
    data_type = data_type,
    format = format,
    directory_to_save = directory_to_save,
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command
  )

  testthat::expect_message(
    download_prism(
      time = time,
      element = "ppt",
      data_type = "normals",
      format = "asc",
      directory_to_save = directory_to_save,
      acknowledgement = acknowledgement,
      download = download,
      remove_command = TRUE
    )
  )

  commands_path <- paste0(
    directory_to_save,
    "PRISM_",
    element,
    "_",
    data_type,
    "_",
    time,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)

  # Set up test data
  time <- "202105"
  element <- "soltotal"
  data_type <- "ts"
  format <- "nc"
  directory_to_save <- paste0(tempdir(), "/prism/")
  acknowledgement <- TRUE
  download <- FALSE
  remove_command <- FALSE

  # Call the function and expect an error
  testthat::expect_error(download_prism(
    time = time,
    element = element,
    data_type = data_type,
    format = format,
    directory_to_save = directory_to_save,
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command
  ))
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_prism
testthat::test_that("process_prism", {
  # Set up test data
  withr::local_package("terra")
  path <- testthat::test_path(
    "..", "testdata", "prism", "PRISM_tmin_30yr_normal_4kmD1_0228_bil_test.nc"
  )
  path_dir <- testthat::test_path(
    "..", "testdata", "prism"
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
    unname(terra::metags(result)[terra::metags(result)$name == "time", 2]), time
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
