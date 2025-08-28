################################################################################
##### unit and integration tests for U.S. EPA AQS functions

################################################################################
##### download_epa
testthat::test_that("download_aqs", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2022
  resolution_temporal <- "daily"
  parameter_code <- 88101
  directory_to_save <- paste0(tempdir(), "/epa/")
  # run download function
  download_data(
    dataset_name = "aqs",
    year = c(year_start, year_end),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    unzip = FALSE,
    remove_zip = FALSE,
    download = FALSE,
    remove_command = FALSE
  )
  # expect sub-directories to be created
  testthat::expect_true(
    length(
      list.files(
        directory_to_save,
        include.dirs = TRUE
      )
    ) ==
      3
  )
  # define file path with commands
  commands_path <-
    paste0(
      download_sanitize_path(directory_to_save),
      "aqs_",
      parameter_code,
      "_",
      year_start,
      "_",
      year_end,
      "_",
      resolution_temporal,
      "_curl_commands.txt"
    )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tets
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  # remove file with commands after test
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_aqs (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year <- 2018
  resolution_temporal <- "daily"
  parameter_code <- 88101
  directory_to_save <- paste0(tempdir(), "/epa/")
  # run download function
  download_data(
    dataset_name = "aqs",
    year = year,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    unzip = FALSE,
    remove_zip = FALSE,
    download = FALSE,
    remove_command = FALSE
  )
  # expect sub-directories to be created
  testthat::expect_true(
    length(
      list.files(
        directory_to_save,
        include.dirs = TRUE
      )
    ) ==
      3
  )
  # define file path with commands
  commands_path <-
    paste0(
      download_sanitize_path(directory_to_save),
      "aqs_",
      parameter_code,
      "_",
      year,
      "_",
      year,
      "_",
      resolution_temporal,
      "_curl_commands.txt"
    )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tets
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  # remove file with commands after test
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_aqs
testthat::test_that("process_aqs", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  aqssub <- testthat::test_path(
    "..",
    "testdata",
    "aqs",
    "aqs_daily_88101_triangle.csv"
  )
  testd <- testthat::test_path(
    "..",
    "testdata",
    "aqs"
  )

  # main test
  testthat::expect_no_error(
    aqsft <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqsst <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqslt <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "terra"
    )
  )

  # expect
  testthat::expect_s4_class(aqsft, "SpatVector")
  testthat::expect_s4_class(aqsst, "SpatVector")
  testthat::expect_s4_class(aqslt, "SpatVector")

  testthat::expect_no_error(
    aqsfs <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsss <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsls <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )
  testthat::expect_s3_class(aqsfs, "sf")
  testthat::expect_s3_class(aqsss, "sf")
  testthat::expect_s3_class(aqsls, "sf")

  testthat::expect_no_error(
    aqsfd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssdd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsld <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsldd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqslddsd <- process_aqs(
      path = aqssub,
      date = "2022-02-04",
      mode = "location",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_s3_class(aqsfd, "data.table")
  testthat::expect_s3_class(aqssd, "data.table")
  testthat::expect_s3_class(aqssdd, "data.table")
  testthat::expect_s3_class(aqsld, "data.table")
  testthat::expect_s3_class(aqsldd, "data.table")
  testthat::expect_s3_class(aqslddsd, "data.table")

  testthat::expect_no_error(
    aqssf <- process_aqs(
      path = testd,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )

  tempd <- tempdir()
  testthat::expect_error(
    process_aqs(
      path = tempd,
      date = c("2022-02-04", "2022-02-28"),
      return_format = "sf"
    )
  )

  # expect
  testthat::expect_s3_class(aqssf, "sf")

  # error cases
  testthat::expect_error(
    process_aqs(testthat::test_path("../testdata", "modis"))
  )
  testthat::expect_error(
    process_aqs(path = 1L)
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = c("January", "Januar"))
  )
  testthat::expect_error(
    process_aqs(
      path = aqssub,
      date = c("2021-08-15", "2021-08-16", "2021-08-17")
    )
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = NULL)
  )
  testthat::expect_no_error(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf",
      extent = c(-79, 33, -78, 36)
    )
  )
  testthat::expect_no_error(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf",
      extent = c(-79, 33, -78, 36)
    )
  )
  testthat::expect_warning(
    process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "data.table",
      extent = c(-79, -78, 33, 36)
    ),
    "Extent is not applicable for data.table. Returning data.table..."
  )
})
