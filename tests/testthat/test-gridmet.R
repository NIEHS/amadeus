################################################################################
##### unit and integration tests for Climatology Group Gridmet functions

################################################################################
##### download_gridmet
testthat::test_that("download_gridmet (no errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2023
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/gridmet/")
  # run download function
  download_data(dataset_name = "gridmet",
                year = c(year_start, year_end),
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "/gridmet_",
                          year_start, "_", year_end,
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 5L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_gridmet (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year <- 2020
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/gridmet/")
  # run download function
  download_data(dataset_name = "gridmet",
                year = year,
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "/gridmet_",
                          year, "_", year,
                          "_curl_commands.txt")
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
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_gridmet (expected errors - invalid years)", {
  testthat::expect_error(
    download_data(
      dataset_name = "gridmet",
      variables = "Precipitation",
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/g/")
    )
  )
})

testthat::test_that("download_gridmet (expected errors - invalid variables)", {
  testthat::expect_error(
    download_data(
      dataset_name = "gridmet",
      variables = "temp",
      year = c(2018, 2018),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/g/")
    )
  )
})

################################################################################
##### process_gridmet
testthat::test_that("process_gridmet", {
  withr::local_package("terra")
  variable <- "Precipitation"
  # expect function
  expect_true(
    is.function(process_gridmet)
  )
  gridmet <-
    process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(gridmet)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(gridmet)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(gridmet))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(gridmet)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(gridmet))
  )
  # expect dimensions according to levels
  expect_true(
    dim(gridmet)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    gridmet_ext <- process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = "Precipitation",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "gridmet",
          "pr"
        ),
      extent = terra::ext(gridmet)
    )
  )
})

testthat::test_that("process_gridmet (single date)", {
  withr::local_package("terra")
  variable <- "Precipitation"
  # expect function
  expect_true(
    is.function(process_gridmet)
  )
  gridmet <-
    process_gridmet(
      date = "2018-01-03",
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(gridmet)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(gridmet)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(gridmet))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(gridmet)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(gridmet))
  )
  # expect dimensions according to levels
  expect_true(
    dim(gridmet)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    gridmet_ext <- process_gridmet(
      date = "2018-01-03",
      variable = "Precipitation",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "gridmet",
          "pr"
        ),
      extent = terra::ext(gridmet)
    )
  )
})

testthat::test_that("process_gridmet_codes", {
  # gridmet
  gc1 <- process_gridmet_codes("all")
  expect_true(ncol(gc1) == 2)
  gc2 <- process_gridmet_codes("sph", invert = TRUE)
  expect_true(class(gc2) == "character")
  expect_true(nchar(gc2) > 7)
  gc3 <- process_gridmet_codes("Near-Surface Specific Humidity")
  expect_true(class(gc3) == "character")
  expect_true(nchar(gc3) < 7)
  # process_variable_codes
  expect_no_error(process_variable_codes("sph", "gridmet"))
  expect_no_error(
    process_variable_codes("Near-Surface Specific Humidity", "gridmet")
  )
  expect_error(
    process_variable_codes("error", "gridmet")
  )
})

################################################################################
##### calculate_gridmet
testthat::test_that("calculate_gridmet", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calculate_gridmet)
  )
  for (r in seq_along(radii)) {
    gridmet <-
      process_gridmet(
        date = c("2018-01-03", "2018-01-03"),
        variable = "pr",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "gridmet",
          "pr"
        )
      )
    gridmet_covariate <-
      calculate_gridmet(
        from = gridmet,
        locs = data.table::data.table(ncp),
        locs_id = "site_id",
        radius = radii[r],
        fun = "mean"
      )
    # set column names
    gridmet_covariate <- calc_setcolumns(
      from = gridmet_covariate,
      lag = 0,
      dataset = "gridmet",
      locs_id = "site_id"
    )
    # expect output is data.frame
    expect_true(
      class(gridmet_covariate) == "data.frame"
    )
    # expect 3 columns
    expect_true(
      ncol(gridmet_covariate) == 3
    )
    # expect numeric value
    expect_true(
      class(gridmet_covariate[, 3]) == "numeric"
    )
    # expect $time is class Date
    expect_true(
      "POSIXt" %in% class(gridmet_covariate$time)
    )
  }
  # with included geometry
  testthat::expect_no_error(
    gridmet_covariate_geom <- calculate_gridmet(
      from = gridmet,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(gridmet_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gridmet_covariate_geom)
  )
})
