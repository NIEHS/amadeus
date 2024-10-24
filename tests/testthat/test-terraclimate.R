################################################################################
##### unit and integration tests for Climatology Group TerraClimate functions

################################################################################
##### download_terraclimate
testthat::test_that("download_terraclimate (no errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2023
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/terracclimate/")
  # run download function
  download_data(dataset_name = "terraclimate",
                year = c(year_start, year_end),
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "/terraclimate_",
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

testthat::test_that("download_terraclimate (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year <- 2019
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/terraclimate/")
  # run download function
  download_data(dataset_name = "terraclimate",
                year = year,
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "/terraclimate_",
                          year, "_", year,
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

testthat::test_that("download_terraclimate (expected errors - years)", {
  testthat::expect_error(
    download_data(
      dataset_name = "terraclimate",
      variables = "Precipitation",
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/epa/")
    )
  )
})

testthat::test_that("download_terraclimate (expected errors - variables)", {
  testthat::expect_error(
    download_data(
      dataset_name = "gridmet",
      variables = "temp",
      year = c(2018, 2018),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/epa/")
    )
  )
})

################################################################################
##### process_terraclimate
testthat::test_that("process_terraclimate", {
  withr::local_package("terra")
  variable <- "ppt"
  # expect function
  expect_true(
    is.function(process_terraclimate)
  )
  terraclimate <-
    process_terraclimate(
      date = c("2018-01-01", "2018-01-01"),
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "terraclimate",
        "ppt"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(terraclimate)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(terraclimate)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(terraclimate))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(terraclimate)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(terraclimate))
  )
  # expect dimensions according to levels
  expect_true(
    dim(terraclimate)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    terraclimate_ext <- process_terraclimate(
      date = c("2018-01-01", "2018-01-01"),
      variable = "ppt",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "terraclimate",
          "ppt"
        ),
      extent = terra::ext(terraclimate)
    )
  )
})

testthat::test_that("process_terraclimate (single date)", {
  withr::local_package("terra")
  variable <- "ppt"
  # expect function
  expect_true(
    is.function(process_terraclimate)
  )
  terraclimate <-
    process_terraclimate(
      date = "2018-01-01",
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "terraclimate",
        "ppt"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(terraclimate)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(terraclimate)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(terraclimate))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(terraclimate)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(terraclimate))
  )
  # expect dimensions according to levels
  expect_true(
    dim(terraclimate)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    terraclimate_ext <- process_terraclimate(
      date = "2018-01-01",
      variable = "ppt",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "terraclimate",
          "ppt"
        ),
      extent = terra::ext(terraclimate)
    )
  )
})

testthat::test_that("process_terraclimate_codes", {
  # terraclimate
  tc1 <- process_terraclimate_codes("all")
  expect_true(ncol(tc1) == 2)
  tc2 <- process_terraclimate_codes("aet", invert = TRUE)
  expect_true(class(tc2) == "character")
  expect_true(nchar(tc2) > 7)
  tc3 <- process_terraclimate_codes("Actual Evapotranspiration")
  expect_true(class(tc3) == "character")
  expect_true(nchar(tc3) < 7)
  # process_variable_codes
  expect_no_error(process_variable_codes("aet", "terraclimate"))
  expect_no_error(
    process_variable_codes("Actual Evapotranspiration", "terraclimate")
  )
  expect_error(
    process_variable_codes("error", "terraclimate")
  )
})

################################################################################
##### calculate_terraclimate
testthat::test_that("calculate_terraclimate", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calculate_terraclimate)
  )
  for (r in seq_along(radii)) {
    terraclimate <-
      process_terraclimate(
        date = c("2018-01-01", "2018-01-01"),
        variable = "Precipitation",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "terraclimate",
          "ppt"
        )
      )
    terraclimate_covariate <-
      calculate_terraclimate(
        from = terraclimate,
        locs = data.table::data.table(ncp),
        locs_id = "site_id",
        radius = radii[r],
        fun = "mean"
      )
    # set column names
    terraclimate_covariate <- calc_setcolumns(
      from = terraclimate_covariate,
      lag = 0,
      dataset = "terraclimate",
      locs_id = "site_id"
    )
    # expect output is data.frame
    expect_true(
      class(terraclimate_covariate) == "data.frame"
    )
    # expect 3 columns
    expect_true(
      ncol(terraclimate_covariate) == 3
    )
    # expect numeric value
    expect_true(
      class(terraclimate_covariate[, 3]) == "numeric"
    )
    # expect date and time column
    expect_true(
      nchar(terraclimate_covariate$time)[1] == 6
    )
  }
  # with included geometry
  testthat::expect_no_error(
    terraclimate_covariate_geom <- calculate_terraclimate(
      from = terraclimate,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(terraclimate_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(terraclimate_covariate_geom)
  )
})
