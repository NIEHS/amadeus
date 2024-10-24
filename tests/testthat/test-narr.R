################################################################################
##### unit and integration tests for NOAA NARR functions
# nolint start

################################################################################
##### download_narr
testthat::test_that("download_narr (no errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2018
  variables <- c(
    "weasd", # monolevel
    "omega", # pressure level
    "soill"  # subsurface
  )
  directory_to_save <- paste0(tempdir(), "/narr/")
  # run download function
  download_data(dataset_name = "narr",
                year = c(year_start, year_end),
                variables = variables,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE)
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_",
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

testthat::test_that("download_narr (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/narr/")
  # run download function
  testthat::expect_no_error(
    download_data(
      dataset_name = "narr",
      year = 2020,
      variables = "weasd",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )
})

testthat::test_that("download_narr (expected errors)", {
  testthat::expect_error(
    download_data(
      dataset_name = "narr",
      variables = "weasd",
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
})

testthat::test_that("narr_variable (expected errors)", {
  # expected error due to unrecognized variable name
  testthat::expect_error(
    narr_variable("uNrEcOgNiZed")
  )
})

##### process_narr
testthat::test_that("process_narr", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  expect_true(
    is.function(process_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      process_narr(
        date = c("2018-01-01", "2018-01-01"),
        variable = variables[v],
        path =
        testthat::test_path(
          "..",
          "testdata",
          "narr",
          variables[v]
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      expect_true(
        dim(narr)[3] == 1
      )
    } else if (variables[v] == "omega") {
      expect_true(
        dim(narr)[3] == 29
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    narr_ext <-
      process_narr(
        date = c("2018-01-01", "2018-01-01"),
        variable = "omega",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "narr",
          "omega"
        ),
        extent = terra::ext(narr)
      )
  )
})

##### calculate_narr
testthat::test_that("calculate_narr", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calculate_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (r in seq_along(radii)) {
      narr <-
        process_narr(
          date = c("2018-01-01", "2018-01-01"),
          variable = variable,
          path =
          testthat::test_path(
            "..",
            "testdata",
            "narr",
            variable
          )
        )
      narr_covariate <-
        calculate_narr(
          from = narr,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      narr_covariate <- calc_setcolumns(
        from = narr_covariate,
        lag = 0,
        dataset = "narr",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(narr_covariate) == "data.frame"
      )
      if (variable == "weasd") {
        # expect 3 columns (no pressure level)
        expect_true(
          ncol(narr_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(narr_covariate[, 3]) == "numeric"
        )
      } else {
        # expect 4 columns
        expect_true(
          ncol(narr_covariate) == 4
        )
        # expect numeric value
        expect_true(
          class(narr_covariate[, 4]) == "numeric"
        )
      }
      # expect $time is class Date
      expect_true(
        "POSIXct" %in% class(narr_covariate$time)
      )
    }
  }
  # with geometry
  testthat::expect_no_error(
    narr_covariate_geom <- calculate_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_geom), 4 # 4 columns because omega has pressure levels
  )
  testthat::expect_true(
    "SpatVector" %in% class(narr_covariate_geom)
  )
})
