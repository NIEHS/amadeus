################################################################################
##### unit and integration tests for NASA SEDAC population functions

################################################################################
##### download_sedac_population
testthat::test_that("download_sedac_population", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c("2020", "all")
  data_formats <- c("GeoTIFF", "ASCII")
  data_resolutions <- cbind(c("30 second"),
                            c("30_sec"))
  directory_to_save <- paste0(tempdir(), "/pop/")
  for (f in seq_along(data_formats)) {
    data_format <- data_formats[f]
    for (y in seq_along(years)) {
      year <- years[y]
      for (r in seq_len(nrow(data_resolutions))) {
        # run download function
        download_data(dataset_name = "sedac_population",
                      year = year,
                      data_format = data_format,
                      data_resolution = data_resolutions[r, 1],
                      directory_to_save = directory_to_save,
                      acknowledgement = TRUE,
                      download = FALSE,
                      unzip = FALSE,
                      remove_zip = FALSE,
                      remove_command = FALSE)
        # expect sub-directories to be created
        testthat::expect_true(
          length(
            list.files(
              directory_to_save, include.dirs = TRUE
            )
          ) == 3
        )
        # define file path with commands
        if (year == "all") {
          year <- "totpop"
        } else {
          year <- year
        }
        if (year == "totpop" && data_resolutions[r, 2] == "30_sec") {
          resolution <- "2pt5_min"
        } else {
          resolution <- data_resolutions[r, 2]
        }
        commands_path <- paste0(download_sanitize_path(directory_to_save),
                                "sedac_population_",
                                year,
                                "_",
                                resolution,
                                "_",
                                Sys.Date(),
                                "_curl_commands.txt")
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 11)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L, method = "GET")
        # implement unit tests
        test_download_functions(directory_to_save = directory_to_save,
                                commands_path = commands_path,
                                url_status = url_status)
        # remove file with commands after test
        file.remove(commands_path)
        # remove temporary population
        unlink(directory_to_save, recursive = TRUE)
      }
    }
  }
})

testthat::test_that("download_sedac_population (coerce data types)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year <- c("all")
  data_formats <- c("GeoTIFF", "ASCII", "netCDF")
  data_resolutions <- c("30 second", "2pt5_min")
  directory_to_save <- paste0(tempdir(), "/pop/")
  for (f in seq_along(data_formats)) {
    download_data(dataset_name = "sedac_population",
                  year = year,
                  data_format = data_formats[f],
                  data_resolution = data_resolutions[1],
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  remove_command = FALSE)
    commands_path <- paste0(directory_to_save,
                            "sedac_population_",
                            "totpop",
                            "_",
                            data_resolutions[2],
                            "_",
                            Sys.Date(),
                            "_curl_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 11)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "GET")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
    unlink(directory_to_save, recursive = TRUE)
  }
})

################################################################################
##### process_sedac_population
testthat::test_that("process_sedac_population (no errors)", {
  withr::local_package("terra")
  paths <- list.files(
    testthat::test_path(
      "..",
      "testdata",
      "population"
    ),
    pattern = ".tif",
    full.names = TRUE
  )
  # expect function
  expect_true(
    is.function(process_sedac_population)
  )
  for (p in seq_along(paths)) {
    pop <-
      process_sedac_population(
        path = paths[p]
      )
    # expect output is a SpatRaster
    expect_true(
      class(pop)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(pop)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(pop))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(pop)[1:2])
    )
  }
  # test with cropping extent
  testthat::expect_no_error(
    pop_ext <- process_sedac_population(
      paths[1],
      extent = terra::ext(pop)
    )
  )
})

testthat::test_that("process_sedac_population (expect null)", {
  pop <-
    process_sedac_population(
      testthat::test_path(
        "..",
        "testdata",
        "population",
        "pLaCeHoLdEr.nc"
      )
    )
  expect_true(
    is.null(pop)
  )
})

testthat::test_that("process_sedac_codes", {
  string <- "2.5 minute"
  testthat::expect_no_error(
    code <- process_sedac_codes(string)
  )
  testthat::expect_equal(code, "2pt5_min")
})

################################################################################
##### calc_sedac_population
testthat::test_that("calc_sedac_population", {
  withr::local_package("terra")
  withr::local_package("data.table")
  paths <- list.files(testthat::test_path(
    "..", "testdata", "population"
  ), full.names = TRUE)
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_sedac_population)
  )
  for (p in seq_along(paths)) {
    path <- paths[p]
    for (r in seq_along(radii)) {
      pop <-
        process_sedac_population(
          path = paths
        )
      pop_covariate <-
        calc_sedac_population(
          from = pop,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      pop_covariate <- calc_setcolumns(
        from = pop_covariate,
        lag = 0,
        dataset = "pop",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(pop_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(pop_covariate) == 3
      )
      # expect numeric value
      expect_true(
        class(pop_covariate[, 3]) == "numeric"
      )
      # expect $time is class integer for year
      expect_true(
        "integer" %in% class(pop_covariate$time)
      )
    }
  }
  # with included geometry
  testthat::expect_no_error(
    pop_covariate_geom <- calc_sedac_population(
      from = pop,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(pop_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(pop_covariate_geom)
  )
})
