################################################################################
##### unit and integration tests for NASA GEOS-CF functions
# nolint start

################################################################################
##### download_geos
testthat::test_that("download_geos", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2019-09-09"
  date_end <- "2019-09-09"
  collections <- c("aqc_tavg_1hr_g1440x721_v1",
                   "chm_inst_1hr_g1440x721_p23")
  directory_to_save <- paste0(tempdir(), "/geos/")
  # run download function
  testthat::expect_no_error(
    download_data(dataset_name = "geos",
                  date = c(date_start, date_end),
                  collection = collections,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE)
  )
  # define file path with commands
  commands_path <- paste0(directory_to_save,
                          "geos_",
                          date_start,
                          "_",
                          date_end,
                          "_wget_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 2)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 2L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)

  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_geos (single date)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date <- "2019-09-09"
  collections <- c("aqc_tavg_1hr_g1440x721_v1",
                   "chm_inst_1hr_g1440x721_p23")
  directory_to_save <- paste0(tempdir(), "/geos/")
  # run download function
  testthat::expect_no_error(
    download_data(dataset_name = "geos",
                  date = date,
                  collection = collections,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE)
  )
  # define file path with commands
  commands_path <- paste0(directory_to_save,
                          "geos_",
                          date,
                          "_",
                          date,
                          "_wget_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 2)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 2L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)

  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_geos
testthat::test_that("process_geos (no errors)", {
  withr::local_package("terra")
  collections <- c(
    "a",
    "c"
  )
  # expect function
  testthat::expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date = c("2018-01-01", "2018-01-01"),
        variable = "O3",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "geos",
          collection
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collection
    if (collection == "a") {
      testthat::expect_true(
        dim(geos)[3] == 1
      )
    } else if (collection == "c") {
      testthat::expect_true(
        dim(geos)[3] == 5
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    geos_ext <- process_geos(
      date = c("2018-01-01", "2018-01-01"),
      variable = "O3",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "geos",
          "c"
        ),
      extent = terra::ext(geos)
    )
  )
})

testthat::test_that("process_geos (single date)", {
  withr::local_package("terra")
  collections <- c(
    "a",
    "c"
  )
  # expect function
  testthat::expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date = "2018-01-01",
        variable = "O3",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "geos",
          collection
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collection
    if (collection == "a") {
      testthat::expect_true(
        dim(geos)[3] == 1
      )
    } else if (collection == "c") {
      testthat::expect_true(
        dim(geos)[3] == 5
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    geos_ext <- process_geos(
      date = "2018-01-01",
      variable = "O3",
      path =
        testthat::test_path(
          "..",
          "testdata",
          "geos",
          "c"
        ),
      extent = terra::ext(geos)
    )
  )
})

testthat::test_that("process_geos (expected errors)", {
  # expect error without variable
  testthat::expect_error(
    process_geos()
  )
  # expect error on directory without data
  testthat::expect_error(
    process_geos(
      variable = "O3",
      path = "./empty/directory"
    )
  )
})

################################################################################
##### calculate_geos
testthat::test_that("calculate_geos", {
  withr::local_package("terra")
  withr::local_package("data.table")
  collections <- c(
    "a",
    "c"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (r in seq_along(radii)) {
      geos <-
        process_geos(
          date = c("2018-01-01", "2018-01-01"),
          variable = "O3",
          path =
          testthat::test_path(
            "..",
            "testdata",
            "geos",
            collection
          )
        )
      geos_covariate <-
        calculate_geos(
          from = geos,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      geos_covariate <- calc_setcolumns(
        from = geos_covariate,
        lag = 0,
        dataset = "geos",
        locs_id = "site_id"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(geos_covariate) == "data.frame"
      )
      # expect 4 columns
      testthat::expect_true(
        ncol(geos_covariate) == 4
      )
      # expect numeric value
      testthat::expect_true(
        class(geos_covariate[, 4]) == "numeric"
      )
      # expect $time is class POSIXt
      testthat::expect_true(
        "POSIXt" %in% class(geos_covariate$time)
      )
    }
  }
  # with included geometry terra
  testthat::expect_no_error(
    geos_covariate_terra <- calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(geos_covariate_terra), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(geos_covariate_terra)
  )

  # with included geometry sf
  testthat::expect_no_error(
    geos_covariate_sf <- calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(geos_covariate_sf), 5
  )
  testthat::expect_true(
    "sf" %in% class(geos_covariate_sf)
  )

  testthat::expect_error(
    calculate_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})
