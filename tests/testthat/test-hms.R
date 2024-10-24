################################################################################
##### unit and integration tests for NOAA HMS functions
# nolint start

################################################################################
##### download_hms
testthat::test_that("download_hms (no errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-08-12"
  date_end <- "2022-09-21"
  directory_to_save <- paste0(tempdir(), "/hms/")
  data_formats <- c("Shapefile", "KML")
  for (d in seq_along(data_formats)) {
    # run download function
    download_data(dataset_name = "smoke",
                  date = c(date_start, date_end),
                  data_format = data_formats[d],
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE)
    # define file path with commands
    commands_path <- paste0(download_sanitize_path(directory_to_save),
                            "hms_smoke_",
                            gsub("-", "", date_start),
                            "_",
                            gsub("-", "", date_end),
                            "_curl_commands.txt")
    # expect sub-directories to be created
    if (data_formats[d] == "Shapefile") {
      expected_folders <- 3
    } else {
      expected_folders <- 2
    }
    testthat::expect_true(
      length(
        list.files(
          directory_to_save, include.dirs = TRUE
          )
        ) == expected_folders
    )
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 10L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
    # remove temporary hms
    unlink(directory_to_save, recursive = TRUE)
  }
})

testthat::test_that("download_hms (expected errors)", {
  error_directory <- paste0(tempdir(), "/error/")
  testthat::expect_error(
    download_data(
      dataset_name = "hms",
      acknowledgement = TRUE,
      directory_to_save = error_directory,
      unzip = FALSE,
      remove_zip = TRUE
    )
  )
  unlink(error_directory, recursive = TRUE)
})

testthat::test_that("download_hms (live)", {
  # function parameters
  date <- "2018-01-01"
  directory <- paste0(tempdir(), "/hms/")
  # run download function
  download_data(
    dataset_name = "hms",
    date = c(date, date),
    directory_to_save = directory,
    acknowledgement = TRUE,
    download = TRUE,
    unzip = TRUE,
    remove_zip = FALSE,
    remove_command = FALSE
  )
  Sys.sleep(1.5)
  testthat::expect_true(
    length(list.files(directory, recursive = TRUE, include.dirs = TRUE)) == 8
  )
  commands <- list.files(directory, pattern = ".txt", full.names = TRUE)
  testthat::expect_true(
    file.exists(commands)
  )
  # remove directory
  unlink(directory, recursive = TRUE)
})

testthat::test_that("download_hms (live + single date)", {
  # function parameters
  date <- "2018-01-10"
  directory <- paste0(tempdir(), "/hms/")
  # run download function
  download_data(
    dataset_name = "hms",
    date = date,
    directory_to_save = directory,
    acknowledgement = TRUE,
    download = TRUE,
    unzip = TRUE,
    remove_zip = FALSE,
    remove_command = FALSE
  )
  Sys.sleep(1.5)
  testthat::expect_true(
    length(list.files(directory, recursive = TRUE, include.dirs = TRUE)) == 8
  )
  commands <- list.files(directory, pattern = ".txt", full.names = TRUE)
  testthat::expect_true(
    file.exists(commands)
  )
  # remove directory
  unlink(directory, recursive = TRUE)
})


################################################################################
##### process_hms
testthat::test_that("process_hms (with polygons)", {
  withr::local_package("terra")
  # expect function
  testthat::expect_true(
    is.function(process_hms)
  )
  hms <-
    process_hms(
      date = c("2022-06-10", "2022-06-13"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      )
    )
  # expect output is a SpatVector or character
  testthat::expect_true(
    methods::is(hms, "SpatVector")
  )
  # expect non-null coordinate reference system
  testthat::expect_false(
    is.null(terra::crs(hms))
  )
  # expect two columns
  testthat::expect_true(
    ncol(hms) == 2
  )
  # expect density and date column
  testthat::expect_true(
    all(c("Density", "Date") %in% names(hms))
  )
  # test with cropping extent
  testthat::expect_no_error(
    hms_ext <- process_hms(
      date = c("2022-06-10", "2022-06-11"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      ),
      extent = terra::ext(hms)
    )
  )
})

testthat::test_that("process_hms (single date)", {
  withr::local_package("terra")
  # expect function
  testthat::expect_true(
    is.function(process_hms)
  )
  hms <-
    process_hms(
      date = "2022-06-10",
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      )
    )
  # expect output is a SpatVector or character
  testthat::expect_true(
    methods::is(hms, "SpatVector")
  )
  # expect non-null coordinate reference system
  testthat::expect_false(
    is.null(terra::crs(hms))
  )
  # expect two columns
  testthat::expect_true(
    ncol(hms) == 2
  )
  # expect density and date column
  testthat::expect_true(
    all(c("Density", "Date") %in% names(hms))
  )
  # test with cropping extent
  testthat::expect_no_error(
    hms_ext <- process_hms(
      date = "2022-06-10",
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      ),
      extent = terra::ext(hms)
    )
  )
})

testthat::test_that("process_hms (absent polygons - 12/31/2018)", {
  withr::local_package("terra")
  # expect function
  testthat::expect_true(
    is.function(process_hms)
  )
  hms <-
    process_hms(
      date = c("2018-12-31", "2018-12-31"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      )
    )
  # expect character
  testthat::expect_true(is.character(hms))
})

################################################################################
##### calculate_hms
testthat::test_that("calculate_hms (no errors)", {
  withr::local_package("terra")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calculate_hms)
  )
  for (r in seq_along(radii)) {
    hms <-
      process_hms(
        date = c("2022-06-10", "2022-06-11"),
        path = testthat::test_path(
          "..",
          "testdata",
          "hms"
        )
      )
    hms_covariate <-
      calculate_hms(
        from = hms,
        locs = ncp,
        locs_id = "site_id",
        radius = radii[r],
        geom = FALSE
      )
    # set column names
    hms_covariate <- calc_setcolumns(
      from = hms_covariate,
      lag = 0,
      dataset = "hms",
      locs_id = "site_id"
    )
    # expect output is data.frame
    expect_true(
      class(hms_covariate) == "data.frame"
    )
    # expect 3 columns
    expect_true(
      ncol(hms_covariate) == 5
    )
    # expect 2 rows
    expect_true(
      nrow(hms_covariate) == 2
    )
    # expect integer for binary value
    expect_true(
      is.integer(hms_covariate[, 3])
    )
    # expect binary
    expect_true(
      all(unique(hms_covariate[, 3]) %in% c(0, 1))
    )
  }
})

testthat::test_that("calculate_hms (with geometry)", {
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms_dir <- testthat::test_path(
    "..", "testdata", "hms"
  )
  hms <-  process_hms(
    date = c("2022-06-10", "2022-06-13"),
    path = hms_dir
  )
  hms_covariate_geom <- calculate_hms(
    from = hms,
    locs = ncp,
    locs_id = "site_id",
    radius = 0,
    geom = TRUE
  )
  # with geometry will have 5 columns
  testthat::expect_equal(
    ncol(hms_covariate_geom), 5
  )
  testthat::expect_s4_class(
    hms_covariate_geom, "SpatVector"
  )
})

testthat::test_that("calculate_hms (absent polygons - 12/31/2018)", {
  withr::local_package("terra")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calculate_hms)
  )
  # expect function
  testthat::expect_true(
    is.function(process_hms)
  )
  hms <-
    process_hms(
      date = c("2018-12-31", "2018-12-31"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      )
    )
  for (r in seq_along(radii)) {
    hms_covar <- calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = radii[r],
      geom = FALSE
    )
    # data frame
    testthat::expect_true(methods::is(hms_covar, "data.frame"))
    # 5 columns
    testthat::expect_equal(ncol(hms_covar), 7)
  }
  for (r in seq_along(radii)) {
    hms_covar <- calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = radii[r],
      geom = TRUE
    )
    # SpatVector
    testthat::expect_true(methods::is(hms_covar, "SpatVector"))
    # 5 columns
    testthat::expect_equal(ncol(hms_covar), 5)
  }
})
# nolint end
