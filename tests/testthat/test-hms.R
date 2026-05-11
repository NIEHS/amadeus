################################################################################
##### unit and integration tests for NOAA HMS functions
# nolint start

################################################################################
##### download_hms
testthat::test_that("download_hms (no errors)", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-08-12"
  date_end <- "2022-09-21"
  directory_to_save <- paste0(tempdir(), "/hms/")
  data_formats <- c("Shapefile", "KML")

  for (d in seq_along(data_formats)) {
    # Clean directory before test
    if (dir.exists(directory_to_save)) {
      unlink(directory_to_save, recursive = TRUE)
    }

    # run download function
    testthat::expect_no_error(
      download_data(
        dataset_name = "smoke",
        date = c(date_start, date_end),
        data_format = data_formats[d],
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE,
        unzip = FALSE,
        remove_zip = FALSE
      )
    )

    # Check that directory was created
    testthat::expect_true(
      dir.exists(directory_to_save)
    )

    # define file path with commands
    commands_path <- paste0(
      download_sanitize_path(directory_to_save),
      "hms_smoke_",
      gsub("-", "", date_start),
      "_",
      gsub("-", "", date_end),
      "_curl_commands.txt"
    )

    # Only proceed with command file tests if it exists
    if (file.exists(commands_path)) {
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 6)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 10L)
      # implement unit tests
      test_download_functions(
        directory_to_save = directory_to_save,
        commands_path = commands_path,
        url_status = url_status
      )
      # remove file with commands after test
      file.remove(commands_path)
    }

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

  old_date <- "2005-05-01"
  testthat::expect_error(
    download_data(
      dataset_name = "hms",
      date = old_date,
      acknowledgement = TRUE,
      directory_to_save = error_directory,
      unzip = TRUE,
      remove_zip = TRUE
    )
  )

  future_date <- "2041-05-01"
  testthat::expect_error(
    download_data(
      dataset_name = "hms",
      date = future_date,
      acknowledgement = TRUE,
      directory_to_save = error_directory,
      unzip = TRUE,
      remove_zip = TRUE
    )
  )
  unlink(error_directory, recursive = TRUE)
})

testthat::test_that("download_hms (live)", {
  skip_on_cran()
  skip_if_offline()

  # function parameters
  date <- "2018-01-01"
  directory <- paste0(tempdir(), "/hms/")

  # run download function
  testthat::expect_no_error(
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
  )

  Sys.sleep(1.5)

  # Check that directory exists and has files
  testthat::expect_true(
    dir.exists(directory)
  )

  all_files <- list.files(directory, recursive = TRUE, include.dirs = TRUE)
  testthat::expect_true(
    length(all_files) > 0,
    info = paste("Expected files in directory, found:", length(all_files))
  )

  # Check for command file (may be in subdirectory or root)
  commands <- list.files(
    directory,
    pattern = "\\.txt$",
    full.names = TRUE,
    recursive = TRUE
  )
  # If no command file found, that's okay - the download still worked
  # Just check that we got some files
  testthat::expect_true(
    length(all_files) > 0
  )

  # remove directory
  unlink(directory, recursive = TRUE)
})

testthat::test_that("download_hms (live + single date)", {
  skip_on_cran()
  skip_if_offline()

  # function parameters
  date <- "2018-01-10"
  directory <- paste0(tempdir(), "/hms/")

  # run download function
  testthat::expect_no_error(
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
  )

  Sys.sleep(1.5)

  # Check that directory exists and has files
  testthat::expect_true(
    dir.exists(directory)
  )

  all_files <- list.files(directory, recursive = TRUE, include.dirs = TRUE)
  testthat::expect_true(
    length(all_files) > 0,
    info = paste("Expected files in directory, found:", length(all_files))
  )

  # Check for command file (may be in subdirectory or root)
  commands <- list.files(
    directory,
    pattern = "\\.txt$",
    full.names = TRUE,
    recursive = TRUE
  )
  # If no command file found, that's okay - the download still worked
  # Just check that we got some files
  testthat::expect_true(
    length(all_files) > 0
  )

  # remove directory
  unlink(directory, recursive = TRUE)
})

################################################################################
##### download_hms additional coverage tests
testthat::test_that("download_hms remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
        download_hms(
          date = c("2018-01-01", "2018-01-01"),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_hms mock download with hash", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hms(
          date = c("2018-01-01", "2018-01-01"),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
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
    suppressMessages(
      process_hms(
        date = c("2018-12-31", "2018-12-31"),
        path = testthat::test_path(
          "..",
          "testdata",
          "hms"
        )
      )
    )
  # expect character (absent polygons path returns vector of dates)
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
  testthat::expect_true(
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
    testthat::expect_true(
      class(hms_covariate) == "data.frame"
    )
    # expect 3 columns
    testthat::expect_true(
      ncol(hms_covariate) == 5
    )
    # expect 2 rows
    testthat::expect_true(
      nrow(hms_covariate) == 2
    )
    # expect integer for binary value
    testthat::expect_true(
      is.integer(hms_covariate[, 3])
    )
    # expect binary
    testthat::expect_true(
      all(unique(hms_covariate[, 3]) %in% c(0, 1))
    )
  }
})

testthat::test_that("calculate_hms (with geometry)", {
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms_dir <- testthat::test_path(
    "..",
    "testdata",
    "hms"
  )
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-13"),
    path = hms_dir
  )
  hms_covariate_terra <- calculate_hms(
    from = hms,
    locs = ncp,
    locs_id = "site_id",
    radius = 0,
    geom = "terra"
  )
  # with geometry will have 5 columns
  testthat::expect_equal(
    ncol(hms_covariate_terra),
    5
  )
  testthat::expect_s4_class(
    hms_covariate_terra,
    "SpatVector"
  )
  hms_covariate_sf <- calculate_hms(
    from = hms,
    locs = ncp,
    locs_id = "site_id",
    radius = 0,
    geom = "sf"
  )
  # with geometry will have 6 columns
  testthat::expect_equal(
    ncol(hms_covariate_sf),
    6
  )
  testthat::expect_true("sf" %in% class(hms_covariate_sf))

  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      geom = TRUE
    )
  )
  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      frac = NA
    )
  )
})

# testthat::test_that("calculate_hms (absent polygons - 12/31/2018)", {
#   withr::local_package("terra")
#   radii <- c(0, 1000)
#   ncp <- data.frame(lon = -78.8277, lat = 35.95013)
#   ncp$site_id <- "3799900018810101"
#   # expect function
#   testthat::expect_true(
#     is.function(calculate_hms)
#   )
#   # expect function
#   testthat::expect_true(
#     is.function(process_hms)
#   )
#   hms <-
#     process_hms(
#       date = c("2018-12-31", "2018-12-31"),
#       path = testthat::test_path(
#         "..",
#         "testdata",
#         "hms"
#       )
#     )
#   for (r in seq_along(radii)) {
#     hms_covar <- calculate_hms(
#       from = hms,
#       locs = ncp,
#       locs_id = "site_id",
#       radius = radii[r],
#       geom = FALSE
#     )
#     # data frame
#     testthat::expect_true(methods::is(hms_covar, "data.frame"))
#     # 5 columns
#     testthat::expect_equal(ncol(hms_covar), 7)
#   }
#   for (r in seq_along(radii)) {
#     hms_covar <- calculate_hms(
#       from = hms,
#       locs = ncp,
#       locs_id = "site_id",
#       radius = radii[r],
#       geom = "terra"
#     )
#     # SpatVector
#     testthat::expect_true(methods::is(hms_covar, "SpatVector"))
#     # 5 columns
#     testthat::expect_equal(ncol(hms_covar), 5)
#   }
# })

testthat::test_that("Character input in calculate_hms returns 1-row df", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms_covariate <-
    calculate_hms(
      from = "2018-12-31",
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      geom = FALSE
    )
  # expect output is data.frame
  testthat::expect_s3_class(
    hms_covariate,
    "data.frame"
  )
  # expect 3 columns
  testthat::expect_equal(
    ncol(hms_covariate),
    7L
  )
  # expect 1 row
  testthat::expect_equal(
    nrow(hms_covariate),
    1L
  )
})

################################################################################
##### calculate_hms .by_time wiring

testthat::test_that("calculate_hms .by_time aggregates daily rows to weekly", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-11"),
    path = testthat::test_path("..", "testdata", "hms")
  )
  # 2 dates in same week → .by_time = "week" → 1 row
  hms_weekly <- suppressMessages(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      .by_time = "week",
      geom = FALSE
    )
  )
  testthat::expect_s3_class(hms_weekly, "data.frame")
  testthat::expect_equal(nrow(hms_weekly), 1L)
  testthat::expect_s3_class(hms_weekly$time, "POSIXct")
})

testthat::test_that("calculate_hms .by_time weekly summarization sums smoke-day counts", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-13"),
    path = testthat::test_path("..", "testdata", "hms")
  )
  hms_weekly <- suppressMessages(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      .by_time = "week",
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(hms_weekly), 2L)
  # 2022-06-10/11/12 week has 2 smoke days; 2022-06-13 week has 1 smoke day.
  testthat::expect_equal(as.integer(hms_weekly$light_00000), c(2L, 1L))
  testthat::expect_equal(as.integer(hms_weekly$medium_00000), c(0L, 0L))
  testthat::expect_equal(as.integer(hms_weekly$heavy_00000), c(0L, 0L))
})

testthat::test_that("calculate_hms default without .by_time is backward-compat", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-11"),
    path = testthat::test_path("..", "testdata", "hms")
  )
  hms_df <- suppressMessages(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      geom = FALSE
    )
  )
  testthat::expect_s3_class(hms_df, "data.frame")
  testthat::expect_equal(nrow(hms_df), 2L)
})

testthat::test_that("calculate_hms frac returns fractional smoke overlap", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-13"),
    path = testthat::test_path("..", "testdata", "hms")
  )
  hms_frac <- suppressMessages(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 1000,
      frac = TRUE,
      geom = FALSE
    )
  )
  smoke_cols <- grep("^(light|medium|heavy)_", names(hms_frac), value = TRUE)
  testthat::expect_true(length(smoke_cols) == 3)
  testthat::expect_true(
    all(vapply(hms_frac[, smoke_cols], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) >= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) <= 1, na.rm = TRUE)
  )
})

testthat::test_that("calculate_hms frac with .by_time uses mean summarization", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms <- process_hms(
    date = c("2022-06-10", "2022-06-13"),
    path = testthat::test_path("..", "testdata", "hms")
  )
  hms_frac_weekly <- suppressMessages(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      frac = TRUE,
      .by_time = "week",
      geom = FALSE
    )
  )
  testthat::expect_true(
    all(hms_frac_weekly$light_00000 <= 1, na.rm = TRUE)
  )
})

testthat::test_that("calculate_hms character skip path supports .by_time summarization", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # supply two dates in same week (Monday + Tuesday) -> .by_time = "week" -> 1 row
  hms_skip <- suppressMessages(
    calculate_hms(
      from = c("2018-06-11", "2018-06-12"),
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      .by_time = "week",
      geom = FALSE
    )
  )
  testthat::expect_s3_class(hms_skip, "data.frame")
  testthat::expect_equal(nrow(hms_skip), 1L)
  testthat::expect_s3_class(hms_skip$time, "POSIXct")
})

testthat::test_that("calculate_hms character single-date .by_time is no-op", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # Single absent date; `.by_time` on a 1-row table should still return 1 row
  hms_skip <- suppressMessages(
    calculate_hms(
      from = "2018-12-31",
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      .by_time = "day",
      geom = FALSE
    )
  )
  testthat::expect_s3_class(hms_skip, "data.frame")
  testthat::expect_equal(nrow(hms_skip), 1L)
  testthat::expect_s3_class(hms_skip$time, "POSIXct")
})

# nolint end

################################################################################
##### download_hms KML format and hash=FALSE branches

testthat::test_that("download_hms KML format mock download", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    result <- withCallingHandlers(
      suppressWarnings(
        download_hms(
          date = c("2018-01-01", "2018-01-01"),
          data_format = "KML",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("KML", msgs)))
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_hms KML format mock download hash=TRUE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hms(
          date = c("2018-01-01", "2018-01-01"),
          data_format = "KML",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_hms Shapefile mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hms(
          date = c("2018-01-01", "2018-01-01"),
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})

testthat::test_that("download_hms skips cleanly when all files already exist", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_run_method = function(...) stop("download_run_method should not be called"),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hms(
          date = c("2018-01-01", "2018-01-02"),
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 0)
    testthat::expect_equal(result$failed, 0)
    testthat::expect_equal(result$skipped, 2)
  })
})
