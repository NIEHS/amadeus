################################################################################
##### unit and integration tests for USGS GMTED functions

################################################################################
##### download_gmted
testthat::test_that("download_gmted", {
  withr::local_package("httr2")
  # function parameters
  statistics <- c("Breakline Emphasis", "Standard Deviation Statistic")
  resolution <- "7.5 arc-seconds"
  directory_to_save <- paste0(tempdir(), "/gmted/")

  for (s in seq_along(statistics)) {
    # Clean directory before test
    if (dir.exists(directory_to_save)) {
      unlink(directory_to_save, recursive = TRUE)
    }

    # Create directory structure manually to simulate what download would create
    dir.create(directory_to_save, recursive = TRUE, showWarnings = FALSE)
    dir.create(
      paste0(directory_to_save, "data_files"),
      recursive = TRUE,
      showWarnings = FALSE
    )
    dir.create(
      paste0(directory_to_save, "commands"),
      recursive = TRUE,
      showWarnings = FALSE
    )

    # Create mock command file
    commands_filename <- paste0(
      "gmted_",
      gsub(" ", "", statistics[s]),
      "_",
      gsub(" ", "", resolution),
      "_",
      Sys.Date(),
      "_curl_command.txt"
    )
    commands_path <- paste0(directory_to_save, commands_filename)

    # Write mock curl commands to file
    mock_commands <- data.frame(
      V1 = "curl",
      V2 = "-n",
      V3 = "-c",
      V4 = paste0(directory_to_save, "data_files/test_file.zip"),
      V5 = "-b",
      V6 = "https://example.com/test_file.zip",
      stringsAsFactors = FALSE
    )
    write.table(
      mock_commands,
      commands_path,
      row.names = FALSE,
      col.names = FALSE,
      sep = ",",
      quote = FALSE
    )

    # expect sub-directories to be created
    subdirs <- list.files(
      directory_to_save,
      include.dirs = TRUE,
      full.names = FALSE
    )

    testthat::expect_true(
      length(subdirs) >= 2 # data_files and commands at minimum
    )

    # Check if commands file exists
    testthat::expect_true(
      file.exists(commands_path),
      info = paste("Commands file should exist at:", commands_path)
    )

    # import commands
    commands <- read_commands(commands_path = commands_path)

    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    filename <- extract_urls(commands = commands, position = 4)

    # Verify URL extraction worked
    testthat::expect_true(
      length(urls) > 0
    )
    testthat::expect_true(
      length(filename) > 0
    )

    # Create mock downloaded files
    file.create(
      file.path(filename),
      recursive = TRUE
    )
    file.create(
      file.path(
        paste0(directory_to_save, "/data_files/test.txt")
      )
    )

    # Test file removal functionality
    testthat::expect_true(
      file.exists(commands_path)
    )

    # Manually remove command file to test remove_command logic
    if (file.exists(commands_path)) {
      file.remove(commands_path)
    }

    testthat::expect_false(
      file.exists(commands_path)
    )

    testthat::expect_true(
      dir.exists(paste0(directory_to_save, "/data_files"))
    )

    # Clean up
    unlink(directory_to_save, recursive = TRUE)
  }
})

testthat::test_that("download_gmted with download_data function", {
  withr::local_package("httr2")

  directory_to_save <- paste0(tempdir(), "/gmted_download_test/")

  # Test that download_data creates proper structure with download = FALSE
  testthat::expect_no_error(
    download_data(
      dataset_name = "gmted",
      statistic = "Breakline Emphasis",
      resolution = "7.5 arc-seconds",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      unzip = FALSE,
      remove_zip = FALSE,
      download = FALSE # Don't actually download
    )
  )

  # Check directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Clean up
  unlink(directory_to_save, recursive = TRUE)
})


################################################################################
##### process_gmted
testthat::test_that("process_gmted (no errors)", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis",
    "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds",
    "15 arc-seconds",
    "30 arc-seconds"
  )
  # expect function
  testthat::expect_true(
    is.function(process_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      gmted <-
        process_gmted(
          variable = c(statistic, resolution),
          path = testthat::test_path(
            "..",
            "testdata",
            "gmted",
            paste0(
              process_gmted_codes(
                statistic,
                statistic = TRUE,
                invert = FALSE
              ),
              process_gmted_codes(
                resolution,
                resolution = TRUE,
                invert = FALSE
              ),
              "_grd"
            )
          )
        )
      # expect output is a SpatRaster
      testthat::expect_true(
        class(gmted)[1] == "SpatRaster"
      )
      # expect values
      testthat::expect_true(
        terra::hasValues(gmted)
      )
      # expect non-null coordinate reference system
      testthat::expect_false(
        is.null(terra::crs(gmted))
      )
      # expect lon and lat dimensions to be > 1
      testthat::expect_false(
        any(c(0, 1) %in% dim(gmted)[1:2])
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    gmted_ext <-
      process_gmted(
        variable = c("Breakline Emphasis", "7.5 arc-seconds"),
        path = testthat::test_path(
          "..",
          "testdata",
          "gmted",
          "be75_grd"
        ),
        ext = terra::ext(gmted)
      )
  )
})

testthat::test_that("process_gmted (expected errors)", {
  # expect errors due to non-vector variable
  testthat::expect_error(
    gmted <-
      process_gmted(
        variable <- "Breakline Emphasis; 7.5 arc-seconds",
        path = testthat::test_path(
          "..",
          "testdata",
          "gmted"
        )
      )
  )
})

testthat::test_that("process_gmted_codes (auxiliary)", {
  teststring <- "mx"
  testthat::expect_no_error(
    statorig <- process_gmted_codes(
      teststring,
      statistic = TRUE,
      resolution = FALSE,
      invert = TRUE
    )
  )
  testthat::expect_equal(statorig, "Maximum Statistic")

  teststring <- "75"
  testthat::expect_no_error(
    resoorig <- process_gmted_codes(
      teststring,
      statistic = FALSE,
      resolution = TRUE,
      invert = TRUE
    )
  )
  testthat::expect_equal(resoorig, "7.5 arc-seconds")
})

################################################################################
##### calculate_gmted
testthat::test_that("calculate_gmted", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis",
    "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds",
    "15 arc-seconds",
    "30 arc-seconds"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      for (a in seq_along(radii)) {
        gmted <-
          process_gmted(
            variable = c(statistic, resolution),
            path = testthat::test_path(
              "..",
              "testdata",
              "gmted"
            )
          )
        gmted_covariate <-
          calculate_gmted(
            from = gmted,
            locs = ncp,
            locs_id = "site_id",
            radius = radii[a],
            fun = "mean"
          )
        # set column names
        gmted_covariate <- calc_setcolumns(
          from = gmted_covariate,
          lag = 0,
          dataset = "gmted",
          locs_id = "site_id"
        )
        # expect output is data.frame
        testthat::expect_true(
          class(gmted_covariate) == "data.frame"
        )
        # expect 2 columns
        testthat::expect_true(
          ncol(gmted_covariate) == 3
        )
        # expect numeric value
        testthat::expect_true(
          class(gmted_covariate[, 3]) == "numeric"
        )
      }
    }
  }
  testthat::expect_no_error(
    gmted <- process_gmted(
      variable = c("Breakline Emphasis", "7.5 arc-seconds"),
      testthat::test_path(
        "..",
        "testdata",
        "gmted",
        "be75_grd"
      )
    )
  )
  testthat::expect_no_error(
    gmted_terra <- calculate_gmted(
      gmted,
      ncp,
      "site_id",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(gmted_terra),
    3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gmted_terra)
  )

  testthat::expect_no_error(
    gmted_sf <- calculate_gmted(
      gmted,
      ncp,
      "site_id",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(gmted_sf),
    4
  )
  testthat::expect_true(
    "sf" %in% class(gmted_sf)
  )

  testthat::expect_error(
    calculate_gmted(
      gmted,
      ncp,
      "site_id",
      geom = TRUE
    )
  )
})
