################################################################################
##### unit and integration tests for USGS GMTED functions

##### download_gmted
testthat::test_that("download_gmted", {
  withr::local_package("httr")
  # function parameters
  statistics <- c("Breakline Emphasis",
                  "Standard Deviation Statistic")
  resolution <- "7.5 arc-seconds"
  directory_to_save <- paste0(tempdir(), "/gmted/")
  for (s in seq_along(statistics)) {
    # run download function
    download_data(dataset_name = "gmted",
                  statistic = statistics[s],
                  resolution = resolution,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  download = FALSE)
    # expect sub-directories to be created
    testthat::expect_true(
      length(
        list.files(
          directory_to_save, include.dirs = TRUE
        )
      ) == 3
    )
    # define file path with commands
    commands_path <- paste0(download_sanitize_path(directory_to_save),
                            "gmted_",
                            gsub(" ", "", statistics[s]),
                            "_",
                            gsub(" ", "", resolution),
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    filename <- extract_urls(commands = commands, position = 4)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)

    file.create(
      file.path(filename),
      recursive = TRUE
    )
    file.create(
      file.path(
        paste0(directory_to_save, "/data_files/test.txt")
      )
    )
    # remove file with commands after test
    # remove temporary gmted
    testthat::expect_no_error(
      download_data(dataset_name = "gmted",
                    statistic = statistics[s],
                    resolution = resolution,
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    unzip = FALSE,
                    remove_zip = TRUE,
                    remove_command = TRUE,
                    download = FALSE)
    )
    testthat::expect_true(
      dir.exists(paste0(directory_to_save, "/data_files"))
    )
    testthat::expect_equal(
      length(
        list.files(
          directory_to_save, recursive = TRUE, include.dirs = TRUE
        )
      ),
      2
    )
    unlink(directory_to_save, recursive = TRUE)
  }
})

##### process_gmted
# test GMTED ####
testthat::test_that("process_gmted (no errors)", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  # expect function
  expect_true(
    is.function(process_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      gmted <-
        process_gmted(
          variable = c(statistic, resolution),
          path =
          testthat::test_path(
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
      expect_true(
        class(gmted)[1] == "SpatRaster"
      )
      # expect values
      expect_true(
        terra::hasValues(gmted)
      )
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(gmted))
      )
      # expect lon and lat dimensions to be > 1
      expect_false(
        any(c(0, 1) %in% dim(gmted)[1:2])
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    gmted_ext <-
      process_gmted(
        variable = c("Breakline Emphasis", "7.5 arc-seconds"),
        path =
        testthat::test_path(
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
  expect_error(
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

##### calculate_gmted
testthat::test_that("calculate_gmted", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
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
            path =
            testthat::test_path(
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
        expect_true(
          class(gmted_covariate) == "data.frame"
        )
        # expect 2 columns
        expect_true(
          ncol(gmted_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(gmted_covariate[, 3]) == "numeric"
        )
      }
    }
  }
  testthat::expect_no_error(
    gmted <- process_gmted(
      variable = c("Breakline Emphasis", "7.5 arc-seconds"),
      testthat::test_path(
        "..", "testdata", "gmted", "be75_grd"
      )
    )
  )
  testthat::expect_no_error(
    gmted_geom <- calculate_gmted(
      gmted,
      ncp,
      "site_id",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(gmted_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gmted_geom)
  )
})
