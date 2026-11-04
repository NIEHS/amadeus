################################################################################
##### unit and integration tests for USGS GMTED functions

################################################################################
##### download_gmted
testthat::test_that("download_gmted with download_data function", {
  withr::local_package("httr2")

  directory_to_save <- paste0(tempdir(), "/gmted_download_test/")

  # Test that download_data creates proper structure with download = FALSE
  testthat::expect_warning(
    download_data(
      dataset_name = "gmted",
      statistic = "Breakline Emphasis",
      resolution = "7.5 arc-seconds",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      unzip = FALSE,
      remove_zip = FALSE,
      download = FALSE # Don't actually download
    ),
    "download=FALSE is deprecated"
  )

  # Check directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Clean up
  unlink(directory_to_save, recursive = TRUE)
})


testthat::test_that("download_gmted remove_command deprecation warning", {
  withr::with_tempdir({
    testthat::expect_warning(
      download_gmted(
        statistic = "Breakline Emphasis",
        resolution = "7.5 arc-seconds",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_gmted mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_gmted(
          statistic = "Breakline Emphasis",
          resolution = "7.5 arc-seconds",
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
        testthat::expect_true(
          paste0("gmted_", radii[a]) %in% names(gmted_covariate)
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

################################################################################
##### download_gmted hash=FALSE and file-already-exists branches

testthat::test_that("download_gmted mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_gmted(
          statistic = "Breakline Emphasis",
          resolution = "7.5 arc-seconds",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_gmted file already exists path", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    withCallingHandlers(
      suppressWarnings(
        download_gmted(
          statistic = "Breakline Emphasis",
          resolution = "7.5 arc-seconds",
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
    testthat::expect_true(any(grepl("already exist", msgs)))
  })
})
