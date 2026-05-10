################################################################################
##### unit and integration tests for NASA SEDAC gRoads functions

################################################################################
##### download_groads
testthat::test_that("download_groads", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  # function parameters
  data_regions <- c("Americas", "Global")
  data_formats <- c("Geodatabase", "Shapefile")
  directory_to_save <- paste0(tempdir(), "/groads/")

  # run download function
  for (r in seq_along(data_regions)) {
    data_region <- data_regions[r]
    for (f in seq_along(data_formats)) {
      # Clean directory before test
      if (dir.exists(directory_to_save)) {
        unlink(directory_to_save, recursive = TRUE)
      }

      testthat::expect_no_error(
        download_data(
          dataset_name = "sedac_groads",
          directory_to_save = directory_to_save,
          acknowledgement = TRUE,
          data_format = data_formats[f],
          data_region = data_region,
          download = FALSE,
          unzip = FALSE,
          remove_zip = FALSE,
          remove_command = FALSE
        )
      )

      # Check that directory was created
      testthat::expect_true(
        dir.exists(directory_to_save)
      )

      # Check that subdirectories exist
      subdirs <- list.files(
        directory_to_save,
        include.dirs = TRUE,
        full.names = FALSE
      )

      testthat::expect_true(
        length(subdirs) >= 1
      )

      # define file path with commands
      commands_path <- paste0(
        download_sanitize_path(directory_to_save),
        "sedac_groads_",
        gsub(" ", "_", tolower(data_region)),
        "_",
        Sys.Date(),
        "_curl_command.txt"
      )

      # Only proceed with command file tests if it exists
      if (file.exists(commands_path)) {
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 11)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L)
        # implement unit tests
        test_download_functions(
          directory_to_save = directory_to_save,
          commands_path = commands_path,
          url_status = url_status
        )
        # remove file with commands after test
        file.remove(commands_path)
      }
    }
  }

  testthat::expect_message(
    download_data(
      dataset_name = "sedac_groads",
      data_format = "Shapefile",
      data_region = "Global",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE,
      remove_zip = FALSE,
      remove_command = TRUE
    )
  )
  # remove temporary groads
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_groads mock download with hash", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
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
        download_groads(
          data_region = "Global",
          data_format = "Shapefile",
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
##### process_groads
testthat::test_that("process_groads", {
  withr::local_package("terra")

  # main test
  testthat::expect_no_error(
    groads <- process_groads(
      path = testthat::test_path("../testdata/groads/groads_test.shp")
    )
  )
  # expect
  testthat::expect_s4_class(groads, "SpatVector")
  # error cases
  testthat::expect_error(
    process_groads(path = 1L),
    regexp = "must be a single file path to a \\.shp or \\.gdb roads file"
  )
  testthat::expect_error(
    process_groads(path = "does/not/exist.shp"),
    regexp = "`path` does not exist"
  )
  withr::with_tempdir({
    bad_path <- file.path(".", "roads.txt")
    writeLines("not a roads vector file", con = bad_path)
    testthat::expect_error(
      process_groads(path = bad_path),
      regexp = "must point to a \\.shp or \\.gdb file"
    )
  })
  testthat::expect_error(
    process_groads(path = NA_character_),
    regexp = "must be a single file path to a \\.shp or \\.gdb roads file"
  )
  # test with cropping extent
  testthat::expect_no_error(
    groads_ext <- process_groads(
      path = testthat::test_path("../testdata/groads/groads_test.shp"),
      extent = terra::ext(groads)
    )
  )
})

################################################################################
##### calculate_groads
testthat::test_that("calculate_groads", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # test data generation
  ncp <- data.frame(
    site_id = c("1", "2"),
    lon = c(-78.899, -78.643669),
    lat = c(35.8774, 35.785342),
    time = c(2022, 2022)
  )
  # ncp <- terra::vect(ncp, keepgeom = TRUE, crs = "EPSG:4326")
  path_groads <- testthat::test_path(
    "..",
    "testdata",
    "groads",
    "groads_test.shp"
  )
  groads <- terra::vect(path_groads)

  testthat::expect_no_error(
    groads_res <- calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000
    )
  )

  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 0
    )
  )

  # expect data.frame
  testthat::expect_s3_class(groads_res, "data.frame")

  # return with geometry terra
  testthat::expect_no_error(
    groads_terra <- calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(groads_terra),
    4
  )
  testthat::expect_true(
    "SpatVector" %in% class(groads_terra)
  )

  # return with geometry sf
  testthat::expect_no_error(
    groads_sf <- calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(groads_sf),
    5
  )
  testthat::expect_true(
    "sf" %in% class(groads_sf)
  )

  # sf extraction with a non-overlapping location should still return geometry
  ncp_sf <- sf::st_as_sf(
    data.frame(
      site_id = c("1", "2", "outside"),
      lon = c(-78.899, -78.643669, -10),
      lat = c(35.8774, 35.785342, 0),
      time = c(2022, 2022, 2022)
    ),
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )
  testthat::expect_no_error(
    groads_sf_partial <- calculate_groads(
      from = groads,
      locs = ncp_sf,
      locs_id = "site_id",
      radius = 5000,
      geom = "sf"
    )
  )
  testthat::expect_true(
    "sf" %in% class(groads_sf_partial)
  )
  testthat::expect_equal(nrow(groads_sf_partial), 3)
  testthat::expect_equal(
    groads_sf_partial$GRD_TOTAL_0_05000[groads_sf_partial$site_id == "outside"],
    0
  )

  testthat::expect_no_error(
    groads_drop <- calculate_groads(
      from = groads,
      locs = ncp_sf,
      locs_id = "site_id",
      radius = 5000,
      geom = "sf",
      drop = TRUE
    )
  )
  testthat::expect_false("outside" %in% groads_drop$site_id)
  testthat::expect_equal(nrow(groads_drop), 2)

  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = TRUE
    )
  )
  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      drop = NA
    )
  )
})

################################################################################
##### download_groads hash=FALSE and file-already-exists branches

testthat::test_that("download_groads mock download hash=FALSE", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
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
        download_groads(
          data_region = "Americas",
          data_format = "Shapefile",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_null(result)
  })
})

testthat::test_that("download_groads file already exists path", {
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
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
        download_groads(
          data_region = "Americas",
          data_format = "Shapefile",
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
    testthat::expect_true(any(grepl("already exists", msgs)))
  })
})

################################################################################
##### download_groads: NASA EarthData token authentication

testthat::test_that("download_groads has nasa_earth_data_token parameter", {
  params <- names(formals(download_groads))
  testthat::expect_true("nasa_earth_data_token" %in% params)
  testthat::expect_null(formals(download_groads)$nasa_earth_data_token)
})

testthat::test_that("download_groads passes token to download_run_method", {
  captured_token <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, token = NULL, ...) {
      captured_token <<- token
      writeLines("ok", destfiles)
      invisible(list(success = 1, failed = 0))
    },
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    get_token = function(token, env_var) {
      if (is.null(token)) "mock-token" else token
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_groads(
        data_region = "Americas",
        data_format = "Geodatabase",
        nasa_earth_data_token = "test-token-abc",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        hash = FALSE
      )
    ))
  })
  testthat::expect_equal(captured_token, "test-token-abc")
})

testthat::test_that("download_groads reads token from env var via get_token", {
  get_token_called_with <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, token = NULL, ...) {
      writeLines("ok", destfiles)
      invisible(list(success = 1, failed = 0))
    },
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    get_token = function(token, env_var) {
      get_token_called_with <<- list(token = token, env_var = env_var)
      "mock-env-token"
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_groads(
        data_region = "Americas",
        data_format = "Geodatabase",
        nasa_earth_data_token = NULL,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE,
        hash = FALSE
      )
    ))
  })
  testthat::expect_equal(
    get_token_called_with$env_var,
    "NASA_EARTHDATA_TOKEN"
  )
})
