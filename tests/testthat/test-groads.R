################################################################################
##### unit and integration tests for NASA SEDAC gRoads functions

################################################################################
##### download_groads
testthat::test_that("download_groads(download=FALSE): previews formats", {
  # ----------------------------------------------------------------------------
  # Exercise real URL planning with a local token and no download execution.
  withr::local_envvar(c(NASA_EARTHDATA_TOKEN = "mock-token"))
  directory_to_save <- file.path(
    normalizePath(withr::local_tempdir(), winslash = "/"), "groads"
  )
  testthat::local_mocked_bindings(
    download_run_method = function(...) stop("Unexpected preview download."),
    .package = "amadeus"
  )
  cases <- data.frame(
    region = c("Americas", "Americas", "Global", "Global"),
    format = c("Geodatabase", "Shapefile", "Geodatabase", "Shapefile"),
    filename = c(
      "groads_v1_americas_gdb.zip", "groads_v1_americas_shp.zip",
      "groads_v1_global_gdb.zip", "groads_v1_global_gdb.zip"
    ),
    remote = c(
      "groads-v1-americas-gdb.zip", "groads-v1-americas-shp.zip",
      "groads-v1-global-gdb.zip", "groads-v1-global-gdb.zip"
    )
  )
  for (i in seq_len(nrow(cases))) {
    # --------------------------------------------------------------------------
    # Global shapefile requests fall back to the provider's geodatabase archive.
    testthat::expect_warning(
      result <- withVisible(suppressMessages(amadeus::download_data(
        dataset_name = "sedac_groads",
        data_region = cases$region[i],
        data_format = cases$format[i],
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        unzip = FALSE
      ))),
      "Setting download=FALSE is deprecated"
    )
    testthat::expect_false(result$visible)
    testthat::expect_identical(result$value, list(
      urls = paste0(
        "https://data.earthdata.nasa.gov/nasa-earth/",
        "human-dimensions/sedac-root/downloads/data/groads/",
        "groads-global-roads-open-access-v1/", cases$remote[i]
      ),
      destfiles = file.path(directory_to_save, "zip_files", cases$filename[i]),
      n_files = 1L
    ))
  }
  # ----------------------------------------------------------------------------
  # Previews create the directory layout but no archives or command files.
  testthat::expect_true(dir.exists(file.path(directory_to_save, "zip_files")))
  testthat::expect_true(dir.exists(file.path(directory_to_save, "data_files")))
  testthat::expect_length(list.files(directory_to_save, recursive = TRUE), 0L)
  # ----------------------------------------------------------------------------
  # Both retained deprecated arguments still issue their own warnings.
  warnings <- testthat::capture_warnings(suppressMessages(
    amadeus::download_data(
      dataset_name = "sedac_groads",
      data_format = "Shapefile",
      data_region = "Global",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE,
      remove_command = TRUE
    )
  ))
  testthat::expect_length(warnings, 2L)
  testthat::expect_match(warnings[1], "download=FALSE")
  testthat::expect_match(warnings[2], "remove_command")
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
    groads_sf_partial$GRD_TOTAL_05000[groads_sf_partial$site_id == "outside"],
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
    testthat::expect_type(result, "list")
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
    testthat::expect_true(any(grepl("already exist", msgs)))
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
