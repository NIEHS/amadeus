################################################################################
##### unit and integration tests for U.S. EPA NEI functions
# nolint start

################################################################################
##### download_nei
testthat::test_that("download_nei", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")

  # run download function
  year <- c(2017L, 2020L)

  # Expect deprecation warning with download = FALSE
  testthat::expect_warning(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      year = year,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # define file path with commands
  commands_path <- paste0(
    download_sanitize_path(directory_to_save),
    "NEI_AADT_",
    paste(year, collapse = "-"),
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )

  # Only proceed if commands file exists
  if (file.exists(commands_path)) {
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <-
      httr::HEAD(urls[1])
    url_status <- url_status$status_code
    # implement unit tests
    test_download_functions(
      directory_to_save = directory_to_save,
      commands_path = commands_path,
      url_status = url_status
    )
    # remove file with commands after test
    file.remove(commands_path)
  }

  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei (live)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")

  # run download function
  year <- c(2017L, 2020L)
  testthat::expect_no_error(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = TRUE,
      year = year,
      remove_command = FALSE,
      unzip = TRUE,
      remove_zip = FALSE # NOW THIS PARAMETER EXISTS!
    )
  )

  # Check that files were downloaded
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Check for zip files if they exist
  zip_dir <- paste0(directory_to_save, "/zip_files")
  if (dir.exists(zip_dir)) {
    testthat::expect_true(
      length(list.files(zip_dir)) > 0
    )
  }

  # Check for data files if they exist
  data_dir <- paste0(directory_to_save, "/data_files")
  if (dir.exists(data_dir)) {
    testthat::expect_true(
      length(list.files(data_dir, recursive = TRUE)) > 0
    )
  }

  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei (expected errors)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  tdir <- tempdir()
  directory_to_save <- paste0(tempdir(), "/epa/")

  # run download function
  year <- c(2017L)
  testthat::expect_warning(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      year = year,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "NEI_AADT_",
    paste(year, collapse = "-"),
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )

  # Only remove if file exists
  if (file.exists(commands_path)) {
    file.remove(commands_path)
  }

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei remove_command deprecation warning", {
  withr::with_tempdir({
    testthat::expect_warning(
        download_nei(
          year = c(2017L, 2017L),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_nei mock download with hash", {
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
        download_nei(
          year = c(2017L, 2017L),
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

testthat::test_that("download_nei epa_certificate_path deprecation warning", {
  skip_on_cran()

  withr::with_tempdir({
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(...) FALSE,
      download_run_method = function(...) {
        invisible(list(success = 0, failed = 0, skipped = 2))
      },
      download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
      .package = "amadeus"
    )

    # epa_certificate_path != NULL should trigger deprecation warning
    testthat::expect_warning(
      suppressMessages(
        download_nei(
          year = 2020,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          epa_certificate_path = "/fake/cert.pem"
        )
      ),
      "epa_certificate_path.*deprecated|deprecated.*epa_certificate"
    )

    # certificate_url != default should also trigger warning
    testthat::expect_warning(
      suppressMessages(
        download_nei(
          year = 2020,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          certificate_url = "https://other.cert.url/cert.crt"
        )
      ),
      "certificate_url.*deprecated|deprecated.*certificate_url"
    )
  })
})

################################################################################
##### process_nei
testthat::test_that("process_nei", {
  withr::local_package("terra")

  path_nei <- testthat::test_path("../testdata", "nei", "")
  path_cnty <- system.file("gpkg/nc.gpkg", package = "sf")
  path_cnty <- terra::vect(path_cnty)
  path_cnty$GEOID <- path_cnty$FIPS

  testthat::expect_no_error(
    neinc <- process_nei(path = path_nei, year = 2017, county = path_cnty)
  )
  testthat::expect_s4_class(neinc, "SpatVector")

  # error cases
  testthat::expect_error(
    process_nei(testthat::test_path("../testdata", "modis"), year = 2017)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2030, county = path_cnty)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = NULL)
  )

  # Test with invalid object - create a simple numeric vector
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = 123)
  )

  testthat::expect_error(
    process_nei("./EmPtY/pAtH", year = 2020, county = path_cnty)
  )

  # Test with invalid path as county
  testthat::expect_error(
    process_nei(path_nei, county = "./invalid_path.shp", year = 2020)
  )

  names(path_cnty)[which(names(path_cnty) == "GEOID")] <- "COUNTYID"
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = path_cnty)
  )
})

################################################################################
##### calculate_nei
testthat::test_that("calculate_nei", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(202401)

  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(ncpath)
  nc <- nc[grep("(Orange|Wake|Durham)", nc$NAME), ]

  neipath <- testthat::test_path("..", "testdata", "nei")

  testthat::expect_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )

  nc$GEOID <- nc$FIPS
  testthat::expect_no_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )
  # inspecting calculated results
  testthat::expect_true(inherits(neiras, "SpatVector"))
  testthat::expect_true(nrow(neiras) == 3)

  # sf case
  testthat::expect_no_error(
    neires <- process_nei(
      path = neipath,
      county = sf::st_as_sf(nc),
      year = 2017
    )
  )
  testthat::expect_true(inherits(neires, "SpatVector"))
  testthat::expect_true(nrow(neires) == 3)

  # error cases
  testthat::expect_error(
    process_nei(neipath, year = 2017)
  )

  # Test with invalid county parameter (already tested above)
  testthat::expect_error(
    process_nei(neipath, county = 123, year = 2017)
  )

  testthat::expect_error(
    process_nei(neipath, nc, year = 2083)
  )

  # calculate_nei
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncp <- terra::vect(ncp, geom = c("lon", "lat"), crs = "EPSG:4326")
  nc <- terra::project(nc, "EPSG:4326")

  testthat::expect_no_error(
    neicalced <- calculate_nei(
      locs = ncp,
      from = neiras
    )
  )
  testthat::expect_true(any(grepl("NEI", names(neicalced))))
  testthat::expect_equal(neicalced$TRF_NEINP_0_00000, 1579079, tolerance = 1)

  # with geometry terra
  testthat::expect_no_error(
    neicalced_terra <- calculate_nei(
      locs = ncp,
      from = neiras,
      geom = "terra"
    )
  )
  testthat::expect_s4_class(neicalced_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    neicalced_sf <- calculate_nei(
      locs = ncp,
      from = neiras,
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(neicalced_sf))

  # more error cases - test with matrix instead of proper spatial object
  testthat::expect_error(
    calculate_nei(
      locs = matrix(c(1, 2, 3, 4), nrow = 2),
      from = neiras
    )
  )

  testthat::expect_error(
    calculate_nei(
      locs = ncp,
      from = neiras,
      geom = TRUE
    )
  )
})
# nolint end

################################################################################
##### download_nei all-files-exist and hash=FALSE branches

testthat::test_that("download_nei all files already exist path", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    withCallingHandlers(
      suppressWarnings(
        download_nei(
          year = c(2017, 2020),
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

testthat::test_that("download_nei mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_nei(
          year = c(2017, 2020),
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

################################################################################
##### download_nei epa_certificate_path deprecation and unzip paths

testthat::test_that("download_nei epa_certificate_path deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_nei(
          year = c(2017, 2020),
          epa_certificate_path = "/fake/cert.pem",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      "deprecated"
    )
  })
})
