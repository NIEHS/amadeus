################################################################################
##### unit and integration tests for U.S. EPA NEI functions
# nolint start

################################################################################
##### download_nei
testthat::test_that("download_nei(download=FALSE): previews archives", {
  # ----------------------------------------------------------------------------
  # Verify the actual preview instead of reading obsolete shell commands.
  directory_to_save <- file.path(
    normalizePath(withr::local_tempdir(), winslash = "/"), "nei"
  )
  testthat::local_mocked_bindings(
    download_run_method = function(...) stop("Unexpected preview download."),
    .package = "amadeus"
  )
  testthat::expect_warning(
    result <- withVisible(suppressMessages(amadeus::download_data(
      dataset_name = "nei",
      year = c(2017L, 2020L),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ))),
    "Setting download=FALSE is deprecated"
  )
  # ----------------------------------------------------------------------------
  # Each selected inventory has its own URL suffix and local archive filename.
  filenames <- c(
    "2017neiApr_onroad_byregions.zip", "2020nei_onroad_byregion.zip"
  )
  testthat::expect_false(result$visible)
  testthat::expect_identical(result$value, list(
    urls = paste0("https://gaftp.epa.gov/air/nei/", c(
      "2017/data_summaries/2017v1/2017neiApr_onroad_byregions.zip",
      "2020/data_summaries/2020nei_onroad_byregion.zip"
    )),
    destfiles = file.path(directory_to_save, "zip_files", filenames),
    n_files = 2L
  ))
  # ----------------------------------------------------------------------------
  # Output directories exist, but previewing writes no archives or commands.
  testthat::expect_true(dir.exists(file.path(directory_to_save, "zip_files")))
  testthat::expect_true(dir.exists(file.path(directory_to_save, "data_files")))
  testthat::expect_length(list.files(directory_to_save, recursive = TRUE), 0L)
})

testthat::test_that("download_nei (live)", {
  skip_on_cran()
  skip_if_offline()
  # ----------------------------------------------------------------------------
  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # Check that files were downloaded
  testthat::expect_true(
    dir.exists(directory_to_save)
  )
  # ----------------------------------------------------------------------------
  # Check for zip files if they exist
  zip_dir <- paste0(directory_to_save, "/zip_files")
  if (dir.exists(zip_dir)) {
    testthat::expect_true(
      length(list.files(zip_dir)) > 0
    )
  }
  # ----------------------------------------------------------------------------
  # Check for data files if they exist
  data_dir <- paste0(directory_to_save, "/data_files")
  if (dir.exists(data_dir)) {
    testthat::expect_true(
      length(list.files(data_dir, recursive = TRUE)) > 0
    )
  }
  # ----------------------------------------------------------------------------
  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei range (live)", {
  skip_on_cran()
  skip_if_offline()
  # ----------------------------------------------------------------------------
  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")
  # ----------------------------------------------------------------------------
  # run download function
  year <- 2017:2020
  testthat::expect_warning(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = TRUE,
      year = year,
      remove_command = FALSE,
      unzip = TRUE,
      remove_zip = FALSE # NOW THIS PARAMETER EXISTS!
    ),
    regexp = "Proceeding with "
  )
  # ----------------------------------------------------------------------------
  # Check that files were downloaded
  testthat::expect_true(
    dir.exists(directory_to_save)
  )
  # ----------------------------------------------------------------------------
  # Check for zip files if they exist
  zip_dir <- paste0(directory_to_save, "/zip_files")
  if (dir.exists(zip_dir)) {
    testthat::expect_true(
      length(list.files(zip_dir)) > 0
    )
  }
  # ----------------------------------------------------------------------------
  # Check for data files if they exist
  data_dir <- paste0(directory_to_save, "/data_files")
  if (dir.exists(data_dir)) {
    testthat::expect_true(
      length(list.files(data_dir, recursive = TRUE)) > 0
    )
  }
  # ----------------------------------------------------------------------------
  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei(year): rejects unavailable inventories", {
  # ----------------------------------------------------------------------------
  # Year validation and previewing do not require provider availability.
  directory_to_save <- file.path(
    normalizePath(withr::local_tempdir(), winslash = "/"), "nei"
  )
  testthat::local_mocked_bindings(
    download_run_method = function(...) stop("Unexpected preview download."),
    .package = "amadeus"
  )
  testthat::expect_error(
    testthat::expect_warning(
      amadeus::download_data(
        dataset_name = "nei",
        year = 2000:2002,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
      ),
      "Setting download=FALSE is deprecated"
    ),
    "No available years to download."
  )
  testthat::expect_false(dir.exists(directory_to_save))
  # ----------------------------------------------------------------------------
  # A supported scalar year returns one planned archive, without a command file.
  testthat::expect_warning(
    result <- suppressMessages(amadeus::download_data(
      dataset_name = "nei",
      year = 2017L,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )),
    "Setting download=FALSE is deprecated"
  )
  testthat::expect_identical(result, list(
    urls = paste0(
      "https://gaftp.epa.gov/air/nei/2017/data_summaries/2017v1/",
      "2017neiApr_onroad_byregions.zip"
    ),
    destfiles = file.path(
      directory_to_save, "zip_files", "2017neiApr_onroad_byregions.zip"
    ),
    n_files = 1L
  ))
  testthat::expect_length(list.files(directory_to_save, recursive = TRUE), 0L)
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
  path_cnty$GEOID20 <- path_cnty$GEOID
  testthat::expect_no_error(
    process_nei(path = path_nei, year = 2017, county = path_cnty)
  )

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
##### download_nei removed certificate argument and unzip paths

testthat::test_that("download_nei(epa_certificate_path): rejects removed arg", {
  # ----------------------------------------------------------------------------
  # Removed certificate arguments are rejected at entry without a warning.
  testthat::expect_error(
    amadeus::download_nei(
      year = c(2017, 2020),
      epa_certificate_path = "/fake/cert.pem",
      directory_to_save = tempdir(),
      acknowledgement = TRUE,
      unzip = FALSE,
      hash = FALSE
    ),
    "unused argument.*epa_certificate_path"
  )
})

################################################################################
##### download_nei unzip and remove_zip paths (covers lines 3096-3114)

testthat::test_that("download_nei unzip and remove_zip path executes", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(urls, destfiles, ...) {
      for (f in destfiles) {
        dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
        tmp <- tempfile(fileext = ".txt")
        writeLines("fake content", tmp)
        utils::zip(f, tmp)
        file.remove(tmp)
      }
      list(success = length(destfiles), failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressMessages(
      download_nei(
        year = c(2017, 2020),
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = TRUE,
        remove_zip = TRUE,
        hash = FALSE
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 2)
  })
})

################################################################################
##### download_nei: scalar year URL construction fix

testthat::test_that("download_nei scalar year=2017 constructs correct URL", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      invisible(list(success = 1, failed = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_nei(
        year = 2017L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE
      )
    ))
  })
  testthat::expect_length(captured_urls, 1L)
  testthat::expect_match(
    captured_urls,
    "gaftp.epa.gov/air/nei/2017/data_summaries/2017v1/2017neiApr"
  )
  testthat::expect_no_match(captured_urls, "/2017/.*2020")
})

testthat::test_that("download_nei scalar year=2020 constructs correct URL", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      invisible(list(success = 1, failed = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_nei(
        year = 2020L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE
      )
    ))
  })
  testthat::expect_length(captured_urls, 1L)
  testthat::expect_match(
    captured_urls,
    "gaftp.epa.gov/air/nei/2020/data_summaries/2020nei_onroad"
  )
  testthat::expect_no_match(captured_urls, "/2020/.*2017")
})

testthat::test_that("download_nei vector year=c(2017,2020) constructs 2 correct URLs", {
  captured_urls <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, ...) {
      captured_urls <<- urls
      invisible(list(success = 2, failed = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(suppressMessages(
      download_nei(
        year = c(2017L, 2020L),
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        unzip = FALSE
      )
    ))
  })
  testthat::expect_length(captured_urls, 2L)
  testthat::expect_match(captured_urls[1], "/2017/.*2017neiApr")
  testthat::expect_match(captured_urls[2], "/2020/.*2020nei")
})

testthat::test_that("download_nei stops on unrecognized year", {
  testthat::expect_error(
    suppressMessages(
      download_nei(
        year = 2019L,
        directory_to_save = tempdir(),
        acknowledgement = TRUE
      )
    ),
    "NEI data is not available for year"
  )
})
