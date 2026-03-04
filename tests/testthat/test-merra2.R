################################################################################
##### unit and integration tests for NASA MERRA2 functions

################################################################################
##### download_merra2
testthat::test_that("download_merra2 (no errors)", {
  skip_on_cran()
  skip_if_offline()
  skip_if(
    Sys.getenv("NASA_EARTHDATA_TOKEN") == "",
    "NASA_EARTHDATA_TOKEN not set"
  )

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-02-14"
  date_end <- "2022-02-15"
  collections <- c("inst1_2d_asm_Nx", "inst3_3d_asm_Np")
  directory_to_save <- paste0(tempdir(), "/merra2/")

  # Test that the function runs without error (requires NASA token)
  testthat::expect_no_error(
    download_data(
      dataset_name = "merra2",
      date = c(date_start, date_end),
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 deprecation warning with download=FALSE", {
  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  date <- "2023-02-14"
  collections <- c("inst1_2d_asm_Nx")
  directory_to_save <- paste0(tempdir(), "/merra2_deprecated/")

  # Expect deprecation warning when using download = FALSE
  testthat::expect_warning(
    download_data(
      dataset_name = "merra2",
      date = date,
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 (single date)", {
  skip_on_cran()
  skip_if_offline()
  skip_if(
    Sys.getenv("NASA_EARTHDATA_TOKEN") == "",
    "NASA_EARTHDATA_TOKEN not set"
  )

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  date <- "2023-02-14"
  collections <- c("inst1_2d_asm_Nx", "inst3_3d_asm_Np")
  directory_to_save <- paste0(tempdir(), "/merra2/")

  # Test that the function runs without error (requires NASA token)
  testthat::expect_no_error(
    download_data(
      dataset_name = "merra2",
      date = date,
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 with NASA token", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # Skip if no NASA token is available
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  # function parameters
  date <- "2024-01-02"
  collections <- c("inst1_2d_int_Nx")
  directory_to_save <- paste0(tempdir(), "/merra2_token/")

  # Test download with token
  testthat::expect_no_error(
    result <- download_data(
      dataset_name = "merra2",
      date = date,
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Check that some files were processed (either downloaded or skipped)
  testthat::expect_true(
    length(list.files(directory_to_save, recursive = TRUE)) > 0
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 fails without NASA token", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # Temporarily unset NASA token
  withr::local_envvar(NASA_EARTHDATA_TOKEN = "")

  directory_to_save <- paste0(tempdir(), "/merra2_notoken/")

  # Should error when no token is available
  testthat::expect_error(
    download_data(
      dataset_name = "merra2",
      date = "2024-01-02",
      collection = "inst1_2d_int_Nx",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    ),
    "NASA_EARTHDATA_TOKEN"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_merra2 (expected errors)", {
  # expected error due to unrecognized collection
  # function parameters
  collections <- "uNrEcOgNiZeD"
  directory_to_save <- testthat::test_path("..", "testdata/", "")
  testthat::expect_error(
    download_data(
      dataset_name = "merra",
      collection = collections,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )
})

testthat::test_that("download_merra2 remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      download_merra2(
        collection = "inst1_2d_asm_Nx",
        date = c("2022-02-14", "2022-02-14"),
        nasa_earth_data_token = "fake_token",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = TRUE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_merra2 all files exist branch", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 0)
  })
})

testthat::test_that("download_merra2 hash = TRUE path", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### process_merra2
testthat::test_that("process_merra2", {
  withr::local_package("terra")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collection <- c(
    "inst1_2d_int_Nx",
    "inst3_2d_gas_Nx",
    "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx",
    "tavg1_2d_chm_Nx",
    "tavg3_3d_udt_Np" #*
  )
  variable <- c(
    "CPT",
    "AODANA",
    "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN",
    "COCL",
    "DUDTANA" #*
  )
  merra2_df <- data.frame(collection, variable)
  # expect function
  testthat::expect_true(
    is.function(process_merra2)
  )
  for (c in seq_along(merra2_df$collection)) {
    merra2 <-
      process_merra2(
        date = c("2018-01-01", "2018-01-01"),
        variable = merra2_df$variable[c],
        path = testthat::test_path(
          "..",
          "testdata",
          "merra2",
          merra2_df$collection[c]
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(merra2)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(merra2)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(merra2) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(merra2)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(merra2))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(merra2))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(merra2)
    )
    # expect 8 levels for 3 hourly data
    testthat::expect_true(
      all(dim(merra2) == c(2, 3, 1))
    )
  }
  class(merra2)
  # test with cropping extent
  testthat::expect_no_error(
    merra2_ext <- process_merra2(
      date = c("2018-01-01", "2018-01-01"),
      variable = "CPT",
      path = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "inst1_2d_int_Nx"
      ),
      extent = terra::ext(merra2)
    )
  )
})

testthat::test_that("process_merra2 (single date)", {
  withr::local_package("terra")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collection <- c(
    "inst1_2d_int_Nx",
    "inst3_2d_gas_Nx",
    "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx",
    "tavg1_2d_chm_Nx",
    "tavg3_3d_udt_Np" #*
  )
  variable <- c(
    "CPT",
    "AODANA",
    "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN",
    "COCL",
    "DUDTANA" #*
  )
  merra2_df <- data.frame(collection, variable)
  # expect function
  testthat::expect_true(
    is.function(process_merra2)
  )
  for (c in seq_along(merra2_df$collection)) {
    merra2 <-
      process_merra2(
        date = "2018-01-01",
        variable = merra2_df$variable[c],
        path = testthat::test_path(
          "..",
          "testdata",
          "merra2",
          merra2_df$collection[c]
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(merra2)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(merra2)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      terra::crs(merra2) == ""
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(merra2)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(merra2))
    )
    # expect time dimension is POSIXt for hourly
    testthat::expect_true(
      "POSIXt" %in% class(terra::time(merra2))
    )
    # expect seconds in time information
    testthat::expect_true(
      "seconds" %in% terra::timeInfo(merra2)
    )
    # expect 8 levels for 3 hourly data
    testthat::expect_true(
      all(dim(merra2) == c(2, 3, 1))
    )
  }
  class(merra2)
  # test with cropping extent
  testthat::expect_no_error(
    merra2_ext <- process_merra2(
      date = "2018-01-01",
      variable = "CPT",
      path = testthat::test_path(
        "..",
        "testdata",
        "merra2",
        "inst1_2d_int_Nx"
      ),
      extent = terra::ext(merra2)
    )
  )
})

################################################################################
##### calculate_merra2
testthat::test_that("calculate_merra2", {
  withr::local_package("terra")
  withr::local_package("data.table")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collections <- c(
    "inst1_2d_int_Nx",
    "inst3_2d_gas_Nx",
    "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx",
    "tavg1_2d_chm_Nx",
    "tavg3_3d_udt_Np" #*
  )
  variables <- c(
    "CPT",
    "AODANA",
    "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN",
    "COCL",
    "DUDTANA" #*
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_merra2)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    variable <- variables[c]
    for (r in seq_along(radii)) {
      merra2 <-
        process_merra2(
          date = c("2018-01-01", "2018-01-01"),
          variable = variable,
          path = testthat::test_path(
            "..",
            "testdata",
            "merra2",
            collection
          )
        )
      merra2_covariate <-
        calculate_merra2(
          from = merra2,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      merra2_covariate <- calc_setcolumns(
        from = merra2_covariate,
        lag = 0,
        dataset = "merra2",
        locs_id = "site_id"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(merra2_covariate) == "data.frame"
      )
      if (grepl("lev", names(merra2)[1])) {
        # expect 4 columns
        testthat::expect_true(
          ncol(merra2_covariate) == 4
        )
        # expect numeric value
        testthat::expect_true(
          class(merra2_covariate[, 4]) == "numeric"
        )
      } else {
        # expect 3 columns
        testthat::expect_true(
          ncol(merra2_covariate) == 3
        )
        # expect numeric value
        testthat::expect_true(
          class(merra2_covariate[, 3]) == "numeric"
        )
      }
      # expect $time is class Date
      testthat::expect_true(
        "POSIXt" %in% class(merra2_covariate$time)
      )
    }
  }
  # with included geometry terra
  testthat::expect_no_error(
    merra2_covariate_terra <- calculate_merra2(
      from = merra2,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(merra2_covariate_terra),
    4
  )
  testthat::expect_true(
    "SpatVector" %in% class(merra2_covariate_terra)
  )

  # with included geometry sf
  testthat::expect_no_error(
    merra2_covariate_sf <- calculate_merra2(
      from = merra2,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(merra2_covariate_sf),
    5
  )
  testthat::expect_true(
    "sf" %in% class(merra2_covariate_sf)
  )

  testthat::expect_error(
    calculate_merra2(
      from = merra2,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})

################################################################################
##### download_merra2 esdt_name_5 (goldsmr5) branch

testthat::test_that("download_merra2 esdt_name_5 branch (goldsmr5)", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) FALSE,
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_merra2(
          collection = "tavg3_3d_cld_Np",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 0)
  })
})

################################################################################
##### download_merra2 with actual download path (httr2 mock returns listing)

testthat::test_that("download_merra2 actual download path via httr2 mock", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  # The MERRA2 nc4 file name format has date at positions 28-35 of basename
  # e.g., MERRA2_400.inst1_2d_asm_Nx.20220214.nc4
  fake_nc4 <- "MERRA2_400.inst1_2d_asm_Nx.20220214.nc4"
  fake_xml <- "MERRA2_400.inst1_2d_asm_Nx.20220214.nc4.xml"
  fake_html <- sprintf(
    '<html><a href="%s">%s</a><a href="%s">%s</a></html>',
    fake_nc4, fake_nc4, fake_xml, fake_xml
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "text/html"),
          body = charToRaw(fake_html)
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})

################################################################################
##### download_merra2 req_perform error triggers warning (lines 1387-1394)

testthat::test_that("download_merra2 handles req_perform error with warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) {
      list(success = 0, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      stop("Simulated MERRA2 listing error")
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      ),
      "Failed to get directory listing"
    )
  })
})

################################################################################
##### download_merra2 download=FALSE early return (covers lines 1401-1410)

testthat::test_that("download_merra2 download=FALSE returns url list", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_hash = function(hash, dir) {
      if (isTRUE(hash)) "fakehash" else NULL
    },
    .package = "amadeus"
  )
  fake_nc4 <- "MERRA2_400.inst1_2d_asm_Nx.20220214.nc4"
  fake_html <- sprintf(
    '<html><a href="%s">%s</a></html>',
    fake_nc4, fake_nc4
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "text/html"),
          body = charToRaw(fake_html)
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp, ...) rawToChar(resp$body),
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2022-02-14", "2022-02-14"),
          nasa_earth_data_token = "fake_token",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_true(is.list(result))
    testthat::expect_true("urls" %in% names(result))
  })
})
