################################################################################
##### unit and integration tests for NASA MERRA2 functions

make_merra2_fwi_fixture <- function(
  path,
  date = "20240811"
) {
  build_layer <- function(offset) {
    layer <- terra::rast(
      nrows = 2,
      ncols = 3,
      xmin = -180.3125,
      xmax = -178.4375,
      ymin = -58.25,
      ymax = -57.25,
      crs = "EPSG:4326"
    )
    terra::values(layer) <- seq_len(terra::ncell(layer)) + offset
    layer
  }

  fwi_layers <- terra::sds(
    build_layer(0),
    build_layer(10),
    build_layer(20),
    build_layer(30),
    build_layer(40),
    build_layer(50)
  )
  names(fwi_layers) <- c(
    "MERRA2.CORRECTED_DC",
    "MERRA2.CORRECTED_DMC",
    "MERRA2.CORRECTED_FFMC",
    "MERRA2.CORRECTED_ISI",
    "MERRA2.CORRECTED_BUI",
    "MERRA2.CORRECTED_FWI"
  )

  fixture_path <- file.path(
    path,
    paste0("FWI.MERRA2.CORRECTED.Daily.Default.", date, ".nc")
  )
  terra::writeCDF(
    fwi_layers,
    fixture_path,
    overwrite = TRUE
  )

  invisible(fixture_path)
}

################################################################################
##### helper coverage for FWI parsing

testthat::test_that("process_collection parses FWI file metadata", {
  fwi_path <- file.path(
    tempdir(),
    "FWI.MERRA2.CORRECTED.Daily.Default.20240811.nc"
  )

  testthat::expect_equal(
    process_collection(fwi_path, source = "merra2", collection = TRUE),
    "fwi"
  )
  testthat::expect_equal(
    process_collection(fwi_path, source = "MERRA2", date = TRUE),
    "20240811"
  )
  testthat::expect_equal(
    process_collection(fwi_path, source = "merra", datetime = TRUE),
    "20240811"
  )
})

testthat::test_that("process_merra2_time supports FWI daily timestamps", {
  withr::local_package("terra")

  fwi_raster <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(fwi_raster) <- 1
  terra::time(fwi_raster) <- as.POSIXct("2024-08-11 00:00:00", tz = "UTC")

  testthat::expect_equal(
    process_merra2_time(collection = "fwi", from = fwi_raster),
    "000000"
  )
})

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
##### download_merra2 FWI

testthat::test_that("download_merra2 FWI builds GlobalFWI URLs", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, token, ...) {
      captured$urls <- urls
      captured$destfiles <- destfiles
      captured$token <- token
      list(success = length(urls), failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )

  withr::with_tempdir({
    result <- suppressMessages(
      download_merra2(
        collection = "fwi",
        date = c("2024-08-11", "2024-08-12"),
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )

    expected_files <- c(
      "FWI.MERRA2.CORRECTED.Daily.Default.20240811.nc",
      "FWI.MERRA2.CORRECTED.Daily.Default.20240812.nc"
    )
    expected_urls <- paste0(
      "https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/",
      "fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/2024/",
      expected_files
    )

    testthat::expect_equal(result$success, 2)
    testthat::expect_null(captured$token)
    testthat::expect_equal(captured$urls, expected_urls)
    testthat::expect_equal(basename(captured$destfiles), expected_files)
    testthat::expect_true(all(grepl("/fwi/", captured$destfiles, fixed = TRUE)))
  })
})

testthat::test_that("download_merra2 supports mixed FWI and standard requests", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(urls, destfiles, token, ...) {
      captured$urls <- urls
      captured$destfiles <- destfiles
      captured$token <- token
      list(success = length(urls), failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  fake_nc4 <- "MERRA2_400.inst1_2d_asm_Nx.20240811.nc4"
  fake_xml <- paste0(fake_nc4, ".xml")
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
    result <- suppressMessages(
      download_merra2(
        collection = c("fwi", "inst1_2d_asm_Nx"),
        date = "2024-08-11",
        nasa_earth_data_token = "fake_token",
        directory_to_save = ".",
        acknowledgement = TRUE
      )
    )

    testthat::expect_true(is.character(captured$token))
    testthat::expect_true(nzchar(captured$token))
    testthat::expect_equal(result$success, 3)
    testthat::expect_true(any(grepl("GlobalFWI", captured$urls, fixed = TRUE)))
    testthat::expect_true(any(grepl("\\.nc4$", captured$destfiles)))
    testthat::expect_true(any(grepl("\\.xml$", captured$destfiles)))
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

testthat::test_that("process_merra2 supports FWI daily corrected files", {
  withr::local_package("terra")

  withr::with_tempdir({
    make_merra2_fwi_fixture(".")
    fwi_variables <- c("DC", "DMC", "FFMC", "ISI", "BUI", "FWI")

    for (fwi_variable in fwi_variables) {
      merra2_fwi <- process_merra2(
        date = "2024-08-11",
        variable = fwi_variable,
        path = "."
      )

      testthat::expect_true(class(merra2_fwi)[1] == "SpatRaster")
      testthat::expect_true(terra::hasValues(merra2_fwi))
      testthat::expect_equal(dim(merra2_fwi), c(2, 3, 1))
      testthat::expect_true("POSIXt" %in% class(terra::time(merra2_fwi)))
      testthat::expect_match(
        names(merra2_fwi),
        paste0("MERRA2\\.CORRECTED\\.", fwi_variable, "_20240811")
      )
      testthat::expect_false(terra::crs(merra2_fwi) == "")
    }
  })
})

testthat::test_that("process_merra2 supports multi-day and raw-name FWI requests", {
  withr::local_package("terra")

  withr::with_tempdir({
    make_merra2_fwi_fixture(".", date = "20240811")
    make_merra2_fwi_fixture(".", date = "20240812")

    merra2_fwi <- process_merra2(
      date = c("2024-08-11", "2024-08-12"),
      variable = "MERRA2.CORRECTED_FWI",
      path = "."
    )

    testthat::expect_equal(terra::nlyr(merra2_fwi), 2)
    testthat::expect_equal(
      names(merra2_fwi),
      c("MERRA2.CORRECTED.FWI_20240811", "MERRA2.CORRECTED.FWI_20240812")
    )
    testthat::expect_equal(
      as.character(as.Date(terra::time(merra2_fwi))),
      c("2024-08-11", "2024-08-12")
    )
  })
})

testthat::test_that("process_merra2 returns informative FWI errors", {
  withr::local_package("terra")

  withr::with_tempdir({
    testthat::expect_error(
      process_merra2(
        date = "2024-08-11",
        variable = "FWI",
        path = "."
      ),
      "No MERRA2 files matching the requested date"
    )

    make_merra2_fwi_fixture(".")
    testthat::expect_error(
      process_merra2(
        date = "2024-08-11",
        variable = "NOT_A_VAR",
        path = "."
      ),
      "Requested variable NOT_A_VAR was not found"
    )
  })
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
##### process_merra2 daily_agg

make_merra2_hourly_fixture <- function(path, n_hours = 3) {
  build_layer <- function(offset) {
    layer <- terra::rast(
      nrows = 2,
      ncols = 3,
      xmin = -180.3125,
      xmax = -178.4375,
      ymin = -3.25,
      ymax = -1.25,
      crs = "EPSG:4267"
    )
    terra::values(layer) <- seq_len(terra::ncell(layer)) + offset
    layer
  }

  layers <- do.call(c, lapply(seq_len(n_hours), function(i) build_layer(i * 10)))
  terra::time(layers) <- as.POSIXct(
    paste0("2018-01-01 0", seq(0, n_hours - 1), ":00:00"),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  # Write directly into path; use varname="CPT" so terra reads layer names
  # with the correct variable name prefix for process_merra2 grep matching
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  fixture_path <- file.path(path, "MERRA2_400.inst1_2d_int_Nx.20180101.nc4")
  suppressWarnings(
    terra::writeCDF(layers, fixture_path, varname = "CPT", overwrite = TRUE)
  )
  invisible(fixture_path)
}

testthat::test_that("process_merra2 daily_agg=FALSE default is unchanged", {
  withr::local_package("terra")
  merra2_default <- suppressMessages(
    process_merra2(
      date = "2018-01-01",
      variable = "CPT",
      path = testthat::test_path("..", "testdata", "merra2", "inst1_2d_int_Nx")
    )
  )
  merra2_explicit_false <- suppressMessages(
    process_merra2(
      date = "2018-01-01",
      variable = "CPT",
      path = testthat::test_path("..", "testdata", "merra2", "inst1_2d_int_Nx"),
      daily_agg = FALSE
    )
  )
  testthat::expect_equal(terra::nlyr(merra2_default), terra::nlyr(merra2_explicit_false))
  testthat::expect_equal(terra::values(merra2_default), terra::values(merra2_explicit_false))
})

testthat::test_that("process_merra2 daily_agg collapses sub-daily layers", {
  withr::local_package("terra")

  withr::with_tempdir({
    tmpdir <- getwd()
    make_merra2_hourly_fixture(tmpdir, n_hours = 3)

    merra2_sub <- suppressMessages(
      process_merra2(date = "2018-01-01", variable = "CPT", path = tmpdir)
    )
    merra2_daily_mean <- suppressMessages(
      process_merra2(date = "2018-01-01", variable = "CPT", path = tmpdir,
                     daily_agg = TRUE, fun = "mean")
    )
    merra2_daily_max <- suppressMessages(
      process_merra2(date = "2018-01-01", variable = "CPT", path = tmpdir,
                     daily_agg = TRUE, fun = "max")
    )

    # 3 sub-daily layers
    testthat::expect_equal(terra::nlyr(merra2_sub), 3)
    # Daily agg collapses to 1 layer for 2D single-variable data
    testthat::expect_equal(terra::nlyr(merra2_daily_mean), 1)
    # CRS is preserved (EPSG:4267 for standard MERRA-2)
    testthat::expect_false(terra::crs(merra2_daily_mean) == "")
    # Time is set to midnight UTC of the aggregated date
    testthat::expect_true("POSIXt" %in% class(terra::time(merra2_daily_mean)))
    testthat::expect_true(all(
      format(as.Date(terra::time(merra2_daily_mean)), "%Y%m%d") == "20180101"
    ))
    # max >= mean for aggregation across identical cells
    testthat::expect_equal(terra::nlyr(merra2_daily_max), 1)
    testthat::expect_true(all(
      terra::values(merra2_daily_max) >= terra::values(merra2_daily_mean),
      na.rm = TRUE
    ))
  })
})

testthat::test_that("process_merra2 daily_agg silently skipped for FWI", {
  withr::local_package("terra")

  withr::with_tempdir({
    tmpdir <- getwd()
    make_merra2_fwi_fixture(tmpdir)
    merra2_fwi_default <- suppressMessages(
      process_merra2(date = "2024-08-11", variable = "FWI", path = tmpdir)
    )
    merra2_fwi_agg <- suppressMessages(
      process_merra2(date = "2024-08-11", variable = "FWI", path = tmpdir,
                     daily_agg = TRUE)
    )
    # FWI is already daily; daily_agg should not change the output
    testthat::expect_equal(terra::nlyr(merra2_fwi_default), terra::nlyr(merra2_fwi_agg))
    testthat::expect_equal(terra::values(merra2_fwi_default), terra::values(merra2_fwi_agg))
  })
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

testthat::test_that("calculate_merra2 supports FWI daily corrected outputs", {
  withr::local_package("terra")
  withr::local_package("data.table")

  withr::with_tempdir({
    make_merra2_fwi_fixture(".")
    merra2_fwi <- process_merra2(
      date = "2024-08-11",
      variable = "FWI",
      path = "."
    )
    ncp <- data.frame(lon = -179.7, lat = -57.8)
    ncp$site_id <- "site-1"

    merra2_fwi_covariate <- calculate_merra2(
      from = merra2_fwi,
      locs = data.table::data.table(ncp),
      locs_id = "site_id",
      radius = 0,
      fun = "mean"
    )
    merra2_fwi_covariate <- calc_setcolumns(
      from = merra2_fwi_covariate,
      lag = 0,
      dataset = "merra2",
      locs_id = "site_id"
    )

    testthat::expect_true(class(merra2_fwi_covariate) == "data.frame")
    testthat::expect_equal(ncol(merra2_fwi_covariate), 3)
    testthat::expect_true("POSIXt" %in% class(merra2_fwi_covariate$time))
    testthat::expect_true(is.numeric(merra2_fwi_covariate[[3]]))
  })
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

################################################################################
##### calculate_merra2 fun_temporal interface

testthat::test_that("calculate_merra2 fun_temporal interface", {
  testthat::expect_true(
    "fun_temporal" %in% names(formals(calculate_merra2))
  )
  testthat::expect_null(
    formals(calculate_merra2)[["fun_temporal"]]
  )
  testthat::expect_no_error(
    amadeus::check_fun_temporal(NULL)
  )
  for (fn in c("mean", "median", "sum", "max", "min")) {
    testthat::expect_no_error(
      amadeus::check_fun_temporal(fn)
    )
  }
  testthat::expect_error(
    amadeus::check_fun_temporal("sd"),
    regexp = "fun_temporal"
  )
  testthat::expect_error(
    amadeus::check_fun_temporal(1L),
    regexp = "fun_temporal"
  )
})

################################################################################
##### calculate_merra2 time_bucket and fun_temporal wiring

testthat::test_that("calculate_merra2 time_bucket in formals", {
  testthat::expect_true(
    "time_bucket" %in% names(formals(calculate_merra2))
  )
  testthat::expect_equal(
    formals(calculate_merra2)[["time_bucket"]],
    "day"
  )
})

testthat::test_that("calculate_merra2 fun_temporal wiring aggregates rows", {
  withr::local_package("terra")
  from_rast <- terra::rast(nrows = 2, ncols = 2, vals = 5)
  terra::ext(from_rast) <- c(-80, -78, 34, 36)
  terra::crs(from_rast) <- "EPSG:4326"
  names(from_rast) <- "SO4_20200101_000000"
  locs_df <- data.frame(site_id = "A", lon = -79, lat = 35)
  fake_extracted <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXlt(
      c("2020-01-01 00:00:00", "2020-01-01 06:00:00"),
      tz = "UTC"
    ),
    so4_0 = c(4.0, 8.0)
  )
  testthat::local_mocked_bindings(
    calc_prepare_locs = function(from, locs, locs_id, radius, geom) {
      sv <- terra::vect(locs_df, geom = c("lon", "lat"), crs = "EPSG:4326")
      list(sv, data.frame(site_id = "A"))
    },
    calc_worker = function(...) fake_extracted,
    .package = "amadeus"
  )
  result_null <- suppressMessages(
    calculate_merra2(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      fun_temporal = NULL,
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(result_null), 2L)
  result_mean <- suppressMessages(
    calculate_merra2(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      fun_temporal = "mean",
      geom = FALSE
    )
  )
  testthat::expect_equal(nrow(result_mean), 1L)
  testthat::expect_equal(result_mean$so4_0, 6)
  testthat::expect_s3_class(result_mean$time, "POSIXct")
})

################################################################################
##### calculate_merra2 level-aware temporal grouping

testthat::test_that("calculate_merra2 fun_temporal level-aware grouping", {
  withr::local_package("terra")
  # "lev" in the layer name triggers merra2_level = 2 in calculate_merra2,
  # which propagates group_cols_extra = "level" to calc_summarize_temporal.
  from_rast <- terra::rast(nrows = 2, ncols = 2, vals = 5)
  terra::ext(from_rast) <- c(-80, -78, 34, 36)
  terra::crs(from_rast) <- "EPSG:4326"
  names(from_rast) <- "SO4_lev001_20200101_000000"
  locs_df <- data.frame(site_id = "A", lon = -79, lat = 35)
  # Two pressure levels (1 and 2), each with two hourly rows
  fake_extracted <- data.frame(
    site_id = c("A", "A", "A", "A"),
    time = as.POSIXlt(
      rep(c("2020-01-01 00:00:00", "2020-01-01 06:00:00"), 2),
      tz = "UTC"
    ),
    level = c("1", "1", "2", "2"),
    so4_0 = c(10.0, 20.0, 30.0, 40.0)
  )
  testthat::local_mocked_bindings(
    calc_prepare_locs = function(from, locs, locs_id, radius, geom) {
      sv <- terra::vect(locs_df, geom = c("lon", "lat"), crs = "EPSG:4326")
      list(sv, data.frame(site_id = "A"))
    },
    calc_worker = function(...) fake_extracted,
    .package = "amadeus"
  )
  result_mean <- suppressMessages(
    calculate_merra2(
      from = from_rast,
      locs = locs_df,
      locs_id = "site_id",
      radius = 0,
      fun_temporal = "mean",
      geom = FALSE
    )
  )
  # Grouping by (site_id, level, day) → 2 rows (one per level)
  testthat::expect_equal(nrow(result_mean), 2L)
  testthat::expect_true("level" %in% names(result_mean))
  lev1 <- result_mean[result_mean$level == "1", "so4_0"]
  lev2 <- result_mean[result_mean$level == "2", "so4_0"]
  testthat::expect_equal(lev1, 15)
  testthat::expect_equal(lev2, 35)
  testthat::expect_s3_class(result_mean$time, "POSIXct")
})
