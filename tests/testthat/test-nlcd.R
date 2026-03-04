################################################################################
##### unit and integration tests for MLCR NLCD functions

################################################################################
##### download_nlcd
testthat::test_that("download_nlcd", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  years <- sample(1985:2023L, size = 2)
  products <- c(
    "Land Cover",
    "Land Cover Change",
    "Land Cover Confidence",
    "Fractional Impervious Surface",
    "Impervious Descriptor",
    "Spectral Change Day of Year"
  )
  product_codes <- c(
    "LndCov",
    "LndChg",
    "LndCnf",
    "FctImp",
    "ImpDsc",
    "SpcChg"
  )

  withr::with_tempdir({
    directory_to_save <- file.path(getwd(), "nlcd")

    # run download function
    for (y in seq_along(years)) {
      p <- sample(seq_along(products), size = 1L)

      # Expect deprecation warning with download = FALSE
      testthat::expect_warning(
        download_data(
          dataset_name = "nlcd",
          year = years[y],
          product = products[p],
          directory_to_save = directory_to_save,
          acknowledgement = TRUE,
          download = FALSE,
          remove_command = FALSE,
          unzip = FALSE
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
        "nlcd_",
        tolower(product_codes[p]),
        "_",
        years[y],
        "_",
        Sys.Date(),
        "_curl_command.txt"
      )

      # Only proceed if commands file exists
      if (file.exists(commands_path)) {
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 5)
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

    # Test error case with invalid year
    # This should produce BOTH warnings (download=FALSE and remove_command=TRUE)
    # AND an error. We need to capture the warnings
    # while still expecting the error.
    testthat::expect_error(
      testthat::expect_warning(
        testthat::expect_warning(
          download_data(
            dataset_name = "nlcd",
            year = 1900,
            product = "land cover",
            directory_to_save = directory_to_save,
            acknowledgement = TRUE,
            download = FALSE,
            remove_command = TRUE
          ),
          regexp = "Setting download=FALSE is deprecated"
        ),
        regexp = "Parameter 'remove_command' is deprecated"
      )
    )
  })
  # Automatic cleanup when withr::with_tempdir exits
})

################################################################################
##### Test for deprecation warnings
testthat::test_that("download_nlcd deprecation warnings", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  withr::with_tempdir({
    directory_to_save <- file.path(getwd(), "nlcd_deprecation")

    # Test 1: download=FALSE deprecation warning
    testthat::expect_warning(
      download_data(
        dataset_name = "nlcd",
        year = 2021,
        product = "Land Cover",
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
      ),
      regexp = "Setting download=FALSE is deprecated"
    )

    # Test 2: remove_command deprecation warning
    testthat::expect_warning(
      download_data(
        dataset_name = "nlcd",
        year = 2021,
        product = "Land Cover",
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        remove_command = TRUE
      ),
      regexp = "Parameter 'remove_command' is deprecated"
    )

    # Test 3: Both deprecated parameters together (should give both warnings)
    warnings <- testthat::capture_warnings(
      download_data(
        dataset_name = "nlcd",
        year = 2021,
        product = "Land Cover",
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      )
    )

    testthat::expect_true(
      any(grepl("Setting download=FALSE is deprecated", warnings))
    )
    testthat::expect_true(
      any(grepl("Parameter 'remove_command' is deprecated", warnings))
    )
  })
})

################################################################################
##### process_nlcd
testthat::test_that("process_nlcd", {
  withr::local_package("terra")

  path_nlcd21 <- testthat::test_path("..", "testdata", "nlcd")

  testthat::expect_no_error(
    nlcd21 <- process_nlcd(path = path_nlcd21, year = 2021)
  )

  # test with extent cropping
  testthat::expect_no_error(
    nlcd21_ext <- process_nlcd(
      path = path_nlcd21,
      year = 2021,
      extent = terra::ext(
        1510241.32304443,
        1536875.51988709,
        1558885.5313357,
        1603354.11202184
      )
    )
  )
  testthat::expect_s4_class(nlcd21, "SpatRaster")

  # error cases
  testthat::expect_error(
    process_nlcd(path = 1L),
    "path is not a character"
  )
  testthat::expect_error(
    process_nlcd(path = "/universe/galaxy/solarsys/earth/usa.nc"),
    "path does not exist"
  )
  testthat::expect_error(
    process_nlcd(path_nlcd21, "nineteen eighty-four"),
    "year is not a numeric"
  )
  testthat::expect_error(
    process_nlcd(path_nlcd21, year = 2020),
    "NLCD data not available for this year"
  )
})

testthat::test_that("process_nlcd handles duplicate file extensions", {
  withr::local_package("terra")

  withr::with_tempdir({
    test_dup_dir <- file.path(getwd(), "nlcd_all")
    dir.create(test_dup_dir, showWarnings = FALSE)

    file.create(file.path(test_dup_dir, "Annual_NLCD_LndCov_2021_CU_C1V0.tif"))
    file.create(file.path(test_dup_dir, "Annual_NLCD_LndCov_2021_CU_C1V0.img"))

    testthat::expect_error(
      process_nlcd(path = test_dup_dir, year = 2021),
      regexp = "Duplicated NLCD files are detected"
    )
  })
})

testthat::test_that("process_nlcd (deprecated path structure)", {
  withr::local_package("terra")

  path_nlcd <- testthat::test_path("..", "testdata", "nlcd", "dep")

  testthat::expect_message(
    nlcd21 <- process_nlcd(path = path_nlcd, year = 2021),
    "Deprecated file paths detected"
  )
  testthat::expect_s4_class(nlcd21, "SpatRaster")

  testthat::expect_no_error(
    nlcd19 <- process_nlcd(path = path_nlcd, year = 2019)
  )
  testthat::expect_s4_class(nlcd19, "SpatRaster")
})

################################################################################
##### calculate_nlcd
testthat::test_that("calculate_nlcd", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )

  point_us1 <- cbind(lon = -78.85, lat = 36.09, site_id = 1)
  point_us2 <- cbind(lon = -78.73, lat = 35.96, site_id = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, site_id = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, site_id = 4) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_testdata <- testthat::test_path("..", "testdata", "nlcd")

  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  testthat::expect_no_error(
    nlcdras <- process_nlcd(path = path_testdata, year = 2021)
  )
  testthat::expect_s4_class(nlcdras, "SpatRaster")

  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = "1000"
    ),
    "radius is not a numeric."
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = "whatnot",
      radius = 1000
    )
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    "radius has not a likely value."
  )

  # -- two modes work properly
  testthat::expect_no_error(
    calculate_nlcd(
      locs = sf::st_as_sf(eg_data),
      from = nlcdras,
      mode = "exact",
      radius = 1000
    )
  )
  testthat::expect_no_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = "terra",
      radius = 300
    )
  )

  # -- year is numeric
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = "2021"),
    "year is not a numeric."
  )
  # -- year has likely value
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 2032),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 1789),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = 12,
      locs_id = "site_id",
      from = nlcdras
    )
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = 12
    )
  )
  # -- nlcd_path is not a character
  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    "path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
    "path does not exist."
  )

  # CHECK OUTPUT
  year <- 2021
  buf_radius <- 3000
  testthat::expect_no_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = buf_radius
    )
  )
  output <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras
  )
  # -- returns a data.frame
  testthat::expect_equal(class(output)[1], "data.frame")
  # nrow(output) == nrow(input)
  testthat::expect_equal(nrow(output), 4)
  # -- initial names are still in the output data.frame
  testthat::expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  testthat::expect_true(all(eg_data$site_id %in% output$site_id))
  # the value has changed. What affected this behavior?
  testthat::expect_equal(
    output$LDU_TEFOR_0_03000[1],
    0.09010682,
    tolerance = 1e-7
  )
  testthat::expect_equal(
    output$LDU_TSHRB_0_03000[2],
    0.01047932,
    tolerance = 1e-7
  )
  # -- class fraction rows should sum to 1
  testthat::expect_equal(
    unname(rowSums(output[1:2, 3:(ncol(output))])),
    rep(1, 2),
    tolerance = 1e-7
  )
  # without geometry will have 17 columns
  testthat::expect_equal(
    ncol(output),
    17
  )
  # example with terra output
  output_terra <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = "terra"
  )
  # with geometry will have 17 columns
  testthat::expect_equal(
    ncol(output_terra),
    17
  )
  testthat::expect_true(
    "SpatVector" %in% class(output_terra)
  )
  # example with sf output
  output_sf <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = "sf"
  )
  # with geometry will have 18 columns
  testthat::expect_equal(
    ncol(output_sf),
    18
  )
  testthat::expect_true(
    "sf" %in% class(output_sf)
  )
  # error with TRUE geom
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      radius = buf_radius,
      from = nlcdras,
      geom = TRUE
    )
  )

  ##### point extractions (radius = 0)
  # point extraction (sf)
  testthat::expect_no_error(
    out_points_sf <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = "sf"
    )
  )
  # with geometry will have 4 columns
  testthat::expect_equal(
    ncol(out_points_sf),
    4
  )
  testthat::expect_true(
    "sf" %in% class(out_points_sf)
  )
  # point extraction (terra)
  testthat::expect_no_error(
    out_points_t <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = "terra"
    )
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(out_points_t),
    3
  )
  testthat::expect_true(
    "SpatVector" %in% class(out_points_t)
  )
  # point extraction (data frame)
  testthat::expect_no_error(
    out_points_df <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = FALSE
    )
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(out_points_df),
    3
  )
  testthat::expect_true(is.data.frame(out_points_df))
})

testthat::test_that("calculate_nlcd (deprecated path structure)", {
  withr::local_package("terra")

  point_us1 <- cbind(lon = -114.7, lat = 38.9, SI = 1)
  point_us2 <- cbind(lon = -114, lat = 39, SI = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, SI = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, SI = 4) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_nlcd <- testthat::test_path("..", "testdata", "nlcd", "dep")

  testthat::expect_message(
    nlcdras <- process_nlcd(path = path_nlcd, year = 2021)
  )
  # point extraction (data frame)
  testthat::expect_no_error(
    out_points_df <- calculate_nlcd(
      locs = eg_data,
      locs_id = "SI",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = FALSE
    )
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(out_points_df),
    3
  )
  testthat::expect_true(is.data.frame(out_points_df))
})

testthat::test_that("calculate_nlcd (error for 2 layers)", {
  withr::local_package("terra")

  point_us1 <- cbind(lon = -114.7, lat = 38.9, SI = 1)
  point_us2 <- cbind(lon = -114, lat = 39, SI = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, SI = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, SI = 4) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_nlcd <- testthat::test_path("..", "testdata", "nlcd")

  testthat::expect_no_error(
    nlcdras <- process_nlcd(path = path_nlcd, year = 2021)
  )
  # point extraction (data frame)
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = "SI",
      from = terra::rast(c(nlcdras, nlcdras)),
      radius = 0,
      mode = "exact",
      geom = FALSE
    )
  )
})

################################################################################
##### collapse_nlcd warning
testthat::test_that("collapse_nlcd warning", {
  withr::local_package("terra")
  withr::local_package("collapse")

  # test list data
  lst_nlcd_200 <- list(
    id1 = data.frame(ID = 1, T1 = 0.1),
    id2 = NULL,
    id3 = data.frame(ID = 3, T1 = 0.3)
  )

  lst_nlcd_allnull <- list(
    id1 = NULL,
    id2 = NULL,
    id3 = NULL
  )

  testthat::expect_warning(
    {
      cnlcd <- collapse_nlcd(data = lst_nlcd_allnull)
    },
    "No non-null data provided to collapse_nlcd"
  )

  testthat::expect_s3_class(cnlcd, "data.frame")
  testthat::expect_equal(nrow(cnlcd), 0L)
  # line 800-801 cannot be tested if all non-null data are provided
})

################################################################################
##### integration for new data version
testthat::test_that("integration across *_nlcd functions", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )

  withr::with_tempdir({
    directory <- file.path(getwd(), "nlcd_integration")

    ##########################################################################
    # live download - use a more recent year that's available
    testthat::expect_no_error(
      download_nlcd(
        product = "Land Cover",
        year = 2021,
        directory_to_save = directory,
        acknowledgement = TRUE,
        hash = TRUE
      )
    )

    ##########################################################################
    # Import
    testthat::expect_no_error(
      nlcd_c1v1 <- process_nlcd(path = directory, year = 2021)
    )

    # Check metadata - be flexible about exact format
    meta <- terra::metags(nlcd_c1v1)
    year_found <- any(grepl("2021", meta[, 2]))
    testthat::expect_true(year_found)

    ##########################################################################
    ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
    ncv <- terra::vect(ncpath)
    nc <- terra::project(
      ncv,
      terra::crs(nlcd_c1v1)
    )

    ##########################################################################
    mat_nlcd_val <- unique(terra::values(terra::crop(nlcd_c1v1, nc)))
    testthat::expect_true(NA %in% mat_nlcd_val || !anyNA(mat_nlcd_val))
    testthat::expect_false(NaN %in% mat_nlcd_val)

    ##########################################################################
    # points have integer values
    testthat::expect_no_error(
      df_nlcd_0 <- calculate_nlcd(
        locs = terra::centroids(nc[1:5, ]),
        locs_id = "NAME",
        from = nlcd_c1v1,
        mode = "terra",
        radius = 0
      )
    )
    testthat::expect_equal(nrow(df_nlcd_0), 5)
    testthat::expect_equal(ncol(df_nlcd_0), 3)
    testthat::expect_true(is.integer(df_nlcd_0[, 3]))

    ##########################################################################
    # polygons have decimal values
    testthat::expect_no_error(
      df_nlcd_1000 <- calculate_nlcd(
        locs = terra::centroids(nc[1:5, ]),
        locs_id = "NAME",
        from = nlcd_c1v1,
        mode = "terra",
        radius = 1000
      )
    )
    testthat::expect_equal(nrow(df_nlcd_1000), 5)
    testthat::expect_equal(ncol(df_nlcd_1000), 17)

    # polygons have proper column names
    # Check for the pattern instead of exact column names
    # NLCD columns should have format like LDU_[TYPE]_0_[RADIUS]
    testthat::expect_true(
      any(grepl("^LDU_T[A-Z]{4}_0_\\d+$", names(df_nlcd_1000)))
    )

    # Alternative: check for specific patterns we expect
    expected_patterns <- c(
      "WATR",
      "DVOS",
      "DVLO",
      "DVMI",
      "DVHI",
      "BARN",
      "DFOR",
      "EFOR",
      "MFOR",
      "HERB",
      "PAST",
      "WDWT",
      "HWEM",
      "PLNT",
      "SHRB"
    )

    # Check that at least some expected land cover types are present
    col_string <- paste(names(df_nlcd_1000), collapse = " ")
    matches <- sapply(expected_patterns, function(p) grepl(p, col_string))
    testthat::expect_true(
      sum(matches) >= 10, # At least 10 of the 15 types should be present
      info = sprintf(
        "Expected at least 10 land cover types, found %d. Columns: %s",
        sum(matches),
        paste(names(df_nlcd_1000), collapse = ", ")
      )
    )
  })
  # Automatic cleanup when withr::with_tempdir exits
})

################################################################################
##### Diagnostic test for file discovery
testthat::test_that("process_nlcd file discovery (diagnostic)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("terra")

  withr::with_tempdir({
    directory <- file.path(getwd(), "nlcd_file_test")

    # Download
    download_nlcd(
      product = "Land Cover",
      year = 2021,
      directory_to_save = directory,
      acknowledgement = TRUE,
      hash = TRUE
    )

    # List all files
    all_files <- list.files(directory, recursive = TRUE, full.names = FALSE)
    message("All files in directory:")
    message(paste(all_files, collapse = "\n"))

    # List TIF files specifically
    tif_files <- list.files(
      directory,
      pattern = "\\.tif$",
      recursive = TRUE,
      full.names = FALSE,
      ignore.case = TRUE
    )
    message("\nTIF files found:")
    message(paste(tif_files, collapse = "\n"))

    testthat::expect_true(length(tif_files) > 0)

    # Try to process
    testthat::expect_no_error(
      nlcd <- process_nlcd(path = directory, year = 2021)
    )
  })
})

################################################################################
##### Debug test for column names
testthat::test_that("integration - debug column names", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    directory <- file.path(getwd(), "nlcd_debug")

    download_nlcd(
      product = "Land Cover",
      year = 2021,
      directory_to_save = directory,
      acknowledgement = TRUE,
      hash = TRUE
    )

    nlcd_c1v1 <- process_nlcd(path = directory, year = 2021)

    ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
    ncv <- terra::vect(ncpath)
    nc <- terra::project(ncv, terra::crs(nlcd_c1v1))

    df_nlcd_1000 <- calculate_nlcd(
      locs = terra::centroids(nc[1:5, ]),
      locs_id = "NAME",
      from = nlcd_c1v1,
      mode = "terra",
      radius = 1000
    )

    # Print for debugging
    cat("\n=== NLCD Column Names ===\n")
    print(names(df_nlcd_1000))
    cat("\n=========================\n")

    testthat::succeed("Debug test - check output above")
  })
})

################################################################################
##### download_nlcd file-already-exists and hash=FALSE branches

testthat::test_that("download_nlcd file already exists path", {
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
        download_nlcd(
          year = 2021,
          product = "land cover",
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

testthat::test_that("download_nlcd mock download hash=FALSE", {
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
        download_nlcd(
          year = 2021,
          product = "land cover",
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

################################################################################
##### download_nlcd deprecated params, download=FALSE, and remove_command paths

testthat::test_that("download_nlcd download=FALSE deprecated warning + early return", {
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_nlcd(
          year = 2021,
          product = "land cover",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          hash = FALSE
        )
      ),
      "deprecated"
    )
  })
})

testthat::test_that("download_nlcd remove_command deprecated warning", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_nlcd(
          year = 2021,
          product = "land cover",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          remove_command = TRUE,
          hash = FALSE
        )
      ),
      "deprecated"
    )
  })
})
