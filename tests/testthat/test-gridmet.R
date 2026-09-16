################################################################################
##### unit and integration tests for Climatology Group Gridmet functions

################################################################################
##### download_gridmet
testthat::test_that("download_gridmet (no errors)", {
  testthat::skip_if_offline()
  withr::local_package("httr2")
  withr::local_package("stringr")
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  # function parameters
  year_start <- 2018
  year_end <- 2023
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/gridmet/")

  # run download function
  result <- suppressWarnings(
    download_data(
      dataset_name = "gridmet",
      year = c(year_start, year_end),
      variables = variables,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Assert structured return values from httr2-based download discovery
  testthat::expect_type(result, "list")
  testthat::expect_equal(
    result$n_files,
    year_end - year_start + 1L
  )
  testthat::expect_true(
    all(
      grepl(
        "northwestknowledge.net/metdata/data/pr_",
        result$urls
      )
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_gridmet (single year)", {
  testthat::skip_if_offline()
  withr::local_package("httr2")
  withr::local_package("stringr")
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  # function parameters
  year <- 2020
  variables <- "Precipitation"
  directory_to_save <- paste0(tempdir(), "/gridmet/")

  # run download function
  result <- suppressWarnings(
    download_data(
      dataset_name = "gridmet",
      year = year,
      variables = variables,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # Assert structured return values from httr2-based download discovery
  testthat::expect_type(result, "list")
  testthat::expect_equal(result$n_files, 1L)
  testthat::expect_true(
    grepl(
      paste0("northwestknowledge.net/metdata/data/pr_", year, ".nc"),
      result$urls
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_gridmet (expected errors - invalid years)", {
  testthat::expect_error(
    download_data(
      dataset_name = "gridmet",
      variables = "Precipitation",
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/g/")
    )
  )
})

testthat::test_that("download_gridmet (expected errors - invalid variables)", {
  testthat::expect_error(
    download_data(
      dataset_name = "gridmet",
      variables = "temp",
      year = c(2018, 2018),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/g/")
    )
  )
})

testthat::test_that("download_gridmet remove_command deprecation warning", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      download_gridmet(
        variables = "Precipitation",
        year = c(2018, 2018),
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_gridmet mock download with hash", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_gridmet(
          variables = "Precipitation",
          year = c(2018, 2018),
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

testthat::test_that(
  "download_gridmet continues when preflight check fails transiently",
  {
    run_called <- FALSE
    testthat::local_mocked_bindings(
      check_url_status = function(...) FALSE,
      check_destfile = function(...) TRUE,
      download_run_method = function(...) {
        run_called <<- TRUE
        list(success = 1, failed = 0, skipped = 0)
      },
      .package = "amadeus"
    )
    withr::with_tempdir({
      testthat::expect_warning(
        result <- suppressMessages(
          download_gridmet(
            variables = "Precipitation",
            year = c(2018, 2018),
            directory_to_save = ".",
            acknowledgement = TRUE
          )
        ),
        regexp = "Preflight URL check failed"
      )
      testthat::expect_true(run_called)
      testthat::expect_equal(result$success, 1L)
    })
  }
)

testthat::test_that("download_gridmet mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_gridmet(
          variables = "Precipitation",
          year = c(2018, 2018),
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

testthat::test_that(
  "download_data(dataset_name='gridmet', year=2020): discovers one exact file",
  {
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(...) TRUE,
      .package = "amadeus"
    )
    year <- 2020
    variables <- "Precipitation"
    directory_to_save <- file.path(withr::local_tempdir(), "gridmet")

    testthat::expect_warning(
      result <- suppressMessages(
        download_data(
          dataset_name = "gridmet",
          year = year,
          variables = variables,
          directory_to_save = directory_to_save,
          acknowledgement = TRUE,
          download = FALSE
        )
      ),
      regexp = "download=FALSE is deprecated"
    )

    testthat::expect_true(dir.exists(directory_to_save))

    testthat::expect_type(result, "list")
    testthat::expect_equal(result$n_files, 1L)
    testthat::expect_identical(
      result$urls,
      "https://www.northwestknowledge.net/metdata/data/pr_2020.nc"
    )
    testthat::expect_identical(
      result$destfiles,
      file.path(directory_to_save, "pr", "pr_2020.nc")
    )
  }
)

testthat::test_that(
  "download_gridmet(acknowledgement=FALSE): stops before creating directories",
  {
    directory_to_save <- file.path(withr::local_tempdir(), "gridmet")

    testthat::expect_error(
      download_gridmet(
        variables = "pr",
        year = 2018,
        directory_to_save = directory_to_save,
        acknowledgement = FALSE
      ),
      regexp = "acknowledgement is set to FALSE"
    )
    testthat::expect_false(dir.exists(directory_to_save))
  }
)

testthat::test_that(
  "download_gridmet(variables=c('pr','tmmx')): forwards exact download arguments",
  {
    captured <- NULL
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(...) TRUE,
      download_run_method = function(...) {
        captured <<- list(...)
        list(success = 4L, failed = 0L, skipped = 0L)
      },
      .package = "amadeus"
    )
    directory_to_save <- withr::local_tempdir()

    result <- suppressMessages(
      download_gridmet(
        variables = c("pr", "tmmx"),
        year = c(2019, 2018),
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        show_progress = FALSE,
        max_tries = 3L,
        rate_limit = 0.25
      )
    )

    expected_files <- c("pr_2018.nc", "pr_2019.nc", "tmmx_2018.nc", "tmmx_2019.nc")
    expected_variables <- c("pr", "pr", "tmmx", "tmmx")
    testthat::expect_identical(
      captured$urls,
      paste0(
        "https://www.northwestknowledge.net/metdata/data/",
        expected_files
      )
    )
    testthat::expect_identical(
      captured$destfiles,
      file.path(directory_to_save, expected_variables, expected_files)
    )
    testthat::expect_null(captured$token)
    testthat::expect_false(captured$show_progress)
    testthat::expect_identical(captured$max_tries, 3L)
    testthat::expect_identical(captured$rate_limit, 0.25)
    testthat::expect_identical(
      result,
      list(success = 4L, failed = 0L, skipped = 0L)
    )
  }
)

testthat::test_that(
  "download_gridmet(existing destination): sends only missing files to downloader",
  {
    captured_urls <- NULL
    testthat::local_mocked_bindings(
      check_url_status = function(...) TRUE,
      check_destfile = function(path) !grepl("pr_2018[.]nc$", path),
      download_run_method = function(urls, ...) {
        captured_urls <<- urls
        list(success = 1L, failed = 0L, skipped = 1L)
      },
      .package = "amadeus"
    )

    result <- suppressMessages(
      download_gridmet(
        variables = "pr",
        year = c(2018, 2019),
        directory_to_save = withr::local_tempdir(),
        acknowledgement = TRUE
      )
    )

    testthat::expect_identical(
      captured_urls,
      "https://www.northwestknowledge.net/metdata/data/pr_2019.nc"
    )
    testthat::expect_equal(result$success, 1L)
  }
)

################################################################################
##### process_gridmet
testthat::test_that("process_gridmet", {
  withr::local_package("terra")
  variable <- "Precipitation"
  # expect function
  testthat::expect_true(
    is.function(process_gridmet)
  )
  gridmet <-
    process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = variable,
      path = testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      )
    )
  # expect output is SpatRaster
  testthat::expect_true(
    class(gridmet)[1] == "SpatRaster"
  )
  # expect values
  testthat::expect_true(
    terra::hasValues(gridmet)
  )
  # expect non-null coordinate reference system
  testthat::expect_false(
    is.null(terra::crs(gridmet))
  )
  # expect lon and lat dimensions to be > 1
  testthat::expect_false(
    any(c(0, 1) %in% dim(gridmet)[1:2])
  )
  # expect non-numeric and non-empty time
  testthat::expect_false(
    any(c("", 0) %in% terra::time(gridmet))
  )
  # expect dimensions according to levels
  testthat::expect_true(
    dim(gridmet)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    gridmet_ext <- process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = "Precipitation",
      path = testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      ),
      extent = terra::ext(gridmet)
    )
  )
})

testthat::test_that("process_gridmet (single date)", {
  withr::local_package("terra")
  variable <- "Precipitation"
  # expect function
  testthat::expect_true(
    is.function(process_gridmet)
  )
  gridmet <-
    process_gridmet(
      date = "2018-01-03",
      variable = variable,
      path = testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      )
    )
  # expect output is SpatRaster
  testthat::expect_true(
    class(gridmet)[1] == "SpatRaster"
  )
  # expect values
  testthat::expect_true(
    terra::hasValues(gridmet)
  )
  # expect non-null coordinate reference system
  testthat::expect_false(
    is.null(terra::crs(gridmet))
  )
  # expect lon and lat dimensions to be > 1
  testthat::expect_false(
    any(c(0, 1) %in% dim(gridmet)[1:2])
  )
  # expect non-numeric and non-empty time
  testthat::expect_false(
    any(c("", 0) %in% terra::time(gridmet))
  )
  # expect dimensions according to levels
  testthat::expect_true(
    dim(gridmet)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    gridmet_ext <- process_gridmet(
      date = "2018-01-03",
      variable = "Precipitation",
      path = testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      ),
      extent = terra::ext(gridmet)
    )
  )
})

testthat::test_that("process_gridmet_codes", {
  # gridmet
  gc1 <- process_gridmet_codes("all")
  testthat::expect_true(ncol(gc1) == 2)
  gc2 <- process_gridmet_codes("sph", invert = TRUE)
  testthat::expect_true(class(gc2) == "character")
  testthat::expect_true(nchar(gc2) > 7)
  gc3 <- process_gridmet_codes("Near-Surface Specific Humidity")
  testthat::expect_true(class(gc3) == "character")
  testthat::expect_true(nchar(gc3) < 7)
  # process_variable_codes
  testthat::expect_no_error(process_variable_codes("sph", "gridmet"))
  testthat::expect_no_error(
    process_variable_codes("Near-Surface Specific Humidity", "gridmet")
  )
  testthat::expect_error(
    process_variable_codes("error", "gridmet")
  )
})



testthat::test_that(
  "process_parse_ncdf_day_codes(GridMET fixture): returns the exact observation date",
  {
    gridmet_file <- testthat::test_path(
      "..",
      "testdata",
      "gridmet",
      "pr",
      "pr_2018.nc"
    )
    gridmet_raw <- terra::rast(gridmet_file)

    testthat::expect_identical(
      names(gridmet_raw),
      "precipitation_amount_day=43101"
    )
    testthat::expect_identical(
      process_parse_ncdf_day_codes(names(gridmet_raw)),
      as.Date("2018-01-03")
    )
    testthat::expect_error(
      process_parse_ncdf_day_codes("precipitation_amount_day=bad"),
      regexp = "Unable to parse gridmet layer time.*day=bad"
    )
  }
)

testthat::test_that(
  "process_gridmet(date='2018-01-03', variable='pr'): returns exact fixture metadata",
  {
    gridmet_file <- testthat::test_path(
      "..",
      "testdata",
      "gridmet",
      "pr",
      "pr_2018.nc"
    )
    gridmet_raw <- terra::rast(gridmet_file)
    gridmet <- suppressMessages(
      process_gridmet(
        date = "2018-01-03",
        variable = "pr",
        path = testthat::test_path("..", "testdata", "gridmet", "pr")
      )
    )

    testthat::expect_s4_class(gridmet, "SpatRaster")
    testthat::expect_equal(dim(gridmet), c(18L, 25L, 1L))
    testthat::expect_identical(names(gridmet), "pr_20180103")
    testthat::expect_identical(terra::time(gridmet), as.Date("2018-01-03"))
    testthat::expect_identical(terra::varnames(gridmet), "pr")
    testthat::expect_identical(terra::longnames(gridmet), "precipitation")
    testthat::expect_identical(
      terra::crs(gridmet, describe = TRUE)$code,
      "4326"
    )
    testthat::expect_true(terra::same.crs(gridmet, gridmet_raw))
    testthat::expect_true(terra::same.crs(gridmet, "EPSG:4326"))
    testthat::expect_equal(
      terra::values(gridmet)[1],
      2.5999999,
      tolerance = 1e-6
    )
  }
)

testthat::test_that(
  "process_gridmet_codes(all codes): supports complete reversible mappings",
  {
    mappings <- process_gridmet_codes("all")

    testthat::expect_identical(dim(mappings), c(17L, 2L))
    testthat::expect_length(unique(mappings[, 1]), 17L)
    testthat::expect_length(unique(mappings[, 2]), 17L)

    for (i in seq_len(nrow(mappings))) {
      testthat::expect_equal(
        unname(process_gridmet_codes(mappings[i, 1])),
        unname(mappings[i, 2])
      )
      testthat::expect_equal(
        unname(process_gridmet_codes(mappings[i, 2], invert = TRUE)),
        unname(mappings[i, 1])
      )
    }
  }
)


################################################################################
##### calculate_gridmet
testthat::test_that("calculate_gridmet", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_gridmet)
  )
  for (r in seq_along(radii)) {
    gridmet <-
      process_gridmet(
        date = c("2018-01-03", "2018-01-03"),
        variable = "pr",
        path = testthat::test_path(
          "..",
          "testdata",
          "gridmet",
          "pr"
        )
      )
    gridmet_covariate <-
      calculate_gridmet(
        from = gridmet,
        locs = data.table::data.table(ncp),
        locs_id = "site_id",
        radius = radii[r],
        fun = "mean"
      )
    # set column names
    gridmet_covariate <- calc_setcolumns(
      from = gridmet_covariate,
      lag = 0,
      dataset = "gridmet",
      locs_id = "site_id"
    )
    # expect output is data.frame
    testthat::expect_true(
      class(gridmet_covariate) == "data.frame"
    )
    # expect 3 columns
    testthat::expect_true(
      ncol(gridmet_covariate) == 3
    )
    # expect numeric value
    testthat::expect_true(
      class(gridmet_covariate[, 3]) == "numeric"
    )
    # expect $time is class Date
    testthat::expect_true(
      "POSIXt" %in% class(gridmet_covariate$time)
    )
  }
  # with included geometry terra
  testthat::expect_no_error(
    gridmet_covariate_terra <- calculate_gridmet(
      from = gridmet,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(gridmet_covariate_terra),
    3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gridmet_covariate_terra)
  )

  # with included geometry sf
  testthat::expect_no_error(
    gridmet_covariate_sf <- calculate_gridmet(
      from = gridmet,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(gridmet_covariate_sf),
    4
  )
  testthat::expect_true(
    "sf" %in% class(gridmet_covariate_sf)
  )

  testthat::expect_error(
    calculate_gridmet(
      from = gridmet,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})

testthat::test_that("calculate_gridmet supports .by_time summaries", {
  withr::local_package("terra")
  locs <- data.frame(lon = -78.8277, lat = 35.95013, site_id = "3799900018810101")
  gridmet <- process_gridmet(
    date = c("2018-01-03", "2018-01-03"),
    variable = "pr",
    path = testthat::test_path("..", "testdata", "gridmet", "pr")
  )

  by_time <- calculate_gridmet(
    from = gridmet,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by_time = "day",
    fun = "mean"
  )

  testthat::expect_true("time" %in% names(by_time))
  testthat::expect_s3_class(by_time$time, "POSIXct")
  testthat::expect_true(any(grepl("_0$", names(by_time))))
})

testthat::test_that("calculate_gridmet summarizes at native daily scale when .by_time is NULL", {
  withr::local_package("terra")
  r <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  r <- c(r, r)
  terra::values(r) <- matrix(c(1, 3), ncol = 2)
  names(r) <- c("pr_20180103", "pr_20180103_dup")
  terra::time(r) <- as.Date(c("2018-01-03", "2018-01-03"))
  locs <- data.frame(lon = 0.5, lat = 0.5, site_id = "s1")

  out <- calculate_gridmet(
    from = r,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    fun = "mean"
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$pr_0, 2)
  testthat::expect_s3_class(out$time, "POSIXct")
})

testthat::test_that("calculate_gridmet errors when deprecated .by is supplied", {
  withr::local_package("terra")
  locs <- data.frame(lon = -78.8277, lat = 35.95013, site_id = "3799900018810101")
  gridmet <- process_gridmet(
    date = c("2018-01-03", "2018-01-03"),
    variable = "pr",
    path = testthat::test_path("..", "testdata", "gridmet", "pr")
  )

  testthat::expect_error(
    calculate_gridmet(
      from = gridmet,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = "day",
      fun = "mean"
    ),
    regexp = "no longer supported"
  )
})


testthat::test_that("calculate_gridmet supports weighted extraction", {
  withr::local_package("terra")
  from <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(from) <- c(1, 2, 3, 4)
  names(from) <- "pr_20200101"

  locs <- terra::as.polygons(terra::ext(from), crs = terra::crs(from))
  locs$site_id <- "poly_1"

  weights <- from
  terra::values(weights) <- c(1, 1, 1, 10)

  res_unweighted <- calculate_gridmet(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0
  )
  res_weighted <- calculate_gridmet(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    weights = weights
  )

  testthat::expect_true(res_weighted$pr_0 != res_unweighted$pr_0)
  testthat::expect_no_error({
    point_res <- calculate_gridmet(
      from = from,
      locs = data.frame(lon = 1, lat = 1, site_id = "pt_1"),
      locs_id = "site_id",
      radius = 0,
      weights = weights
    )
    testthat::expect_true(is.numeric(point_res$pr_0))
  })
})

testthat::test_that("calculate_gridmet accepts polygon weights and validates CRS", {
  withr::local_package("terra")
  from <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(from) <- c(1, 2, 3, 4)
  names(from) <- "pr_20200101"

  locs <- terra::as.polygons(terra::ext(from), crs = terra::crs(from))
  locs$site_id <- "poly_1"

  weight_poly <- terra::as.polygons(from)
  weight_poly$wt <- c(1, 2, 3, 4)
  testthat::expect_no_error(
    calculate_gridmet(
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      weights = weight_poly
    )
  )

  bad_weights <- from
  terra::crs(bad_weights) <- ""
  testthat::expect_error(
    calculate_gridmet(
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      weights = bad_weights
    ),
    "missing CRS"
  )
})


testthat::test_that(
  "calculate_gridmet(radius=1000, geom='terra'): uses R as radius and 2R as diameter",
  {
    input_radius <- 1000
    gridmet <- terra::rast(
      nrows = 6,
      ncols = 6,
      xmin = -3000,
      xmax = 3000,
      ymin = -3000,
      ymax = 3000,
      crs = "EPSG:3857"
    )
    terra::values(gridmet) <- 1
    names(gridmet) <- "pr_20200101"
    terra::time(gridmet) <- as.Date("2020-01-01")

    result <- suppressMessages(
      calculate_gridmet(
        from = gridmet,
        locs = data.frame(site_id = "site_1", lon = 0, lat = 0),
        locs_id = "site_id",
        radius = input_radius,
        geom = "terra"
      )
    )
    result_extent <- as.vector(terra::ext(result))
    output_diameters <- c(
      x = result_extent[2] - result_extent[1],
      y = result_extent[4] - result_extent[3]
    )
    output_radii <- output_diameters / 2

    testthat::expect_s4_class(result, "SpatVector")
    testthat::expect_identical(terra::geomtype(result), "polygons")
    testthat::expect_true(terra::same.crs(result, gridmet))
    testthat::expect_true("pr_1000" %in% names(result))
    testthat::expect_equal(
      unname(output_diameters),
      rep(2 * input_radius, 2),
      tolerance = 1e-8
    )
    testthat::expect_equal(
      unname(output_radii),
      rep(input_radius, 2),
      tolerance = 1e-8
    )
    testthat::expect_lte(max(output_radii), input_radius + 1e-8)
  }
)

testthat::test_that(
  "calculate_gridmet(locs in EPSG:4326 and EPSG:3857): extracts equal values",
  {
    gridmet <- suppressMessages(
      process_gridmet(
        date = "2018-01-03",
        variable = "pr",
        path = testthat::test_path("..", "testdata", "gridmet", "pr")
      )
    )
    locs_4326 <- sf::st_as_sf(
      data.frame(
        site_id = "site_1",
        lon = -78.8277,
        lat = 35.95013
      ),
      coords = c("lon", "lat"),
      crs = 4326
    )
    locs_3857 <- sf::st_transform(locs_4326, 3857)

    result_4326 <- suppressMessages(
      calculate_gridmet(
        from = gridmet,
        locs = locs_4326,
        locs_id = "site_id",
        radius = 0
      )
    )
    result_3857 <- suppressMessages(
      calculate_gridmet(
        from = gridmet,
        locs = locs_3857,
        locs_id = "site_id",
        radius = 0
      )
    )

    testthat::expect_identical(result_4326$site_id, result_3857$site_id)
    testthat::expect_identical(result_4326$time, result_3857$time)
    testthat::expect_equal(
      result_4326$pr_0,
      result_3857$pr_0,
      tolerance = 1e-8
    )
  }
)

testthat::test_that(
  "calculate_gridmet(radius=3, fun=<multiple>): applies requested spatial summary",
  {
    withr::local_package("terra")

    gridmet <- terra::rast(
      nrows = 3,
      ncols = 3,
      xmin = 0,
      xmax = 3,
      ymin = 0,
      ymax = 3,
      crs = "EPSG:3857"
    )
    terra::values(gridmet) <- c(1:8, 100)
    names(gridmet) <- "pr_20210101"
    terra::time(gridmet) <- as.Date("2021-01-01")

    locs <- terra::vect(
      data.frame(
        site_id = "site_1",
        x = 1.5,
        y = 1.5
      ),
      geom = c("x", "y"),
      crs = terra::crs(gridmet)
    )

    expected <- c(
      max = 100,
      min = 1,
      median = 5,
      mean = mean(c(1:8, 100))
    )

    observed <- vapply(
      names(expected),
      function(summary_fun) {
        result <- suppressMessages(
          calculate_gridmet(
            from = gridmet,
            locs = locs,
            locs_id = "site_id",
            radius = 3,
            fun = summary_fun
          )
        )

        value_columns <- setdiff(
          names(result),
          c("site_id", "time")
        )

        testthat::expect_s3_class(result, "data.frame")
        testthat::expect_equal(nrow(result), 1L)
        testthat::expect_length(value_columns, 1L)
        testthat::expect_type(result[[value_columns]], "double")

        result[[value_columns]][[1L]]
      },
      numeric(1)
    )

    testthat::expect_named(observed, names(expected))
    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-7
    )
  }
)
