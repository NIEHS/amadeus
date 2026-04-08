################################################################################
##### unit and integration tests for NOAA NARR functions

################################################################################
##### download_narr
testthat::test_that("download_narr (no errors)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2018
  variables <- c(
    "weasd", # monolevel
    "omega", # pressure level
    "soill" # subsurface
  )
  directory_to_save <- paste0(tempdir(), "/narr/")

  # Expect deprecation warning with download = FALSE
  testthat::expect_warning(
    download_data(
      dataset_name = "narr",
      year = c(year_start, year_end),
      variables = variables,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  # define path with commands
  commands_path <- paste0(
    directory_to_save,
    "narr_",
    year_start,
    "_",
    year_end,
    "_curl_commands.txt"
  )

  # Only proceed if commands file exists
  if (file.exists(commands_path)) {
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 3L)
    # implement unit tests
    test_download_functions(
      directory_to_save = directory_to_save,
      commands_path = commands_path,
      url_status = url_status
    )
    # remove file with commands after test
    file.remove(commands_path)
  }

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_narr (single year)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/narr/")

  # Expect deprecation warning
  testthat::expect_warning(
    download_data(
      dataset_name = "narr",
      year = 2020,
      variables = "weasd",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_narr (expected errors)", {
  testthat::expect_error(
    download_data(
      dataset_name = "narr",
      variables = "weasd",
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
})

testthat::test_that("narr_variable (expected errors)", {
  # expected error due to unrecognized variable name
  testthat::expect_error(
    narr_variable("uNrEcOgNiZed")
  )
})

testthat::test_that("download_narr without download=FALSE", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/narr_new/")

  # Test without download=FALSE (new httr2 method, no deprecation warning)
  testthat::expect_no_error(
    download_data(
      dataset_name = "narr",
      year = 2020,
      variables = "weasd",
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

testthat::test_that("download_narr remove_command deprecation warning", {
  withr::with_tempdir({
    testthat::expect_warning(
      download_narr(
        year = 2020,
        variables = "weasd",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_narr mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_narr(
          year = 2020,
          variables = "weasd",
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
##### process_narr
testthat::test_that("process_narr", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  testthat::expect_true(
    is.function(process_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      process_narr(
        date = c("2018-01-01", "2018-01-05"),
        variable = variables[v],
        path = testthat::test_path(
          "..",
          "testdata",
          "narr",
          variables[v]
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      testthat::expect_true(
        dim(narr)[3] == 5
      )
    } else if (variables[v] == "omega") {
      testthat::expect_true(
        dim(narr)[3] == 145
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    narr_ext <-
      process_narr(
        date = c("2018-01-01", "2018-01-05"),
        variable = "omega",
        path = testthat::test_path(
          "..",
          "testdata",
          "narr",
          "omega"
        ),
        extent = terra::ext(narr)
      )
  )
})

testthat::test_that("process_narr (single date)", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  testthat::expect_true(
    is.function(process_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      process_narr(
        date = "2018-01-01",
        variable = variables[v],
        path = testthat::test_path(
          "..",
          "testdata",
          "narr",
          variables[v]
        )
      )
    # expect output is SpatRaster
    testthat::expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    testthat::expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    testthat::expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    testthat::expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    testthat::expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      testthat::expect_true(
        dim(narr)[3] == 1
      )
    } else if (variables[v] == "omega") {
      testthat::expect_true(
        dim(narr)[3] == 29
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    narr_ext <-
      process_narr(
        date = "2018-01-01",
        variable = "omega",
        path = testthat::test_path(
          "..",
          "testdata",
          "narr",
          "omega"
        ),
        extent = terra::ext(narr)
      )
  )
})

################################################################################
##### calculate_narr
testthat::test_that("calculate_narr", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (r in seq_along(radii)) {
      narr <-
        process_narr(
          date = "2018-01-01",
          variable = variable,
          path = testthat::test_path(
            "..",
            "testdata",
            "narr",
            variable
          )
        )
      narr_covariate <-
        calculate_narr(
          from = narr,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      narr_covariate <- calc_setcolumns(
        from = narr_covariate,
        lag = 0,
        dataset = "narr",
        locs_id = "site_id"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(narr_covariate) == "data.frame"
      )
      if (variable == "weasd") {
        # expect 3 columns (no pressure level)
        testthat::expect_true(
          ncol(narr_covariate) == 3
        )
        # expect numeric value
        testthat::expect_true(
          class(narr_covariate[, 3]) == "numeric"
        )
      } else {
        # expect 4 columns
        testthat::expect_true(
          ncol(narr_covariate) == 4
        )
        # expect numeric value
        testthat::expect_true(
          class(narr_covariate[, 4]) == "numeric"
        )
      }
      # expect $time is class Date
      testthat::expect_true(
        "POSIXct" %in% class(narr_covariate$time)
      )
    }
  }
  # with geometry terra
  testthat::expect_no_error(
    narr_covariate_terra <- calculate_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_terra),
    4 # 4 columns because omega has pressure levels
  )
  testthat::expect_true(
    "SpatVector" %in% class(narr_covariate_terra)
  )
  # with geometry sf
  testthat::expect_no_error(
    narr_covariate_sf <- calculate_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_sf),
    5 # 5 columns because omega has pressure levels
  )
  testthat::expect_true(
    "sf" %in% class(narr_covariate_sf)
  )

  testthat::expect_error(
    calculate_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
})

testthat::test_that("calculate_narr supports .by/.by_time summaries", {
  withr::local_package("terra")
  locs <- data.frame(lon = -78.8277, lat = 35.95013, site_id = "3799900018810101")
  narr <- process_narr(
    date = "2018-01-01",
    variable = "omega",
    path = testthat::test_path("..", "testdata", "narr", "omega")
  )

  by_time <- calculate_narr(
    from = narr,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "day",
    fun = "mean"
  )

  testthat::expect_true("time" %in% names(by_time))
  testthat::expect_s3_class(by_time$time, "POSIXct")
  testthat::expect_true("level" %in% names(by_time))
})

################################################################################
##### download_narr hash=FALSE branch

testthat::test_that("download_narr mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_narr(
          year = 2020,
          variables = "weasd",
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
##### narr_variable: all variables return correct URL category

testthat::test_that("narr_variable mono variables return monolevel URL", {
  mono <- c(
    "acpcp", "air.2m", "air.sfc", "albedo", "apcp", "bgrun",
    "bmixl.hl1", "cape", "ccond", "cdcon", "cdlyr", "cfrzr",
    "cicep", "cin", "cnwat", "crain", "csnow", "dlwrf", "dpt.2m",
    "dswrf", "evap", "gflux", "hcdc", "hgt.tropo", "hlcy", "hpbl",
    "lcdc", "lftx4", "lhtfl", "mcdc", "mconv.hl1", "mslet", "mstav",
    "pevap", "pottmp.hl1", "pottmp.sfc", "prate", "pres.sfc",
    "pres.tropo", "prmsl", "pr_wtr", "rcq", "rcs", "rcsol", "rct",
    "rhum.2m", "shtfl", "shum.2m", "snod", "snohf", "snom", "snowc",
    "soilm", "ssrun", "tcdc", "tke.hl1", "ulwrf.ntat", "ulwrf.sfc",
    "ustm", "uswrf.ntat", "uswrf.sfc", "uwnd.10m", "veg", "vis",
    "vstm", "vvel.hl1", "vwnd.10m", "vwsh.tropo", "wcconv", "wcinc",
    "wcuflx", "wcvflx", "weasd", "wvconv", "wvinc", "wvuflx", "wvvflx"
  )
  for (v in mono) {
    result <- narr_variable(v)
    testthat::expect_true(
      grepl("monolevel", result[[1]]),
      label = paste0("narr_variable('", v, "') should return monolevel URL")
    )
    testthat::expect_equal(
      result[[2]], "",
      label = paste0("narr_variable('", v, "') months should be empty string")
    )
  }
})

testthat::test_that("narr_variable pressure variables return pressure URL", {
  pressure <- c("air", "hgt", "omega", "shum", "tke", "uwnd", "vwnd")
  for (v in pressure) {
    result <- narr_variable(v)
    testthat::expect_true(
      grepl("pressure", result[[1]]),
      label = paste0("narr_variable('", v, "') should return pressure URL")
    )
    testthat::expect_equal(
      length(result[[2]]), 12L,
      label = paste0("narr_variable('", v, "') should return 12 months")
    )
  }
})

testthat::test_that("narr_variable soil variables return subsurface URL", {
  soil <- c("soill", "soilw", "tsoil")
  for (v in soil) {
    result <- narr_variable(v)
    testthat::expect_true(
      grepl("subsurface", result[[1]]),
      label = paste0("narr_variable('", v, "') should return subsurface URL")
    )
    testthat::expect_equal(
      length(result[[2]]), 12L,
      label = paste0("narr_variable('", v, "') should return 12 months")
    )
  }
})

################################################################################
##### download_narr: mock-based download test for all variables

testthat::test_that("download_narr mock download all mono variables", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  mono <- c(
    "acpcp", "air.2m", "air.sfc", "albedo", "apcp", "bgrun",
    "bmixl.hl1", "cape", "ccond", "cdcon", "cdlyr", "cfrzr",
    "cicep", "cin", "cnwat", "crain", "csnow", "dlwrf", "dpt.2m",
    "dswrf", "evap", "gflux", "hcdc", "hgt.tropo", "hlcy", "hpbl",
    "lcdc", "lftx4", "lhtfl", "mcdc", "mconv.hl1", "mslet", "mstav",
    "pevap", "pottmp.hl1", "pottmp.sfc", "prate", "pres.sfc",
    "pres.tropo", "prmsl", "pr_wtr", "rcq", "rcs", "rcsol", "rct",
    "rhum.2m", "shtfl", "shum.2m", "snod", "snohf", "snom", "snowc",
    "soilm", "ssrun", "tcdc", "tke.hl1", "ulwrf.ntat", "ulwrf.sfc",
    "ustm", "uswrf.ntat", "uswrf.sfc", "uwnd.10m", "veg", "vis",
    "vstm", "vvel.hl1", "vwnd.10m", "vwsh.tropo", "wcconv", "wcinc",
    "wcuflx", "wcvflx", "weasd", "wvconv", "wvinc", "wvuflx", "wvvflx"
  )
  withr::with_tempdir({
    for (v in mono) {
      result <- suppressWarnings(suppressMessages(
        download_narr(
          year = 2020,
          variables = v,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      ))
      testthat::expect_type(result, "list")
    }
  })
})

testthat::test_that("download_narr mock download all pressure variables", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  pressure <- c("air", "hgt", "omega", "shum", "tke", "uwnd", "vwnd")
  withr::with_tempdir({
    for (v in pressure) {
      result <- suppressWarnings(suppressMessages(
        download_narr(
          year = 2020,
          variables = v,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      ))
      testthat::expect_type(result, "list")
    }
  })
})

testthat::test_that("download_narr mock download all soil variables", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  soil <- c("soill", "soilw", "tsoil")
  withr::with_tempdir({
    for (v in soil) {
      result <- suppressWarnings(suppressMessages(
        download_narr(
          year = 2020,
          variables = v,
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      ))
      testthat::expect_type(result, "list")
    }
  })
})

################################################################################
##### download_narr pressure and subsurface variables (covers lines 861, 863)

testthat::test_that("download_narr pressure variable (omega) branch", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_narr(
          year = 2020,
          variables = "omega",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
  })
})

testthat::test_that("download_narr subsurface variable (soill) branch", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_narr(
          year = 2020,
          variables = "soill",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
  })
})
