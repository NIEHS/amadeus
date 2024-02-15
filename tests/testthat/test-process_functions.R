# Tests for data import functions

testthat::test_that("process_sedac_population returns expected.", {
  withr::local_package("terra")
  paths <- list.files(
    "../testdata/population/",
    pattern = ".tif",
    full.names = TRUE
    )
  # expect function
  expect_true(
    is.function(process_sedac_population)
  )
  for (p in seq_along(paths)) {
    pop <-
      process_sedac_population(
        path = paths[p]
      )
    # expect output is a SpatRaster
    expect_true(
      class(pop)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(pop)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(pop))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(pop)[1:2])
    )
  }
})

testthat::test_that("process_hms returns expected.", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  # expect function
  expect_true(
    is.function(process_hms)
  )
  for (d in seq_along(densities)) {
    hms <-
      process_hms(
        date_start = "2022-06-10",
        date_end = "2022-06-11",
        variable = densities[d],
        path =
        "../testdata/hms/"
      )
    # expect output is a SpatVector or character
    expect_true(
      class(hms)[1] %in% c("SpatVector", "character")
    )
    if (class(hms)[1] == "SpatVector") {
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(hms))
      )
      # expect two columns
      expect_true(
        ncol(hms) == 2
      )
      # expect density and date column
      expect_true(
        all(c("Density", "Date") %in% names(hms))
      )
    } else if (class(hms)[1] == "character") {
      # expect first is density type
      expect_true(
        hms[1] %in% c("Light", "Medium", "Heavy")
      )
      # expect other elements are 8 character dates
      expect_true(
        all(nchar(hms[2:length(hms)]) == 8)
      )
    }
  }
})

testthat::test_that("process_gmted returns expected.", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  # expect function
  expect_true(
    is.function(process_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      gmted <-
        process_gmted(
          variable = c(statistic, resolution),
          path =
            paste0(
              "../testdata/gmted/",
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
              "_grd/"
            )
        )
      # expect output is a SpatRaster
      expect_true(
        class(gmted)[1] == "SpatRaster"
      )
      # expect values
      expect_true(
        terra::hasValues(gmted)
      )
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(gmted))
      )
      # expect lon and lat dimensions to be > 1
      expect_false(
        any(c(0, 1) %in% dim(gmted)[1:2])
      )
    }
  }
})

testthat::test_that("import_gmted returns error with non-vector variable.", {
  expect_error(
    gmted <-
      process_gmted(
        variable <- "Breakline Emphasis; 7.5 arc-seconds",
        path = "../testdata/gmted/gmted"
      )
  )
})

testthat::test_that("process_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  expect_true(
    is.function(process_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      process_narr(
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = variables[v],
        path =
        paste0(
          "../testdata/narr/",
          variables[v]
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      expect_true(
        dim(narr)[3] == 1
      )
    } else if (variables[v] == "omega") {
      expect_true(
        dim(narr)[3] == 29
      )
    }
  }
})

testthat::test_that("process_geos returns expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  # expect function
  expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = "O3",
        path = paste0(
          "../testdata/geos/",
          collection
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collectoin
    if (collection == "aqc_tavg_1hr_g1440x721_v1") {
      expect_true(
        dim(geos)[3] == 24
      )
    } else if (collection == "chm_inst_1hr_g1440x721_p23") {
      expect_true(
        dim(geos)[3] == 23 * 24
      )
    }
  }
})

testthat::test_that("process_geos expected errors.", {
  # expect error without variable
  expect_error(
    process_geos()
  )
  # expect error on directory without data
  expect_error(
    process_geos(
      variable = "O3",
      path = "./"
    )
  )
})
