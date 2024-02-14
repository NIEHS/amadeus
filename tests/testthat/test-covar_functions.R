# Tests for covariate generation functions

testthat::test_that("calc_hms returns expected.", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  radii <- c(0, 1000)
  locs <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(calc_hms)
  )
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (r in seq_along(radii)) {
      hms <-
        process_hms(
          date_start = "2022-06-10",
          date_end = "2022-06-11",
          variable = density,
          path =
          "../testdata/hms/"
        )
      hms_covariate <-
        calc_hms(
          from = hms,
          locs = locs,
          locs_id = "site_id",
          radius = radii[r]
        )
      # expect output is data.frame
      expect_true(
        class(hms_covariate) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covariate) == 3
      )
      # expect 9 rows
      expect_true(
        nrow(hms_covariate) == 6
      )
      # expect integer for binary value
      expect_true(
        class(hms_covariate[, 3]) == "integer"
      )
      # expect binary
      expect_true(
        all(unique(hms_covariate[, 3]) %in% c(0, 1))
      )
    }
  }
})

testthat::test_that("calc_gmted returns expected.", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  radii <- c(0, 1000)
  locs <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(calc_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      for (a in seq_along(radii)) {
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
        gmted_covariate <-
          calc_gmted(
            from = gmted,
            locs = locs,
            locs_id = "site_id",
            radius = radii[a],
            fun = "mean"
          )
        # expect output is data.frame
        expect_true(
          class(gmted_covariate) == "data.frame"
        )
        # expect 2 columns
        expect_true(
          ncol(gmted_covariate) == 2
        )
        # expect numeric value
        expect_true(
          class(gmted_covariate[, 2]) == "numeric"
        )
      }
    }
  }
})

testthat::test_that("calc_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  radii <- c(0, 1000)
  locs <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(calc_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (r in seq_along(radii)) {
      narr <-
        process_narr(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          variable = variable,
          path =
          paste0(
            "../testdata/narr/",
            variable
          )
        )
      narr_covariate <-
        calc_narr(
          from = narr,
          locs = locs,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # expect output is data.frame
      expect_true(
        class(narr_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(narr_covariate) == 4
      )
      # expect numeric value
      expect_true(
        class(narr_covariate[, 4]) == "numeric"
      )
      # expect date column
      expect_true(
        class(narr_covariate[, 2]) == "Date"
      )
    }
  }
})

testthat::test_that("calc_geos returns as expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  radii <- c(0, 1000)
  locs <- data.frame(readRDS("../testdata/sites_nc.RDS"))
  # expect function
  expect_true(
    is.function(calc_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (r in seq_along(radii)) {
      geos <-
        process_geos(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          variable = "O3",
          path =
          paste0(
            "../testdata/geos/",
            collection
          )
        )
      geos_covariate <-
        calc_geos(
          from = geos,
          locs = locs,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # expect output is data.frame
      expect_true(
        class(geos_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(geos_covariate) == 4
      )
      # expect numeric value
      expect_true(
        class(geos_covariate[, 4]) == "numeric"
      )
      # expect date and time column
      expect_true(
        "POSIXt" %in% class(geos_covariate$date)
      )
    }
  }
})
