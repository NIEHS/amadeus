# Tests for covariate generation functions

testthat::test_that("import_hms returns expected.", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  buffers <- c(0, 1000)
  sites <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(covar_hms)
  )
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (b in seq_along(buffers)) {
      hms <-
        import_hms_explore(
          date_start = "2018-12-30",
          date_end = "2019-01-01",
          variable = density,
          directory_with_data =
          "../testdata/hms/hms/"
        )
      hms_covar <-
        covar_hms(
          data = hms,
          sites = sites,
          identifier = "site_id",
          buffer = buffers[b]
        )
      # expect output is data.frame
      expect_true(
        class(hms_covar) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covar) == 3
      )
      # expect 9 rows
      expect_true(
        nrow(hms_covar) == 9
      )
      # expect numeric value
      expect_true(
        class(hms_covar[, 3]) == "numeric"
      )
      # expect binary
      expect_true(
        all(unique(hms_covar[, 3]) %in% c(0, 1))
      )
    }
  }
})

testthat::test_that("covar_gmted returns expected.", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  buffers <- c(0, 1000)
  sites <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(covar_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      for (b in seq_along(buffers)) {
        gmted <-
          import_gmted(
            variable = c(statistic, resolution),
            directory_with_data =
            "../testdata/gmted/gmted"
          )
        gmted_covar <-
          covar_gmted(
            data = gmted,
            sites = sites,
            identifier = "site_id",
            buffer = buffers[b],
            fun = "mean"
          )
        # expect output is data.frame
        expect_true(
          class(gmted_covar) == "data.frame"
        )
        # expect 2 columns
        expect_true(
          ncol(gmted_covar) == 2
        )
        # expect numeric value
        expect_true(
          class(gmted_covar[, 2]) == "numeric"
        )
      }
    }
  }
})

testthat::test_that("covar_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  buffers <- c(0, 1000)
  sites <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(covar_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (b in seq_along(buffers)) {
      narr <-
        import_narr(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          variable = variable,
          directory_with_data =
          paste0(
            "../testdata/narr/",
            variables[v]
          )
        )
      narr_covar <-
        covar_narr(
          data = narr,
          sites = sites,
          identifier = "site_id",
          buffer = buffers[b],
          fun = "mean"
        )
      # expect output is data.frame
      expect_true(
        class(narr_covar) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(narr_covar) == 4
      )
      # expect numeric value
      expect_true(
        class(narr_covar[, 4]) == "numeric"
      )
      # expect date column
      expect_true(
        class(narr_covar[, 2]) == "Date"
      )
    }
  }
})

testthat::test_that("covar_geos returns as expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  buffers <- c(0, 1000)
  sites <- data.frame(readRDS("../testdata/sites_nc.RDS"))
  # expect function
  expect_true(
    is.function(covar_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (b in seq_along(buffers)) {
      geos <-
        import_geos(
          date_start = "2018-01-01",
          date_end = "2018-01-01",
          variable = "O3",
          directory_with_data =
          paste0(
            "../testdata/geos/",
            collection
          )
        )
      geos_covar <-
        covar_geos(
          data = geos,
          sites = sites,
          identifier = "site_id",
          buffer = buffers[b],
          fun = "mean"
        )
      # expect output is data.frame
      expect_true(
        class(geos_covar) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(geos_covar) == 4
      )
      # expect numeric value
      expect_true(
        class(geos_covar[, 4]) == "numeric"
      )
      # expect date and time column
      expect_true(
        "POSIXt" %in% class(geos_covar$date)
      )
    }
  }
})
