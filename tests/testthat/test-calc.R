################################################################################
##### unit and integration tests for calculate_covariates and auxiliary functions

################################################################################
##### calculate_covariates
testthat::test_that("calculate_covariates (expected errors)", {
  withr::local_package("rlang")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  candidates <-
    c("modis", "koppen-geiger",
      "koeppen-geiger", "koppen", "koeppen",
      "geos", "dummies", "gmted",
      "sedac_groads", "groads", "roads",
      "ecoregions", "ecoregion", "hms", "smoke",
      "gmted", "narr", "geos",
      "sedac_population", "population", "nlcd",
      "merra", "MERRA", "merra2", "MERRA2",
      "tri", "nei", "prism", "huc", "cdl")
  for (cand in candidates) {
    testthat::expect_error(
      calculate_covariates(covariate = cand)
    )
  }
})

testthat::test_that("calculate_covariates (no errors)", {
  withr::local_package("rlang")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"),
                keepgeom = TRUE, crs = "EPSG:4326")
  ncpt$time <- c(2018)
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )

  testthat::expect_no_error(
    tri_c <- calculate_covariates(
      covariate = "tri",
      from = tri_r,
      locs = ncpt,
      radius = 50000L
    )
  )
  testthat::expect_true(is.data.frame(tri_c))

  candidates <-
    c("modis", "koppen-geiger",
      "koeppen-geiger", "koppen", "koeppen",
      "geos", "dummies", "gmted",
      "sedac_groads", "groads", "roads",
      "ecoregions", "ecoregion", "hms", "smoke",
      "gmted", "narr", "geos",
      "sedac_population", "population", "nlcd",
      "merra", "merra2",
      "gridmet", "terraclimate",
      "tri", "nei")
  for (cand in candidates) {
    testthat::expect_error(
      calculate_covariates(covariate = cand)
    )
  }
})

################################################################################
##### calculate_lagged
testthat::test_that("calculate_lagged (geom = FALSE)", {
  withr::local_package("terra")
  withr::local_package("data.table")
  lags <- c(0, 1, 2)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_lagged)
  )
  for (l in seq_along(lags)) {
    narr <-
      process_narr(
        date = c("2018-01-01", "2018-01-10"),
        variable = "weasd",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "narr",
          "weasd"
        )
      )
    narr_covariate <-
      calculate_narr(
        from = narr,
        locs = ncp,
        locs_id = "site_id",
        radius = 0,
        fun = "mean"
      )
    # set column names
    narr_covariate <- calc_setcolumns(
      from = narr_covariate,
      lag = 0,
      dataset = "narr",
      locs_id = "site_id"
    )
    # expect identical if lag = 0
    if (lags[l] == 0) {
      narr_lagged <- calculate_lagged(
        from = narr_covariate,
        date = c("2018-01-01", "2018-01-10"),
        lag = lags[l],
        locs_id = "site_id",
        time_id = "time"
      )
      testthat::expect_identical(narr_lagged, narr_covariate)
    } else {
      # expect error because 2018-01-01 will not have lag data from 2017-12-31
      testthat::expect_error(
        calculate_lagged(
          from = narr_covariate,
          date = c("2018-01-01", "2018-01-10"),
          lag = lags[l],
          locs_id = "site_id",
          time_id = "time"
        )
      )
      narr_lagged <- calculate_lagged(
        from = narr_covariate,
        date = c("2018-01-05", "2018-01-10"),
        lag = lags[l],
        locs_id = "site_id",
        time_id = "time"
      )
      # expect output is data.frame
      testthat::expect_true(
        class(narr_lagged) == "data.frame"
      )
      # expect lag day
      testthat::expect_true(grepl("_[0-9]{1}$", colnames(narr_lagged)[3]))
      # expect no NA
      testthat::expect_true(all(!is.na(narr_lagged)))
    }
  }
})

testthat::test_that("calculate_lagged (geom = 'sf/terra')", {
  withr::local_package("terra")
  withr::local_package("data.table")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calculate_lagged)
  )
  narr <- process_narr(
    date = c("2018-01-01", "2018-01-10"),
    variable = "weasd",
    path =
      testthat::test_path(
        "..",
        "testdata",
        "narr",
        "weasd"
      )
  )
  narr_covariate <-
    calculate_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean"
    )
  # set column names
  narr_covariate <- calc_setcolumns(
    from = narr_covariate,
    lag = 0,
    dataset = "narr",
    locs_id = "site_id"
  )

  # expect error with geom = "terra" and locs as data.frame
  testthat::expect_error(
    calculate_lagged(
      from = narr_covariate,
      date = c("2018-01-02", "2018-01-04"),
      lag = 1,
      geom = "terra"
    )
  )

  # expect error with geom = "sf" and locs as data.frame
  testthat::expect_error(
    calculate_lagged(
      from = narr_covariate,
      date = c("2018-01-02", "2018-01-04"),
      lag = 1,
      geom = "sf"
    )
  )
})

################################################################################
##### calc_check_time
testthat::test_that("calc_check_time", {
  testthat::expect_error(
    # provide integer instead of data.frame to provoke error
    calc_check_time(12, TRUE)
  )
  testthat::expect_message(
    # provide data.frame without time to provoke message
    calc_check_time(
      data.frame(x = 10, y = 20),
      true
    )
  )
})

################################################################################
##### calc_message
testthat::test_that("calc_message",
  {
    testthat::expect_no_error(
      calc_message("gmted", "mean", "2020", "year", NULL)
    )
    testthat::expect_no_error(
      calc_message("narr", "shum", 2000, "year", NULL)
    )
  }
)

################################################################################
##### calc_time
testthat::test_that("calc_time", {
  testthat::expect_no_error(
    rr <- calc_time("eternal", "timeless")
  )
  testthat::expect_true(rr == "eternal")
})

################################################################################
##### calc_worker
testthat::test_that("calc_worker", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("exactextractr")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = -78.8277, lat = 35.95013, time = "boundless")
  ncp$site_id <- "3799900018810101"
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"),
                keepgeom = TRUE, crs = "EPSG:4326")
  nc <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(nc)
  nc <- terra::project(nc, "EPSG:4326")
  ncrast <- terra::rast(nc, resolution = 0.05)
  terra::values(ncrast) <- rgamma(terra::ncell(ncrast), 1, 1e-4)

  testthat::expect_no_error(
    cwres <-
      calc_worker(
        from = ncrast,
        dataset = "whatever",
        locs_vector = ncpt,
        locs_df = ncp,
        time = ncpt$time,
        time_type = "timeless",
        radius = 1e5,
        max_cells = 3e7
      )
  )
  testthat::expect_s3_class(cwres, "data.frame")
})
