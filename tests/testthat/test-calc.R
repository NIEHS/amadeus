################################################################################
##### unit and integration test for calculate_covariates and auxiliary functions

################################################################################
##### calculate_covariates
testthat::test_that("calculate_covariates (expected errors)", {
  withr::local_package("rlang")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))

  candidates <-
    c(
      "modis",
      "koppen-geiger",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "dummies",
      "gmted",
      "sedac_groads",
      "groads",
      "roads",
      "ecoregions",
      "ecoregion",
      "hms",
      "smoke",
      "gmted",
      "narr",
      "geos",
      "sedac_population",
      "population",
      "nlcd",
      "merra",
      "MERRA",
      "merra2",
      "MERRA2",
      "tri",
      "nei",
      "mcd14dl",
      "edgar",
      "prism",
      "huc",
      "cdl",
      "goes",
      "goes_adp",
      "GOES",
      "improve",
      "IMPROVE"
    )
  for (cand in candidates) {
    testthat::expect_error(
      suppressWarnings(calculate_covariates(covariate = cand))
    )
  }
})

testthat::test_that("calculate_covariates (no errors)", {
  withr::local_package("rlang")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"), keepgeom = TRUE, crs = "EPSG:4326")
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

  withr::with_tempdir({
    edgar_raster <- terra::rast(
      ncols = 3,
      nrows = 2,
      xmin = -80,
      xmax = -77,
      ymin = 35,
      ymax = 37,
      crs = "EPSG:4326"
    )
    terra::values(edgar_raster) <- seq_len(terra::ncell(edgar_raster))
    names(edgar_raster) <- "emi_nox"
    edgar_path <- file.path(".", "edgar_2021_total_emi.tif")
    terra::writeRaster(edgar_raster, edgar_path, overwrite = TRUE)
    edgar_r <- process_edgar(path = edgar_path)

    testthat::expect_no_error(
      edgar_c <- calculate_covariates(
        covariate = "edgar",
        from = edgar_r,
        locs = ncpt,
        radius = 0
      )
    )
    testthat::expect_true(is.data.frame(edgar_c))

    edgar_polygons <- terra::as.polygons(edgar_raster)
    edgar_polygons <- sf::st_as_sf(edgar_polygons[1:2, ])
    edgar_polygons$site_id <- c("poly_1", "poly_2")

    testthat::expect_no_error(
      edgar_poly_c <- calculate_covariates(
        covariate = "edgar",
        from = edgar_r,
        locs = edgar_polygons,
        locs_id = "site_id",
        radius = 0,
        geom = "sf"
      )
    )
    testthat::expect_s3_class(edgar_poly_c, "sf")
    testthat::expect_equal(nrow(edgar_poly_c), 2)
  })

  withr::with_tempdir({
    mcd14dl_path <- file.path(".", "MODIS_C6_1_Global_MCD14DL_NRT_2026074.txt")
    data.table::fwrite(
      data.frame(
        latitude = 35.95013,
        longitude = -78.8277,
        acq_date = "2026-03-15",
        acq_time = 1230,
        frp = 11.5
      ),
      mcd14dl_path
    )
    mcd14dl_r <- amadeus:::process_mcd14dl(
      path = mcd14dl_path,
      date = "2026-03-15"
    )

    mcd14dl_c <- calculate_covariates(
      covariate = "mcd14dl",
      from = mcd14dl_r,
      locs = ncpt,
      locs_id = "site_id",
      radius = 0
    )
    testthat::expect_true(is.data.frame(mcd14dl_c))
    testthat::expect_equal(mcd14dl_c$fire_count_00000, 1)
  })

  withr::with_tempdir({
    fire_points <- terra::vect(
      data.frame(
        longitude = c(-78.8277, -78.82),
        latitude = c(35.95013, 35.955),
        time = c(20260315L, 20260315L),
        fire_count = c(1L, 1L),
        frp = c(11.5, 5)
      ),
      geom = c("longitude", "latitude"),
      keepgeom = TRUE,
      crs = "EPSG:4326"
    )
    locs_sf <- sf::st_as_sf(
      data.frame(
        site_id = "site_1",
        lon = -78.8277,
        lat = 35.95013
      ),
      coords = c("lon", "lat"),
      crs = 4326
    )

    direct_calc <- amadeus:::calculate_mcd14dl(
      from = fire_points,
      locs = locs_sf,
      locs_id = "site_id",
      radius = c(0L, 1000L),
      geom = "sf"
    )
    testthat::expect_s3_class(direct_calc, "sf")
    testthat::expect_equal(direct_calc$fire_count_00000, 1)
    testthat::expect_equal(direct_calc$fire_count_01000, 2)
    testthat::expect_equal(direct_calc$frp_01000, 16.5)

    testthat::expect_error(
      amadeus:::calculate_mcd14dl(from = terra::rast(), locs = locs_sf),
      "SpatVector returned by process_mcd14dl"
    )
    testthat::expect_error(
      amadeus:::calculate_mcd14dl(
        from = fire_points[, c("time", "fire_count")],
        locs = locs_sf
      ),
      "missing required MCD14DL fields"
    )
    testthat::expect_error(
      amadeus:::calculate_mcd14dl(from = fire_points, locs = list()),
      "convertible to sf"
    )
  })

  candidates <-
    c(
      "modis",
      "koppen-geiger",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "dummies",
      "gmted",
      "sedac_groads",
      "groads",
      "roads",
      "ecoregions",
      "ecoregion",
      "hms",
      "smoke",
      "gmted",
      "narr",
      "geos",
      "sedac_population",
      "population",
      "nlcd",
      "merra",
      "merra2",
      "gridmet",
      "terraclimate",
      "tri",
      "nei",
      "mcd14dl",
      "edgar"
    )
  for (cand in candidates) {
    testthat::expect_error(
      suppressWarnings(calculate_covariates(covariate = cand))
    )
  }

  testthat::expect_error(calculate_covariates(covariate = "aqs"))
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
        path = testthat::test_path(
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
    path = testthat::test_path(
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
      fun = "mean",
      geom = "terra"
    )

  # expect error with geom = "terra" and locs as data.frame
  testthat::expect_error(
    calculate_lagged(
      from = data.frame(narr_covariate),
      date = c("2018-01-02", "2018-01-04"),
      lag = 1,
      geom = "terra",
      locs_id = "site_id"
    )
  )

  # expect error with geom = "sf" and locs as data.frame
  testthat::expect_error(
    calculate_lagged(
      from = data.frame(narr_covariate),
      date = c("2018-01-02", "2018-01-04"),
      lag = 1,
      geom = "sf",
      locs_id = "site_id"
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
testthat::test_that("calc_message", {
  testthat::expect_no_error(
    calc_message("gmted", "mean", "2020", "year", NULL)
  )
  testthat::expect_no_error(
    calc_message("narr", "shum", 2000, "year", NULL)
  )
})

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
    terra::vect(ncp, geom = c("lon", "lat"), keepgeom = TRUE, crs = "EPSG:4326")
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

################################################################################
##### calc_summarize_temporal
testthat::test_that("calc_summarize_temporal returns input when NULL", {
  df <- data.frame(
    site_id = c("A", "A", "B"),
    time = as.POSIXct(
      c("2020-01-01 06:00", "2020-01-01 18:00", "2020-01-01 06:00"),
      tz = "UTC"
    ),
    pm25_0 = c(10, 20, 5)
  )
  result <- calc_summarize_temporal(df, fun_temporal = NULL)
  testthat::expect_identical(result, df)
})

testthat::test_that("calc_summarize_temporal daily mean", {
  df <- data.frame(
    site_id = c("A", "A", "B", "B"),
    time = as.POSIXct(
      c(
        "2020-01-01 06:00",
        "2020-01-01 18:00",
        "2020-01-02 06:00",
        "2020-01-02 18:00"
      ),
      tz = "UTC"
    ),
    pm25_0 = c(10, 20, 5, 15)
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "mean",
    locs_id = "site_id"
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true("time" %in% names(result))
  testthat::expect_true("pm25_0" %in% names(result))
  testthat::expect_equal(
    result[result$site_id == "A", "pm25_0"],
    15
  )
  testthat::expect_equal(
    result[result$site_id == "B", "pm25_0"],
    10
  )
  testthat::expect_s3_class(result$time, "Date")
})

testthat::test_that("calc_summarize_temporal preserves geometry", {
  df <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXct(
      c("2020-06-01 00:00", "2020-06-01 12:00"),
      tz = "UTC"
    ),
    pm25_0 = c(8, 12),
    geometry = c(
      "POINT (-80 35)",
      "POINT (-80 35)"
    )
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "mean",
    locs_id = "site_id"
  )
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true("geometry" %in% names(result))
  testthat::expect_equal(result$geometry, "POINT (-80 35)")
  testthat::expect_equal(result$pm25_0, 10)
})

testthat::test_that("calc_summarize_temporal group_cols_extra", {
  df <- data.frame(
    site_id = c("A", "A", "A", "A"),
    time = as.POSIXct(
      c(
        "2020-01-01 06:00",
        "2020-01-01 18:00",
        "2020-01-01 06:00",
        "2020-01-01 18:00"
      ),
      tz = "UTC"
    ),
    level = c("850", "850", "500", "500"),
    pm_0 = c(10, 20, 30, 40)
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "mean",
    locs_id = "site_id",
    group_cols_extra = "level"
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true("level" %in% names(result))
  lev850 <- result[result$level == "850", "pm_0"]
  lev500 <- result[result$level == "500", "pm_0"]
  testthat::expect_equal(lev850, 15)
  testthat::expect_equal(lev500, 35)
})

testthat::test_that("calc_summarize_temporal time_bucket month", {
  df <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXct(
      c("2020-01-10", "2020-01-20"),
      tz = "UTC"
    ),
    v_0 = c(4, 8)
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "sum",
    locs_id = "site_id",
    time_bucket = "month"
  )
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$v_0, 12)
})

testthat::test_that("calc_summarize_temporal time_bucket week", {
  df <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXct(
      c("2020-06-01", "2020-06-03"),
      tz = "UTC"
    ),
    v_0 = c(2, 8)
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "mean",
    locs_id = "site_id",
    time_bucket = "week"
  )
  # Both dates fall in the same ISO week → 1 row
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$v_0, 5)
})

testthat::test_that("calc_summarize_temporal time_bucket year", {
  df <- data.frame(
    site_id = c("A", "A"),
    time = as.POSIXct(
      c("2020-03-15", "2020-09-20"),
      tz = "UTC"
    ),
    v_0 = c(4, 8)
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "sum",
    locs_id = "site_id",
    time_bucket = "year"
  )
  # Both dates in the same year → 1 row
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$v_0, 12)
})

testthat::test_that("calc_summarize_temporal single-row input returns 1 row", {
  df <- data.frame(
    site_id = "A",
    time = as.POSIXct("2020-01-01 06:00", tz = "UTC"),
    v_0 = 7
  )
  result <- calc_summarize_temporal(
    df,
    fun_temporal = "mean",
    locs_id = "site_id"
  )
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$v_0, 7)
})

testthat::test_that("calc_summarize_temporal errors on bad args", {
  df <- data.frame(
    site_id = "A",
    time = Sys.time(),
    v = 1
  )
  testthat::expect_error(
    calc_summarize_temporal(df, "mean", locs_id = "missing_col"),
    regexp = "locs_id"
  )
  testthat::expect_error(
    calc_summarize_temporal(df, "mean", time_col = "no_such"),
    regexp = "time_col"
  )
  testthat::expect_error(
    calc_summarize_temporal(
      df, "mean",
      group_cols_extra = "not_a_col"
    ),
    regexp = "not_a_col"
  )
  df_nocov <- data.frame(site_id = "A", time = Sys.time())
  testthat::expect_error(
    calc_summarize_temporal(df_nocov, "mean"),
    regexp = "No covariate"
  )
})

##### check_fun_temporal

testthat::test_that("check_fun_temporal accepts NULL and valid strings", {
  testthat::expect_null(check_fun_temporal(NULL))
  testthat::expect_null(check_fun_temporal("mean"))
  testthat::expect_null(check_fun_temporal("median"))
  testthat::expect_null(check_fun_temporal("sum"))
  testthat::expect_null(check_fun_temporal("max"))
  testthat::expect_null(check_fun_temporal("min"))
})

testthat::test_that("check_fun_temporal errors on non-character input", {
  testthat::expect_error(
    check_fun_temporal(42),
    regexp = "single"
  )
  testthat::expect_error(
    check_fun_temporal(c("mean", "sum")),
    regexp = "single"
  )
})

testthat::test_that("check_fun_temporal errors on unknown function name", {
  testthat::expect_error(
    check_fun_temporal("variance"),
    regexp = "must be one of"
  )
})
