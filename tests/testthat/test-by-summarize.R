################################################################################
##### .by summarization integration and coverage tests

make_test_locs_sf <- function() {
  sf::st_as_sf(
    data.frame(
      site_id = c("s1", "s2"),
      lon = c(0.25, 1.25),
      lat = c(0.50, 0.50)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
}

make_test_regions_sf <- function() {
  sf::st_as_sf(
    data.frame(
      region_id = c("west", "east"),
      wkt = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
}

make_test_raster <- function(type = c("goes", "geos", "merra2_lev", "merra2_date", "merra2_hour")) {
  type <- match.arg(type)
  r <- terra::rast(
    ncols = 2,
    nrows = 1,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:4326",
    nlyrs = 4
  )
  terra::values(r[[1]]) <- c(1, 2)
  terra::values(r[[2]]) <- c(3, 4)
  terra::values(r[[3]]) <- c(5, 6)
  terra::values(r[[4]]) <- c(7, 8)
  names(r) <- switch(
    type,
    goes = c(
      "aod_20200101_010000",
      "aod_20200101_130000",
      "aod_20200102_010000",
      "aod_20200102_130000"
    ),
    geos = c(
      "no2_lev=850_20200101_010000",
      "no2_lev=850_20200101_130000",
      "no2_lev=850_20200102_010000",
      "no2_lev=850_20200102_130000"
    ),
    merra2_lev = c(
      "pm25_lev=850_20200101_010000",
      "pm25_lev=850_20200101_130000",
      "pm25_lev=850_20200102_010000",
      "pm25_lev=850_20200102_130000"
    ),
    merra2_date = c(
      "pm25_20200101",
      "pm25_20200102",
      "pm25_20200103",
      "pm25_20200104"
    ),
    merra2_hour = c(
      "pm25_20200101_010000",
      "pm25_20200101_130000",
      "pm25_20200102_010000",
      "pm25_20200102_130000"
    )
  )
  r
}

make_test_hms <- function() {
  hms_sf <- sf::st_as_sf(
    data.frame(
      Date = c("20200101", "20200101", "20200102", "20200102"),
      Density = c("Light", "Heavy", "Light", "Heavy"),
      wkt = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
  terra::vect(hms_sf)
}

modis_mock_preprocess <- function(path, date, ...) {
  r <- terra::rast(
    ncols = 2,
    nrows = 1,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(r) <- c(10, 20)
  names(r) <- "Cloud_Fraction"
  r
}

testthat::test_that("calculate_covariates wrapper handles .by modes", {
  withr::local_options(list(sf_use_s2 = FALSE))
  from <- make_test_raster("goes")
  locs <- make_test_locs_sf()
  regions_sf <- make_test_regions_sf()
  regions_terra <- terra::vect(regions_sf)

  res_null <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0
  )
  testthat::expect_equal(nrow(res_null), 8L)

  res_id <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "site_id",
    fun_temporal = "mean"
  )
  testthat::expect_equal(nrow(res_id), 2L)

  res_temporal <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "day",
    fun_temporal = "sum"
  )
  testthat::expect_equal(nrow(res_temporal), 4L)

  res_id_time <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "site_id",
    .by_time = "day",
    fun_temporal = "sum"
  )
  testthat::expect_equal(nrow(res_id_time), 4L)

  res_sf <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    geom = "sf",
    .by = regions_sf,
    fun_temporal = "mean"
  )
  testthat::expect_true("region_id" %in% names(res_sf))
  testthat::expect_equal(nrow(res_sf), 2L)

  res_terra <- calculate_covariates(
    covariate = "goes",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    geom = "sf",
    .by = regions_terra,
    .by_time = "day",
    fun_temporal = "sum"
  )
  testthat::expect_true("region_id" %in% names(res_terra))
  testthat::expect_equal(nrow(res_terra), 4L)

  testthat::expect_error(
    calculate_covariates(
      covariate = "goes",
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = 42
    ),
    regexp = "\\.by"
  )
  testthat::expect_error(
    calculate_covariates(
      covariate = "goes",
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = "site_id",
      .by_time = 5
    ),
    regexp = "\\.by_time"
  )
})

testthat::test_that("dataset-specific .by tests for GOES GEOS MERRA2", {
  withr::local_options(list(sf_use_s2 = FALSE))
  locs <- make_test_locs_sf()
  regions_sf <- make_test_regions_sf()
  regions_terra <- terra::vect(regions_sf)

  goes <- make_test_raster("goes")
  geos <- make_test_raster("geos")
  merra2_lev <- make_test_raster("merra2_lev")
  merra2_date <- make_test_raster("merra2_date")
  merra2_hour <- make_test_raster("merra2_hour")

  dataset_runs <- list(
    list(fun = calculate_goes, from = goes),
    list(fun = calculate_geos, from = geos),
    list(fun = calculate_merra2, from = merra2_lev)
  )

  for (d in dataset_runs) {
    res_null <- d$fun(from = d$from, locs = locs, locs_id = "site_id", radius = 0)
    testthat::expect_true(nrow(res_null) >= 4L)

    res_time <- d$fun(
      from = d$from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = "day",
      fun_temporal = "sum"
    )
    testthat::expect_equal(length(unique(res_time$time)), 2L)

    res_id <- d$fun(
      from = d$from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      .by = "site_id",
      fun_temporal = "mean"
    )
    testthat::expect_equal(nrow(res_id), 2L)

    res_sf <- d$fun(
      from = d$from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      geom = "sf",
      .by = regions_sf,
      .by_time = "day",
      fun_temporal = "sum"
    )
    testthat::expect_true("region_id" %in% names(res_sf))
    testthat::expect_equal(nrow(res_sf), 4L)

    res_terra <- d$fun(
      from = d$from,
      locs = locs,
      locs_id = "site_id",
      radius = 0,
      geom = "sf",
      .by = regions_terra,
      fun_temporal = "mean"
    )
    testthat::expect_true("region_id" %in% names(res_terra))
    testthat::expect_equal(nrow(res_terra), 2L)
  }

  # cover all merra2 time parsing branches
  testthat::expect_s3_class(
    calculate_merra2(from = merra2_date, locs = locs, locs_id = "site_id", radius = 0),
    "data.frame"
  )
  testthat::expect_s3_class(
    calculate_merra2(from = merra2_hour, locs = locs, locs_id = "site_id", radius = 0),
    "data.frame"
  )
})

testthat::test_that("calculate_hms supports .by combinations and defaults", {
  withr::local_options(list(sf_use_s2 = FALSE))
  locs <- make_test_locs_sf()
  from_hms <- make_test_hms()

  res_default <- calculate_hms(
    from = from_hms,
    locs = locs,
    locs_id = "site_id",
    radius = 0
  )
  testthat::expect_true(nrow(res_default) >= 4L)

  res_time <- calculate_hms(
    from = from_hms,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "day",
    fun_temporal = "sum"
  )
  testthat::expect_true("time" %in% names(res_time))

  res_id_time <- calculate_hms(
    from = from_hms,
    locs = locs,
    locs_id = "site_id",
    radius = 0,
    .by = "site_id",
    .by_time = "day",
    fun_temporal = "sum"
  )
  testthat::expect_equal(nrow(res_id_time), 4L)

  no_smoke_dates <- c("2020-01-01", "2020-01-02")
  res_no_smoke <- calculate_hms(
    from = no_smoke_dates,
    locs = data.frame(site_id = c("s1", "s2"), lon = c(0.1, 1.1), lat = c(0.2, 0.2)),
    locs_id = "site_id",
    radius = 0,
    .by = "day",
    fun_temporal = "sum"
  )
  testthat::expect_true(is.data.frame(res_no_smoke))
})

testthat::test_that("calculate_modis supports .by options and errors", {
  withr::local_options(list(sf_use_s2 = FALSE))
  locs <- make_test_locs_sf()
  fake_paths <- c("MOD.A2020001.h00v00.fake.hdf", "MOD.A2020002.h00v00.fake.hdf")

  res_null <- calculate_modis(
    from = fake_paths,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = modis_mock_preprocess,
    name_covariates = "cloud_fraction_",
    subdataset = "Cloud_Fraction",
    fun_summary = "mean",
    geom = FALSE,
    scale = "* 1"
  )
  testthat::expect_true(is.data.frame(res_null))

  res_by <- calculate_modis(
    from = fake_paths,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    preprocess = modis_mock_preprocess,
    name_covariates = "cloud_fraction_",
    subdataset = "Cloud_Fraction",
    fun_summary = "mean",
    fun_temporal = "mean",
    .by = "day",
    geom = FALSE,
    scale = "* 1"
  )
  testthat::expect_true(is.data.frame(res_by))
  testthat::expect_true("time" %in% names(res_by))

  testthat::expect_error(
    calculate_modis(
      from = fake_paths,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      preprocess = modis_mock_preprocess,
      name_covariates = "cloud_fraction_",
      subdataset = "Cloud_Fraction",
      fun_summary = "mean",
      fun_temporal = "not_a_function",
      .by = "day",
      geom = FALSE,
      scale = "* 1"
    ),
    regexp = "must be one of"
  )
})

testthat::test_that("auxiliary .by helpers hit major branches and errors", {
  df <- data.frame(
    site_id = c("s1", "s1", "s2", "s2"),
    time = as.POSIXct(
      c("2020-01-01 01:00", "2020-01-01 12:00", "2020-01-02 01:00", "2020-01-02 12:00"),
      tz = "UTC"
    ),
    value = c(1, 2, 3, 4),
    geometry = c(
      "POINT (0.2 0.5)",
      "POINT (0.2 0.5)",
      "POINT (1.2 0.5)",
      "POINT (1.2 0.5)"
    )
  )
  regions_sf <- make_test_regions_sf()

  testthat::expect_equal(normalize_by_time_unit("day"), "day")
  testthat::expect_equal(normalize_by_time_unit("days"), "day")
  testthat::expect_error(normalize_by_time_unit("fortnight"), regexp = "time-unit")

  minute_bucket <- bucket_time_by_unit(df$time, "minute")
  hour_bucket <- bucket_time_by_unit(df$time, "hour")
  day_bucket <- bucket_time_by_unit(df$time, "day")
  quarter_bucket <- bucket_time_by_unit(df$time, "quarter")
  testthat::expect_s3_class(minute_bucket, "POSIXct")
  testthat::expect_s3_class(hour_bucket, "POSIXct")
  testthat::expect_s3_class(day_bucket, "Date")
  testthat::expect_s3_class(quarter_bucket, "Date")

  testthat::expect_error(check_by("missing_col", data = data.frame(x = 1)), regexp = "not found")
  testthat::expect_warning(check_by(NULL, .by_time = "time"), regexp = "ignored")
  testthat::expect_error(check_by(.by = c("a", "b")), regexp = "\\.by")

  by_time <- calc_summarize_by(df, .by = "day", fun_summary = "sum", locs_id = "site_id")
  testthat::expect_equal(nrow(by_time), 2L)
  by_col <- calc_summarize_by(df, .by = "site_id", .by_time = "day", fun_summary = "mean")
  testthat::expect_equal(nrow(by_col), 2L)
  by_space <- calc_summarize_by(df, .by = regions_sf, .by_time = "day", fun_summary = "sum")
  testthat::expect_true("region_id" %in% names(by_space))

  testthat::expect_error(
    calc_summarize_by(df[, c("site_id", "time", "value")], .by = regions_sf, fun_summary = "sum"),
    regexp = "geometry"
  )
  testthat::expect_error(
    calc_summarize_by(df, .by = "site_id", .by_time = "fortnight", fun_summary = "sum"),
    regexp = "\\.by_time"
  )
  testthat::expect_error(
    calc_summarize_by(df, .by = "site_id", fun_summary = 1),
    regexp = "character string or function"
  )
  testthat::expect_error(
    calc_summarize_by(df[, c("site_id", "time")], .by = "site_id", fun_summary = "sum"),
    regexp = "No numeric covariate columns"
  )
})

testthat::test_that("backend compatibility: purrr", {
  withr::local_options(list(sf_use_s2 = FALSE))
  df <- data.frame(
    site_id = c("s1", "s1", "s2", "s2"),
    time = as.POSIXct(
      c("2020-01-01 01:00", "2020-01-01 12:00", "2020-01-02 01:00", "2020-01-02 12:00"),
      tz = "UTC"
    ),
    value = c(1, 2, 3, 4)
  )

  testthat::skip_if_not_installed("purrr")
  purrr_out <- purrr::map(
    list("day", "site_id"),
    \(x) calc_summarize_by(df, .by = x, .by_time = if (x == "site_id") "day" else NULL, fun_summary = "sum")
  )
  testthat::expect_length(purrr_out, 2L)
})

testthat::test_that("backend compatibility: future", {
  withr::local_options(list(sf_use_s2 = FALSE))
  df <- data.frame(
    site_id = c("s1", "s1", "s2", "s2"),
    time = as.POSIXct(
      c("2020-01-01 01:00", "2020-01-01 12:00", "2020-01-02 01:00", "2020-01-02 12:00"),
      tz = "UTC"
    ),
    value = c(1, 2, 3, 4)
  )
  testthat::skip_if_not_installed("future")
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::sequential)
  sum_fun <- calc_summarize_by
  fut <- future::future(
    sum_fun(df, .by = "day", fun_summary = "sum", locs_id = "site_id"),
    seed = TRUE
  )
  fut_out <- future::value(fut)
  testthat::expect_true(is.data.frame(fut_out))
})

testthat::test_that("backend compatibility: crew", {
  withr::local_options(list(sf_use_s2 = FALSE))
  testthat::skip_if_not_installed("crew")
  ctrl <- crew::crew_controller_local(workers = 1L, seconds_idle = 1)
  on.exit(ctrl$terminate(), add = TRUE)
  ctrl$start()
  ctrl$push(
    command = quote(amadeus:::calc_summarize_by(
      covar = data.frame(
        site_id = c("s1", "s1"),
        time = as.POSIXct(c("2020-01-01 01:00", "2020-01-01 02:00"), tz = "UTC"),
        value = c(1, 2)
      ),
      .by = "day",
      fun_summary = "sum",
      locs_id = "site_id"
    )),
    name = "crew_by_test"
  )
  ctrl$wait()
  crew_res <- ctrl$pop()
  testthat::expect_equal(crew_res$status[[1]], "success")
})

testthat::test_that("backend compatibility: targets tar_script", {
  withr::local_options(list(sf_use_s2 = FALSE))
  testthat::skip_if_not_installed("targets")
  withr::with_tempdir({
    targets::tar_script({
      library(amadeus)
      library(terra)
      library(sf)
      list(
        targets::tar_target(
          by_res,
          {
            locs_obj <- sf::st_as_sf(
              data.frame(
                site_id = c("s1", "s2"),
                lon = c(0.25, 1.25),
                lat = c(0.5, 0.5)
              ),
              coords = c("lon", "lat"),
              crs = 4326
            )
            from_obj <- terra::rast(
              ncols = 2, nrows = 1, xmin = 0, xmax = 2, ymin = 0, ymax = 1,
              crs = "EPSG:4326", nlyrs = 2
            )
            terra::values(from_obj[[1]]) <- c(1, 2)
            terra::values(from_obj[[2]]) <- c(3, 4)
            names(from_obj) <- c("aod_20200101_010000", "aod_20200101_130000")
            calculate_goes(
              from = from_obj,
              locs = locs_obj,
              locs_id = "site_id",
              radius = 0,
              .by = "day",
              fun_temporal = "sum"
            )
          },
          format = "rds"
        )
      )
    }, ask = FALSE)
    targets::tar_make(callr_function = NULL)
    tar_out <- targets::tar_read(by_res)
    testthat::expect_true(is.data.frame(tar_out))
    testthat::expect_true("time" %in% names(tar_out))
  })
})

################################################################################
##### additional branch coverage for check_by and calc_summarize_by

testthat::test_that("check_by errors when data is not a data.frame", {
  testthat::expect_error(
    check_by("x", data = list(x = 1, y = 2)),
    "must be a data.frame"
  )
})

testthat::test_that("calc_summarize_by missed branches", {
  withr::local_options(list(sf_use_s2 = FALSE))

  df <- data.frame(
    site_id = c("s1", "s1", "s2", "s2"),
    time = as.POSIXct(
      c("2020-01-01 01:00", "2020-01-01 12:00", "2020-01-02 01:00", "2020-01-02 12:00"),
      tz = "UTC"
    ),
    value = c(1, 2, 3, 4),
    geometry = c(
      "POINT (0.2 0.5)",
      "POINT (0.2 0.5)",
      "POINT (1.2 0.5)",
      "POINT (1.2 0.5)"
    )
  )

  # line 951: fun_summary as character vector of length > 1
  testthat::expect_error(
    calc_summarize_by(df, .by = "day", fun_summary = c("mean", "sum"), locs_id = "site_id"),
    "single function name"
  )

  # line 955: fun_summary as a function object
  by_fun <- calc_summarize_by(df, .by = "day", fun_summary = mean, locs_id = "site_id")
  testthat::expect_s3_class(by_fun, "data.frame")
  testthat::expect_equal(nrow(by_fun), 2L)

  # line 970: .by_time is an existing column name in covar
  df2 <- df
  df2$period <- as.Date(df2$time)
  by_col_bytime <- calc_summarize_by(df2, .by = "site_id", .by_time = "period",
                                     fun_summary = "mean")
  testthat::expect_s3_class(by_col_bytime, "data.frame")
  testthat::expect_true("period" %in% names(by_col_bytime))

  # line 979: .by_time is a unit alias but time_col not present in covar
  df3 <- df[, c("site_id", "value")]  # no 'time' column
  df3$grp <- c("A", "A", "B", "B")
  testthat::expect_error(
    calc_summarize_by(df3, .by = "grp", .by_time = "day", fun_summary = "mean"),
    "time_col"
  )

  # line 992: by_kind == "time_unit" but locs_id column missing
  df4 <- df[, c("time", "value")]
  testthat::expect_error(
    calc_summarize_by(df4, .by = "day", fun_summary = "mean", locs_id = "site_id"),
    "locs_id"
  )

  # line 996: by_kind == "time_unit" but time_col missing
  df5 <- df[, c("site_id", "value")]
  testthat::expect_error(
    calc_summarize_by(df5, .by = "day", fun_summary = "mean",
                      locs_id = "site_id", time_col = "time"),
    "time column"
  )

  # line 1016: spatial .by object with no identifier columns (only geometry)
  regions_geom_only <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  regions_sf_noattr <- sf::st_sf(geometry = regions_geom_only)
  testthat::expect_error(
    calc_summarize_by(df, .by = regions_sf_noattr, fun_summary = "sum"),
    "identifier column"
  )

  # line 1029: CRS mismatch triggers st_transform
  regions_sf_3857 <- sf::st_transform(make_test_regions_sf(), 3857)
  by_space_crs <- calc_summarize_by(df, .by = regions_sf_3857,
                                    .by_time = "day", fun_summary = "sum")
  testthat::expect_true("region_id" %in% names(by_space_crs))

  # lines 1053-1058: missing grouping column via group_cols_extra
  testthat::expect_error(
    calc_summarize_by(df, .by = "site_id", fun_summary = "sum",
                      group_cols_extra = "no_such_col"),
    "Grouping column"
  )
})

testthat::test_that("calc_summarize_by CRS mismatch triggers st_transform", {
  withr::local_options(list(sf_use_s2 = FALSE))

  # Covar with geometry WKT in EPSG:4326
  df_4326 <- data.frame(
    site_id = c("s1", "s2"),
    time = as.POSIXct(c("2020-01-01 00:00", "2020-01-02 00:00"), tz = "UTC"),
    value = c(1.0, 2.0),
    geometry = c("POINT (0.25 0.5)", "POINT (1.25 0.5)")
  )

  # Spatial .by in EPSG:3857 (different CRS)
  regions_3857 <- sf::st_transform(
    sf::st_as_sf(
      data.frame(
        region_id = "r1",
        wkt = "POLYGON ((0 0, 2 0, 2 1, 0 1, 0 0))"
      ),
      wkt = "wkt", crs = 4326
    ),
    3857
  )

  # Mock sf::st_as_sf so covar gets CRS 4326 (different from regions_3857's 3857)
  original_st_as_sf <- sf::st_as_sf
  call_count <- 0L
  testthat::local_mocked_bindings(
    st_as_sf = function(x, ...) {
      call_count <<- call_count + 1L
      result <- original_st_as_sf(x, ...)
      # On the first call (covar), set CRS to 4326
      if (call_count == 1L && inherits(result, "sf")) {
        sf::st_crs(result) <- 4326
      }
      result
    },
    .package = "sf"
  )

  result <- calc_summarize_by(df_4326, .by = regions_3857,
                               fun_summary = "mean", .by_time = "day")
  testthat::expect_s3_class(result, "data.frame")
})
