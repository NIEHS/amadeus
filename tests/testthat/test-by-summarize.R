################################################################################
##### .by_time summarization integration tests

make_test_locs_sf <- function() {
  sf::st_as_sf(
    data.frame(site_id = c("s1", "s2"), lon = c(0.25, 1.25), lat = c(0.50, 0.50)),
    coords = c("lon", "lat"), crs = 4326
  )
}

make_test_raster <- function(type = c("goes", "geos", "merra2_lev")) {
  type <- match.arg(type)
  r <- terra::rast(ncols = 2, nrows = 1, xmin = 0, xmax = 2, ymin = 0, ymax = 1, crs = "EPSG:4326", nlyrs = 4)
  terra::values(r[[1]]) <- c(1, 2)
  terra::values(r[[2]]) <- c(3, 4)
  terra::values(r[[3]]) <- c(5, 6)
  terra::values(r[[4]]) <- c(7, 8)
  names(r) <- switch(
    type,
    goes = c("aod_20200101_010000", "aod_20200101_130000", "aod_20200102_010000", "aod_20200102_130000"),
    geos = c("no2_lev=850_20200101_010000", "no2_lev=850_20200101_130000", "no2_lev=850_20200102_010000", "no2_lev=850_20200102_130000"),
    merra2_lev = c("pm25_lev=850_20200101_010000", "pm25_lev=850_20200101_130000", "pm25_lev=850_20200102_010000", "pm25_lev=850_20200102_130000")
  )
  r
}

testthat::test_that("calc_summarize_by supports .by_time temporal-only summarization", {
  df <- data.frame(
    site_id = c("A", "A", "B", "B"),
    level = c("850", "850", "850", "850"),
    time = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 12:00", "2020-01-02 00:00", "2020-01-02 12:00"), tz = "UTC"),
    value = c(1, 3, 2, 4)
  )

  out_day <- calc_summarize_by(df, .by_time = "day", fun_summary = "mean", locs_id = "site_id", group_cols_extra = "level")
  testthat::expect_equal(nrow(out_day), 2L)
  testthat::expect_true(all(c("site_id", "time", "level", "value") %in% names(out_day)))

  out_none <- calc_summarize_by(df, .by_time = NULL, fun_summary = "mean", locs_id = "site_id")
  testthat::expect_identical(out_none, df)
})

testthat::test_that("calculate APIs reject deprecated .by argument", {
  locs <- make_test_locs_sf()
  from <- make_test_raster("goes")

  testthat::expect_error(
    calculate_covariates(covariate = "goes", from = from, locs = locs, locs_id = "site_id", radius = 0, .by = "day"),
    regexp = "no longer supported"
  )

  testthat::expect_error(
    calculate_goes(from = from, locs = locs, locs_id = "site_id", radius = 0, .by = "day"),
    regexp = "no longer supported"
  )
})

testthat::test_that("dataset-specific temporal summarization works via .by_time", {
  locs <- make_test_locs_sf()

  goes <- calculate_goes(from = make_test_raster("goes"), locs = locs, locs_id = "site_id", radius = 0, .by_time = "day")
  geos <- calculate_geos(from = make_test_raster("geos"), locs = locs, locs_id = "site_id", radius = 0, .by_time = "day")
  merra2 <- calculate_merra2(from = make_test_raster("merra2_lev"), locs = locs, locs_id = "site_id", radius = 0, .by_time = "day")

  testthat::expect_equal(length(unique(goes$time)), 2L)
  testthat::expect_equal(length(unique(geos$time)), 2L)
  testthat::expect_equal(length(unique(merra2$time)), 2L)
})
