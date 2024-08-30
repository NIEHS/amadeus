################################################################################
##### unit and integration tests for process_covariates and auxiliary functions
# nolint start

################################################################################
##### process_covariates
testthat::test_that("process_covariates", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # main test
  testthat::expect_no_error(
    covar <- process_covariates(
      covariate = "tri",
      path = testthat::test_path(
        "..",
        "testdata",
        "tri",
        ""
      )
    )
  )
  testthat::expect_no_error(
    covar <- process_covariates(
      covariate = "TRI",
      path = testthat::test_path(
        "..",
        "testdata",
        "tri",
        ""
      )
    )
  )
  # expect
  testthat::expect_s4_class(covar, "SpatVector")

  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      "^VNP46A2",
      full.names = TRUE
    )

  corn <- process_blackmarble_corners()
  testthat::expect_warning(
    bm_proc <- process_covariates(
      covariate = "blackmarble",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_warning(
    process_covariates(
      covariate = "Blackmarble",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_warning(
    process_covariates(
      covariate = "BLACKMARBLE",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_s4_class(bm_proc, "SpatRaster")

  covar_types <- c("modis_swath", "modis_merge",
                   "koppen-geiger",
                   "blackmarble",
                   "koeppen-geiger", "koppen", "koeppen",
                   "geos", "dummies", "gmted",
                   "hms", "smoke",
                   "sedac_population", "population",
                   "sedac_groads", "groads", "roads",
                   "nlcd", "narr", "nei",
                   "ecoregions", "ecoregion", "huc", "cropscape", "cdl",
                   "prism", "terraclimate", "gridmet")
  for (cty in covar_types) {
    testthat::expect_error(
      process_covariates(
        covariate = cty
      )
    )
  }

  # match.args works?
  testthat::expect_error(
    process_covariates(covariate = "MODIS_ALL")
  )
})

################################################################################
##### process_conformity
testthat::test_that("process_conformity", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100
  dfsf <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )

  testthat::expect_no_error(
    process_conformity(locs = df, check_time = TRUE)
  )
  testthat::expect_no_error(
    process_conformity(locs = dfsf, check_time = TRUE)
  )
  testthat::expect_no_error(
    process_conformity(locs = df, check_time = FALSE)
  )
  # error cases
  dfe <- df
  names(dfe)[3] <- "date"
  testthat::expect_error(
    process_conformity(locs = dfe, check_time = TRUE)
  )

})

################################################################################
##### process_collection
testthat::test_that("process_collection", {
  path <- list.files(
    testthat::test_path(
      "..",
      "testdata",
      "geos",
      "a"
    ),
    full.names = TRUE
  )
  expect_error(
    process_collection(
      path = path,
      source = "geos",
      collection = TRUE,
      date = TRUE,
      datetime = TRUE
    )
  )
  path_split_d <- process_collection(
    path = path,
    source = "geos",
    date = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- process_collection(
    path = path,
    source = "geos",
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})

################################################################################
##### process_locs_vector
testthat::test_that("process_locs_vector", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  narr <-
    process_narr(
      date = c("2018-01-01", "2018-01-01"),
      variable = "weasd",
      path = testthat::test_path(
        "..",
        "testdata",
        "narr",
        "weasd"
      )
    )
  # expect error when missing `lat` or `lon`
  expect_error(
    calc_narr(
      from = narr,
      locs = subset(
        ncp,
        select = "lon"
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are SpatVector (points)
  expect_no_error(
    calc_narr(
      from = narr,
      locs = terra::vect(
        ncp,
        geom = c("lon", "lat"),
        crs = "EPSG:4326"
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are SpatVector (polygons)
  expect_no_error(
    calc_narr(
      from = narr,
      locs = terra::buffer(
        terra::vect(
          ncp,
          geom = c("lon", "lat"),
          crs = "EPSG:4326"
        ),
        1000
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are sf
  expect_no_error(
    calc_narr(
      from = narr,
      locs = sf::st_as_sf(
        ncp,
        coords = c("lon", "lat"),
        crs = "EPSG:4326"
      ),
      locs_id = "site_id"
    )
  )
  # error if one of "lat" or "lon" is missing (or both)
  ncpp <- data.frame(long = -78.8277, lat = 35.95013)
  ncpp$site_id <- "3799900018810101"

  expect_error(
    process_locs_vector(
      locs = ncpp, crs = "EPSG:4326", 0
    )
  )
  expect_error(
    process_locs_vector(array(1))
  )
})

################################################################################
##### process_locs_vector + process_locs_radius
testthat::test_that("process_locs_vector + process_locs_radius", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100
  dfsf <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )
  dftr <- terra::vect(dfsf)

  testthat::expect_no_error(
    dftrb00 <- process_locs_radius(dftr, 0)
  )
  testthat::expect_no_error(
    dftrb1k <- process_locs_radius(dftr, 1000L)
  )
  testthat::expect_true(terra::geomtype(dftrb00) == "points")
  testthat::expect_true(terra::geomtype(dftrb1k) == "polygons")
  testthat::expect_s4_class(dftrb00, "SpatVector")
  testthat::expect_s4_class(dftrb1k, "SpatVector")
})

testthat::test_that("process_locs_vector tests", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  dfsf <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )
  dftr <- terra::vect(dfsf)

  testthat::expect_no_error(
    dftr1 <- process_locs_vector(dftr, "EPSG:4326", 0)
  )
  testthat::expect_no_error(
    dfsftr <- process_locs_vector(dfsf, "EPSG:4326", 0)
  )
  testthat::expect_no_error(
    dfdftr <- process_locs_vector(df, "EPSG:4326", 0)
  )
  testthat::expect_no_error(
    dfdftrb <- process_locs_vector(df, "EPSG:4326", radius = 1000L)
  )
  testthat::expect_s4_class(dftr1, "SpatVector")
  testthat::expect_s4_class(dfsftr, "SpatVector")
  testthat::expect_s4_class(dfdftr, "SpatVector")
  testthat::expect_s4_class(dfdftrb, "SpatVector")
  testthat::expect_true(terra::geomtype(dfdftr) == "points")
  testthat::expect_true(terra::geomtype(dfdftrb) == "polygons")
})

################################################################################
##### apply extent
testthat::test_that("apply_extent", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  dfsf <- sf::st_as_sf(
    df,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = FALSE
  )
  dftr <- terra::vect(dfsf)

  testthat::expect_no_error(
    dftr1 <- apply_extent(dftr, c(-112, -101, 33.5, 40.9))
  )
  testthat::expect_no_error(
    dfsftr <- apply_extent(dfsf, c(-112, -101, 33.5, 40.9))
  )
  testthat::expect_no_error(
    dfdftr <-
      apply_extent(df, c(-112, -101, 33.5, 40.9), geom = c("lon", "lat"))
  )
  testthat::expect_s4_class(dftr1, "SpatVector")
  testthat::expect_s3_class(dfsftr, "sf")
  testthat::expect_s4_class(dfdftr, "SpatVector")
})
# nolint end
