################################################################################
##### unit and integration tests for process_covariates and auxiliary functions
# nolint start

################################################################################
##### process_covariates
testthat::test_that("process_covariates", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
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

  aqs_proc <- process_covariates(
    covariate = "aqs",
    path = testthat::test_path(
      "..",
      "testdata",
      "aqs",
      "aqs_daily_88101_triangle.csv"
    ),
    date = c("2022-02-04", "2022-02-28"),
    mode = "location",
    return_format = "terra"
  )
  testthat::expect_s4_class(aqs_proc, "SpatVector")

  aqs_proc_sf <- process_covariates(
    covariate = "aqs",
    path = testthat::test_path(
      "..",
      "testdata",
      "aqs",
      "aqs_daily_88101_triangle.csv"
    ),
    date = c("2022-02-04", "2022-02-28"),
    mode = "location",
    return_format = "sf"
  )
  testthat::expect_s3_class(aqs_proc_sf, "sf")

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

    edgar_proc <- process_covariates(
      covariate = "edgar",
      path = edgar_path
    )
    testthat::expect_s4_class(edgar_proc, "SpatRaster")
  })

  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      "^VNP46A2",
      full.names = TRUE
    )

  corn <- process_blackmarble_corners()
  suppressWarnings(
    bm_proc <- process_covariates(
      covariate = "blackmarble",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  suppressWarnings(
    process_covariates(
      covariate = "Blackmarble",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  suppressWarnings(
    process_covariates(
      covariate = "BLACKMARBLE",
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_s4_class(bm_proc, "SpatRaster")

  withr::with_tempdir({
    mcd14ml_path <- file.path(".", "MODIS_C6_1_Global_MCD14ML_NRT_2026074.txt")
    data.table::fwrite(
      data.frame(
        latitude = 35.95013,
        longitude = -78.8277,
        acq_date = "2026-03-15",
        acq_time = 1230,
        frp = 11.5
      ),
      mcd14ml_path
    )

    mcd14ml_proc <- process_covariates(
      covariate = "mcd14ml",
      path = mcd14ml_path,
      date = "2026-03-15"
    )
    testthat::expect_s4_class(mcd14ml_proc, "SpatVector")
  })

  covar_types <- c(
    "modis_swath",
    "modis_merge",
    "mcd14ml",
    "koppen-geiger",
    "blackmarble",
    "koeppen-geiger",
    "koppen",
    "koeppen",
    "geos",
    "dummies",
      "gmted",
      "aqs",
      "hms",
      "smoke",
    "sedac_population",
    "population",
    "sedac_groads",
    "groads",
    "roads",
    "nlcd",
    "narr",
    "nei",
    "ecoregions",
    "ecoregion",
    "huc",
    "cropscape",
    "cdl",
      "prism",
      "terraclimate",
      "gridmet",
      "edgar"
    )
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
  testthat::expect_error(
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
  testthat::expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- process_collection(
    path = path,
    source = "geos",
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  testthat::expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})

testthat::test_that("process_parse_ncdf_day_codes parses gridmet day codes", {
  parsed <- process_parse_ncdf_day_codes(
    c("precipitation_amount_day=43101", "precipitation_amount_day=43102")
  )
  testthat::expect_equal(parsed[1], as.Date("2018-01-03"))
  testthat::expect_equal(parsed[2], as.Date("2018-01-04"))
  testthat::expect_error(
    process_parse_ncdf_day_codes("precipitation_amount_day=bad"),
    regexp = "Unable to parse"
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
  testthat::expect_error(
    calculate_narr(
      from = narr,
      locs = subset(
        ncp,
        select = "lon"
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are SpatVector (points)
  testthat::expect_no_error(
    calculate_narr(
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
  testthat::expect_no_error(
    calculate_narr(
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
  testthat::expect_no_error(
    calculate_narr(
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

  testthat::expect_error(
    process_locs_vector(
      locs = ncpp,
      crs = "EPSG:4326",
      0
    )
  )
  testthat::expect_error(
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

testthat::test_that("process_covariates dispatches goes and improve", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::with_tempdir({
    # "goes", "goes_adp", "GOES" all route to process_goes
    for (alias in c("goes", "goes_adp", "GOES")) {
      testthat::expect_error(
        process_covariates(covariate = alias, path = ".")
      )
    }
    # "improve" and "IMPROVE" route to process_improve (no-files error)
    for (alias in c("improve", "IMPROVE")) {
      testthat::expect_error(
        process_covariates(covariate = alias, path = ".")
      )
    }
  })
})
# nolint end

################################################################################
##### process_modis_swath: all-NA layers branch coverage

testthat::test_that("process_modis_swath emits message when all layers are NA", {
  withr::local_package("terra")
  withr::local_package("sf")

  msgs <- character(0)
  # Pass an empty path so paths_today is empty, making mod06_element empty
  result <- withCallingHandlers(
    process_modis_swath(
      path = character(0),
      date = as.Date("2020-01-01"),
      subdataset = "Cloud_Fraction",
      suffix = ":mod06:",
      resolution = 0.5
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  testthat::expect_true(any(grepl("All layers are NA", msgs)))
  testthat::expect_s4_class(result, "SpatRaster")
})

testthat::test_that("process_modis_daily expands single date and rethrows unexpected errors", {
  testthat::expect_error(
    process_modis_daily(
      path = 1,
      date = "2020-01-01",
      subdataset = "LST_Day_1km",
      return_type = "list"
    )
  )
})
