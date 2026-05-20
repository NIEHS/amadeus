withr::local_package("terra")
withr::local_package("sf")

testthat::test_that("targeted download branches are exercised", {
  temp_dir <- withr::local_tempdir()

  aqs_normalized <- 0L
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    check_destfile = function(...) TRUE,
    download_run_method = function(...) {
      list(success = 1, failed = 0, skipped = 0)
    },
    download_unzip = function(...) NULL,
    download_remove_zips = function(...) NULL,
    download_hash = function(hash, directory) {
      testthat::expect_true(hash)
      testthat::expect_true(dir.exists(directory))
      "hash-ok"
    },
    download_normalize_aqs_unzip = function(...) {
      aqs_normalized <<- aqs_normalized + 1L
      invisible(NULL)
    },
    .package = "amadeus"
  )

  aqs_hash <- amadeus::download_aqs(
    resolution_temporal = "daily",
    parameter_code = "88101",
    year = 2020,
    directory_to_save = temp_dir,
    acknowledgement = TRUE,
    unzip = TRUE,
    hash = TRUE
  )
  testthat::expect_identical(aqs_hash, "hash-ok")
  testthat::expect_equal(aqs_normalized, 1L)

  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    check_url_status = function(...) FALSE,
    .package = "amadeus"
  )
  testthat::expect_error(
    amadeus::download_geos(
      collection = "aqc_tavg_1hr_g1440x721_v1",
      date = "2024-01-01",
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    "Invalid date returns HTTP code 404"
  )

  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    .package = "amadeus"
  )
  testthat::expect_error(
    amadeus::download_merra2(
      collection = "not_a_collection",
      date = "2024-01-01",
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    "Requested collection is not recognized"
  )

  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    check_url_status = function(...) FALSE,
    .package = "amadeus"
  )
  testthat::expect_error(
    amadeus::download_merra2(
      collection = "inst1_2d_asm_Nx",
      date = "2024-01-01",
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    "Invalid date returns HTTP code 404"
  )

  groads_calls <- list(run = 0L, unzip = 0L, remove = 0L)
  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    download_run_method = function(...) {
      groads_calls$run <<- groads_calls$run + 1L
      invisible(NULL)
    },
    check_destfile = function(...) FALSE,
    download_unzip = function(...) {
      groads_calls$unzip <<- groads_calls$unzip + 1L
      invisible(NULL)
    },
    download_remove_zips = function(...) {
      groads_calls$remove <<- groads_calls$remove + 1L
      invisible(NULL)
    },
    download_hash = function(...) "groads-hash",
    .package = "amadeus"
  )
  testthat::expect_warning(
    groads_skip <- amadeus::download_groads(
      data_region = "Global",
      data_format = "Shapefile",
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "download=FALSE"
  )
  testthat::expect_match(groads_skip$destfiles, "groads_v1_global_gdb\\.zip$")

  groads_hash <- amadeus::download_groads(
    data_region = "Africa",
    data_format = "Shapefile",
    directory_to_save = temp_dir,
    acknowledgement = TRUE,
    hash = TRUE
  )
  testthat::expect_identical(groads_hash, "groads-hash")
  testthat::expect_identical(groads_calls$run, 0L)
  testthat::expect_identical(groads_calls$unzip, 1L)
  testthat::expect_identical(groads_calls$remove, 1L)

  population_calls <- list(run = 0L, unzip = 0L, remove = 0L)
  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    download_run_method = function(...) {
      population_calls$run <<- population_calls$run + 1L
      invisible(NULL)
    },
    check_destfile = function(...) FALSE,
    download_unzip = function(...) {
      population_calls$unzip <<- population_calls$unzip + 1L
      invisible(NULL)
    },
    download_remove_zips = function(...) {
      population_calls$remove <<- population_calls$remove + 1L
      invisible(NULL)
    },
    download_hash = function(...) "population-hash",
    .package = "amadeus"
  )

  testthat::expect_warning(
    pop_all_tif <- amadeus::download_population(
      data_resolution = "30 second",
      data_format = "GeoTIFF",
      year = "all",
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "download=FALSE"
  )
  testthat::expect_match(pop_all_tif$destfiles, "totpop_2pt5_min_nc\\.zip$")

  testthat::expect_warning(
    pop_all_ascii <- amadeus::download_population(
      data_format = "ASCII",
      year = "all",
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "download=FALSE"
  )
  testthat::expect_match(pop_all_ascii$destfiles, "totpop_1_deg_nc\\.zip$")

  pop_hash <- amadeus::download_population(
    data_format = "netCDF",
    year = "2020",
    directory_to_save = temp_dir,
    acknowledgement = TRUE,
    hash = TRUE
  )
  testthat::expect_identical(pop_hash, "population-hash")
  testthat::expect_identical(population_calls$run, 0L)
  testthat::expect_identical(population_calls$unzip, 1L)
  testthat::expect_identical(population_calls$remove, 1L)

  testthat::local_mocked_bindings(
    get_token = function(...) "token",
    .package = "amadeus"
  )
  testthat::expect_error(
    amadeus::download_modis(
      product = "MOD09GA",
      version = "061",
      date = c("2023-12-31", "2024-01-01"),
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    "dates should be in the same year"
  )
  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = NA_character_
    ),
    "single character value"
  )
  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = "   "
    ),
    "must be \"US\", a two-letter state code, or \"tbl\""
  )
})

testthat::test_that("targeted calculate branches are exercised", {
  fire_df <- data.frame(
    site = c("f1", "f2", "f3"),
    lon = c(0, 1, 0),
    lat = c(0, 0, 1),
    time = c(20200101L, 20200101L, 20200102L),
    fire_count = c(2, 3, 4),
    frp = c(10, 20, 30)
  )
  fire_vec <- terra::vect(fire_df, geom = c("lon", "lat"), crs = "EPSG:4326")

  locs <- data.frame(
    site_id = c("A", "B"),
    lon = c(0, 10),
    lat = c(0, 10)
  )
  locs_vect <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")

  mcd14 <- amadeus::calculate_modis(
    from = fire_vec,
    locs = locs_vect,
    locs_id = "site_id",
    radius = c(0L, 200000L),
    geom = FALSE,
    fun_summary = "sum"
  )
  testthat::expect_equal(nrow(mcd14), 4)
  testthat::expect_equal(
    subset(mcd14, site_id == "A" & format(time, "%Y%m%d") == "20200101")$fire_count_00000,
    2
  )
  testthat::expect_equal(
    subset(mcd14, site_id == "A" & format(time, "%Y%m%d") == "20200101")$frp_200000,
    30
  )
  testthat::expect_true(all(subset(mcd14, site_id == "B")$fire_count_200000 == 0))

  mcd14_geom <- amadeus::calculate_modis(
    from = fire_vec,
    locs = locs_vect,
    locs_id = "site_id",
    radius = 0L,
    geom = "terra",
    fun_summary = "sum"
  )
  testthat::expect_s4_class(mcd14_geom, "SpatVector")

  lagged_from <- fire_vec
  lagged_from$ozone_0_00000 <- c(1, 2, 3)
  lagged_from$time <- as.POSIXct(
    c("2020-01-01", "2020-01-02", "2020-01-03"),
    tz = "UTC"
  )
  lagged_from$site_id <- c("A", "A", "A")
  lagged_geom <- amadeus::calculate_lagged(
    from = lagged_from,
    date = c("2020-01-02", "2020-01-03"),
    lag = 1,
    locs_id = "site_id",
    geom = "terra"
  )
  testthat::expect_s4_class(lagged_geom, "SpatVector")
  testthat::expect_true("ozone_1_00000" %in% names(lagged_geom))

  prism_rast <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  values(prism_rast) <- c(1, 2, 3, 4)
  names(prism_rast) <- "ppt"

  poly_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 4326
  )
  poly_locs <- sf::st_sf(site_id = "poly1", geometry = poly_geom)

  prism_poly <- amadeus::calculate_prism(
    from = prism_rast,
    locs = poly_locs,
    locs_id = "site_id",
    radius = 0,
    geom = "sf"
  )
  testthat::expect_s3_class(prism_poly, "sf")
  testthat::expect_true("geometry" %in% names(prism_poly))
  testthat::expect_true("ppt_0" %in% names(prism_poly))

  cropscape_rast <- prism_rast
  values(cropscape_rast) <- c(1, 1, 2, 2)
  cropscape_poly <- amadeus::calculate_cropscape(
    from = cropscape_rast,
    locs = poly_locs,
    locs_id = "site_id",
    radius = 0,
    geom = "sf"
  )
  testthat::expect_s3_class(cropscape_poly, "sf")
  testthat::expect_true("geometry" %in% names(cropscape_poly))
  testthat::expect_true(any(grepl("^cropscape_0_", names(cropscape_poly))))

  point_locs <- data.frame(site_id = "p1", lon = 0.5, lat = 0.5)
  testthat::expect_error(
    amadeus::calculate_edgar(
      from = list(),
      locs = point_locs,
      locs_id = "site_id"
    ),
    "`from` must be a SpatRaster object"
  )
  testthat::expect_error(
    amadeus::calculate_edgar(
      from = prism_rast,
      locs = point_locs,
      locs_id = "site_id",
      radius = c(0, 1)
    ),
    "`radius` must be numeric\\(1\\)"
  )
})

testthat::test_that("calculate_modis handles SpatRaster and SpatVector inputs distinctly", {
  locs <- sf::st_as_sf(
    data.frame(site_id = "site_1", lon = 0, lat = 0),
    coords = c("lon", "lat"),
    crs = 4326
  )

  modis_raster <- terra::rast(
    ncols = 1,
    nrows = 1,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(modis_raster) <- 12
  names(modis_raster) <- "mock_layer"

  raster_result <- amadeus::calculate_modis(
    from = modis_raster,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    name_covariates = "mock_",
    scale = "* 1"
  )
  testthat::expect_true(is.data.frame(raster_result))
  testthat::expect_true("mock_00000" %in% names(raster_result))
  testthat::expect_true(all(is.na(raster_result$time)))
  testthat::expect_true(is.na(attr(raster_result, "dates_dropped")))

  fire_points <- terra::vect(
    data.frame(
      lon = 0,
      lat = 0,
      time = 20200101L,
      fire_count = 2L,
      frp = 10
    ),
    geom = c("lon", "lat"),
    keepgeom = TRUE,
    crs = "EPSG:4326"
  )

  vector_result <- amadeus::calculate_modis(
    from = fire_points,
    locs = locs,
    locs_id = "site_id",
    radius = 0L,
    fun_summary = "sum"
  )
  testthat::expect_true(is.data.frame(vector_result))
  testthat::expect_true(all(c("fire_count_00000", "frp_00000") %in% names(vector_result)))
  testthat::expect_equal(vector_result$fire_count_00000, 2)
  testthat::expect_equal(format(vector_result$time, "%Y%m%d"), "20200101")
  testthat::expect_true(is.na(attr(vector_result, "dates_dropped")))

  testthat::expect_error(
    amadeus::calculate_modis(
      from = fire_points,
      from_secondary = modis_raster,
      locs = locs,
      locs_id = "site_id",
      radius = 0L,
      fun_summary = "sum"
    ),
    "from_secondary is only supported for character or SpatRaster inputs"
  )
})

testthat::test_that("legacy .by is rejected across remaining calculate APIs", {
  locs_df <- data.frame(site_id = "s1", lon = 0.5, lat = 0.5)

  from_rast <- terra::rast(
    nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326"
  )
  terra::values(from_rast) <- 1
  names(from_rast) <- "mock_20200101"

  from_vect <- terra::vect(
    data.frame(x = 0.5, y = 0.5, id = "a"),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )

  expect_by_error <- function(expr) {
    testthat::expect_error(expr, regexp = "no longer supported")
  }

  expect_by_error(amadeus::calculate_covariates(
    covariate = "prism", from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_koppen_geiger(
    from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_nlcd(
    from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_ecoregion(
    from = from_vect, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_tri(
    from = from_vect, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_nei(
    from = from_vect, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_gmted(
    from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_population(
    from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_groads(
    from = from_vect, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_cropscape(
    from = from_rast, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
  expect_by_error(amadeus::calculate_huc(
    from = from_vect, locs = locs_df, locs_id = "site_id", .by = "day"
  ))
})

testthat::test_that("collapse_nlcd returns empty df when rowbind is empty", {
  # Passing a list with one 0-row data frame produces empty rowbind result
  empty_df <- data.frame(site_id = character(0), value = numeric(0))
  testthat::expect_warning(
    result <- amadeus:::collapse_nlcd(list(empty_df)),
    "empty data frame"
  )
  testthat::expect_equal(nrow(result), 0L)
})

testthat::test_that("generate_time_sequence handles collection ending in '3'", {
  ts <- amadeus::generate_time_sequence("aqc_tavg_1hr_g1440x721_v1_collection3")
  testthat::expect_equal(ts[1], "0000")
  testthat::expect_equal(length(ts), 24L)
})

testthat::test_that("setup_nasa_token interactive branch is exercised", {
  # Mock readline to simulate user entering a token
  testthat::local_mocked_bindings(
    readline = function(prompt = "") "interactive_token_123",
    interactive = function() TRUE,
    .package = "base"
  )
  withr::local_envvar(NASA_EARTHDATA_TOKEN = "")
  testthat::expect_no_error(
    suppressMessages(
      amadeus::setup_nasa_token(method = "session")
    )
  )
  testthat::expect_equal(Sys.getenv("NASA_EARTHDATA_TOKEN"), "interactive_token_123")
})

testthat::test_that("download_merra2 download=FALSE returns url list", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    check_url_status = function(...) TRUE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    result <- suppressWarnings(
      withCallingHandlers(
        amadeus::download_merra2(
          collection = "inst1_2d_asm_Nx",
          date = c("2024-01-01", "2024-01-01"),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE
        ),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_true("urls" %in% names(result))
    testthat::expect_true("n_files" %in% names(result))
    testthat::expect_true(any(grepl("Skipping download", msgs)))
  })
})

testthat::test_that("download_gridmet scalar year expands to two-element year", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(urls, ...) list(success = length(urls), failed = 0, skipped = 0),
    download_hash = function(...) NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      amadeus::download_gridmet(
        variables = "Precipitation",
        year = 2020L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$n_files, 1L)
  })
})

testthat::test_that("download_terraclimate scalar year expands to two-element year", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(urls, ...) list(success = length(urls), failed = 0, skipped = 0),
    download_hash = function(...) NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      amadeus::download_terraclimate(
        variables = "Precipitation",
        year = 2020L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$n_files, 1L)
  })
})

testthat::test_that("calculate_nei errors when locs cannot be converted to SpatVector", {
  # A list that terra::vect cannot handle
  testthat::expect_error(
    amadeus::calculate_nei(
      from = terra::vect(data.frame(x = 0, y = 0), geom = c("x", "y"), crs = "EPSG:4326"),
      locs = list(not_spatial = TRUE)
    ),
    "unable to be converted"
  )
})

testthat::test_that("calc_weighted_fun weighted sum and default passthrough", {
  testthat::expect_equal(
    amadeus:::calc_weighted_fun("sum", weighted = TRUE),
    "weighted_sum"
  )
  testthat::expect_equal(
    amadeus:::calc_weighted_fun("min", weighted = TRUE),
    "min"
  )
})

testthat::test_that("calc_prepare_exact_geoms polygon, unsupported geom, projected non-finite radius", {
  r <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326"
  )
  poly_sv <- terra::as.polygons(r)

  result_poly <- amadeus:::calc_prepare_exact_geoms(poly_sv, radius = 1000)
  testthat::expect_s3_class(result_poly, "sf")

  line_sv <- terra::vect(sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  ))
  testthat::expect_error(
    amadeus:::calc_prepare_exact_geoms(line_sv, radius = 1000),
    "Unsupported location geometry"
  )

  pts_proj <- terra::vect(
    data.frame(x = 500000, y = 4000000), geom = c("x", "y"), crs = "EPSG:32618"
  )
  result_proj <- amadeus:::calc_prepare_exact_geoms(pts_proj, radius = -1)
  testthat::expect_s3_class(result_proj, "sf")
})

testthat::test_that("calc_prepare_weights error and edge-case branches", {
  from_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(from_r) <- 1:16
  names(from_r) <- "val"

  # non-SpatRaster `from` with weights supplied
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = list(), weights = from_r),
    "must be a SpatRaster"
  )

  # sf polygon weights converted to SpatVector
  poly_sf <- sf::st_sf(
    val = 1.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
  result_sf <- amadeus:::calc_prepare_weights(from = from_r, weights = poly_sf)
  testthat::expect_s4_class(result_sf, "SpatRaster")

  # invalid weights type (not NULL/SpatRaster/SpatVector/sf/character)
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = 42L),
    "must be NULL, SpatRaster, polygon SpatVector"
  )

  # multi-layer raster weights
  wt_multi <- c(from_r, from_r)
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = wt_multi),
    "exactly one layer"
  )

  # negative raster weights
  wt_neg <- from_r
  terra::values(wt_neg) <- -1
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = wt_neg),
    "non-negative"
  )

  # `from` missing CRS with raster weights
  from_nocrs <- from_r
  terra::crs(from_nocrs) <- ""
  wt_valid <- from_r
  terra::values(wt_valid) <- 1
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_nocrs, weights = wt_valid),
    "missing CRS"
  )

  # vector polygon weights with non-polygon (points) geometry
  pts_sv <- terra::vect(
    data.frame(x = 1, y = 1), geom = c("x", "y"), crs = "EPSG:4326"
  )
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = pts_sv),
    "must contain polygons"
  )

  # `from` missing CRS with vector polygon weights
  poly_sv <- terra::as.polygons(from_r)[1]
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_nocrs, weights = poly_sv),
    "missing CRS"
  )

  # vector polygon weights that do not overlap `from`
  poly_far <- terra::vect(sf::st_sf(
    val = 1.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(100, 50, 102, 50, 102, 52, 100, 52, 100, 50), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  ))
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = poly_far),
    "do not overlap"
  )
})

testthat::test_that("calculate functions pass weights through correctly", {
  from_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(from_r) <- 1:16
  names(from_r) <- "val"
  weights_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(weights_r) <- 1

  poly_locs <- sf::st_sf(
    site_id = "p1",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  # calculate_covariates dispatches and passes weights (line 191)
  suppressMessages(
    res_cc <- amadeus::calculate_covariates(
      covariate = "edgar",
      from = from_r,
      locs = poly_locs,
      locs_id = "site_id",
      weights = weights_r
    )
  )
  testthat::expect_equal(nrow(res_cc), 1L)

  # calculate_prism weights passed to exact_extract (line 3416)
  suppressMessages(
    res_prism <- amadeus::calculate_prism(
      from = from_r, locs = poly_locs, locs_id = "site_id",
      radius = 0, weights = weights_r
    )
  )
  testthat::expect_equal(nrow(res_prism), 1L)

  # calculate_edgar weights passed to exact_extract (line 3608)
  suppressMessages(
    res_edgar <- amadeus::calculate_edgar(
      from = from_r, locs = poly_locs, locs_id = "site_id",
      radius = 0, weights = weights_r
    )
  )
  testthat::expect_equal(nrow(res_edgar), 1L)

  # calculate_cropscape weights passed to exact_extract (line 3780)
  suppressMessages(
    res_cs <- suppressWarnings(amadeus::calculate_cropscape(
      from = from_r, locs = poly_locs, locs_id = "site_id",
      radius = 0, weights = weights_r
    ))
  )
  testthat::expect_equal(nrow(res_cs), 1L)

  # calculate_drought polygon locs + weights (line 4165)
  drought_r <- from_r
  terra::time(drought_r) <- as.Date("2020-01-01")
  names(drought_r) <- "spei_01_2020-01-01"
  res_drought <- amadeus::calculate_drought(
    from = drought_r, locs = poly_locs, locs_id = "site_id",
    radius = 0, weights = weights_r
  )
  testthat::expect_equal(nrow(res_drought), 1L)
})

testthat::test_that("calc_prepare_weights covers vector numeric column and path branches", {
  from_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(from_r) <- 1:16
  names(from_r) <- "val"

  # SpatVector polygon with exactly one numeric column -> lines 346, (347 not triggered)
  poly_sv_one <- terra::vect(sf::st_sf(
    val = 2.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  ))
  result_one <- amadeus:::calc_prepare_weights(from = from_r, weights = poly_sv_one)
  testthat::expect_s4_class(result_one, "SpatRaster")

  # SpatVector polygon with multiple numeric columns -> lines 347-352 (message)
  poly_sv_multi <- terra::vect(sf::st_sf(
    val = 2.0, val2 = 3.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  ))
  testthat::expect_message(
    amadeus:::calc_prepare_weights(from = from_r, weights = poly_sv_multi),
    "Multiple numeric columns"
  )

  # SpatVector polygon with negative value -> line 355 stop
  poly_sv_neg <- terra::vect(sf::st_sf(
    val = -1.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  ))
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = poly_sv_neg),
    "non-negative"
  )

  # Path-based weights: vector file path -> lines 372-374
  tmpdir <- withr::local_tempdir()
  shp_path <- file.path(tmpdir, "wt.gpkg")
  sf::st_write(sf::st_sf(
    val = 1.0,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  ), shp_path, quiet = TRUE)
  result_path_vec <- amadeus:::calc_prepare_weights(from = from_r, weights = shp_path)
  testthat::expect_s4_class(result_path_vec, "SpatRaster")

  # Path-based weights: invalid path -> line 376 stop
  testthat::expect_error(
    amadeus:::calc_prepare_weights(from = from_r, weights = file.path(tmpdir, "no_file.xyz")),
    "could not be read"
  )
})

testthat::test_that("calculate_prism derives time from metags when terra::time returns NA", {
  from_r <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326"
  )
  terra::values(from_r) <- c(1, 2, 3, 4)
  names(from_r) <- "ppt"

  locs <- data.frame(site_id = "s1", lon = 0.5, lat = 0.5)

  # Mock calc_worker so we don't need real data
  testthat::local_mocked_bindings(
    calc_worker = function(...) data.frame(site_id = "s1", ppt_0 = 2.5),
    .package = "amadeus"
  )

  # 8-digit date in metags (YYYYMMDD -> lines 3472-3473)
  meta_r8 <- from_r
  terra::metags(meta_r8) <- data.frame(name = "time", value = "20200115")
  r8 <- amadeus::calculate_prism(
    from = meta_r8, locs = locs, locs_id = "site_id",
    radius = 0, .by_time = "day"
  )
  testthat::expect_true("time" %in% names(r8))
  testthat::expect_equal(as.character(as.Date(r8$time[1])), "2020-01-15")

  # 6-digit date in metags (YYYYMM -> lines 3474-3475)
  meta_r6 <- from_r
  terra::metags(meta_r6) <- data.frame(name = "time", value = "202003")
  r6 <- amadeus::calculate_prism(
    from = meta_r6, locs = locs, locs_id = "site_id",
    radius = 0, .by_time = "month"
  )
  testthat::expect_true("time" %in% names(r6))
  testthat::expect_equal(as.character(as.Date(r6$time[1])), "2020-03-01")

  # 4-digit year in metags (YYYY -> lines 3476-3477)
  meta_r4 <- from_r
  terra::metags(meta_r4) <- data.frame(name = "time", value = "2021")
  r4 <- amadeus::calculate_prism(
    from = meta_r4, locs = locs, locs_id = "site_id",
    radius = 0, .by_time = "year"
  )
  testthat::expect_true("time" %in% names(r4))
  testthat::expect_equal(as.character(as.Date(r4$time[1])), "2021-01-01")
})

testthat::test_that("calculate_drought with point locs and weights uses exact_extract path", {
  from_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(from_r) <- as.numeric(1:16)
  terra::time(from_r) <- as.Date("2020-06-01")
  names(from_r) <- "spei_01_2020-06-01"

  weights_r <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:4326"
  )
  terra::values(weights_r) <- 1

  # Point locs + weights -> lines 4185-4189 (calc_prepare_exact_geoms + exact_extract)
  pt_locs <- data.frame(site_id = "p1", lon = 1.0, lat = 1.0)
  res <- amadeus::calculate_drought(
    from = from_r, locs = pt_locs, locs_id = "site_id",
    radius = 10000, weights = weights_r
  )
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_true(any(grepl("spei", names(res))))
})
