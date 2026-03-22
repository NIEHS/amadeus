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

  mcd14 <- amadeus:::calculate_mcd14dl(
    from = fire_vec,
    locs = locs_vect,
    locs_id = "site_id",
    radius = c(0L, 200000L),
    geom = FALSE
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

  mcd14_geom <- amadeus:::calculate_mcd14dl(
    from = fire_vec,
    locs = locs_vect,
    locs_id = "site_id",
    radius = 0L,
    geom = "terra"
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
