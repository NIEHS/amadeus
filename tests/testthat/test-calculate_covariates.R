## test for calculating covariates

testthat::test_that("calc_koppen_geiger works well", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97
    )
  site_faux <- terra::vect(site_faux, crs = "EPSG:4326")
  kp_path <- testthat::test_path("..", "testdata", "koppen_subset.tif")

  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path = kp_path,
      sites = site_faux
    )
  )
  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path = kp_path,
      sites = sf::st_as_sf(site_faux)
    )
  )
  # the result is a data frame
  testthat::expect_s3_class(kg_res, "data.frame")
  # ncol is equal to 6
  testthat::expect_equal(ncol(kg_res), 6)
  # should have only one climate zone
  testthat::expect_equal(sum(unlist(kg_res[, -1])), 1)
})

testthat::test_that("calc_dummies works well", {

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97,
      time = "2022-01-01"
    )

  testthat::expect_no_error(
    dum_res <- calc_temporal_dummies(
      sites = site_faux,
      domain_year = seq(2018L, 2022L)
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(dum_res, "data.frame")
  # ncol is equal to 12 + 5 + 7 + 4
  testthat::expect_equal(ncol(dum_res), 28L)
  # should have each of the indicator groups
  testthat::expect_equal(sum(unlist(dum_res[, -1:-4])), 3L)

  # error cases
  site_faux_err <- site_faux
  colnames(site_faux_err)[4] <- "date"
  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = site_faux_err
    )
  )

  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = as.matrix(site_faux_err)
    )
  )

})

testthat::test_that("calc_ecoregion works well", {

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  site_faux <-
    data.frame(
      site_id = "37999109988101",
      lon = -77.576,
      lat = 39.40,
      date = as.Date("2022-01-01")
    )
  site_faux <-
    terra::vect(
                site_faux,
                geom = c("lon", "lat"),
                crs = "EPSG:4326")
  site_faux <- terra::project(site_faux, "EPSG:5070")

  testthat::expect_no_error(
    ecor_res <- calc_ecoregion(
      path = testthat::test_path("..", "testdata", "eco_l3_clip.gpkg"),
      sites = site_faux,
      id_col = "site_id"
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(ecor_res, "data.frame")
  # ncol is equal to 2 + 5 + 2 + 1 + 1
  testthat::expect_equal(ncol(ecor_res), 3L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
                         sum(unlist(ecor_res[, dum_cn])), 2L)
})


testthat::test_that("calc_modis works well.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_package("lwgeom")
  withr::local_package("foreach")
  withr::local_package("doParallel")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         foreachDoparLocal = TRUE)
  )

  site_faux <-
    data.frame(
      site_id = "37999904288101",
      lon = -78.87,
      lat = 35.8734,
      time = as.Date("2021-08-15")
    )
  site_faux <-
    terra::vect(
                site_faux,
                geom = c("lon", "lat"),
                crs = "EPSG:4326")

  # case 1: standard mod11a1
  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calc_modis(
          path = path_mod11,
          product = "MOD11A1",
          sites = sf::st_as_sf(site_faux),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          nthreads = 1L
        )
    )
  )
  testthat::expect_s3_class(calc_mod11, "data.frame")

  # case 2: swath mod06l2
  path_mod06 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "MOD06",
      full.names = TRUE
    )
  testthat::expect_no_error(
    suppressWarnings(
      modis_mosaic_mod06(
        paths = path_mod06,
        date_in = "2021-08-15"
      )
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06 <-
        calc_modis(
          path = path_mod06,
          product = "MOD06_L2",
          sites = site_faux,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
          nthreads = 1
        )
    )
  )
  testthat::expect_s3_class(calc_mod06, "data.frame")

  # case 3: VIIRS
  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "VNP46",
      full.names = TRUE
    )

  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46 <-
        calc_modis(
          path = path_vnp46,
          product = "VNP46A2",
          sites = site_faux,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          nthreads = 1
        )
    )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # error cases
  testthat::expect_error(
    modis_get_vrt(path = site_faux)
  )
  testthat::expect_error(
    modis_get_vrt(
      paths = path_mod11,
      product = "MOD11A1",
      date_in = "2021-08-15",
      foo = 3L
    )
  )
  testthat::expect_error(
    modis_get_vrt(
      paths = path_mod11,
      product = "MOD11A1",
      date_in = "2021~08~15",
      foo = "mean"
    )
  )

  site_faux_r <- site_faux
  names(site_faux_r)[1] <- "ID"
  testthat::expect_error(
    modis_worker(raster = rast(nrow = 3, ncol = 3), date = "2021-08-15",
                 sites_in = site_faux_r)
  )
  testthat::expect_error(
    modis_worker(raster = rast(nrow = 3, ncol = 3), date = "2021-08-15",
                 sites_in = matrix(c(1, 3, 4, 5), nrow = 2))
  )
  testthat::expect_error(
    modis_worker(raster = rast(nrow = 3, ncol = 3), date = "2021-08-15",
                 sites_in = sf::st_as_sf(site_faux))
  )
  site_faux2 <- site_faux
  names(site_faux2)[2] <- "date"
  testthat::expect_error(
    modis_worker(raster = rast(nrow = 3, ncol = 3), date = "2021-08-15",
                 sites_in = sf::st_as_sf(site_faux2))
  )

  testthat::expect_error(
    calc_modis(path = site_faux)
  )
  testthat::expect_error(
    calc_modis(path = path_mod11, product = "MOD11A1", sites = list(1, 2, 3))
  )
  testthat::expect_warning(
    calc_modis(
      path = path_vnp46,
      product = "VNP46A2",
      sites = site_faux,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      nthreads = 1
    )
  )

})


testthat::test_that("Check extract_nlcd_ratio works", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("spData")

  point_us1 <- cbind(lon = -114.7, lat = 38.9, dem = 40)
  point_us2 <- cbind(lon = -114, lat = 39, dem = 15)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, dem = 100) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, dem = 15) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_testdata <-
    testthat::test_path(
      "..",
      "testdata"
    )
  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    radius = "1000",
                    path = path_testdata),
    "radius is not a numeric."
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    radius = -3,
                    path = path_testdata),
    "radius has not a likely value."
  )
  # -- year is numeric
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    year = "2019",
                    path = path_testdata),
    "year is not a numeric."
  )
  # -- year has likely value
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    year = 20192,
                    path = path_testdata),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    year = 2020,
                    path = path_testdata),
    "NLCD data not available for this year."
  )
  # -- data_vect is a SpatVector
  testthat::expect_error(
    calc_nlcd_ratio(sites = 12,
                    path = path_testdata),
    "sites is not a terra::SpatVector."
  )
  # -- nlcd_path is not a character
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    path = 2),
    "path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  testthat::expect_error(
    calc_nlcd_ratio(sites = eg_data,
                    path = nice_sentence),
    "path does not exist."
  )

  # CHECK OUTPUT
  year <- 2021
  buf_radius <- 3000
  testthat::expect_no_error(
    calc_nlcd_ratio(
      sites = eg_data,
      year = year,
      radius = buf_radius,
      path = path_testdata
    )
  )
  output <- calc_nlcd_ratio(
    sites = eg_data,
    year = year,
    radius = buf_radius,
    path = path_testdata
  )
  # -- returns a SpatVector
  testthat::expect_equal(class(output)[1], "SpatVector")
  # -- crs is the same than input
  testthat::expect_true(terra::same.crs(eg_data, output))
  # -- out-of-mainland-US points removed (France and Alaska)
  testthat::expect_equal(nrow(output), 2)
  # -- initial names are still in the output SpatVector
  testthat::expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  testthat::expect_equal(
    output$LDU_EFO_0_03000_2021[1], 0.7940682, tolerance = 1e-7
  )
  testthat::expect_equal(
    output$LDU_SHB_0_03000_2021[2], 0.9987249, tolerance = 1e-7
  )
  # -- class fraction rows should sum to 1
  testthat::expect_equal(
    rowSums(as.data.frame(output[, 2:ncol(output)])),
    rep(1, 2),
    tolerance = 1e-7
  )
})


testthat::test_that("NEI calculation", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(202401)

  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(ncpath)
  nc <- nc[grep("(Orange|Wake|Durham)", nc$NAME), ]

  testthat::expect_error(
    calc_nei(neipath,
      ncp,
      year = 2017,
      county_shp = nc,
      sites_epsg = "EPSG:4267"
    )
  )
  testthat::expect_error(
    calc_nei(neipath,
      ncpt,
      year = 2010,
      county_shp = nc,
      sites_epsg = "EPSG:4267"
    )
  )
  testthat::expect_error(
    calc_nei(neipath,
      ncpt,
      year = 2017,
      county_shp = "your/file/is/nowhere",
      sites_epsg = "EPSG:4267"
    )
  )

  nc$GEOID <- nc$FIPS
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018
  ncpt <- ncp# as.data.frame(ncp, geom = "XY")

  # test data should be prepared
  neipath <- testthat::test_path("..", "testdata", "nei")
  testthat::expect_no_error(
    ncnei <-
      calc_nei(neipath,
        ncpt,
        year = 2017,
        county_shp = nc,
        sites_epsg = "EPSG:4267"
      )
  )
  testthat::expect_no_error(
    calc_nei(neipath,
      ncpt,
      year = 2017,
      county_shp = ncpath,
      sites_epsg = "EPSG:4267"
    )
  )

  testthat::expect_true(any(grepl("NEI17", names(ncnei))))
  testthat::expect_equal(ncnei$TRF_NEI17_0_00000, 1579079, tolerance = 1)

  # error cases
  testthat::expect_error(
    calc_nei(neipath,
      sf::st_as_sf(ncpt, coords = c("lon", "lat")),
      year = 2017,
      county_shp = nc,
      sites_epsg = "EPSG:4267"
    )
  )
  ncpsf <- sf::st_as_sf(ncpt, coords = c("lon", "lat"), remove = FALSE)
  sf::st_crs(ncpsf) <- "EPSG:4267"
  testthat::expect_no_error(
    calc_nei(neipath,
      ncpsf,
      year = 2017,
      county_shp = nc,
      sites_epsg = "EPSG:4267"
    )
  )
  testthat::expect_no_error(
    calc_nei(neipath,
      convert_stobj_to_stdt(ncpsf),
      year = 2017,
      county_shp = ncpath,
      sites_epsg = "EPSG:4267"
    )
  )

})
