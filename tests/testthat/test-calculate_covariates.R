## test for calculating covariates
## 1. Koppen-Geiger ####
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
  site_faux <- terra::vect(site_faux, crs = "EPSG:4326", keepgeom = TRUE)
  kp_path <- testthat::test_path("..", "testdata", "koppen_subset.tif")

  testthat::expect_no_error(
    kgras <- process_koppen_geiger(path = kp_path)
  )

  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      from = kgras,
      locs = site_faux
    )
  )
  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      from = kgras,
      locs = sf::st_as_sf(site_faux)
    )
  )
  # the result is a data frame
  testthat::expect_s3_class(kg_res, "data.frame")
  # ncol is equal to 7
  testthat::expect_equal(ncol(kg_res), 7)
  # should have only one climate zone
  testthat::expect_equal(sum(unlist(kg_res[, c(-1, -2)])), 1)
  # with included geometry
  testthat::expect_no_error(
    kg_geom <- calc_koppen_geiger(
      from = kgras,
      locs = sf::st_as_sf(site_faux),
      geom = TRUE
    )
  )
  testthat::expect_equal(ncol(kg_geom), 7)
  testthat::expect_true("SpatVector" %in% class(kg_geom))
})

## 2. Temporal Dummies ####
testthat::test_that("calc_dummies works well", {

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97,
      time = as.POSIXlt("2022-01-01")
    )

  testthat::expect_no_error(
    dum_res <- calc_temporal_dummies(
      locs = site_faux,
      year = seq(2018L, 2022L)
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
      locs = site_faux_err
    )
  )

  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      locs = as.matrix(site_faux_err)
    )
  )

})

testthat::test_that("calc_temporal_dummies errors.", {
  withr::local_package("terra")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  testthat::expect_error(
    calc_temporal_dummies(
      ncp
    )
  )
  testthat::expect_error(
    calc_temporal_dummies(
      terra::vect(ncp)
    )
  )
})

## 3. Ecoregions ####
testthat::test_that("calc_ecoregion works well", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  ecol3 <- testthat::test_path("..", "testdata", "eco_l3_clip.gpkg")
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
                keepgeom = TRUE,
                crs = "EPSG:4326")
  site_faux <- terra::project(site_faux, "EPSG:5070")

  testthat::expect_no_error(
    erras <- process_ecoregion(ecol3)
  )

  testthat::expect_no_error(
    ecor_res <- calc_ecoregion(
      from = erras,
      locs = sf::st_as_sf(site_faux),
      locs_id = "site_id"
    )
  )

  testthat::expect_no_error(
    ecor_res <- calc_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id"
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(ecor_res, "data.frame")
  # ncol is equal to 2 + 5 + 2 + 1 + 1
  testthat::expect_equal(ncol(ecor_res), 4L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
    sum(unlist(ecor_res[, dum_cn])), 2L
  )

  testthat::expect_no_error(
    ecor_geom <- calc_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(ecor_geom), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(ecor_geom)
  )
})

## 4. MODIS-VIIRS ####
testthat::test_that("calc_modis works well.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_package("lwgeom")
  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L
    )
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
                keepgeom = FALSE,
                crs = "EPSG:4326")

  # case 1: standard mod11a1
  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_error(
    base_mod11 <-
      process_modis_merge(
        path = path_mod11,
        date = "2021-08-15",
        subdataset = "(LST_)",
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(base_mod11, "SpatRaster")

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calc_modis_par(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          nthreads = 1L
        )
    )
  )
  testthat::expect_s3_class(calc_mod11, "data.frame")

  # ... _add arguments test
  aux <- 0L
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calc_modis_par(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          nthreads = 1L
        )
    )
  )

  # case 2: swath mod06l2
  path_mod06 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "MOD06",
      full.names = TRUE
    )
  testthat::expect_no_error(
    suppressWarnings(
      cloud0 <- process_modis_swath(
        path = path_mod06,
        subdataset = c("Cloud_Fraction_Day"),
        date = "2021-08-15"
      )
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06 <-
        calc_modis_par(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
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
  testthat::expect_warning(
    base_vnp <- process_blackmarble(
      path = path_vnp46,
      date = "2018-08-13",
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46 <-
        calc_modis_par(
          from = path_vnp46,
          locs = site_faux,
          preprocess = process_blackmarble,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          nthreads = 1,
          tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
        )
    )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # error cases
  testthat::expect_error(
    process_modis_merge(path = site_faux)
  )
  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = "2021-08-15",
      fun_agg = 3L
    )
  )
  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = "2021~08~15",
      fun_agg = "mean"
    )
  )

  site_faux_r <- site_faux
  names(site_faux_r)[1] <- "ID"
  testthat::expect_error(
    calc_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = site_faux_r
    )
  )
  testthat::expect_error(
    calc_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = matrix(c(1, 3, 4, 5), nrow = 2)
    )
  )
  testthat::expect_error(
    calc_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux)
    )
  )
  testthat::expect_error(
    calc_modis_daily(
      from = terra::rast(nrow = 3, ncol = 3, vals = 1:9, names = "a"),
      date = "2021-08-15",
      locs = array(1:12, dim = c(2, 2, 3))
    )
  )
  site_faux0 <- site_faux
  names(site_faux0)[2] <- "date"
  testthat::expect_error(
    calc_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux0)
    )
  )
  site_faux2 <- site_faux
  #site_faux2[, 4] <- NULL

  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  mcd_merge <-
    process_modis_merge(
      path = path_mcd19,
      date = "2021-08-15",
      subdataset = "(Optical_Depth)"
    )

  testthat::expect_no_error(
    calc_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_"
    )
  )

  testthat::expect_error(
    calc_modis_par(from = site_faux)
  )
  testthat::expect_error(
    calc_modis_par(from = path_mod11, product = "MOD11A1", locs = list(1, 2, 3))
  )
  testthat::expect_error(
    calc_modis_par(
      from = path_vnp46,
      locs = site_faux,
      preprocess = "fountain",
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      nthreads = 1
    )
  )
  testthat::expect_warning(
    calc_modis_par(
      from = path_vnp46,
      locs = site_faux,
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      nthreads = 2,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )
  testthat::expect_warning(
    flushed <- calc_modis_par(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      nthreads = 1,
      radius = c(-1000, 0L)
    )
  )
  testthat::expect_s3_class(flushed, "data.frame")
  testthat::expect_true(unlist(flushed[, 2]) == -99999)

})

## 5. NLCD ####
testthat::test_that("Check calc_nlcd works", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_options(
    list(sf_use_s2 = FALSE, future.resolve.recursive = 2L)
  )

  point_us1 <- cbind(lon = -114.7, lat = 38.9, site_id = 1)
  point_us2 <- cbind(lon = -114, lat = 39, site_id = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, site_id = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, site_id = 4) # france
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
  testthat::expect_no_error(
    nlcdras <- process_nlcd(path = path_testdata)
  )
  testthat::expect_s4_class(nlcdras, "SpatRaster")

  testthat::expect_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              radius = "1000"),
    "radius is not a numeric."
  )
  testthat::expect_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "whatnot",
              radius = 1000)
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              radius = -3),
    "radius has not a likely value."
  )

  # -- two modes work properly
  testthat::expect_no_error(
    calc_nlcd(locs = sf::st_as_sf(eg_data),
              from = nlcdras,
              mode = "exact",
              radius = 1000)
  )
  testthat::expect_no_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "terra",
              radius = 300)
  )
  # -- multicore mode works properly
  testthat::expect_no_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "exact",
              radius = 1000,
              nthreads = 2L)
  )
  testthat::expect_no_error(
    calc_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "terra",
              radius = 1000,
              nthreads = 2L)
  )


  # -- year is numeric
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = "2021"),
    "year is not a numeric."
  )
  # -- year has likely value
  testthat::expect_error(
    process_nlcd(path = path_testdata,
                 year = 2032),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    process_nlcd(path = path_testdata,
                 year = 1789),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    calc_nlcd(locs = 12,
              locs_id = "site_id",
              from = nlcdras)
  )
  testthat::expect_error(
    calc_nlcd(locs = eg_data,
              from = 12)
  )
  # -- nlcd_path is not a character
  testthat::expect_error(
    process_nlcd(path = 3,
                 year = 2),
    "path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  testthat::expect_error(
    process_nlcd(
                 path = nice_sentence),
    "path does not exist."
  )

  # CHECK OUTPUT
  year <- 2021
  buf_radius <- 3000
  testthat::expect_no_error(
    calc_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = buf_radius
    )
  )
  output <- calc_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras
  )
  # -- returns a data.frame
  testthat::expect_equal(class(output)[1], "data.frame")
  # nrow(output) == nrow(input)
  testthat::expect_equal(nrow(output), 4)
  # -- initial names are still in the output data.frame
  testthat::expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  # the value has changed. What affected this behavior?
  testthat::expect_equal(
    output$LDU_TEFOR_0_03000[1], 0.8119843, tolerance = 1e-7
  )
  testthat::expect_equal(
    output$LDU_TSHRB_0_03000[2], 0.9630467, tolerance = 1e-7
  )
  # -- class fraction rows should sum to 1
  testthat::expect_equal(
    unname(rowSums(output[1:2, 3:(ncol(output))])),
    rep(1, 2),
    tolerance = 1e-7
  )
  # without geometry will have 11 columns
  testthat::expect_equal(
    ncol(output), 15
  )
  output_geom <- calc_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = TRUE
  )
  # with geometry will have 12 columns
  testthat::expect_equal(
    ncol(output_geom), 15
  )
  testthat::expect_true(
    "SpatVector" %in% class(output_geom)
  )
})

## 6. NEI ####
testthat::test_that("NEI calculation", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(202401)

  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(ncpath)
  nc <- nc[grep("(Orange|Wake|Durham)", nc$NAME), ]

  neipath <- testthat::test_path("..", "testdata", "nei")

  testthat::expect_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )

  nc$GEOID <- nc$FIPS
  testthat::expect_no_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )
  # inspecting calculated results
  testthat::expect_true(inherits(neiras, "SpatVector"))
  testthat::expect_true(nrow(neiras) == 3)

  # sf case
  testthat::expect_no_error(
    neires <- process_nei(
      path = neipath,
      county = sf::st_as_sf(nc),
      year = 2017
    )
  )
  testthat::expect_true(inherits(neires, "SpatVector"))
  testthat::expect_true(nrow(neires) == 3)

  # error cases
  testthat::expect_error(
    process_nei(neipath, year = 2017)
  )
  testthat::expect_error(
    process_nei(neipath, "Orion/Betelgeuse", year = 2017)
  )
  testthat::expect_error(
    process_nei(neipath, nc, year = 2083)
  )

  # calc_nei
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncp <- terra::vect(ncp, keepgeom = TRUE, crs = "EPSG:4326")
  nc <- terra::project(nc, "EPSG:4326")

  testthat::expect_no_error(
    neicalced <- calc_nei(
      locs = ncp,
      from = neiras
    )
  )
  testthat::expect_true(any(grepl("NEI", names(neicalced))))
  testthat::expect_equal(neicalced$TRF_NEINP_0_00000, 1579079, tolerance = 1)

  # more error cases
  testthat::expect_condition(
    calc_nei(
      locs = "jittered",
      from = neiras
    )
  )

})

## 7. TRI ####
testthat::test_that("TRI calculation", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"),
                keepgeom = TRUE, crs = "EPSG:4326")
  ncpt$time <- 2018L
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  testthat::expect_s4_class(tri_r, "SpatVector")

  testthat::expect_no_error(
    tri_c <- calc_tri(
      from = tri_r,
      locs = ncpt,
      radius = 50000L
    )
  )
  testthat::expect_true(is.data.frame(tri_c))

  testthat::expect_no_error(
    calc_tri(
      from = tri_r,
      locs = sf::st_as_sf(ncpt),
      radius = 50000L
    )
  )
  testthat::expect_error(
    calc_tri(
      from = tempdir(),
      locs = ncpt,
      radius = 50000L
    )
  )
  testthat::expect_error(
    calc_tri(
      from = paste0(tdir, "/tri/"),
      locs = ncpt[, 1:2],
      radius = 50000L
    )
  )
  testthat::expect_error(
    calc_tri(
      from = paste0(tdir, "/tri/"),
      locs = ncpt,
      radius = "As far as the Earth's radius"
    )
  )
})

## 8. SEDC ####
testthat::test_that("calc_sedc tests", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"),
                keepgeom = TRUE, crs = "EPSG:4326")
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  tri_r <- terra::project(tri_r, terra::crs(ncpt))

  targcols <- grep("FUGITIVE_", names(tri_r), value = TRUE)
  testthat::expect_no_error(
    tri_sedc <-
      calc_sedc(
        locs = ncpt,
        from = tri_r,
        locs_id = "site_id",
        sedc_bandwidth = 30000,
        target_fields = targcols
      )
  )
  testthat::expect_s3_class(tri_sedc, "data.frame")

  testthat::expect_no_error(
    calc_sedc(
      locs = sf::st_as_sf(ncpt),
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      sedc_bandwidth = 30000,
      target_fields = targcols
    )
  )

  # warning case: duplicate field names between locs and from
  ncpta <- ncpt
  ncpta$YEAR <- 2018
  testthat::expect_warning(
    calc_sedc(
      locs = ncpta,
      from = sf::st_as_sf(tri_r),
      locs_id = "site_id",
      sedc_bandwidth = 30000,
      target_fields = targcols
    )
  )

})

## 9. HMS ####
testthat::test_that("calc_hms returns expected.", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_hms)
  )
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (r in seq_along(radii)) {
      hms <-
        process_hms(
          date = c("2022-06-10", "2022-06-11"),
          variable = density,
          path = testthat::test_path(
            "..",
            "testdata",
            "hms"
          )
        )
      hms_covariate <-
        calc_hms(
          from = hms,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          geom = FALSE
        )
      # set column names
      hms_covariate <- calc_setcolumns(
        from = hms_covariate,
        lag = 0,
        dataset = "hms",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(hms_covariate) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covariate) == 3
      )
      # expect 2 rows
      expect_true(
        nrow(hms_covariate) == 2
      )
      # expect integer for binary value
      expect_true(
        class(hms_covariate[, 3]) == "integer"
      )
      # expect binary
      expect_true(
        all(unique(hms_covariate[, 3]) %in% c(0, 1))
      )
    }
  }
})

testthat::test_that("calc_hms with geom = TRUE", {
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  hms_dir <- testthat::test_path(
    "..", "testdata", "hms"
  )
  hms <-  process_hms(
    date = c("2022-06-10", "2022-06-13"),
    variable = "light",
    path = hms_dir
  )
  hms_covariate_geom <- calc_hms(
    from = hms,
    locs = ncp,
    locs_id = "site_id",
    radius = 0,
    geom = TRUE
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(hms_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(hms_covariate_geom)
  )
})

testthat::test_that("calc_hms returns expected with missing polygons.", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_hms)
  )
  hms_dir <- testthat::test_path(
    "..", "testdata", "hms"
  )
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (r in seq_along(radii)) {
      hms <-
        process_hms(
          date = c("2022-06-10", "2022-06-13"),
          variable = density,
          path = hms_dir
        )
      hms_covariate <-
        calc_hms(
          from = hms,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          geom = FALSE
        )
      # set column names
      hms_covariate <- calc_setcolumns(
        from = hms_covariate,
        lag = 0,
        dataset = "hms",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(hms_covariate) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covariate) == 3
      )
      # expect 4 rows
      expect_true(
        nrow(hms_covariate) == 4
      )
      # expect integer for binary value
      expect_true(
        class(hms_covariate[, 3]) == "integer"
      )
      # expect binary
      expect_true(
        all(unique(hms_covariate[, 3]) %in% c(0, 1))
      )
    }
  }
})

## 10. GMTED ####
testthat::test_that("calc_gmted returns expected.", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
      for (a in seq_along(radii)) {
        gmted <-
          process_gmted(
            variable = c(statistic, resolution),
            path =
            testthat::test_path(
              "..",
              "testdata",
              "gmted"
            )
          )
        gmted_covariate <-
          calc_gmted(
            from = gmted,
            locs = ncp,
            locs_id = "site_id",
            radius = radii[a],
            fun = "mean"
          )
        # set column names
        gmted_covariate <- calc_setcolumns(
          from = gmted_covariate,
          lag = 0,
          dataset = "gmted",
          locs_id = "site_id"
        )
        # expect output is data.frame
        expect_true(
          class(gmted_covariate) == "data.frame"
        )
        # expect 2 columns
        expect_true(
          ncol(gmted_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(gmted_covariate[, 3]) == "numeric"
        )
      }
    }
  }
  testthat::expect_no_error(
    gmted <- process_gmted(
      variable = c("Breakline Emphasis", "7.5 arc-seconds"),
      testthat::test_path(
        "..", "testdata", "gmted", "be75_grd"
      )
    )
  )
  testthat::expect_no_error(
    gmted_geom <- calc_gmted(
      gmted,
      ncp,
      "site_id",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(gmted_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gmted_geom)
  )
})

## 11. NARR ####
testthat::test_that("calc_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_narr)
  )
  for (v in seq_along(variables)) {
    variable <- variables[v]
    for (r in seq_along(radii)) {
      narr <-
        process_narr(
          date = c("2018-01-01", "2018-01-01"),
          variable = variable,
          path =
          testthat::test_path(
            "..",
            "testdata",
            "narr",
            variable
          )
        )
      narr_covariate <-
        calc_narr(
          from = narr,
          locs = ncp,
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      narr_covariate <- calc_setcolumns(
        from = narr_covariate,
        lag = 0,
        dataset = "narr",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(narr_covariate) == "data.frame"
      )
      if (variable == "weasd") {
        # expect 3 columns (no pressure level)
        expect_true(
          ncol(narr_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(narr_covariate[, 3]) == "numeric"
        )
      } else {
        # expect 4 columns
        expect_true(
          ncol(narr_covariate) == 4
        )
        # expect numeric value
        expect_true(
          class(narr_covariate[, 4]) == "numeric"
        )
      }
      # expect $time is class Date
      expect_true(
        "POSIXct" %in% class(narr_covariate$time)
      )
    }
  }
  # with geometry
  testthat::expect_no_error(
    narr_covariate_geom <- calc_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(narr_covariate_geom), 4 # 4 columns because omega has pressure levels
  )
  testthat::expect_true(
    "SpatVector" %in% class(narr_covariate_geom)
  )
})

## 11. GEOS-CF ####
testthat::test_that("calc_geos returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  collections <- c(
    "a",
    "c"
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    for (r in seq_along(radii)) {
      geos <-
        process_geos(
          date = c("2018-01-01", "2018-01-01"),
          variable = "O3",
          path =
          testthat::test_path(
            "..",
            "testdata",
            "geos",
            collection
          )
        )
      geos_covariate <-
        calc_geos(
          from = geos,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      geos_covariate <- calc_setcolumns(
        from = geos_covariate,
        lag = 0,
        dataset = "geos",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(geos_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(geos_covariate) == 4
      )
      # expect numeric value
      expect_true(
        class(geos_covariate[, 4]) == "numeric"
      )
      # expect $time is class POSIXt
      expect_true(
        "POSIXt" %in% class(geos_covariate$time)
      )
    }
  }
  # with included geometry
  testthat::expect_no_error(
    geos_covariate_geom <- calc_geos(
      from = geos,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(geos_covariate_geom), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(geos_covariate_geom)
  )
})

## 12. SEDAC: Population ####
testthat::test_that("calc_sedac_population returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  paths <- list.files(testthat::test_path(
    "..", "testdata", "population"
  ), full.names = TRUE)
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_sedac_population)
  )
  for (p in seq_along(paths)) {
    path <- paths[p]
    for (r in seq_along(radii)) {
      pop <-
        process_sedac_population(
          path = paths
        )
      pop_covariate <-
        calc_sedac_population(
          from = pop,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      pop_covariate <- calc_setcolumns(
        from = pop_covariate,
        lag = 0,
        dataset = "pop",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(pop_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(pop_covariate) == 3
      )
      # expect numeric value
      expect_true(
        class(pop_covariate[, 3]) == "numeric"
      )
      # expect $time is class integer for year
      expect_true(
        "integer" %in% class(pop_covariate$time)
      )
    }
  }
  # with included geometry
  testthat::expect_no_error(
    pop_covariate_geom <- calc_sedac_population(
      from = pop,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(pop_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(pop_covariate_geom)
  )
})

## 13. SEDAC: Global Roads ####
testthat::test_that("groads calculation works", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # test data generation
  ncp <- data.frame(
    site_id = c("1", "2"),
    lon = c(-78.899, -78.643669),
    lat = c(35.8774, 35.785342),
    time = c(2022, 2022)
  )
  # ncp <- terra::vect(ncp, keepgeom = TRUE, crs = "EPSG:4326")
  path_groads <- testthat::test_path("..", "testdata", "groads_test.shp")
  groads <- terra::vect(path_groads)

  testthat::expect_no_error(
    groads_res <- calc_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000
    )
  )

  testthat::expect_error(
    calc_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 0
    )
  )

  # expect data.frame
  testthat::expect_s3_class(groads_res, "data.frame")

  # return with geometry
  testthat::expect_no_error(
    groads_geom <- calc_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(groads_geom), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(groads_geom)
  )
})


## 14. MERRA2 ####
testthat::test_that("calc_merra2 returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collections <- c(
    "inst1_2d_int_Nx", "inst3_2d_gas_Nx", "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx", "tavg1_2d_chm_Nx", "tavg3_3d_udt_Np" #*
  )
  variables <- c(
    "CPT", "AODANA", "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN", "COCL", "DUDTANA" #*
  )
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_merra2)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    variable <- variables[c]
    for (r in seq_along(radii)) {
      merra2 <-
        process_merra2(
          date = c("2018-01-01", "2018-01-01"),
          variable = variable,
          path =
          testthat::test_path(
            "..",
            "testdata",
            "merra2",
            collection
          )
        )
      merra2_covariate <-
        calc_merra2(
          from = merra2,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
        )
      # set column names
      merra2_covariate <- calc_setcolumns(
        from = merra2_covariate,
        lag = 0,
        dataset = "merra2",
        locs_id = "site_id"
      )
      # expect output is data.frame
      expect_true(
        class(merra2_covariate) == "data.frame"
      )
      if (grepl("lev", names(merra2)[1])) {
        # expect 4 columns
        expect_true(
          ncol(merra2_covariate) == 4
        )
        # expect numeric value
        expect_true(
          class(merra2_covariate[, 4]) == "numeric"
        )
      } else {
        # expect 3 columns
        expect_true(
          ncol(merra2_covariate) == 3
        )
        # expect numeric value
        expect_true(
          class(merra2_covariate[, 3]) == "numeric"
        )
      }
      # expect $time is class Date
      expect_true(
        "POSIXt" %in% class(merra2_covariate$time)
      )
    }
  }
  # with included geometry
  testthat::expect_no_error(
    merra2_covariate_geom <- calc_merra2(
      from = merra2,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(merra2_covariate_geom), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(merra2_covariate_geom)
  )
})

## 15. GRIDMET ####
testthat::test_that("calc_gridmet returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_gridmet)
  )
  for (r in seq_along(radii)) {
    gridmet <-
      process_gridmet(
        date = c("2018-01-03", "2018-01-03"),
        variable = "pr",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "gridmet",
          "pr"
        )
      )
    gridmet_covariate <-
      calc_gridmet(
        from = gridmet,
        locs = data.table::data.table(ncp),
        locs_id = "site_id",
        radius = radii[r],
        fun = "mean"
      )
    # set column names
    gridmet_covariate <- calc_setcolumns(
      from = gridmet_covariate,
      lag = 0,
      dataset = "gridmet",
      locs_id = "site_id"
    )
    # expect output is data.frame
    expect_true(
      class(gridmet_covariate) == "data.frame"
    )
    # expect 3 columns
    expect_true(
      ncol(gridmet_covariate) == 3
    )
    # expect numeric value
    expect_true(
      class(gridmet_covariate[, 3]) == "numeric"
    )
    # expect $time is class Date
    expect_true(
      "POSIXt" %in% class(gridmet_covariate$time)
    )
  }
  # with included geometry
  testthat::expect_no_error(
    gridmet_covariate_geom <- calc_gridmet(
      from = gridmet,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(gridmet_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(gridmet_covariate_geom)
  )
})

## 16. TerraClimate ####
testthat::test_that("calc_terraclimate returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  radii <- c(0, 1000)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  expect_true(
    is.function(calc_terraclimate)
  )
  for (r in seq_along(radii)) {
    terraclimate <-
      process_terraclimate(
        date = c("2018-01-01", "2018-01-01"),
        variable = "Precipitation",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "terraclimate",
          "ppt"
        )
      )
    terraclimate_covariate <-
      calc_terraclimate(
        from = terraclimate,
        locs = data.table::data.table(ncp),
        locs_id = "site_id",
        radius = radii[r],
        fun = "mean"
      )
    # set column names
    terraclimate_covariate <- calc_setcolumns(
      from = terraclimate_covariate,
      lag = 0,
      dataset = "terraclimate",
      locs_id = "site_id"
    )
    # expect output is data.frame
    expect_true(
      class(terraclimate_covariate) == "data.frame"
    )
    # expect 3 columns
    expect_true(
      ncol(terraclimate_covariate) == 3
    )
    # expect numeric value
    expect_true(
      class(terraclimate_covariate[, 3]) == "numeric"
    )
    # expect date and time column
    expect_true(
      nchar(terraclimate_covariate$time)[1] == 6
    )
  }
  # with included geometry
  testthat::expect_no_error(
    terraclimate_covariate_geom <- calc_terraclimate(
      from = terraclimate,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  )
  testthat::expect_equal(
    ncol(terraclimate_covariate_geom), 3
  )
  testthat::expect_true(
    "SpatVector" %in% class(terraclimate_covariate_geom)
  )
})

## 17. Lagged variables ####
testthat::test_that("calc_lagged returns as expected.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  lags <- c(0, 1, 2)
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calc_lagged)
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
      calc_narr(
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
      narr_lagged <- calc_lagged(
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
        calc_lagged(
          from = narr_covariate,
          date = c("2018-01-01", "2018-01-10"),
          lag = lags[l],
          locs_id = "site_id",
          time_id = "time"
        )
      )
      narr_lagged <- calc_lagged(
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

## 17.1 calc_lag with SpatVector
testthat::test_that("calc_lagged error with SpatVector.", {
  withr::local_package("terra")
  withr::local_package("data.table")
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  # expect function
  testthat::expect_true(
    is.function(calc_lagged)
  )
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
  narr_covariate_geom <-
    calc_narr(
      from = narr,
      locs = ncp,
      locs_id = "site_id",
      radius = 0,
      fun = "mean",
      geom = TRUE
    )
  testthat::expect_error(
    calc_lagged(from = narr_covariate_geom)
  )
})

## 18. Wrapper ####
testthat::test_that("calc_covariates wrapper works", {

  withr::local_package("rlang")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  candidates <-
    c("modis", "koppen-geiger",
      "koeppen-geiger", "koppen", "koeppen",
      "geos", "dummies", "gmted",
      "sedac_groads", "groads", "roads",
      "ecoregions", "ecoregion", "hms","smoke",
      "gmted", "narr", "geos",
      "sedac_population", "population", "nlcd",
      "merra", "MERRA", "merra2", "MERRA2",
      "tri", "nei")
  for (cand in candidates) {
    testthat::expect_error(
      calc_covariates(covariate = cand)
    )
  }
})


testthat::test_that("calc_covariates wrapper works", {

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
    tri_c <- calc_covariates(
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
      calc_covariates(covariate = cand)
    )
  }
})

# calc check time
testthat::test_that("calc_check_time identifies missing `time` column.", {
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

# Calc message
testthat::test_that("calc_message exception",
  {
    testthat::expect_no_error(
      calc_message("gmted", "mean", "2020", "year", NULL)
    )
    testthat::expect_no_error(
      calc_message("narr", "shum", 2000, "year", NULL)
    )
  }
)

# calc time
testthat::test_that("calc time remains", {
  testthat::expect_no_error(
    rr <- calc_time("eternal", "timeless")
  )
  testthat::expect_true(rr == "eternal")
})

# calc worker
testthat::test_that("calc_worker remaining", {
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
