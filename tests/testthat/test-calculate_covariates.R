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
  testthat::expect_equal(ncol(ecor_res), 3L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
    sum(unlist(ecor_res[, dum_cn])), 2L
  )
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
                keepgeom = TRUE,
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
        paths = path_mod11,
        date_in = "2021-08-15",
        subdataset = "(LST_)",
        foo = "mean"
      )
  )
  testthat::expect_s4_class(base_mod11, "SpatRaster")

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calc_modis_par(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          fun_hdf = process_modis_merge,
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
      process_modis_swath(
        paths = path_mod06,
        date_in = "2021-08-15"
      )
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06 <-
        calc_modis_par(
          from = path_mod06,
          locs = site_faux,
          fun_hdf = process_modis_swath,
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
    base_vnp <- process_bluemarble(
      paths = path_vnp46,
      date_in = "2018-08-13",
      tile_df = process_bluemarble_corners(c(9, 10), c(5, 5))
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46 <-
        calc_modis_par(
          from = path_vnp46,
          locs = site_faux,
          fun_hdf = process_bluemarble,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          nthreads = 1,
          tile_df = process_bluemarble_corners(c(9, 10), c(5, 5))
        )
    )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # error cases
  testthat::expect_error(
    process_modis_merge(paths = site_faux)
  )
  testthat::expect_error(
    process_modis_merge(
      paths = path_mod11,
      date_in = "2021-08-15",
      foo = 3L
    )
  )
  testthat::expect_error(
    process_modis_merge(
      paths = path_mod11,
      date_in = "2021~08~15",
      foo = "mean"
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
  site_faux2[, 4] <- NULL

  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  mcd_merge <-
    process_modis_merge(
      paths = path_mcd19,
      date_in = "2021-08-15",
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
      fun_hdf = "fountain",
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      nthreads = 1
    )
  )
  testthat::expect_warning(
    calc_modis_par(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      nthreads = 1
    )
  )
  testthat::expect_warning(
    flushed <- calc_modis_par(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      fun_hdf = process_bluemarble,
      subdataset = 3L,
      nthreads = 1,
      radius = c(-1000, 0L),
      tile_df = process_bluemarble_corners(c(9, 10), c(5, 5))
    )
  )
  testthat::expect_s3_class(flushed, "data.frame")
  testthat::expect_true(any(unlist(flushed) == -99999))

})


testthat::test_that("Check extract_nlcd_ratio works", {
  withr::local_package("terra")
  withr::local_package("exactextractr")

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
  testthat::expect_no_error(
    nlcdras <- process_nlcd(path = path_testdata)
  )
  testthat::expect_s4_class(nlcdras, "SpatRaster")

  testthat::expect_error(
    calc_nlcd_ratio(locs = eg_data,
                    from = nlcdras,
                    radius = "1000"),
    "radius is not a numeric."
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calc_nlcd_ratio(locs = eg_data,
                    from = nlcdras,
                    radius = -3),
    "radius has not a likely value."
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
  # -- data_vect is a SpatVector
  testthat::expect_error(
    calc_nlcd_ratio(locs = 12,
                    from = nlcdras),
    "locs is not a terra::SpatVector."
  )
  testthat::expect_error(
    calc_nlcd_ratio(locs = eg_data,
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
    calc_nlcd_ratio(
      locs = eg_data,
      from = nlcdras,
      radius = buf_radius
    )
  )
  output <- calc_nlcd_ratio(
    locs = eg_data,
    radius = buf_radius,
    from = nlcdras
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
  testthat::expect_s4_class(neiras, "SpatVector")

  # sf case
  testthat::expect_no_error(
    process_nei(
      path = neipath,
      county = sf::st_as_sf(nc),
      year = 2017
    )
  )


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
  ncp$time <- 2018
  ncp <- terra::vect(ncp, keepgeom = TRUE, crs = "EPSG:4326")
  nc <- terra::project(nc, "EPSG:4326")

  testthat::expect_no_error(
    neicalced <- calc_nei(
      locs = ncp,
      from = neiras
    )
  )
  testthat::expect_true(any(grepl("NEI17", names(neicalced))))
  testthat::expect_equal(neicalced$TRF_NEI17_0_00000, 1579079, tolerance = 1)

  # more error cases
  testthat::expect_condition(
    calc_nei(
      locs = "jittered",
      from = neiras
    )
  )

  names(ncp)[4] <- "year"
  testthat::expect_error(
    calc_nei(
      locs = ncp,
      from = neiras
    )
  )

})


testthat::test_that("TRI calculation", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

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


testthat::test_that("calc_sedc tests", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018
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
          date_start = "2022-06-10",
          date_end = "2022-06-11",
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
          radius = radii[r]
        )
      # expect output is data.frame
      expect_true(
        class(hms_covariate) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covariate) == 3
      )
      # expect 6 rows
      expect_true(
        nrow(hms_covariate) == 6
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
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (r in seq_along(radii)) {
      hms <-
        process_hms(
          date_start = "2022-06-10",
          date_end = "2022-06-13",
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
          radius = radii[r]
        )
      # expect output is data.frame
      expect_true(
        class(hms_covariate) == "data.frame"
      )
      # expect 3 columns
      expect_true(
        ncol(hms_covariate) == 3
      )
      # expect 12 rows
      expect_true(
        nrow(hms_covariate) == 12
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
              "gmted",
              paste0(
                process_gmted_codes(
                  statistic,
                  statistic = TRUE,
                  invert = FALSE
                ),
                process_gmted_codes(
                  resolution,
                  resolution = TRUE,
                  invert = FALSE
                ),
                "_grd"
              )
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
        # expect output is data.frame
        expect_true(
          class(gmted_covariate) == "data.frame"
        )
        # expect 2 columns
        expect_true(
          ncol(gmted_covariate) == 2
        )
        # expect numeric value
        expect_true(
          class(gmted_covariate[, 2]) == "numeric"
        )
      }
    }
  }
})

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
          date_start = "2018-01-01",
          date_end = "2018-01-01",
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
      # expect output is data.frame
      expect_true(
        class(narr_covariate) == "data.frame"
      )
      # expect 4 columns
      expect_true(
        ncol(narr_covariate) == 4
      )
      # expect numeric value
      expect_true(
        class(narr_covariate[, 4]) == "numeric"
      )
      # expect date column
      expect_true(
        class(narr_covariate[, 2]) == "Date"
      )
    }
  }
})

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
          date_start = "2018-01-01",
          date_end = "2018-01-01",
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
      # expect date and time column
      expect_true(
        "POSIXt" %in% class(geos_covariate$date)
      )
    }
  }
})

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
          path = path
        )
      pop_covariate <-
        calc_sedac_population(
          from = pop,
          locs = data.table::data.table(ncp),
          locs_id = "site_id",
          radius = radii[r],
          fun = "mean"
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
      # expect date and time column
      expect_true(
        "integer" %in% class(pop_covariate$year)
      )
    }
  }
})
