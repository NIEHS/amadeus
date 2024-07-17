# test process_covariates ####
testthat::test_that("test generic process_covariates", {
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
                   "prism", "olm", "openlandmap", "terraclimate", "gridmet")
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

# test MODIS suites ####
testthat::test_that("test MODIS prefilter", {
  # main test
  txt_products <- c("MOD11A1", "MOD13A2", "MOD09GA", "MCD19A2")
  txt_exp_output <-
    c(
      MOD11A1 = "(LST_)",
      MOD13A2 = "(NDVI)",
      MOD09GA = "(sur_refl_b0)",
      MCD19A2 = "(Optical_Depth)"
    )
  txt_exp_output <- unname(txt_exp_output)
  # expect
  testthat::expect_message(
    mcdtest <- process_modis_sds("MCD19A2")
  )
  testthat::expect_equal(
    mcdtest, "(Optical_Depth)"
  )
  testthat::expect_no_error(
    process_modis_sds("MCD19A2", "(cos|RelAZ|Angle)")
  )
  for (i in 1:3) {
    testthat::expect_equal(
      process_modis_sds(txt_products[i]), txt_exp_output[i]
    )
  }
  testthat::expect_no_error(
    filt_other <- process_modis_sds("ignored", "(cos)")
  )
  testthat::expect_equal(filt_other, "(cos)")

})


testthat::test_that("process_flatten_sds", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  mcd19 <- testthat::test_path(
    "..", "testdata", "modis", "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
  )
  mod09 <- testthat::test_path(
    "..", "testdata", "modis", "MOD09GA.A2021227.h11v05.061.2021229035936.hdf"
  )

  # main test: mcd19
  testthat::expect_no_error(
    mcdaggr <-
      process_flatten_sds(
        path = mcd19,
        subdataset = "Optical_Depth",
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(mcdaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(mcdaggr), 2L)
  testthat::expect_equal(
    all(grepl("^Optical", names(mcdaggr))),
    TRUE
  )

  # flatten error
  path_mod06 <-
    testthat::test_path(
      "..", "testdata", "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )

  testthat::expect_error(
    process_flatten_sds(
      path = path_mod06,
      subdataset = "(Fraction)",
      fun_agg = "mean"
    )
  )

  # mod09 test
  mod09_sub <-
    sprintf("HDF4_EOS:EOS_GRID:%s:MODIS_Grid_500m_2D:sur_refl_b01_1", mod09)
  # main test: mcd19
  testthat::expect_no_error(
    modaggr <-
      process_flatten_sds(
        path = mod09_sub,
        subdataset = NULL,
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(modaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(modaggr), 1L)
  testthat::expect_true(grepl("^500m Surface", names(modaggr)))
})


testthat::test_that("process_modis_merge is good to go", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_error(
    process_modis_merge(
      path = path_mod11,
      date = "2021-08-15",
      subdataset = "(LST_)"
    )
  )
  # case 2: standard mod13a2
  path_mod13 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD13A2.A2021225.h11v05.061.2021320163751.hdf"
    )
  testthat::expect_no_error(
    process_modis_merge(
      path = path_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)"
    )
  )

  # case 3: standard mcd19a2
  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  testthat::expect_no_error(
    process_modis_merge(
      path = path_mcd19,
      date = "2021-08-15",
      subdataset = "(Optical_Depth)"
    )
  )

  # case 3: standard mcd19a2
  path_mod09 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD09GA.A2021227.h11v05.061.2021229035936.hdf"
    )
  testthat::expect_no_error(
    process_modis_merge(
      path = path_mod09,
      date = "2021-08-15",
      subdataset = "(sur_refl_b0)"
    )
  )

  # multiple files
  paths_mod13 <- list.files(
    testthat::test_path("../testdata/modis/"),
    pattern = "MOD13A2",
    full.names = TRUE
  )
  testthat::expect_no_error(
    process_modis_merge(
      path = paths_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)"
    )
  )
  testthat::expect_error(
    process_modis_merge(
      path = paths_mod13,
      date = "2021-08-13",
      subdataset = "(NDVI)",
      fun_agg = 3L
    )
  )

})


testthat::test_that("VNP46 preprocess tests", {
  withr::local_package("terra")

  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      "^VNP46A2",
      full.names = TRUE
    )

  testthat::expect_no_error(
    corn <- process_blackmarble_corners()
  )
  testthat::expect_error(
    process_blackmarble_corners(hrange = c(99, 104))
  )

  testthat::expect_warning(
    vnp46_proc <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_warning(
    vnp46_proc2 <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      subdataset = c(3L, 5L),
      date = "2018-08-13"
    )
  )

  testthat::expect_s4_class(vnp46_proc2, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc2), 2L)

  testthat::expect_error(
    process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018~08~13"
    )
  )

})


testthat::test_that("Swath warping abides", {
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod06 <-
    testthat::test_path(
      "..", "testdata", "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )
  path_mod06 <-
    sprintf("HDF4_EOS:EOS_SWATH:%s:mod06:Cloud_Fraction_Night", path_mod06)
  # internal warning from stars
  testthat::expect_warning(
    warped <- process_modis_warp(
      path = path_mod06
    )
  )
  testthat::expect_s3_class(warped, "stars")
  testthat::expect_equal(
    unname(stars::st_res(warped)[1]), 0.1, tolerance = 1e-6
  )

  path_mod06s <-
    list.files(
      testthat::test_path("..", "testdata", "modis"),
      pattern = "MOD06_L2",
      full.names = TRUE
    )

  testthat::expect_warning(
    warped4 <- process_modis_swath(
      path = path_mod06s,
      date = "2021-08-15",
      subdataset = c("Cloud_Fraction_Night", "Cloud_Fraction_Day")
    )
  )
  testthat::expect_s4_class(warped4, "SpatRaster")


})


testthat::test_that("Other MODIS function errors", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))
  path_mod06 <-
    testthat::test_path(
      "..", "testdata", "modis",
      "MOD06_L2.A2021227.0320.061.2021227134022.hdf"
    )
  path_mod06e <-
    sprintf("HDF4_EOS:EOS_SWATH:%s:mod06:Cloud_Fraction_Night", path_mod06)

  testthat::expect_no_error(
    suppressWarnings(
      process_modis_swath(
        path = path_mod06,
        subdataset = "Cloud_Fraction_Night",
        date = "2021-08-15"
      )
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021~08~15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021-13-15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      path = path_mod06,
      subdataset = "Cloud_Fraction_Night",
      date = "2021-12-45"
    )
  )
})


# test Ecoregions ####
testthat::test_that("read ecoregion", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_eco <- testthat::test_path("..", "testdata", "eco_l3_clip.gpkg")
  testthat::expect_no_error(
    eco <- process_ecoregion(path_eco)
  )

  # test with cropping extent
  testthat::expect_no_error(
    process_ecoregion(path_eco, extent = terra::ext(eco))
  )
  ecotemp <- sf::st_read(path_eco)
  # nolint start
  addpoly <-
    "POLYGON ((-70.2681 43.6787, -70.252234 43.677145, -70.251036 -43.680758, -70.268666 43.681505, -70.2681 43.6787))"
  # nolint end
  addpoly <- sf::st_as_sfc(addpoly, crs = "EPSG:4326")
  addpoly <- sf::st_transform(addpoly, sf::st_crs(ecotemp))
  ecotemp[1, "geom"] <- addpoly
  tdir <- tempdir()
  sf::st_write(ecotemp, paste0(tdir, "/ecoregions.gpkg"), append = FALSE)
  testthat::expect_no_error(
    suppressWarnings(process_ecoregion(paste0(tdir, "/ecoregions.gpkg")))
  )
})


# test NLCD ####
testthat::test_that("process_nlcd tests", {
  withr::local_package("terra")

  path_nlcd19 <-
    testthat::test_path(
      "..",
      "testdata"
    )

  testthat::expect_no_error(
    nlcd19 <- process_nlcd(path = path_nlcd19, year = 2019)
  )
  # test with extent cropping
  testthat::expect_no_error(
    nlcd19_ext <- process_nlcd(
      path = path_nlcd19,
      year = 2019,
      extent = terra::ext(-1580000, -1520000, 1920000, 1980000)
    )
  )
  testthat::expect_s4_class(nlcd19, "SpatRaster")
  testthat::expect_equal(unname(terra::metags(nlcd19, name = "year")), "2019")

  # error cases
  testthat::expect_error(
    process_nlcd(path = 1L)
  )
  testthat::expect_error(
    process_nlcd(path = "/universe/galaxy/solarsys/earth/usa.nc")
  )
  testthat::expect_error(
    process_nlcd(path_nlcd19, "nineteen eighty-four")
  )
  testthat::expect_error(
    process_nlcd(path_nlcd19, year = 2020)
  )
  # make duplicate with tif and img
  tdir <- tempdir()
  dir.create(paste0(tdir, "/nlcd_all"))
  file.create(paste0(tdir, "/nlcd_all/nlcd_2019_land_cover_20240624.tif"))
  file.create(paste0(tdir, "/nlcd_all/nlcd_2019_land_cover_20240624.img"))
  testthat::expect_error(
    process_nlcd(path = paste0(tdir, "/nlcd_all"), year = 2019)
  )

})


# test Koppen-Geiger ####
testthat::test_that("process_koppen_geiger tests", {
  withr::local_package("terra")
  path_kgeiger <-
    testthat::test_path("../testdata", "koppen_subset.tif")

  testthat::expect_no_error(
    kgeiger <- process_koppen_geiger(path_kgeiger)
  )

  # test with cropping extent
  testthat::expect_no_error(
    kgeiger_ext <- process_koppen_geiger(
      path_kgeiger,
      extent = terra::ext(kgeiger)
    )
  )

  testthat::expect_s4_class(kgeiger, "SpatRaster")
})

# test TRI ####
testthat::test_that("process_tri tests", {
  withr::local_package("terra")
  path_tri <- testthat::test_path("../testdata", "tri", "")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri)
  )
  testthat::expect_s4_class(tri_r, "SpatVector")

  # test with cropping extent
  testthat::expect_no_error(
    tri_r_ext <- process_tri(
      path = path_tri,
      extent = terra::ext(tri_r)
    )
  )
  testthat::expect_s4_class(tri_r, "SpatVector")
})

# test NEI ####
testthat::test_that("process_nei tests", {
  withr::local_package("terra")

  path_nei <- testthat::test_path("../testdata", "nei", "")
  path_cnty <- system.file("gpkg/nc.gpkg", package = "sf")
  path_cnty <- terra::vect(path_cnty)
  path_cnty$GEOID <- path_cnty$FIPS

  testthat::expect_no_error(
    neinc <- process_nei(path = path_nei, year = 2017, county = path_cnty)
  )
  testthat::expect_s4_class(neinc, "SpatVector")

  # error cases
  testthat::expect_error(
    process_nei(testthat::test_path("../testdata", "modis"), year = 2017)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2030, county = path_cnty)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = NULL)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = array(1, 2))
  )
  names(path_cnty)[which(names(path_cnty) == "GEOID")] <- "COUNTYID"
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = path_cnty)
  )

})


## ephemeral: process_conformity tests
testthat::test_that("process_conformity tests", {
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

# test SEDAC population ####
testthat::test_that("process_sedac_population returns expected.", {
  withr::local_package("terra")
  paths <- list.files(
    testthat::test_path(
      "..",
      "testdata",
      "population"
    ),
    pattern = ".tif",
    full.names = TRUE
  )
  # expect function
  expect_true(
    is.function(process_sedac_population)
  )
  for (p in seq_along(paths)) {
    pop <-
      process_sedac_population(
        path = paths[p]
      )
    # expect output is a SpatRaster
    expect_true(
      class(pop)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(pop)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(pop))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(pop)[1:2])
    )
  }
  # test with cropping extent
  testthat::expect_no_error(
    pop_ext <- process_sedac_population(
      paths[1],
      extent = terra::ext(pop)
    )
  )
})

testthat::test_that("process_sedac_population returns null for netCDF.", {
  pop <-
    process_sedac_population(
      testthat::test_path(
        "..",
        "testdata",
        "population",
        "pLaCeHoLdEr.nc"
      )
    )
  expect_true(
    is.null(pop)
  )
})

testthat::test_that("sedac_codes", {
  string <- "2.5 minute"
  testthat::expect_no_error(
    code <- process_sedac_codes(string)
  )
  testthat::expect_equal(code, "2pt5_min")
})


# test HMS ####
testthat::test_that("process_hms returns expected.", {
  withr::local_package("terra")
  # expect function
  testthat::expect_true(
    is.function(process_hms)
  )

  hms <-
    process_hms(
      date = c("2022-06-10", "2022-06-11"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      )
    )
  # expect output is a SpatVector or character
  testthat::expect_true(
    class(hms)[1] %in% c("SpatVector", "character")
  )
  if (class(hms)[1] == "SpatVector") {
    # expect non-null coordinate reference system
    testthat::expect_false(
      is.null(terra::crs(hms))
    )
    # expect two columns
    testthat::expect_true(
      ncol(hms) == 2
    )
    # expect density and date column
    testthat::expect_true(
      all(c("Density", "Date") %in% names(hms))
    )
  } else if (class(hms)[1] == "character") {
    # expect first is density type
    testthat::expect_true(
      hms[1] %in% c("Light", "Medium", "Heavy")
    )
    # expect other elements are 10 character dates
    testthat::expect_true(
      all(nchar(hms[2:length(hms)]) == 10)
    )
  }
  # test with cropping extent
  testthat::expect_no_error(
    hms_ext <- process_hms(
      date = c("2022-06-10", "2022-06-11"),
      path = testthat::test_path(
        "..",
        "testdata",
        "hms"
      ),
      extent = terra::ext(hms)
    )
  )
})

# test GMTED ####
testthat::test_that("process_gmted returns expected.", {
  withr::local_package("terra")
  statistics <- c(
    "Breakline Emphasis", "Systematic Subsample"
  )
  resolutions <- c(
    "7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"
  )
  # expect function
  expect_true(
    is.function(process_gmted)
  )
  for (s in seq_along(statistics)) {
    statistic <- statistics[s]
    for (r in seq_along(resolutions)) {
      resolution <- resolutions[r]
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
      # expect output is a SpatRaster
      expect_true(
        class(gmted)[1] == "SpatRaster"
      )
      # expect values
      expect_true(
        terra::hasValues(gmted)
      )
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(gmted))
      )
      # expect lon and lat dimensions to be > 1
      expect_false(
        any(c(0, 1) %in% dim(gmted)[1:2])
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    gmted_ext <-
        process_gmted(
          variable = c("Breakline Emphasis", "7.5 arc-seconds"),
          path =
          testthat::test_path(
            "..",
            "testdata",
            "gmted",
            "be75_grd"
          ),
          ext = terra::ext(gmted)
        )
  )
})

testthat::test_that("import_gmted returns error with non-vector variable.", {
  expect_error(
    gmted <-
      process_gmted(
        variable <- "Breakline Emphasis; 7.5 arc-seconds",
        path = testthat::test_path(
          "..",
          "testdata",
          "gmted"
        )
      )
  )
})

testthat::test_that("gmted_codes inversion", {
  teststring <- "mx"
  testthat::expect_no_error(
    statorig <- process_gmted_codes(
      teststring,
      statistic = TRUE,
      resolution = FALSE,
      invert = TRUE
    )
  )
  testthat::expect_equal(statorig, "Maximum Statistic")

  teststring <- "75"
  testthat::expect_no_error(
    resoorig <- process_gmted_codes(
      teststring,
      statistic = FALSE,
      resolution = TRUE,
      invert = TRUE
    )
  )
  testthat::expect_equal(resoorig, "7.5 arc-seconds")
})


## test NARR ####
testthat::test_that("process_narr returns expected.", {
  withr::local_package("terra")
  variables <- c(
    "weasd",
    "omega"
  )
  # expect function
  expect_true(
    is.function(process_narr)
  )
  for (v in seq_along(variables)) {
    narr <-
      process_narr(
        date = c("2018-01-01", "2018-01-01"),
        variable = variables[v],
        path =
        testthat::test_path(
          "..",
          "testdata",
          "narr",
          variables[v]
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(narr)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(narr)
    )
    # expect non-null coordinate reference system
    expect_false(
      is.null(terra::crs(narr))
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(narr)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(narr))
    )
    # expect dimensions according to levels
    if (variables[v] == "weasd") {
      expect_true(
        dim(narr)[3] == 1
      )
    } else if (variables[v] == "omega") {
      expect_true(
        dim(narr)[3] == 29
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    narr_ext <-
      process_narr(
        date = c("2018-01-01", "2018-01-01"),
        variable = "omega",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "narr",
          "omega"
        ),
        extent = terra::ext(narr)
    )
  )
})

# test GEOS-CF ####
testthat::test_that("process_geos returns expected.", {
  withr::local_package("terra")
  collections <- c(
    "a",
    "c"
  )
  # expect function
  expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
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
    # expect output is SpatRaster
    expect_true(
      class(geos)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(geos)
    )
    # expect non-null coordinate reference system
    expect_false(
      terra::crs(geos) == ""
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(geos)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(geos))
    )
    # expect time dimension is POSIXt for hourly
    expect_true(
      "POSIXt" %in% class(terra::time(geos))
    )
    # expect seconds in time information
    expect_true(
      "seconds" %in% terra::timeInfo(geos)
    )
    # expect dimensions according to collection
    if (collection == "a") {
      expect_true(
        dim(geos)[3] == 1
      )
    } else if (collection == "c") {
      expect_true(
        dim(geos)[3] == 5
      )
    }
  }
  # test with cropping extent
  testthat::expect_no_error(
    geos_ext <- process_geos(
        date = c("2018-01-01", "2018-01-01"),
        variable = "O3",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "geos",
          "c"
        ),
        extent = terra::ext(geos)
      )
  )
})

testthat::test_that("process_geos expected errors.", {
  # expect error without variable
  expect_error(
    process_geos()
  )
  # expect error on directory without data
  expect_error(
    process_geos(
      variable = "O3",
      path = "./"
    )
  )
})

# test support functions ####
testthat::test_that("proccess support functions return expected.", {
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

testthat::test_that("process_locs_vector vector data and missing columns.", {
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

# test AQS ####
testthat::test_that("process_aqs", {
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  aqssub <- testthat::test_path(
    "..",
    "testdata",
    "aqs_daily_88101_triangle.csv"
  )
  testd <- testthat::test_path(
    "..", "testdata"
  )

  # main test
  testthat::expect_no_error(
    aqsft <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqsst <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "terra"
    )
  )
  testthat::expect_no_error(
    aqslt <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "terra"
    )
  )

  # expect
  testthat::expect_s4_class(aqsft, "SpatVector")
  testthat::expect_s4_class(aqsst, "SpatVector")
  testthat::expect_s4_class(aqslt, "SpatVector")

  testthat::expect_no_error(
    aqsfs <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsss <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "sf"
    )
  )
  testthat::expect_no_error(
    aqsls <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )
  testthat::expect_s3_class(aqsfs, "sf")
  testthat::expect_s3_class(aqsss, "sf")
  testthat::expect_s3_class(aqsls, "sf")

  testthat::expect_no_error(
    aqsfd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "date-location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqssdd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "available-data",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsld <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "data.table"
    )
  )
  testthat::expect_no_error(
    aqsldd <- process_aqs(
      path = aqssub,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      data_field = "Arithmetic.Mean",
      return_format = "data.table"
    )
  )
  testthat::expect_s3_class(aqsfd, "data.table")
  testthat::expect_s3_class(aqssd, "data.table")
  testthat::expect_s3_class(aqssdd, "data.table")
  testthat::expect_s3_class(aqsld, "data.table")
  testthat::expect_s3_class(aqsldd, "data.table")

  testthat::expect_no_error(
    aqssf <- process_aqs(
      path = testd,
      date = c("2022-02-04", "2022-02-28"),
      mode = "location",
      return_format = "sf"
    )
  )

  tempd <- tempdir()
  testthat::expect_error(
    process_aqs(
      path = tempd,
      date = c("2022-02-04", "2022-02-28"),
      return_format = "sf"
    )
  )

  # expect
  testthat::expect_s3_class(aqssf, "sf")

  # error cases
  testthat::expect_error(
    process_aqs(testthat::test_path("../testdata", "modis"))
  )
  testthat::expect_error(
    process_aqs(path = 1L)
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = c("January", "Januar"))
  )
  testthat::expect_error(
    process_aqs(path = aqssub, date = c("2021-08-15"))
  )
  testthat::expect_no_error(
    process_aqs(
      path = aqssub, date = c("2022-02-04", "2022-02-28"),
      mode = "available-data", return_format = "sf",
      extent = c(-79, 33, -78, 36)
    )
  )
  testthat::expect_warning(
    process_aqs(
      path = aqssub, date = c("2022-02-04", "2022-02-28"),
      mode = "available-data", return_format = "data.table",
      extent = c(-79, -78, 33, 36)
    ),
    "Extent is not applicable for data.table. Returning data.table..."
  )
})

# test SEDAC GRoads ####
testthat::test_that("test process_sedac_groads", {
  withr::local_package("terra")

  # main test
  testthat::expect_no_error(
    groads <- process_sedac_groads(
      path = testthat::test_path("../testdata/groads_test.shp")
    )
  )
  # expect
  testthat::expect_s4_class(groads, "SpatVector")
  # error cases
  testthat::expect_error(
    process_sedac_groads(path = 1L)
  )
  # test with cropping extent
  testthat::expect_no_error(
    groads_ext <- process_sedac_groads(
      path = testthat::test_path("../testdata/groads_test.shp"),
      extent = terra::ext(groads)
    )
  )
})

# test MERRA2 ####
testthat::test_that("process_merra2 returns as expected.", {
  withr::local_package("terra")
  #* indicates three dimensional data that has subset to single
  #* pressure level for test data set
  collection <- c(
    "inst1_2d_int_Nx", "inst3_2d_gas_Nx", "inst3_3d_chm_Nv", #*
    "inst6_3d_ana_Np", #*
    "statD_2d_slv_Nx", "tavg1_2d_chm_Nx", "tavg3_3d_udt_Np" #*
  )
  variable <- c(
    "CPT", "AODANA", "AIRDENS", #*
    "SLP", #*
    "HOURNORAIN", "COCL", "DUDTANA" #*
  )
  merra2_df <- data.frame(collection, variable)
  # expect function
  expect_true(
    is.function(process_merra2)
  )
  for (c in seq_along(merra2_df$collection)) {
    merra2 <-
      process_merra2(
        date = c("2018-01-01", "2018-01-01"),
        variable = merra2_df$variable[c],
        path =
        testthat::test_path(
          "..",
          "testdata",
          "merra2",
          merra2_df$collection[c]
        )
      )
    # expect output is SpatRaster
    expect_true(
      class(merra2)[1] == "SpatRaster"
    )
    # expect values
    expect_true(
      terra::hasValues(merra2)
    )
    # expect non-null coordinate reference system
    expect_false(
      terra::crs(merra2) == ""
    )
    # expect lon and lat dimensions to be > 1
    expect_false(
      any(c(0, 1) %in% dim(merra2)[1:2])
    )
    # expect non-numeric and non-empty time
    expect_false(
      any(c("", 0) %in% terra::time(merra2))
    )
    # expect time dimension is POSIXt for hourly
    expect_true(
      "POSIXt" %in% class(terra::time(merra2))
    )
    # expect seconds in time information
    expect_true(
      "seconds" %in% terra::timeInfo(merra2)
    )
    # expect 8 levels for 3 hourly data
    expect_true(
      all(dim(merra2) == c(2, 3, 1))
    )
  }
  class(merra2)
  # test with cropping extent
  testthat::expect_no_error(
    merra2_ext <- process_merra2(
        date = c("2018-01-01", "2018-01-01"),
        variable = "CPT",
        path =
        testthat::test_path(
          "..",
          "testdata",
          "merra2",
          "inst1_2d_int_Nx"
        ),
        extent = terra::ext(merra2)
      )
  )
})

# test GridMET ####
testthat::test_that("process_gridmet returns expected.", {
  withr::local_package("terra")
  variable <- "Precipitation"
  # expect function
  expect_true(
    is.function(process_gridmet)
  )
  gridmet <-
    process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(gridmet)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(gridmet)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(gridmet))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(gridmet)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(gridmet))
  )
  # expect dimensions according to levels
  expect_true(
    dim(gridmet)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    gridmet_ext <- process_gridmet(
      date = c("2018-01-03", "2018-01-03"),
      variable = "Precipitation",
      path =
      testthat::test_path(
        "..",
        "testdata",
        "gridmet",
        "pr"
      ),
      extent = terra::ext(gridmet)
    )
  )
})

# test TerraClimate ####
testthat::test_that("process_terraclimate returns expected.", {
  withr::local_package("terra")
  variable <- "ppt"
  # expect function
  expect_true(
    is.function(process_terraclimate)
  )
  terraclimate <-
    process_terraclimate(
      date = c("2018-01-01", "2018-01-01"),
      variable = variable,
      path =
      testthat::test_path(
        "..",
        "testdata",
        "terraclimate",
        "ppt"
      )
    )
  # expect output is SpatRaster
  expect_true(
    class(terraclimate)[1] == "SpatRaster"
  )
  # expect values
  expect_true(
    terra::hasValues(terraclimate)
  )
  # expect non-null coordinate reference system
  expect_false(
    is.null(terra::crs(terraclimate))
  )
  # expect lon and lat dimensions to be > 1
  expect_false(
    any(c(0, 1) %in% dim(terraclimate)[1:2])
  )
  # expect non-numeric and non-empty time
  expect_false(
    any(c("", 0) %in% terra::time(terraclimate))
  )
  # expect dimensions according to levels
  expect_true(
    dim(terraclimate)[3] == 1
  )
  # test with cropping extent
  testthat::expect_no_error(
    terraclimate_ext <- process_terraclimate(
      date = c("2018-01-01", "2018-01-01"),
      variable = "ppt",
      path =
      testthat::test_path(
        "..",
        "testdata",
        "terraclimate",
        "ppt"
      ),
      extent = terra::ext(terraclimate)
    )
  )
})

testthat::test_that("gridmet and terraclimate auxiliary functions.", {
  # gridmet
  gc1 <- process_gridmet_codes("all")
  expect_true(ncol(gc1) == 2)
  gc2 <- process_gridmet_codes("sph", invert = TRUE)
  expect_true(class(gc2) == "character")
  expect_true(nchar(gc2) > 7)
  gc3 <- process_gridmet_codes("Near-Surface Specific Humidity")
  expect_true(class(gc3) == "character")
  expect_true(nchar(gc3) < 7)
  # terraclimate
  tc1 <- process_terraclimate_codes("all")
  expect_true(ncol(gc1) == 2)
  tc2 <- process_terraclimate_codes("aet", invert = TRUE)
  expect_true(class(gc2) == "character")
  expect_true(nchar(gc2) > 7)
  tc3 <- process_terraclimate_codes("Actual Evapotranspiration")
  expect_true(class(gc3) == "character")
  expect_true(nchar(gc3) < 7)
  # process_variable_codes
  expect_no_error(process_variable_codes("sph", "gridmet"))
  expect_no_error(
    process_variable_codes("Near-Surface Specific Humidity", "gridmet")
  )
  expect_error(
    process_variable_codes("error", "gridmet")
  )
  expect_no_error(process_variable_codes("aet", "terraclimate"))
  expect_no_error(
    process_variable_codes("Actual Evapotranspiration", "terraclimate")
  )
  expect_error(
    process_variable_codes("error", "terraclimate")
  )
})

# test PRISM ####
testthat::test_that(
  "process_prism returns a SpatRaster object with correct metadata",
  {
    # Set up test data
    withr::local_package("terra")
    path <- testthat::test_path(
      "..", "testdata", "prism", "PRISM_tmin_30yr_normal_4kmD1_0228_bil_test.nc"
    )
    path_dir <- testthat::test_path(
      "..", "testdata", "prism"
    )
    element <- "tmin"
    time <- "0228"

    # Call the function
    testthat::expect_no_error(result <- process_prism(path, element, time))
    testthat::expect_no_error(result2 <- process_prism(path_dir, element, time))

    # Check the return type
    testthat::expect_true(inherits(result, "SpatRaster"))
    testthat::expect_true(inherits(result2, "SpatRaster"))

    # Check the metadata
    testthat::expect_equal(unname(terra::metags(result)["time"]), time)
    testthat::expect_equal(unname(terra::metags(result)["element"]), element)

    # Set up test data
    path_bad <- "/path/to/nonexistent/folder"
    element_bad <- "invalid_element"
    time_bad <- "invalid_time"

    # Call the function and expect an error
    testthat::expect_error(process_prism(NULL, element, time))
    testthat::expect_error(
      testthat::expect_warning(
        process_prism(path_bad, element, time)
      )
    )
    testthat::expect_error(process_prism(path_dir, element_bad, time))
    testthat::expect_error(process_prism(path_dir, element, time_bad))

    # test with cropping extent
    testthat::expect_no_error(
      result_ext <- process_prism(path, element, time,
      extent = terra::ext(result))
    )
  }
)


# test CropScape ####
testthat::test_that(
  "process_cropscape returns a SpatRaster object with correct metadata", {
    # Set up test data
    withr::local_package("terra")
    filepath <-
      testthat::test_path("..", "testdata/cropscape/cdl_30m_r_nc_2019_sub.tif")
    dirpath <- testthat::test_path("..", "testdata/cropscape")
    year <- 2019

    # Call the function
    testthat::expect_no_error(result <- process_cropscape(filepath, year))
    testthat::expect_no_error(process_cropscape(dirpath, year))

    # test with cropping extent
    testthat::expect_no_error(
      result_ext <- process_cropscape(
        filepath, year, extent = terra::ext(result)
      )
    )

    # Check the return type
    testthat::expect_true(inherits(result, "SpatRaster"))

    # Check the metadata
    testthat::expect_equal(
      unname(terra::metags(result)["year"]),
      as.character(year)
    )

    # error cases
    testthat::expect_error(process_cropscape(path = 0, year = "MILLENNIUM"))
    testthat::expect_error(
      process_cropscape(path = "/home/some/path", year = "MILLENNIUM")
    )
  }
)

# test HUC ####
testthat::test_that("process_huc",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("nhdplusTools")
    withr::local_options(list(sf_use_s2 = FALSE))
    # Set up test data
    path <- testthat::test_path(
      "..", "testdata", "huc12", "NHDPlus_test.gpkg"
    )

    # Call the function
    testthat::expect_error(process_huc(path))
    testthat::expect_no_error(
      result <-
        process_huc(
          path,
          layer_name = "NHDPlus_test",
          huc_level = "HUC_12",
          huc_header = "030202"
        )
    )
    testthat::expect_true(inherits(result, "SpatVector"))

    # query case
    testthat::expect_no_error(
      result <-
        process_huc(
          path,
          layer_name = "NHDPlus_test",
          huc_level = "HUC_12",
          huc_header = "030202"
        )
    )
    testthat::expect_true(inherits(result, "SpatVector"))

    testthat::expect_error(
      process_huc(
        path,
        layer_name = "HUc",
        huc_level = "HUC_12",
        huc_header = "030202"
      )
    )

    # Set up test data
    path2 <- testthat::test_path(
      "..", "testdata", "huc12"
    )

    # Call the function and expect an error
    testthat::expect_error(process_huc(path2))

    # test with cropping extent
    testthat::expect_no_error(
      huc_ext <- process_huc(
          path,
          layer_name = "NHDPlus_test",
          huc_level = "HUC_12",
          huc_header = "030202",
          extent = terra::ext(result)
        )
    )
  }
)

# test OpenLandMap ####
# nolint start
testthat::test_that("process_olm", {
  withr::local_package("terra")
  tmwm <- testthat::test_path("..", "testdata", "openlandmap",
    "no2_s5p.l3.trop.tmwm.p50_p90_2km_a_20180501_20221130_go_epsg.4326_v20221219_test.tif")
  testthat::expect_no_error(
    olm <- process_olm(path = tmwm)
  )
  testthat::expect_s4_class(olm, "SpatRaster")
  testthat::expect_error(
    process_olm(path = 1L)
  )

  # test with cropping extent
  testthat::expect_no_error(
    olm_ext <- process_olm(path = tmwm, extent = terra::ext(olm))
  )
})
# nolint end

# AUX tests ####
testthat::test_that("loc_radius tests", {
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

# apply_extent
testthat::test_that("apply_extent tests", {
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
