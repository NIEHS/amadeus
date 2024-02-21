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
      paths = path_mod11,
      date_in = "2021-08-15",
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
      paths = path_mod13,
      date_in = "2021-08-13",
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
      paths = path_mcd19,
      date_in = "2021-08-15",
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
      paths = path_mod09,
      date_in = "2021-08-15",
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
      paths = paths_mod13,
      date_in = "2021-08-13",
      subdataset = "(NDVI)"
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
    corn <- process_bluemarble_corners()
  )
  testthat::expect_error(
    process_bluemarble_corners(hrange = c(99, 104))
  )

  testthat::expect_warning(
    vnp46_proc <- process_bluemarble(
      paths = path_vnp46[1],
      tile_df = corn,
      date_in = "2018-08-13"
    )
  )
  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_warning(
    vnp46_proc2 <- process_bluemarble(
      paths = path_vnp46[1],
      tile_df = corn,
      subdataset = c(3L, 5L),
      date_in = "2018-08-13"
    )
  )

  testthat::expect_s4_class(vnp46_proc2, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc2), 2L)

  testthat::expect_error(
    process_bluemarble(
      paths = path_vnp46[1],
      tile_df = corn,
      date_in = "2018~08~13"
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
    unname(stars::st_res(warped)[1]), 0.25, tolerance = 1e-6
  )

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
        paths = path_mod06e,
        date_in = "2021-08-15"
      )
    )
  )
  testthat::expect_error(
    process_modis_swath(
      paths = path_mod06e,
      date_in = "2021~08~15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      paths = path_mod06e,
      date_in = "2021-13-15"
    )
  )
  testthat::expect_error(
    process_modis_swath(
      paths = path_mod06e,
      date_in = "2021-12-45"
    )
  )
})



testthat::test_that("read ecoregion", {
  withr::local_package("terra")

  path_eco <- testthat::test_path("..", "testdata", "eco_l3_clip.gpkg")

  testthat::expect_no_error(
    process_ecoregion(path_eco)
  )
})


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

})



testthat::test_that("process_koppen_geiger tests", {
  withr::local_package("terra")
  path_kgeiger <-
    testthat::test_path("../testdata", "koppen_subset.tif")

  testthat::expect_no_error(
    kgeiger <- process_koppen_geiger(path_kgeiger)
  )

  testthat::expect_s4_class(kgeiger, "SpatRaster")
})


testthat::test_that("process_tri tests", {
  withr::local_package("terra")
  path_tri <- testthat::test_path("../testdata", "tri", "")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri)
  )
  testthat::expect_s4_class(tri_r, "SpatVector")
})


testthat::test_that("process_nei tests", {
  withr::local_package("terra")

  path_nei <- testthat::test_path("../testdata", "nei", "")
  path_cnty <- system.file("gpkg/nc.gpkg", package = "sf")
  path_cnty <- terra::vect(path_cnty)
  path_cnty$GEOID <- path_cnty$FIPS

  testthat::expect_no_error(
    neinc <- process_nei(path = path_nei, year = 2020, county = path_cnty)
  )
  testthat::expect_s4_class(neinc, "SpatVector")

  # error cases
  testthat::expect_error(
    process_nei(path_nei, year = 2030)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = NULL)
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
  dfst <- convert_stobj_to_stdt(df)
  dfst$crs_stdt <- "EPSG:4326"
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
  testthat::expect_no_error(
    process_conformity(locs = dfst, check_time = TRUE)
  )
  # error cases
  dfe <- df
  names(dfe)[3] <- "date"
  testthat::expect_error(
    process_conformity(locs = dfe, check_time = TRUE)
  )

})

testthat::test_that("process_sedac_population returns expected.", {
  withr::local_package("terra")
  paths <- list.files(
    "../testdata/population/",
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
})

testthat::test_that("process_sedac_population returns null for netCDF.", {
  pop <-
    process_sedac_population(
      "../testdata/population/placeholder.nc"
    )
  expect_true(
    is.null(pop)
  )
})

testthat::test_that("process_hms returns expected (June data).", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  # expect function
  expect_true(
    is.function(process_hms)
  )
  for (d in seq_along(densities)) {
    hms <-
      process_hms(
        date_start = "2022-06-10",
        date_end = "2022-06-11",
        variable = densities[d],
        path =
        "../testdata/hms/"
      )
    # expect output is a SpatVector or character
    expect_true(
      class(hms)[1] %in% c("SpatVector", "character")
    )
    if (class(hms)[1] == "SpatVector") {
      # expect non-null coordinate reference system
      expect_false(
        is.null(terra::crs(hms))
      )
      # expect two columns
      expect_true(
        ncol(hms) == 2
      )
      # expect density and date column
      expect_true(
        all(c("Density", "Date") %in% names(hms))
      )
    } else if (class(hms)[1] == "character") {
      # expect first is density type
      expect_true(
        hms[1] %in% c("Light", "Medium", "Heavy")
      )
      # expect other elements are 8 character dates
      expect_true(
        all(nchar(hms[2:length(hms)]) == 8)
      )
    }
  }
})


testthat::test_that("process_hms returns expected (December data).", {
  withr::local_package("terra")
  densities <- c(
    "Light",
    "Medium",
    "Heavy"
  )
  radii <- c(0, 1000)
  locs <- readRDS("../testdata/sites_nc.RDS")
  # expect function
  expect_true(
    is.function(calc_hms)
  )
  for (d in seq_along(densities)) {
    density <- densities[d]
    for (r in seq_along(radii)) {
      hms <-
        process_hms(
          date_start = "2018-12-31",
          date_end = "2018-12-31",
          variable = density,
          path = "../testdata/hms/"
        )
      # expect output is character
      expect_true(
        class(hms) == "character"
      )
      # expect 2 observations
      expect_true(
        length(hms) == 2
      )
    }
  }
})

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
          paste0(
            "../testdata/gmted/",
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
            "_grd/"
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
})

testthat::test_that("import_gmted returns error with non-vector variable.", {
  expect_error(
    gmted <-
      process_gmted(
        variable <- "Breakline Emphasis; 7.5 arc-seconds",
        path = "../testdata/gmted/gmted"
      )
  )
})

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
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = variables[v],
        path =
        paste0(
          "../testdata/narr/",
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
})

testthat::test_that("process_geos returns expected.", {
  withr::local_package("terra")
  collections <- c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_inst_1hr_g1440x721_p23"
  )
  # expect function
  expect_true(
    is.function(process_geos)
  )
  for (c in seq_along(collections)) {
    collection <- collections[c]
    geos <-
      process_geos(
        date_start = "2018-01-01",
        date_end = "2018-01-01",
        variable = "O3",
        path = paste0(
          "../testdata/geos/",
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
    # expect dimensions according to collectoin
    if (collection == "aqc_tavg_1hr_g1440x721_v1") {
      expect_true(
        dim(geos)[3] == 24
      )
    } else if (collection == "chm_inst_1hr_g1440x721_p23") {
      expect_true(
        dim(geos)[3] == 23 * 24
      )
    }
  }
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

testthat::test_that("proccess support functions return expected.", {
  path <- list.files(
    "../testdata/geos/aqc_tavg_1hr_g1440x721_v1/",
    full.names = TRUE
  )
  expect_error(
    process_geos_collection(
      path = path,
      collection = TRUE,
      date = TRUE,
      datetime = TRUE
    )
  )
  path_split_d <- process_geos_collection(
    path = path,
    date = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_d)) == 8
  )
  path_split_dt <- process_geos_collection(
    path = path,
    datetime = TRUE
  )
  # expect YYYYMMDD dates
  expect_true(
    unique(nchar(path_split_dt)) == 12
  )
})

testthat::test_that("process_locs_vector vector data and missing columns.", {
  withr::local_package("terra")
  locs <- readRDS("../testdata/sites_nc.RDS")
  narr <-
    process_narr(
      date_start = "2018-01-01",
      date_end = "2018-01-01",
      variable = "weasd",
      path = "../testdata/narr/weasd/"
    )
  # expect error when missing `lat` or `lon`
  expect_error(
    calc_narr(
      from = narr,
      locs = subset(
        locs,
        select = "lon"
      ),
      locs_id = "site_id"
    )
  )
  # expect error when sites are SpatVector
  expect_error(
    calc_narr(
      from = narr,
      locs = terra::vect(
        locs,
        geom = c("lon", "lat"),
        crs = "EPSG:4326"
      ),
      locs_id = "site_id"
    )
  )
})
