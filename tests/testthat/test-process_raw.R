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
        nsds = "Optical_Depth",
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(mcdaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(mcdaggr), 2L)
  testthat::expect_equal(
    all(grepl("^Optical", names(mcdaggr))),
    TRUE
  )

  # mod09 test
  mod09_sub <-
    sprintf("HDF4_EOS:EOS_GRID:%s:MODIS_Grid_500m_2D:sur_refl_b01_1", mod09)
  # main test: mcd19
  testthat::expect_no_error(
    modaggr <-
      process_flatten_sds(
        path = mod09_sub,
        nsds = NULL,
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
      regex_sds = "(LST_)"
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
      regex_sds = "(NDVI)"
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
      regex_sds = "(Optical_Depth)"
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
      regex_sds = "(sur_refl_b0)"
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
    testthat::test_path("../testdata", "nlcd_2019_land_cover_l48_20210604.tif")

  testthat::expect_no_error(
    nlcd19 <- process_nlcd(path = path_nlcd19, year = 2019)
  )
  testthat::expect_s4_class(nlcd19, "SpatRaster")
  testthat::expect_equal(terra::metags(nlcd19, name = "year"), "2019")

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

  testthat::expect_no_error(
    neinc <- process_nei(path = path_nei, year = 2020, auxfile = path_cnty)
  )
  testthat::expect_s4_class(neinc, "SpatVector")

  # error cases
  testthat::expect_error(
    process_nei(path_nei, year = 2030)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, auxfile = NULL)
  )

})


