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
    mcdtest <- modis_prefilter_sds("MCD19A2")
  )
  testthat::expect_equal(
    mcdtest, "(Optical_Depth)"
  )
  testthat::expect_no_error(
    modis_prefilter_sds("MCD19A2", "(cos|RelAZ|Angle)")
  )
  for (i in 1:3) {
    testthat::expect_equal(
      modis_prefilter_sds(txt_products[i]), txt_exp_output[i]
    )
  }
  testthat::expect_no_error(
    filt_other <- modis_prefilter_sds("ignored", "(cos)")
  )
  testthat::expect_equal(filt_other, "(cos)")

})


testthat::test_that("modis_aggregate_sds", {
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
      modis_aggregate_sds(
        path = mcd19,
        product = "MCD19A2",
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
      modis_aggregate_sds(
        path = mod09_sub,
        product = "MOD09GA",
        nsds = NULL,
        fun_agg = "mean"
      )
  )
  testthat::expect_s4_class(modaggr, "SpatRaster")
  testthat::expect_equal(terra::nlyr(modaggr), 1L)
  testthat::expect_true(grepl("^500m Surface", names(modaggr)))
})


testthat::test_that("modis_get_vrt is good to go", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_error(
    modis_get_vrt(
      paths = path_mod11,
      "MOD11A1",
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
    modis_get_vrt(
      paths = path_mod13,
      "MOD13A2",
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
    modis_get_vrt(
      paths = path_mcd19,
      "MCD19A2",
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
    modis_get_vrt(
      paths = path_mod09,
      "MOD09GA",
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

  testthat::expect_warning(
    vnp46_proc <- modis_preprocess_vnp46(
      paths = path_vnp46[1],
      date_in = "2018-08-13"
    )
  )
  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_error(
    modis_preprocess_vnp46(
      paths = path_vnp46[1],
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
    warped <- modis_warp_stars(
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
  path_mod06 <-
    sprintf("HDF4_EOS:EOS_SWATH:%s:mod06:Cloud_Fraction_Night", path_mod06)

  testthat::expect_no_error(
    suppressWarnings(
      modis_mosaic_mod06(
        paths = path_mod06,
        date_in = "2021-08-15"
      )
    )
  )
  testthat::expect_error(
    modis_mosaic_mod06(
      paths = path_mod06,
      date_in = "2021~08~15"
    )
  )
  testthat::expect_error(
    modis_mosaic_mod06(
      paths = path_mod06,
      date_in = "2021-13-15"
    )
  )
  testthat::expect_error(
    modis_mosaic_mod06(
      paths = path_mod06,
      date_in = "2021-12-45"
    )
  )
})
