################################################################################
##### unit and integration tests for NASA MODIS functions
# nolint start

################################################################################
##### download_modis
testthat::test_that("download_modis (MODIS-MOD09GA)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringi")
  withr::local_package("jsonlite")

  # Skip if no NASA token
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  # function parameters
  years <- 2020
  product <- "MOD09GA"
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")

  for (y in seq_along(years)) {
    date_start <- paste0(years[y], "-06-20")
    date_end <- paste0(years[y], "-06-24")

    # Test with deprecation warning
    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = product,
        version = version,
        extent = c(-124, 25, -105, 40),
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )

    testthat::expect_true(dir.exists(directory_to_save))
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD09GA + single date)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # Skip if no NASA token
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  # function parameters
  product <- "MOD09GA"
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date <- "2021-04-12"

  testthat::expect_warning(
    download_data(
      dataset_name = "modis",
      date = date,
      product = product,
      version = version,
      extent = c(-124, 25, -105, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD06L2)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # Skip if no NASA token
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  # function parameters
  product <- "MOD06_L2"
  version <- "061"
  date_start <- "2019-02-18"
  date_end <- "2019-02-28"
  directory_to_save <- paste0(tempdir(), "/mod/")

  testthat::expect_warning(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = c(-124, 25, -105, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (expected errors)", {
  withr::local_package("httr2")
  withr::local_package("stringr")

  # function parameters
  years <- 2020
  product <- c("MOD09GA", "MOD11A1", "MOD13A2", "MCD19A2")
  product <- sample(product, 1L)
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- paste0(years, "-06-25")
  date_end <- paste0(years, "-06-30")
  vec_extent <- c(-124, 25, -105, 40)

  # with token (if available) - should work
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") != "") {
    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = product,
        version = version,
        extent = vec_extent,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )
  }

  # no token - should error
  withr::local_envvar(NASA_EARTHDATA_TOKEN = "")
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    "NASA_EARTHDATA_TOKEN"
  )

  # year difference between date_start and date_end
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, "2024-03-28"),
      product = "MOD11A1",
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )

  # null version
  testthat::expect_error(
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = product,
      version = NULL,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MOD + MYD products)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")

  # Skip if no NASA token
  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  # function parameters
  products <- c(
    "MOD09GA",
    "MYD09GA",
    "MOD09GQ",
    "MYD09GQ"
  )
  version <- "061"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- "2021-06-01"
  date_end <- "2021-06-30"
  vec_extent <- c(-124, 25, -105, 40)

  for (p in seq_along(products)) {
    cat("Testing product:", products[p], "\n")

    testthat::expect_warning(
      download_data(
        dataset_name = "modis",
        date = c(date_start, date_end),
        product = products[p],
        version = version,
        extent = vec_extent,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = FALSE
      ),
      "Setting download=FALSE is deprecated"
    )
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis with NASA token", {
  skip_on_cran()
  skip_if_offline()

  if (Sys.getenv("NASA_EARTHDATA_TOKEN") == "") {
    skip("NASA_EARTHDATA_TOKEN not set")
  }

  directory_to_save <- paste0(tempdir(), "/mod_token/")

  # Test that download works with token
  testthat::expect_no_error(
    download_data(
      dataset_name = "modis",
      date = "2024-01-01",
      product = "MOD09GA",
      version = "061",
      extent = c(-80, 35, -75, 40),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  testthat::expect_true(dir.exists(directory_to_save))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis remove_command warning and hash=TRUE (mock)", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(
                list(
                  rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                  href = paste0(
                    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                    "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                  )
                )
              )
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::with_tempdir({
    # Test remove_command=TRUE warning
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          remove_command = TRUE,
          hash = FALSE
        )
      ),
      "remove_command.*deprecated"
    )

    # Test hash=TRUE return
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that(
  "download_modis warns when date range exceeds 31 days (mock)",
  {
    skip_on_cran()

    testthat::local_mocked_bindings(
      get_token = function(...) "fake_token",
      download_run_method = function(...) {
        invisible(list(success = 1, failed = 0, skipped = 0))
      },
      download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
      .package = "amadeus"
    )

    testthat::local_mocked_bindings(
      req_perform = function(req, path = NULL, ...) {
        structure(
          list(
            status_code = 200L,
            headers = list(`Content-Type` = "application/json"),
            body = charToRaw("{}")
          ),
          class = "httr2_response"
        )
      },
      resp_body_json = function(resp, ...) {
        list(
          feed = list(
            entry = list(
              list(
                links = list(
                  list(
                    rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                    href = paste0(
                      "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                      "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                    )
                  )
                )
              )
            )
          )
        )
      },
      .package = "httr2"
    )

    withr::with_tempdir({
      # date range > 31 days in same year triggers warning
      testthat::expect_warning(
        suppressMessages(
          download_modis(
            date = c("2023-01-01", "2023-03-15"),
            product = "MOD09GA",
            directory_to_save = ".",
            acknowledgement = TRUE,
            hash = FALSE
          )
        ),
        "31 days"
      )
    })
  }
)

################################################################################
##### process_modis*
testthat::test_that("process_modis_sds", {
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
    mcdtest,
    "(Optical_Depth)"
  )
  testthat::expect_no_error(
    process_modis_sds("MCD19A2", "(cos|RelAZ|Angle)")
  )
  for (i in 1:3) {
    testthat::expect_equal(
      process_modis_sds(txt_products[i]),
      txt_exp_output[i]
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
    "..",
    "testdata",
    "modis",
    "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
  )
  mod09 <- testthat::test_path(
    "..",
    "testdata",
    "modis",
    "MOD09GA.A2021227.h11v05.061.2021229035936.hdf"
  )

  # main test: mcd19
  # expect warning for new strict terra/GDAL HDF4 warning
  testthat::expect_no_warning(
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
      "..",
      "testdata",
      "modis",
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


testthat::test_that("process_modis_merge", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_warning(
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
  testthat::expect_no_warning(
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
  testthat::expect_no_warning(
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
  testthat::expect_no_warning(
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
  testthat::expect_no_warning(
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


testthat::test_that("process_blackmarble*", {
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

  # terra no longer produces "unknown extent" for VNP46 HDF files in newer versions
  testthat::expect_no_error(
    vnp46_proc <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )

  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_no_error(
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


testthat::test_that("process_modis_warp + process_modis_swath", {
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_mod06 <-
    testthat::test_path(
      "..",
      "testdata",
      "modis",
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
    unname(stars::st_res(warped)[1]),
    0.1,
    tolerance = 1e-6
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


testthat::test_that("process_modis (expected errors)", {
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))
  path_mod06 <-
    testthat::test_path(
      "..",
      "testdata",
      "modis",
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

################################################################################
##### calc_modis*
testthat::test_that("calculate_modis", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("stars")
  withr::local_package("lwgeom")
  withr::local_options(
    list(
      sf_use_s2 = FALSE
    )
  )

  site_faux <-
    data.frame(
      site_id = "37999904288101",
      lon = -89.87,
      lat = 39.8734,
      time = as.Date("2021-08-15")
    )
  site_faux <-
    terra::vect(
      site_faux,
      geom = c("lon", "lat"),
      keepgeom = FALSE,
      crs = "EPSG:4326"
    )

  # case 1: standard mod11a1
  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_warning(
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
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          scale = NULL
        )
    )
  )
  testthat::expect_s3_class(calc_mod11, "data.frame")
  # all values are >14000 (unscaled values)
  testthat::expect_true(
    all(calc_mod11[, grep("MOD", names(calc_mod11))] > 14000)
  )

  # ... _add arguments test
  aux <- 0L
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11 <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          scale = "* 1"
        )
    )
  )

  # with geometry terra
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_terra <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = "terra",
          scale = "* 1"
        )
    )
  )
  testthat::expect_s4_class(calc_mod11_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_sf <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = "sf",
          scale = "* 1"
        )
    )
  )
  testthat::expect_true(inherits(calc_mod11_sf, "sf"))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_modis_merge,
      package_list_add = c("MASS"),
      export_list_add = c("aux"),
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      subdataset = "(LST_)",
      geom = TRUE,
      scale = "* 1"
    )
  )

  # no error with scale and convert to C
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod11_scale <-
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          package_list_add = c("MASS"),
          export_list_add = c("aux"),
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)",
          geom = FALSE,
          scale = "* 0.02 - 273.15"
        )
    )
  )
  # all values below 27 C (manually inspected) to ensure proper scaled
  testthat::expect_true(
    all(calc_mod11_scale[, grep("MOD", names(calc_mod11_scale))] < 27)
  )

  # warning with scale = NULL
  testthat::expect_warning(
    calc_mod11_scalew <-
      calculate_modis(
        from = path_mod11,
        locs = sf::st_as_sf(site_faux),
        preprocess = process_modis_merge,
        package_list_add = c("MASS"),
        export_list_add = c("aux"),
        name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
        subdataset = "(LST_)",
        geom = FALSE,
        scale = NULL
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
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_")
        )
    )
  )
  testthat::expect_s3_class(calc_mod06, "data.frame")

  # with geometry terra
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06_terra <-
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
          geom = "terra"
        )
    )
  )
  testthat::expect_s4_class(calc_mod06_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    suppressWarnings(
      calc_mod06_sf <-
        calculate_modis(
          from = path_mod06,
          locs = site_faux,
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = process_modis_swath,
          name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
          geom = "sf"
        )
    )
  )
  testthat::expect_true(inherits(calc_mod06_sf, "sf"))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_mod06,
      locs = site_faux,
      subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
      preprocess = process_modis_swath,
      name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
      geom = TRUE
    )
  )

  # case 3: VIIRS (expect "unknown extent" warnings)
  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "VNP46",
      full.names = TRUE
    )

  testthat::expect_no_error(
    base_vnp <- process_blackmarble(
      path = path_vnp46,
      date = "2018-08-13",
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )

  testthat::expect_no_error(
    calc_vnp46 <-
      calculate_modis(
        from = path_vnp46,
        locs = site_faux,
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        scale = "* 1"
      )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # with geometry terra
  testthat::expect_no_error(
    calc_vnp46_terra <-
      calculate_modis(
        from = path_vnp46,
        locs = site_faux,
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        geom = "terra",
        scale = "* 1"
      )
  )
  testthat::expect_s4_class(calc_vnp46_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    calc_vnp46_sf <-
      calculate_modis(
        from = path_vnp46,
        locs = sf::st_as_sf(site_faux),
        preprocess = process_blackmarble,
        name_covariates = c("MOD_NITLT_0_"),
        subdataset = 3L,
        tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
        geom = "sf",
        scale = "* 1"
      )
  )
  testthat::expect_true("sf" %in% class(calc_vnp46_sf))

  # with geometry error
  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_"),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
      geom = TRUE,
      scale = "* 1"
    )
  )

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
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = site_faux_r
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = matrix(c(1, 3, 4, 5), nrow = 2)
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux)
    )
  )
  testthat::expect_error(
    calculate_modis_daily(
      from = terra::rast(nrow = 3, ncol = 3, vals = 1:9, names = "a"),
      date = "2021-08-15",
      locs = array(1:12, dim = c(2, 2, 3))
    )
  )
  site_faux0 <- site_faux
  names(site_faux0)[2] <- "date"
  testthat::expect_error(
    calculate_modis_daily(
      from = rast(nrow = 3, ncol = 3),
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux0)
    )
  )
  site_faux2 <- site_faux
  # site_faux2[, 4] <- NULL

  path_mcd19 <-
    testthat::test_path(
      "../testdata/modis/",
      "MCD19A2.A2021227.h11v05.061.2023149160635.hdf"
    )
  testthat::expect_no_warning(
    mcd_merge <-
      process_modis_merge(
        path = path_mcd19,
        date = "2021-08-15",
        subdataset = "(Optical_Depth)"
      )
  )

  testthat::expect_no_error(
    calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_"
    )
  )

  # test calculate_modis_daily directly with geometry terra
  testthat::expect_no_error(
    calc_mod_terra <- calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_",
      geom = "terra"
    )
  )
  testthat::expect_s4_class(calc_mod_terra, "SpatVector")

  # test calculate_modis_daily directly with geometry sf
  testthat::expect_no_error(
    calc_mod_sf <- calculate_modis_daily(
      from = mcd_merge,
      date = "2021-08-15",
      locs = sf::st_as_sf(site_faux2),
      radius = 1000,
      name_extracted = "MCD_EXTR_1K_",
      geom = "sf"
    )
  )
  testthat::expect_true(inherits(calc_mod_sf, "sf"))

  testthat::expect_error(
    calculate_modis(from = site_faux, scale = "* 1")
  )
  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      product = "MOD11A1",
      locs = list(1, 2, 3),
      scale = "* 1"
    )
  )
  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = "fountain",
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L
    )
  )

  # Test expects name_covariates warning (may also get scale or unknown extent)
  testthat::expect_warning(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    ),
    "name_covariates|unknown extent|scale"
  )

  # Test with negative radius
  testthat::expect_no_error(
    flushed <- calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = "* 1"
    )
  )
  testthat::expect_s3_class(flushed, "data.frame")
  testthat::expect_true(unlist(flushed[, 2]) == -99999)

  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = 0.01
    )
  )
})
# nolint end

################################################################################
##### download_modis single-date and hash=FALSE branches (no skip_on_cran)

testthat::test_that("download_modis single date branch (mock, no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(
        feed = list(
          entry = list(
            list(
              links = list(
                list(
                  rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
                  href = paste0(
                    "https://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.061/",
                    "2023.01.01/MOD09GA.A2023001.h10v05.061.hdf"
                  )
                )
              )
            )
          )
        )
      )
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})

################################################################################
##### download_modis remove_command, 31-day warning, no granules (no skip_on_cran)

testthat::test_that("download_modis no granules found path (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) {
      invisible(list(success = 1, failed = 0, skipped = 0))
    },
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(
          status_code = 200L,
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw("{}")
        ),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(feed = list(entry = list()))  # Empty entry list -> no granules
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_error(
      suppressWarnings(
        suppressMessages(
          download_modis(
            date = "2023-01-01",
            product = "MOD09GA",
            directory_to_save = ".",
            acknowledgement = TRUE,
            hash = FALSE
          )
        )
      ),
      "No granules found"
    )
  })
})

################################################################################
##### download_modis remove_command warning (covers lines 2707-2710)

testthat::test_that("download_modis remove_command deprecated warning (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(status_code = 200L, headers = list(`Content-Type` = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(feed = list(entry = list(
        list(links = list(
          list(rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
               href = "https://example.com/MOD09GA.A2023001.h08v04.006.hdf")
        ))
      )))
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          remove_command = TRUE,
          hash = FALSE
        )
      ),
      "remove_command.*deprecated"
    )
  })
})

################################################################################
##### download_modis 31-day warning (covers lines 2722-2724)

testthat::test_that("download_modis 31-day range warning (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(status_code = 200L, headers = list(`Content-Type` = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(feed = list(entry = list(
        list(links = list(
          list(rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
               href = "https://example.com/MOD09GA.A2023001.h08v04.006.hdf")
        ))
      )))
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_modis(
          date = c("2023-01-01", "2023-03-01"),
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = FALSE
        )
      ),
      "31 days"
    )
  })
})

################################################################################
##### download_modis hash=TRUE (covers line 2821)

testthat::test_that("download_modis hash=TRUE returns fakehash (no skip)", {
  testthat::local_mocked_bindings(
    get_token = function(...) "fake_token",
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      structure(
        list(status_code = 200L, headers = list(`Content-Type` = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_json = function(resp, ...) {
      list(feed = list(entry = list(
        list(links = list(
          list(rel = "http://esipfed.org/ns/fedsearch/1.1/data#",
               href = "https://example.com/MOD09GA.A2023001.h08v04.006.hdf")
        ))
      )))
    },
    .package = "httr2"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_modis(
          date = "2023-01-01",
          product = "MOD09GA",
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})
