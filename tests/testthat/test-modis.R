################################################################################
##### unit and integration tests for NASA MODIS functions
# nolint start

################################################################################
##### download_modis
testthat::test_that("download_modis (MODIS-MOD09GA)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- 2020
  product <- "MOD09GA"
  version <- "61"
  horizontal_tiles <- c(12, 13)
  vertical_tiles <- c(5, 6)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- paste0(tempdir(), "/mod/")
  for (y in seq_along(years)) {
    date_start <- paste0(years[y], "-06-20")
    date_end <- paste0(years[y], "-06-24")
    # run download function
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
    # define file path with commands
    commands_path <- paste0(
      directory_to_save,
      product,
      "_",
      date_start,
      "_",
      date_end,
      "_wget_commands.txt"
    )
    # import commands
    commands <- read_commands(commands_path = commands_path)[, 2]
    # extract urls
    urls <- extract_urls(commands = commands, position = 4)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 3L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD09GA + single date)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  product <- "MOD09GA"
  version <- "61"
  horizontal_tiles <- c(12, 13)
  vertical_tiles <- c(5, 6)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date <- "2021-04-12"
  download_data(dataset_name = "modis",
                date = date,
                product = product,
                version = version,
                horizontal_tiles = horizontal_tiles,
                vertical_tiles = vertical_tiles,
                nasa_earth_data_token = nasa_earth_data_token,
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE,
                remove_command = FALSE)
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    product,
    "_",
    date,
    "_",
    date,
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)[, 2]
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 3L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MODIS-MOD06L2)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  product <- "MOD06_L2"
  version <- "61"
  date_start <- "2019-02-18"
  date_end <- "2019-02-18"
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  horizontal_tiles <- c(8, 10)
  vertical_tiles <- c(4, 5)
  directory_to_save <- paste0(tempdir(), "/mod/")

  testthat::expect_error(
    kax <- download_data(dataset_name = "modis",
                    date = c(date_start, date_end),
                    product = product,
                    version = version,
                    horizontal_tiles = horizontal_tiles,
                    vertical_tiles = vertical_tiles,
                    nasa_earth_data_token = nasa_earth_data_token,
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    download = FALSE,
                    mod06_links = NULL,
                    remove_command = FALSE)
  )
  # link check
  tdir <- tempdir()
  faux_urls <-
    rbind(
      c(4387858920,
        paste0(
          "/archive/allData/61/MOD06_L2/2019/049/",
          "MOD06_L2.A2019049.0720.061.2019049194350.hdf"
        ),
        28267915)
    )

  faux_urls <- data.frame(faux_urls)
  mod06_scenes <- paste0(tdir, "/mod06_example.csv")
  write.csv(faux_urls, mod06_scenes, row.names = FALSE)

  download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  mod06_links = mod06_scenes,
                  remove_command = FALSE)

  # define file path with commands
  commands_path <- list.files(
    directory_to_save,
    pattern = "_wget_commands.txt",
    full.names = TRUE
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)[, 2]
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})


testthat::test_that("download_modis (expected errors)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- 2020
  product <- c("MOD09GA", "MOD11A1", "MOD13A2", "MCD19A2")
  product <- sample(product, 1L)
  version <- "61"
  horizontal_tiles <- c(12, 13)
  vertical_tiles <- c(5, 6)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- paste0(years, "-06-25")
  date_end <- paste0(years, "-06-28")

  # no token
  testthat::expect_no_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # no token
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = NULL,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # year difference between date_start and date_end
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, "2024-03-28"),
                  product = "MOD11A1",
                  version = version,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # null version
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = NULL,
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # invalid tile range (horizontal)
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = "61",
                  horizontal_tiles = c(-13, -3),
                  vertical_tiles = vertical_tiles,
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # invalid tile range (horizontal)
  testthat::expect_error(
    download_data(dataset_name = "modis",
                  date = c(date_start, date_end),
                  product = product,
                  version = "61",
                  horizontal_tiles = horizontal_tiles,
                  vertical_tiles = c(100, 102),
                  nasa_earth_data_token = nasa_earth_data_token,
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE)
  )

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    product,
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)[, 2]
  # extract urls
  urls <- extract_urls(commands = commands, position = 4)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 2L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_modis (MOD + MYD products)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  products <- c(
    "MOD09GA", "MYD09GA", "MOD09GQ", "MYD09GQ", "MOD09A1", "MYD09A1",
    "MOD09Q1", "MYD09Q1", "MOD11A1", "MYD11A1", "MOD11A2", "MYD11A2",
    "MOD11B1", "MYD11B1", "MOD13A1", "MYD13A1", "MOD13A2", "MYD13A2",
    "MOD13A3", "MYD13A3"
  )
  version <- "61"
  horizontal_tiles <- c(10, 10)
  vertical_tiles <- c(4, 5)
  nasa_earth_data_token <- "tOkEnPlAcEhOlDeR"
  directory_to_save <- paste0(tempdir(), "/mod/")
  date_start <- "2021-06-01"
  date_end <- "2021-06-30"
  for (p in seq_along(products)) {
    cat("Testing product:", products[p], "\n")
    # run download function
    download_data(
      dataset_name = "modis",
      date = c(date_start, date_end),
      product = products[p],
      version = version,
      horizontal_tiles = horizontal_tiles,
      vertical_tiles = vertical_tiles,
      nasa_earth_data_token = nasa_earth_data_token,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
    # define file path with commands
    commands_path <- paste0(
      directory_to_save,
      products[p],
      "_",
      date_start,
      "_",
      date_end,
      "_wget_commands.txt"
    )
    # import commands
    commands <- read_commands(commands_path = commands_path)[, 2]
    # extract urls
    urls <- extract_urls(commands = commands, position = 4)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 3L, method = "HEAD")
    # implement unit tests
    test_download_functions(
      directory_to_save = directory_to_save,
      commands_path = commands_path,
      url_status = url_status
    )
    # remove file with commands after test
    file.remove(commands_path)
  }
  unlink(directory_to_save, recursive = TRUE)
})

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


testthat::test_that("process_modis_merge", {
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

  testthat::expect_no_warning(
    vnp46_proc <- process_blackmarble(
      path = path_vnp46[1],
      tile_df = corn,
      date = "2018-08-13"
    )
  )
  testthat::expect_s4_class(vnp46_proc, "SpatRaster")
  testthat::expect_equal(terra::nlyr(vnp46_proc), 1L)

  testthat::expect_no_warning(
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


testthat::test_that("process_modis (expected errors)", {
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
        calculate_modis(
          from = path_mod11,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_modis_merge,
          name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
          subdataset = "(LST_)"
        )
    )
  )
  testthat::expect_s3_class(calc_mod11, "data.frame")

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
          subdataset = "(LST_)"
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
          geom = "terra"
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
          geom = "sf"
        )
    )
  )
  testthat::expect_true("sf" %in% class(calc_mod11_sf))

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
        geom = TRUE
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
  testthat::expect_true("sf" %in% class(calc_mod06_sf))

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

  # case 3: VIIRS
  path_vnp46 <-
    list.files(
      testthat::test_path("..", "testdata/modis"),
      "VNP46",
      full.names = TRUE
    )
  testthat::expect_no_warning(
    base_vnp <- process_blackmarble(
      path = path_vnp46,
      date = "2018-08-13",
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )

  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46 <-
        calculate_modis(
          from = path_vnp46,
          locs = site_faux,
          preprocess = process_blackmarble,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
        )
    )
  )
  testthat::expect_s3_class(calc_vnp46, "data.frame")

  # with geometry terra
  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46_terra <-
        calculate_modis(
          from = path_vnp46,
          locs = site_faux,
          preprocess = process_blackmarble,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
          geom = "terra"
        )
    )
  )
  testthat::expect_s4_class(calc_vnp46_terra, "SpatVector")


  # with geometry sf
  testthat::expect_no_error(
    suppressWarnings(
      calc_vnp46_sf <-
        calculate_modis(
          from = path_vnp46,
          locs = sf::st_as_sf(site_faux),
          preprocess = process_blackmarble,
          name_covariates = c("MOD_NITLT_0_"),
          subdataset = 3L,
          tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
          geom = "sf"
        )
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
      geom = TRUE
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
  testthat::expect_true("sf" %in% class(calc_mod_sf))

  testthat::expect_error(
    calculate_modis(from = site_faux)
  )
  testthat::expect_error(
    calculate_modis(from = path_mod11, product = "MOD11A1", locs = list(1, 2, 3))
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
  testthat::expect_warning(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = process_blackmarble,
      name_covariates = c("MOD_NITLT_0_", "MOD_K1_"),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5))
    )
  )
  testthat::expect_no_warning(
    flushed <- calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c("MOD_NITLT_0_"),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L)
    )
  )
  testthat::expect_s3_class(flushed, "data.frame")
  testthat::expect_true(unlist(flushed[, 2]) == -99999)

})
# nolint end
