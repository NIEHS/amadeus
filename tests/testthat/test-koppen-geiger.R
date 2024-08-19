################################################################################
##### unit and integration tests for Koppen Geiger functions

################################################################################
##### download_koppen_geiger
testthat::test_that("download_koppen_geiger", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  time_periods <- c("Present", "Future")
  data_resolutions <- c("0.0083")
  directory_to_save <- paste0(tempdir(), "/kop/")
  # run download function
  for (p in seq_along(time_periods)) {
    time_period <- time_periods[p]
    for (d in seq_along(data_resolutions)) {
      download_data(dataset_name = "koppen",
                    time_period = time_period,
                    data_resolution = data_resolutions[d],
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    unzip = FALSE,
                    remove_zip = FALSE,
                    download = FALSE,
                    remove_command = FALSE)
      # define file path with commands
      commands_path <- paste0(download_sanitize_path(directory_to_save),
                              "koppen_geiger_",
                              time_period,
                              "_",
                              gsub("\\.",
                                   "p",
                                   data_resolutions[d]),
                              "_",
                              Sys.Date(),
                              "_wget_command.txt")
      # expect sub-directories to be created
      testthat::expect_true(
        length(
          list.files(
            directory_to_save, include.dirs = TRUE
          )
        ) == 3
      )
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 2)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
      # implement unit tests
      test_download_functions(directory_to_save = directory_to_save,
                              commands_path = commands_path,
                              url_status = url_status)
      # remove file with commands after test
      file.remove(commands_path)
    }
  }
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_koppen_geiger
testthat::test_that("process_koppen_geiger", {
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

  path_kgeiger_f <-
    testthat::test_path("../testdata", "kop", "Beck_KG_V1_future_0p5.tif")
  testthat::expect_no_error(
    kgeiger_f <- process_koppen_geiger(path_kgeiger_f)
  )
})

################################################################################
##### calc_koppen_geiger
testthat::test_that("calc_koppen_geiger", {
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
