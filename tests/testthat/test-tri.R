################################################################################
##### unit and integration tests for U.S. EPA TRI functions

################################################################################
##### download_tri
testthat::test_that("download_tri", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/tri/")
  # run download function
  download_data(dataset_name = "tri",
                directory_to_save = directory_to_save,
                acknowledgement = TRUE,
                download = FALSE,
                remove_command = FALSE)
  year_start <- 2018L
  year_end <- 2022L

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "TRI_",
    year_start, "_", year_end,
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "SKIP")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_tri (single year)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/tri/")
  year <- 2019L
  # run download function
  download_data(
    year = year,
    dataset_name = "tri",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    remove_command = FALSE
  )

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "TRI_",
    year, "_", year,
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 1L, method = "SKIP")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})


################################################################################
##### process_tri
testthat::test_that("process_tri", {
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

################################################################################
##### calc_tri
testthat::test_that("calc_tri", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("tidyr")
  withr::local_package("data.table")
  withr::local_options(sf_use_s2 = FALSE)

  ncp <- data.frame(lon = c(-78.8277, -78.0000), lat = c(35.95013, 80.000))
  ncp$site_id <- c("3799900018810101", "3799900018819999")
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
      radius = c(1500L, 50000L)
    )
  )
  testthat::expect_true(is.data.frame(tri_c))

  # with geometry
  testthat::expect_no_error(
    tri_c_geom <- calc_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L),
      geom = TRUE
    )
  )
  testthat::expect_s4_class(tri_c_geom, "SpatVector")

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
