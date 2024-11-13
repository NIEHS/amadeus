################################################################################
##### unit and integration tests for NASA SEDAC gRoads functions

################################################################################
##### download_sedac_groads
testthat::test_that("download_sedac_groads", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  data_regions <- c("Americas", "Global")
  data_formats <- c("Geodatabase", "Shapefile")
  directory_to_save <- paste0(tempdir(), "/groads/")
  # run download function
  for (r in seq_along(data_regions)) {
    data_region <- data_regions[r]
    for (f in seq_along(data_formats)) {
      download_data(dataset_name = "sedac_groads",
                    directory_to_save = directory_to_save,
                    acknowledgement = TRUE,
                    data_format = data_formats[f],
                    data_region = data_region,
                    download = FALSE,
                    unzip = FALSE,
                    remove_zip = FALSE,
                    remove_command = FALSE)
      # expect sub-directories to be created
      testthat::expect_true(
        length(
          list.files(
            directory_to_save, include.dirs = TRUE
          )
        ) == 3
      )
      # define file path with commands
      commands_path <- paste0(download_sanitize_path(directory_to_save),
                              "sedac_groads_",
                              gsub(" ", "_", tolower(data_region)),
                              "_",
                              Sys.Date(),
                              "_curl_command.txt")
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 11)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L, method = "GET")
      # implement unit tests
      test_download_functions(directory_to_save = directory_to_save,
                              commands_path = commands_path,
                              url_status = url_status)
      # remove file with commands after test
      file.remove(commands_path)
    }
  }

  testthat::expect_message(
    download_data(dataset_name = "sedac_groads",
                  data_format = "Shapefile",
                  data_region = "Global",
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE,
                  remove_command = TRUE)
  )
  # remove temporary groads
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_sedac_groads
testthat::test_that("process_sedac_groads", {
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

################################################################################
##### calculate_sedac_groads
testthat::test_that("calculate_groads", {
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
    groads_res <- calculate_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000
    )
  )

  testthat::expect_error(
    calculate_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 0
    )
  )

  # expect data.frame
  testthat::expect_s3_class(groads_res, "data.frame")

  # return with geometry terra
  testthat::expect_no_error(
    groads_terra <- calculate_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(groads_terra), 4
  )
  testthat::expect_true(
    "SpatVector" %in% class(groads_terra)
  )

  # return with geometry sf
  testthat::expect_no_error(
    groads_sf <- calculate_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(groads_sf), 5
  )
  testthat::expect_true(
    "sf" %in% class(groads_sf)
  )

  testthat::expect_error(
    calculate_sedac_groads(
      from = groads,
      locs = ncp,
      locs_id = "site_id",
      radius = 5000,
      geom = TRUE
    )
  )
})
