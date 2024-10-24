################################################################################
##### unit and integration tests for MLCR NLCD functions

################################################################################
##### download_nlcd
testthat::test_that("download_nlcd", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c(2021, 2019, 2016)
  collections <- c(rep("Coterminous United States", 2), "Alaska")
  collection_codes <- c(rep("l48", 2), "ak")
  directory_to_save <- paste0(tempdir(), "/nlcd/")
  # run download function
  for (y in seq_along(years)) {
    download_data(dataset_name = "nlcd",
                  year = years[y],
                  collection = collections[y],
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = FALSE,
                  unzip = FALSE,
                  remove_zip = FALSE)
    # define file path with commands
    commands_path <- paste0(download_sanitize_path(directory_to_save),
                            "nlcd_",
                            years[y],
                            "_land_cover_",
                            collection_codes[y],
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
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
    urls <- extract_urls(commands = commands, position = 5)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
  testthat::expect_error(
    download_data(dataset_name = "nlcd",
                  year = 2000,
                  collection = "Coterminous United States",
                  directory_to_save = directory_to_save,
                  acknowledgement = TRUE,
                  download = FALSE,
                  remove_command = TRUE,
                  unzip = FALSE,
                  remove_zip = FALSE)
  )
  # remove temporary nlcd
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_nlcd
testthat::test_that("process_nlcd", {
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

################################################################################
##### calculate_nlcd
testthat::test_that("calculate_nlcd", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_options(
    list(sf_use_s2 = FALSE, future.resolve.recursive = 2L)
  )

  point_us1 <- cbind(lon = -114.7, lat = 38.9, site_id = 1)
  point_us2 <- cbind(lon = -114, lat = 39, site_id = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, site_id = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, site_id = 4) # france
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
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              radius = "1000"),
    "radius is not a numeric."
  )
  testthat::expect_error(
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "whatnot",
              radius = 1000)
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              radius = -3),
    "radius has not a likely value."
  )

  # -- two modes work properly
  testthat::expect_no_error(
    calculate_nlcd(locs = sf::st_as_sf(eg_data),
              from = nlcdras,
              mode = "exact",
              radius = 1000)
  )
  testthat::expect_no_error(
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "terra",
              radius = 300)
  )
  # -- multicore mode works properly
  testthat::expect_no_error(
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "exact",
              radius = 1000,
              nthreads = 2L)
  )
  testthat::expect_no_error(
    calculate_nlcd(locs = eg_data,
              from = nlcdras,
              mode = "terra",
              radius = 1000,
              nthreads = 2L)
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
  testthat::expect_error(
    calculate_nlcd(locs = 12,
              locs_id = "site_id",
              from = nlcdras)
  )
  testthat::expect_error(
    calculate_nlcd(locs = eg_data,
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
    calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = buf_radius
    )
  )
  output <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras
  )
  # -- returns a data.frame
  testthat::expect_equal(class(output)[1], "data.frame")
  # nrow(output) == nrow(input)
  testthat::expect_equal(nrow(output), 4)
  # -- initial names are still in the output data.frame
  testthat::expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  # the value has changed. What affected this behavior?
  testthat::expect_equal(
    output$LDU_TEFOR_0_03000[1], 0.8119843, tolerance = 1e-7
  )
  testthat::expect_equal(
    output$LDU_TSHRB_0_03000[2], 0.9630467, tolerance = 1e-7
  )
  # -- class fraction rows should sum to 1
  testthat::expect_equal(
    unname(rowSums(output[1:2, 3:(ncol(output))])),
    rep(1, 2),
    tolerance = 1e-7
  )
  # without geometry will have 11 columns
  testthat::expect_equal(
    ncol(output), 15
  )
  # example with terra output
  output_terra <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = "terra"
  )
  # with geometry will have 15 columns
  testthat::expect_equal(
    ncol(output_terra), 15
  )
  testthat::expect_true(
    "SpatVector" %in% class(output_terra)
  )
  # example with sf output
  output_sf <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = "sf"
  )
  # with geometry will have 16 columns
  testthat::expect_equal(
    ncol(output_sf), 16
  )
  testthat::expect_true(
    "sf" %in% class(output_sf)
  )
  # error with TRUE geom
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      radius = buf_radius,
      from = nlcdras,
      geom = TRUE
    )
  )
})
