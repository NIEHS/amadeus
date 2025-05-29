################################################################################
##### unit and integration tests for MLCR NLCD functions

################################################################################
##### download_nlcd
testthat::test_that("download_nlcd", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- sample(1985:2023L, size = 2)
  products <- c(
    "Land Cover",
    "Land Cover Change",
    "Land Cover Confidence",
    "Fractional Impervious Surface",
    "Impervious Descriptor",
    "Spectral Change Day of Year"
  )
  product_codes <- c(
    "LndCov",
    "LndChg",
    "LndCnf",
    "FctImp",
    "ImpDsc",
    "SpcChg"
  )
  directory_to_save <- paste0(tempdir(), "/nlcd/")
  # run download function
  for (y in seq_along(years)) {
    p <- sample(seq_len(length(products)), size = 1L)
    download_data(
      dataset_name = "nlcd",
      year = years[y],
      product = products[p],
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    )
    # define file path with commands
    commands_path <- paste0(
      download_sanitize_path(directory_to_save),
      "nlcd_",
      tolower(product_codes[p]),
      "_",
      years[y],
      "_",
      Sys.Date(),
      "_curl_command.txt"
    )
    # expect sub-directories to be created
    testthat::expect_true(
      length(list.files(directory_to_save, include.dirs = TRUE)) == 1
    )
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 5)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(
      directory_to_save = directory_to_save,
      commands_path = commands_path,
      url_status = url_status
    )
    # remove file with commands after test
    file.remove(commands_path)
  }
  testthat::expect_error(
    download_data(
      dataset_name = "nlcd",
      year = 1900,
      product = "land cover",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    )
  )
  # remove temporary nlcd
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_nlcd
testthat::test_that("process_nlcd", {
  withr::local_package("terra")

  path_nlcd21 <- testthat::test_path("..", "testdata", "nlcd")

  testthat::expect_no_error(
    nlcd21 <- process_nlcd(path = path_nlcd21, year = 2021)
  )
  # test with extent cropping
  testthat::expect_no_error(
    nlcd21_ext <- process_nlcd(
      path = path_nlcd21,
      year = 2021,
      extent = terra::ext(
        1510241.32304443,
        1536875.51988709,
        1558885.5313357,
        1603354.11202184
      )
    )
  )
  testthat::expect_s4_class(nlcd21, "SpatRaster")

  # error cases
  testthat::expect_error(
    process_nlcd(path = 1L)
  )
  testthat::expect_error(
    process_nlcd(path = "/universe/galaxy/solarsys/earth/usa.nc")
  )
  testthat::expect_error(
    process_nlcd(path_nlcd21, "nineteen eighty-four")
  )
  testthat::expect_error(
    process_nlcd(path_nlcd21, year = 2020)
  )
  # make duplicate with tif and img
  tdir <- tempdir()
  dir.create(paste0(tdir, "/nlcd_all"))
  file.create(paste0(tdir, "/nlcd_all/Annual_NLCD_LndCov_2021_CU_C1V0.tif"))
  file.create(paste0(tdir, "/nlcd_all/Annual_NLCD_LndCov_2021_CU_C1V0.img"))
  testthat::expect_error(
    process_nlcd(path = paste0(tdir, "/nlcd_all"), year = 2021)
  )
})

testthat::test_that("process_nlcd (deprecated path structure.)", {
  withr::local_package("terra")

  path_nlcd <- testthat::test_path("..", "testdata")

  testthat::expect_message(
    nlcd21 <- process_nlcd(path = path_nlcd, year = 2021)
  )
  testthat::expect_s4_class(nlcd21, "SpatRaster")

  testthat::expect_no_error(
    nlcd19 <- process_nlcd(path = path_nlcd, year = 2019)
  )
  testthat::expect_s4_class(nlcd19, "SpatRaster")
})

################################################################################
##### calculate_nlcd
testthat::test_that("calculate_nlcd", {
  withr::local_package("terra")
  withr::local_package("exactextractr")
  withr::local_package("sf")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )

  point_us1 <- cbind(lon = -78.85, lat = 36.09, site_id = 1)
  point_us2 <- cbind(lon = -78.73, lat = 35.96, site_id = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, site_id = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, site_id = 4) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_testdata <- testthat::test_path("..", "testdata", "nlcd")

  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  testthat::expect_no_error(
    nlcdras <- process_nlcd(path = path_testdata, year = 2021)
  )
  testthat::expect_s4_class(nlcdras, "SpatRaster")

  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = "1000"
    ),
    "radius is not a numeric."
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = "whatnot",
      radius = 1000
    )
  )
  # -- buf_radius has likely value
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    "radius has not a likely value."
  )

  # -- two modes work properly
  testthat::expect_no_error(
    calculate_nlcd(
      locs = sf::st_as_sf(eg_data),
      from = nlcdras,
      mode = "exact",
      radius = 1000
    )
  )
  testthat::expect_no_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = "terra",
      radius = 300
    )
  )

  # -- year is numeric
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = "2021"),
    "year is not a numeric."
  )
  # -- year has likely value
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 2032),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 1789),
    "NLCD data not available for this year."
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = 12,
      locs_id = "site_id",
      from = nlcdras
    )
  )
  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = 12
    )
  )
  # -- nlcd_path is not a character
  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    "path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
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
  testthat::expect_true(all(eg_data$site_id %in% output$site_id))
  # the value has changed. What affected this behavior?
  testthat::expect_equal(
    output$LDU_TEFOR_0_03000[1],
    0.09010682,
    tolerance = 1e-7
  )
  testthat::expect_equal(
    output$LDU_TSHRB_0_03000[2],
    0.01047932,
    tolerance = 1e-7
  )
  # -- class fraction rows should sum to 1
  testthat::expect_equal(
    unname(rowSums(output[1:2, 3:(ncol(output))])),
    rep(1, 2),
    tolerance = 1e-7
  )
  # without geometry will have 17 columns
  testthat::expect_equal(
    ncol(output),
    17
  )
  # example with terra output
  output_terra <- calculate_nlcd(
    locs = eg_data,
    locs_id = "site_id",
    radius = buf_radius,
    from = nlcdras,
    geom = "terra"
  )
  # with geometry will have 17 columns
  testthat::expect_equal(
    ncol(output_terra),
    17
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
  # with geometry will have 18 columns
  testthat::expect_equal(
    ncol(output_sf),
    18
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

  ##### point extractions (radius = 0)
  # point extraction (sf)
  testthat::expect_no_error(
    out_points_sf <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = "sf"
    )
  )
  # with geometry will have 4 columns
  testthat::expect_equal(
    ncol(out_points_sf),
    4
  )
  testthat::expect_true(
    "sf" %in% class(out_points_sf)
  )
  # point extraction (terra)
  testthat::expect_no_error(
    out_points_t <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = "terra"
    )
  )
  # with geometry will have 4 columns
  testthat::expect_equal(
    ncol(out_points_t),
    3
  )
  testthat::expect_true(
    "SpatVector" %in% class(out_points_t)
  )
  # point extraction (data frame)
  testthat::expect_no_error(
    out_points_df <- calculate_nlcd(
      locs = eg_data,
      locs_id = "site_id",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = FALSE
    )
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(out_points_df),
    3
  )
  testthat::expect_true(is.data.frame(out_points_df))
})


testthat::test_that("calculate_nlcd (deprecated path stucture)", {
  withr::local_package("terra")

  point_us1 <- cbind(lon = -114.7, lat = 38.9, SI = 1)
  point_us2 <- cbind(lon = -114, lat = 39, SI = 2)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, SI = 3) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, SI = 4) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) |>
    as.data.frame() |>
    terra::vect(crs = "EPSG:4326")

  path_nlcd <- testthat::test_path("..", "testdata")

  testthat::expect_message(
    nlcdras <- process_nlcd(path = path_nlcd, year = 2021)
  )
  # point extraction (data frame)
  testthat::expect_no_error(
    out_points_df <- calculate_nlcd(
      locs = eg_data,
      locs_id = "SI",
      from = nlcdras,
      radius = 0,
      mode = "exact",
      geom = FALSE
    )
  )
  # with geometry will have 3 columns
  testthat::expect_equal(
    ncol(out_points_df),
    3
  )
  testthat::expect_true(is.data.frame(out_points_df))
})
