################################################################################
##### unit and integration tests for U.S. EPA NEI functions
# nolint start

################################################################################
##### download_nei
testthat::test_that("download_nei", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")
  certificate <- system.file(
    "extdata/cacert_gaftp_epa.pem",
    package = "amadeus"
  )
  # run download function
  year <- c(2017L, 2020L)
  download_data(
    dataset_name = "nei",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    year = year,
    remove_command = FALSE,
    epa_certificate_path = certificate
  )
  # expect sub-directories to be created
  testthat::expect_true(
    length(
      list.files(
        directory_to_save,
        include.dirs = TRUE
      )
    ) ==
      3
  )
  # define file path with commands
  commands_path <- paste0(
    download_sanitize_path(directory_to_save),
    "NEI_AADT_",
    paste(year, collapse = "-"),
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )

  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <-
    httr::HEAD(urls[1], config = httr::config(cainfo = certificate))
  url_status <- url_status$status_code
  # implement unit tests
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )
  # remove file with commands after test
  file.remove(commands_path)
  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei (live)", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/nei/")
  certificate <- system.file(
    "extdata/cacert_gaftp_epa.pem",
    package = "amadeus"
  )
  # run download function
  year <- c(2017L, 2020L)
  testthat::expect_no_error(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = TRUE,
      year = year,
      remove_command = FALSE,
      epa_certificate_path = certificate,
      unzip = TRUE
    )
  )
  testthat::expect_equal(
    length(list.files(paste0(directory_to_save, "/zip_files"))),
    2
  )
  testthat::expect_equal(
    length(list.files(
      paste0(directory_to_save, "/data_files"),
      recursive = TRUE
    )),
    12
  )
  # remove temporary nei
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_nei (expected errors)", {
  # expected errors due to invalid certificate
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  tdir <- tempdir()
  directory_to_save <- paste0(tempdir(), "/epa/")
  certificate <- file.path(tdir, "cacert_gaftp_epa.pem")
  # remove if there is a preexisting file
  if (file.exists(certificate)) {
    file.remove(certificate)
    file.remove(gsub("pem", "crt", certificate))
  }

  # run download function
  year <- c(2017L)
  testthat::expect_message(
    download_data(
      dataset_name = "nei",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      year = year,
      remove_command = FALSE,
      epa_certificate_path = certificate
    )
  )
  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "NEI_AADT_",
    paste(year, collapse = "-"),
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  # remove file with commands after test
  testthat::expect_true(file.exists(commands_path))
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_nei
testthat::test_that("process_nei", {
  withr::local_package("terra")

  path_nei <- testthat::test_path("../testdata", "nei", "")
  path_cnty <- system.file("gpkg/nc.gpkg", package = "sf")
  path_cnty <- terra::vect(path_cnty)
  path_cnty$GEOID <- path_cnty$FIPS

  testthat::expect_no_error(
    neinc <- process_nei(path = path_nei, year = 2017, county = path_cnty)
  )
  testthat::expect_s4_class(neinc, "SpatVector")

  # error cases
  testthat::expect_error(
    process_nei(testthat::test_path("../testdata", "modis"), year = 2017)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2030, county = path_cnty)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = NULL)
  )
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = array(1, 2))
  )
  testthat::expect_error(
    process_nei("./EmPtY/pAtH", year = 2020, county = path_cnty)
  )
  names(path_cnty)[which(names(path_cnty) == "GEOID")] <- "COUNTYID"
  testthat::expect_error(
    process_nei(path_nei, year = 2020, county = path_cnty)
  )
})

################################################################################
##### calculate_nei
testthat::test_that("calculate_nei", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("data.table")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(202401)

  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(ncpath)
  nc <- nc[grep("(Orange|Wake|Durham)", nc$NAME), ]

  neipath <- testthat::test_path("..", "testdata", "nei")

  testthat::expect_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )

  nc$GEOID <- nc$FIPS
  testthat::expect_no_error(
    neiras <- process_nei(
      path = neipath,
      county = nc,
      year = 2017
    )
  )
  # inspecting calculated results
  testthat::expect_true(inherits(neiras, "SpatVector"))
  testthat::expect_true(nrow(neiras) == 3)

  # sf case
  testthat::expect_no_error(
    neires <- process_nei(
      path = neipath,
      county = sf::st_as_sf(nc),
      year = 2017
    )
  )
  testthat::expect_true(inherits(neires, "SpatVector"))
  testthat::expect_true(nrow(neires) == 3)

  # error cases
  testthat::expect_error(
    process_nei(neipath, year = 2017)
  )
  testthat::expect_error(
    process_nei(neipath, "Orion/Betelgeuse", year = 2017)
  )
  testthat::expect_error(
    process_nei(neipath, nc, year = 2083)
  )

  # calculate_nei
  ncp <- data.frame(lon = -78.8277, lat = 35.95013)
  ncp$site_id <- "3799900018810101"
  ncp$time <- 2018L
  ncp <- terra::vect(ncp, keepgeom = TRUE, crs = "EPSG:4326")
  nc <- terra::project(nc, "EPSG:4326")

  testthat::expect_no_error(
    neicalced <- calculate_nei(
      locs = ncp,
      from = neiras
    )
  )
  testthat::expect_true(any(grepl("NEI", names(neicalced))))
  testthat::expect_equal(neicalced$TRF_NEINP_0_00000, 1579079, tolerance = 1)

  # with geometry terra
  testthat::expect_no_error(
    neicalced_terra <- calculate_nei(
      locs = ncp,
      from = neiras,
      geom = "terra"
    )
  )
  testthat::expect_s4_class(neicalced_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    neicalced_sf <- calculate_nei(
      locs = ncp,
      from = neiras,
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(neicalced_sf))

  # more error cases
  testthat::expect_condition(
    calculate_nei(
      locs = "jittered",
      from = neiras
    )
  )
  testthat::expect_error(
    calculate_nei(
      locs = ncp,
      from = neiras,
      geom = TRUE
    )
  )
})
# nolint end
