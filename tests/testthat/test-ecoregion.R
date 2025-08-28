################################################################################
##### unit and integration tests for U.S. EPA Ecoregion functions
# nolint start

################################################################################
##### download_ecoregion
testthat::test_that("download_ecoregion", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  directory_to_save <- paste0(tempdir(), "/eco/")
  certificate <- system.file(
    "extdata/cacert_gaftp_epa.pem",
    package = "amadeus"
  )
  # run download function
  download_data(
    dataset_name = "ecoregion",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    unzip = FALSE,
    remove_zip = FALSE,
    download = FALSE,
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
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 3)
  # check HTTP URL status
  url_status <-
    httr::HEAD(urls, config = httr::config(cainfo = certificate))
  url_status <- url_status$status_code
  # implement unit tets
  test_download_functions(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status
  )

  file.create(
    file.path(directory_to_save, "zip_files", "us_eco_l3_state_boundaries.zip"),
    recursive = TRUE
  )
  testthat::expect_no_error(
    download_data(
      dataset_name = "ecoregion",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      unzip = FALSE,
      remove_zip = TRUE,
      download = FALSE,
      remove_command = TRUE,
      epa_certificate_path = certificate
    )
  )
  testthat::expect_true(
    dir.exists(paste0(directory_to_save, "/data_files"))
  )
  testthat::expect_equal(
    length(
      list.files(
        directory_to_save,
        recursive = TRUE,
        include.dirs = TRUE
      )
    ),
    1
  )
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_ecoregion (expected errors)", {
  # expected errors due to invalid certificate
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  tdir <- tempdir(check = TRUE)
  directory_to_save <- paste0(tempdir(), "/epa/")
  certificate <- file.path(tdir, "cacert_gaftp_epa.pem")
  # remove if there is a preexisting file
  if (file.exists(certificate)) {
    file.remove(certificate)
    file.remove(gsub("pem", "crt", certificate))
  }

  # run download function

  testthat::expect_message(
    download_data(
      dataset_name = "ecoregion",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE,
      unzip = FALSE,
      remove_zip = FALSE,
      epa_certificate_path = certificate
    )
  )
  # unlink dir
  unlink(tdir)

  # define file path with commands
  commands_path <- paste0(
    directory_to_save,
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )

  # remove file with commands after test
  testthat::expect_true(file.exists(commands_path))
  file.remove(commands_path)
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### process_ecoregion
testthat::test_that("process_ecoregion", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_eco <- testthat::test_path(
    "..",
    "testdata",
    "ecoregions",
    "eco_l3_clip.gpkg"
  )
  testthat::expect_no_error(
    eco <- process_ecoregion(path_eco)
  )

  # test with cropping extent
  testthat::expect_no_error(
    process_ecoregion(path_eco, extent = terra::ext(eco))
  )
  ecotemp <- sf::st_read(path_eco)
  # nolint start
  addpoly <-
    "POLYGON ((-70.2681 43.6787, -70.252234 43.677145, -70.251036 -43.680758, -70.268666 43.681505, -70.2681 43.6787))"
  # nolint end
  addpoly <- sf::st_as_sfc(addpoly, crs = "EPSG:4326")
  addpoly <- sf::st_transform(addpoly, sf::st_crs(ecotemp))
  ecotemp[1, "geom"] <- addpoly
  tdir <- tempdir()
  sf::st_write(ecotemp, paste0(tdir, "/ecoregions.gpkg"), append = FALSE)
  testthat::expect_no_error(
    suppressWarnings(process_ecoregion(paste0(tdir, "/ecoregions.gpkg")))
  )
})

################################################################################
##### calculate_ecoregion
testthat::test_that("calculate_ecoregion", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  ecol3 <- testthat::test_path(
    "..",
    "testdata",
    "ecoregions",
    "eco_l3_clip.gpkg"
  )
  site_faux <-
    data.frame(
      site_id = "37999109988101",
      lon = -77.576,
      lat = 39.40,
      date = as.Date("2022-01-01")
    )
  site_faux <-
    terra::vect(
      site_faux,
      geom = c("lon", "lat"),
      keepgeom = TRUE,
      crs = "EPSG:4326"
    )
  site_faux <- terra::project(site_faux, "EPSG:5070")

  testthat::expect_no_error(
    erras <- process_ecoregion(ecol3)
  )

  testthat::expect_no_error(
    ecor_res <- calculate_ecoregion(
      from = erras,
      locs = sf::st_as_sf(site_faux),
      locs_id = "site_id"
    )
  )

  testthat::expect_no_error(
    ecor_res <- calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id"
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(ecor_res, "data.frame")
  # ncol is equal to 2 + 5 + 2 + 1 + 1
  testthat::expect_equal(ncol(ecor_res), 4L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
    sum(unlist(ecor_res[, dum_cn])),
    2L
  )

  testthat::expect_no_error(
    ecor_terra <- calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      geom = "terra"
    )
  )
  testthat::expect_equal(
    ncol(ecor_terra),
    4
  )
  testthat::expect_true(
    "SpatVector" %in% class(ecor_terra)
  )

  testthat::expect_no_error(
    ecor_sf <- calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      geom = "sf"
    )
  )
  testthat::expect_equal(
    ncol(ecor_sf),
    5
  )
  testthat::expect_true(
    "sf" %in% class(ecor_sf)
  )

  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      geom = TRUE
    )
  )

  # Test unmatched locations are correctly reintroduced with NA values in ecoregion columns
  site_unmatched <-
    data.frame(
      site_id = "99999999999999",
      lon = -179.99,
      lat = -79.99,
      date = as.Date("2022-01-01")
    )
  site_unmatched <-
    terra::vect(
      site_unmatched,
      geom = c("lon", "lat"),
      keepgeom = TRUE,
      crs = "EPSG:4326"
    )
  site_unmatched <- terra::project(site_unmatched, "EPSG:5070")

  site_combined <- rbind(sf::st_drop_geometry(site_faux), site_unmatched)
  site_combined <- sf::st_as_sf(
    site_combined,
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )

  ecor_res2 <- calculate_ecoregion(
    from = erras,
    locs = site_combined,
    locs_id = "site_id"
  )

  testthat::expect_true(
    "99999999999999" %in% ecor_res2$site_id,
    info = "Unmatched site should be present."
  )

  extra_cols <- setdiff(
    colnames(ecor_res2),
    colnames(sf::st_drop_geometry(site_combined))
  )
  unmatched_row <- ecor_res2[ecor_res2$site_id == "99999999999999", ]

  testthat::expect_true(
    all(is.na(unmatched_row[, extra_cols])),
    info = "Unmatched locations should have NA in ecoregion-related columns."
  )

  # Ensure the warning message appears when unmatched locations exist
  testthat::expect_message(
    calculate_ecoregion(
      from = erras,
      locs = site_combined,
      locs_id = "site_id"
    ),
    "Warning: only .* locations provided had matching ecoregions.",
    fixed = FALSE
  )
})
# nolint end
