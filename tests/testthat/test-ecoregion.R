################################################################################
##### unit and integration tests for U.S. EPA Ecoregion functions
# nolint start

################################################################################
##### download_ecoregion
testthat::test_that("download_ecoregion returns proper URL list", {
  withr::with_tempdir({
    # Suppress deprecation warning
    result <- suppressWarnings(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check return structure
    testthat::expect_type(result, "list")
    testthat::expect_named(result, c("urls", "destfiles", "n_files"))
    testthat::expect_equal(length(result$urls), length(result$destfiles))
    testthat::expect_equal(result$n_files, length(result$urls))

    # Check single file expected
    testthat::expect_equal(result$n_files, 1)

    # Check URL is valid format
    testthat::expect_true(grepl("^https?://", result$urls))
    testthat::expect_true(grepl("\\.zip$", result$destfiles))
  })
})

testthat::test_that("download_ecoregion validates URL", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    result <- suppressWarnings(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check URL is accessible
    testthat::expect_true(check_url_status(result$urls))
  })
})

testthat::test_that("download_ecoregion creates proper directory structure", {
  withr::with_tempdir({
    suppressWarnings(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Check directories were created
    testthat::expect_true(dir.exists("zip_files"))
    testthat::expect_true(dir.exists("data_files"))
  })
})

testthat::test_that("download_ecoregion skips existing files", {
  withr::with_tempdir({
    # Create existing file with content (more than 0 bytes)
    dir.create("zip_files", recursive = TRUE)
    zip_path <- "zip_files/us_eco_l3_state_boundaries.zip"
    # Write actual binary content to simulate a real zip file
    writeBin(raw(1000), zip_path)

    result <- suppressWarnings(
      download_ecoregion(
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE
      )
    )

    # Should skip existing file
    testthat::expect_equal(result$n_files, 1)

    # Verify original file still exists and has content
    testthat::expect_true(file.exists(zip_path))
    testthat::expect_gt(file.size(zip_path), 0)
  })
})

testthat::test_that("download_ecoregion (LIVE - small download)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    result <- download_ecoregion(
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = FALSE
    )

    # Check file was downloaded
    zip_files <- list.files("zip_files", pattern = "\\.zip$")
    testthat::expect_true(length(zip_files) > 0)

    # Check file size is reasonable
    zip_path <- list.files("zip_files", pattern = "\\.zip$", full.names = TRUE)[
      1
    ]
    testthat::expect_gt(file.size(zip_path), 1000)
  })
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
  sf::st_write(
    ecotemp,
    paste0(tdir, "/ecoregions.gpkg"),
    append = FALSE,
    quiet = TRUE
  )
  testthat::expect_no_error(
    suppressWarnings(process_ecoregion(paste0(tdir, "/ecoregions.gpkg")))
  )
})

testthat::test_that("process_ecoregion validates inputs", {
  # Test with invalid path
  testthat::expect_error(
    process_ecoregion("/invalid/path/to/file.gpkg"),
    "path"
  )

  # Test with non-spatial file
  withr::with_tempdir({
    writeLines("not a spatial file", "test.txt")
    testthat::expect_error(
      process_ecoregion("test.txt")
    )
  })
})

testthat::test_that("process_ecoregion returns SpatVector", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_eco <- testthat::test_path(
    "..",
    "testdata",
    "ecoregions",
    "eco_l3_clip.gpkg"
  )

  eco <- process_ecoregion(path_eco)

  testthat::expect_s4_class(eco, "SpatVector")
  # is.valid() returns a logical vector (one per feature)
  testthat::expect_true(all(terra::is.valid(eco)))
  testthat::expect_gt(terra::nrow(eco), 0)
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

testthat::test_that("calculate_ecoregion validates inputs", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  ecol3 <- testthat::test_path(
    "..",
    "testdata",
    "ecoregions",
    "eco_l3_clip.gpkg"
  )
  erras <- process_ecoregion(ecol3)

  # Test with invalid locs
  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = "not a spatial object",
      locs_id = "site_id"
    )
  )

  # Test with missing locs_id
  site_faux <- data.frame(
    lon = -77.576,
    lat = 39.40
  )
  site_faux <- terra::vect(
    site_faux,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )

  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "nonexistent_id"
    )
  )
})

################################################################################
##### Integration test: download -> process -> calculate workflow
testthat::test_that("download_ecoregion integration (basic)", {
  skip_on_cran()
  skip_if_offline()

  withr::with_tempdir({
    # Download ecoregion data
    result <- download_ecoregion(
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = TRUE
    )

    # Check that download succeeded
    data_dir <- "./data_files"
    testthat::expect_true(dir.exists(data_dir))

    # Check for shapefile or geopackage
    spatial_files <- list.files(
      data_dir,
      pattern = "\\.(shp|gpkg)$",
      recursive = TRUE,
      full.names = TRUE
    )
    testthat::expect_true(
      length(spatial_files) > 0,
      info = "At least one spatial file should be extracted"
    )
  })
})

# nolint end
