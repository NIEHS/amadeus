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
  testthat::expect_equal(
    colnames(ecor_res)[-(1:2)],
    c("DUM_E2083_0_00000", "DUM_E3064_0_00000")
  )

  testthat::expect_no_error(
    ecor_res_full <- calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      colnames = "full_ecoregion"
    )
  )
  testthat::expect_equal(
    colnames(ecor_res_full)[-(1:2)],
    c(
      "DUM_E2_SOUTHEASTERN_USA_PLAINS_0_00000",
      "DUM_E3_NORTHERN_PIEDMONT_0_00000"
    )
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
      colnames = "unsupported"
    )
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

  site_base <- sf::st_drop_geometry(sf::st_as_sf(site_faux))
  site_combined <- rbind(site_base, site_unmatched)
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

testthat::test_that("calculate_ecoregion full_ecoregion falls back to NA_L3NAME", {
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
  testthat::expect_true(
    all(c("NA_L2NAME", "US_L3NAME", "NA_L3NAME") %in% names(erras))
  )

  erras_no_us <- erras[, setdiff(names(erras), "US_L3NAME")]

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
    ecor_res <- calculate_ecoregion(
      from = erras_no_us,
      locs = site_faux,
      locs_id = "site_id",
      colnames = "full_ecoregion"
    )
  )
  testthat::expect_equal(
    colnames(ecor_res)[-(1:2)],
    c(
      "DUM_E2_SOUTHEASTERN_USA_PLAINS_0_00000",
      "DUM_E3_NORTHERN_PIEDMONT_0_00000"
    )
  )
})

testthat::test_that("calculate_ecoregion full_ecoregion sanitizes missing names", {
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
  erras$NA_L2NAME[erras$L2_KEY == "8.3  SOUTHEASTERN USA PLAINS"] <- NA_character_
  erras$US_L3NAME[erras$L3_KEY == "64  Northern Piedmont"] <- "Northern/Piedmont"

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
    ecor_res <- calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = "site_id",
      colnames = "full_ecoregion"
    )
  )
  testthat::expect_equal(
    colnames(ecor_res)[-(1:2)],
    c(
      "DUM_E2_UNKNOWN_0_00000",
      "DUM_E3_NORTHERN_PIEDMONT_0_00000"
    )
  )
})

testthat::test_that("calculate_ecoregion full_ecoregion disambiguates duplicates", {
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

  unique_idx <- !duplicated(erras$L3_KEY)
  erras_unique <- erras[unique_idx, ]
  erras_unique$US_L3NAME[1:2] <- "Duplicate Name"

  locs <- sf::st_point_on_surface(sf::st_as_sf(erras_unique[1:2, ]))
  locs <- terra::vect(locs)
  locs$site_id <- c("dup_1", "dup_2")

  testthat::expect_no_error(
    ecor_res <- calculate_ecoregion(
      from = erras_unique,
      locs = locs,
      locs_id = "site_id",
      colnames = "full_ecoregion"
    )
  )
  testthat::expect_true(
    all(c(
      "DUM_E3_DUPLICATE_NAME_0_00000",
      "DUM_E3_DUPLICATE_NAME_0_00000_1"
    ) %in% colnames(ecor_res))
  )
})

testthat::test_that("calc_return_locs covers geometry return branches", {
  withr::local_package("terra")
  withr::local_package("sf")

  empty_wkt <- data.frame(
    site_id = character(),
    geometry = character(),
    stringsAsFactors = FALSE
  )
  empty_xy <- data.frame(
    site_id = character(),
    lon = numeric(),
    lat = numeric(),
    stringsAsFactors = FALSE
  )
  empty_plain <- data.frame(site_id = character(), stringsAsFactors = FALSE)

  testthat::expect_s3_class(
    amadeus:::calc_return_locs(
      covar = empty_wkt,
      POSIXt = FALSE,
      geom = "sf",
      crs = "EPSG:4326"
    ),
    "sf"
  )
  testthat::expect_s4_class(
    amadeus:::calc_return_locs(
      covar = empty_xy,
      POSIXt = FALSE,
      geom = "terra",
      crs = "EPSG:4326"
    ),
    "SpatVector"
  )
  testthat::expect_warning(
    empty_plain_out <- amadeus:::calc_return_locs(
      covar = empty_plain,
      POSIXt = FALSE,
      geom = "sf",
      crs = "EPSG:4326"
    ),
    "no geometry columns were found"
  )
  testthat::expect_s3_class(empty_plain_out, "data.frame")

  wkt_df <- data.frame(
    site_id = "site_1",
    geometry = "POINT(-78 35)",
    stringsAsFactors = FALSE
  )
  xy_df <- data.frame(
    site_id = "site_2",
    lon = -78,
    lat = 35,
    stringsAsFactors = FALSE
  )
  plain_df <- data.frame(site_id = "site_3", stringsAsFactors = FALSE)

  testthat::expect_s4_class(
    amadeus:::calc_return_locs(
      covar = wkt_df,
      POSIXt = FALSE,
      geom = "terra",
      crs = "EPSG:4326"
    ),
    "SpatVector"
  )
  testthat::expect_s3_class(
    amadeus:::calc_return_locs(
      covar = xy_df,
      POSIXt = FALSE,
      geom = "sf",
      crs = "EPSG:4326"
    ),
    "sf"
  )
  testthat::expect_warning(
    plain_out <- amadeus:::calc_return_locs(
      covar = plain_df,
      POSIXt = FALSE,
      geom = "terra",
      crs = "EPSG:4326"
    ),
    "no geometry columns were found"
  )
  testthat::expect_s3_class(plain_out, "data.frame")
})

testthat::test_that("download_ecoregion remove_command deprecation warning", {
  withr::with_tempdir({
    testthat::expect_warning(
        download_ecoregion(
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = FALSE,
          remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_ecoregion mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_ecoregion(
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_ecoregion mock download hash = FALSE", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_ecoregion(
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      )
    )
    testthat::expect_null(result)
  })
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

################################################################################
##### download_ecoregion file-already-exists branch

testthat::test_that("download_ecoregion file already exists path", {
  testthat::local_mocked_bindings(
    check_destfile = function(...) FALSE,
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    msgs <- character(0)
    withCallingHandlers(
      suppressWarnings(
        download_ecoregion(
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    testthat::expect_true(any(grepl("already exists", msgs)))
  })
})

################################################################################
##### calculate_ecoregion missing field coverage

testthat::test_that("calculate_ecoregion errors when required field missing from intersection", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_eco <- testthat::test_path(
    "..",
    "testdata",
    "ecoregions",
    "eco_l3_clip.gpkg"
  )
  erras <- process_ecoregion(path_eco)

  site_faux <- data.frame(
    site_id = "37999109988101",
    lon = -77.576,
    lat = 39.40
  )
  site_vect <- terra::vect(site_faux, geom = c("lon", "lat"), crs = "EPSG:4326")

  # Mock terra::intersect to return a SpatVector without L2_KEY / L3_KEY fields
  testthat::local_mocked_bindings(
    intersect = function(x, y) {
      sf_obj <- sf::st_as_sf(
        data.frame(site_id = "37999109988101", some_other_field = 1L,
                   lon = -77.576, lat = 39.40),
        coords = c("lon", "lat"), crs = 4326
      )
      terra::vect(sf_obj)
    },
    .package = "terra"
  )
  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_vect,
      locs_id = "site_id"
    ),
    "Required ecoregion field missing"
  )
})
