################################################################################
##### unit and integration tests for USGS NHD functions

################################################################################
##### download_huc
testthat::test_that("download_huc (no errors, url discovery)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/huc/")
  allregions <- c("Lower48", "Islands")
  alltypes <- c("Seamless", "OceanCatchment")

  for (region in allregions) {
    for (type in alltypes) {
      result <- suppressWarnings(download_huc(
        region,
        type,
        directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
      ))
      testthat::expect_true(is.list(result))
      testthat::expect_true(!is.null(result$urls))
      testthat::expect_equal(result$n_files, 1)
      testthat::expect_true(grepl("^https://", result$urls))
    }
  }
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_huc deprecation warnings", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/huc_dep/")

  testthat::expect_warning(
    download_huc(
      "Lower48",
      "Seamless",
      directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    download_huc(
      "Lower48",
      "Seamless",
      directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    ),
    regexp = "remove_command.*deprecated"
  )

  testthat::expect_warning(
    download_huc(
      "Lower48",
      "Seamless",
      directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = TRUE
    ),
    regexp = "unzip.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})


testthat::test_that("download_huc mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_huc(
          region = "Lower48",
          type = "Seamless",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### process_huc
testthat::test_that("process_huc", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("nhdplusTools")
  withr::local_options(list(sf_use_s2 = FALSE))
  # Set up test data
  path <- testthat::test_path(
    "..",
    "testdata",
    "huc12",
    "NHDPlus_test.gpkg"
  )

  # Call the function
  testthat::expect_error(process_huc(path))
  testthat::expect_no_error(
    result <-
      process_huc(
        path,
        layer_name = "NHDPlus_test",
        huc_level = "HUC_12",
        huc_header = "030202"
      )
  )
  testthat::expect_true(inherits(result, "SpatVector"))

  # query case
  testthat::expect_no_error(
    result <-
      process_huc(
        path,
        layer_name = "NHDPlus_test",
        huc_level = "HUC_12",
        huc_header = "030202"
      )
  )
  testthat::expect_true(inherits(result, "SpatVector"))

  testthat::expect_error(
    process_huc(
      path,
      layer_name = "HUc",
      huc_level = "HUC_12",
      huc_header = "030202"
    )
  )

  # Set up test data
  path2 <- testthat::test_path(
    "..",
    "testdata",
    "huc12"
  )

  # Call the function and expect an error
  testthat::expect_error(process_huc(path2))

  # test with cropping extent
  testthat::expect_no_error(
    huc_ext <- process_huc(
      path,
      layer_name = "NHDPlus_test",
      huc_level = "HUC_12",
      huc_header = "030202",
      extent = terra::ext(result)
    )
  )
})


################################################################################
##### calculate_huc
testthat::test_that("calculate_huc", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("nhdplusTools")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Set up test data
  path <- testthat::test_path(
    "..",
    "testdata",
    "huc12",
    "NHDPlus_test.gpkg"
  )
  huc_vect <- process_huc(
    path,
    layer_name = "NHDPlus_test",
    huc_level = "HUC_12",
    huc_header = "030202"
  )

  # faux loc
  locs_v <- data.frame(
    site_id = c("loc1", "loc2", "loc3"),
    lon = c(-77.0365, -77.0434, -77.0283),
    lat = c(38.8977, 38.9097, 38.8895)
  )
  locs_v <-
    terra::vect(locs_v, geom = c("lon", "lat"), crs = "epsg:4326")

  # runs ok
  testthat::expect_message(
    huc_df <- calculate_huc(
      huc_vect,
      locs = locs_v,
      locs_id = "site_id"
    ),
    "Calculating HUC covariates..."
  )

  # error 1 - invalid from object
  testthat::expect_error(
    calculate_huc(from = 0, locs = locs_v, locs_id = "site_id"),
    "`from` must be the output of process_huc()."
  )
})
