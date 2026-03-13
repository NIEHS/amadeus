################################################################################
##### unit and integration tests for U.S. EPA TRI functions

################################################################################
##### download_tri
testthat::test_that("download_tri (url discovery)", {
  directory_to_save <- paste0(tempdir(), "/tri/")

  result <- suppressWarnings(download_data(
    dataset_name = "tri",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    remove_command = FALSE
  ))

  # 5 years (2018-2022) → 5 files
  testthat::expect_equal(result$n_files, 5L)
  testthat::expect_true(all(grepl("^https://", result$urls)))
  testthat::expect_true(all(grepl("tri/mv_tri_basic_download", result$urls)))
  testthat::expect_true(all(grepl("_US/csv$", result$urls)))
  testthat::expect_true(all(grepl("tri_raw_[0-9]{4}\\.csv$", result$destfiles)))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_tri (single year, url discovery)", {
  directory_to_save <- paste0(tempdir(), "/tri/")
  year <- 2019L

  result <- suppressWarnings(download_data(
    year = year,
    dataset_name = "tri",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    download = FALSE,
    remove_command = FALSE
  ))

  testthat::expect_equal(result$n_files, 1L)
  testthat::expect_true(grepl("2019", result$urls))
  testthat::expect_true(grepl("^https://", result$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_tri supports state and tribal jurisdictions", {
  directory_to_save <- paste0(tempdir(), "/tri_jurisdiction/")

  state_result <- suppressWarnings(download_data(
    year = 2024L,
    dataset_name = "tri",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    jurisdiction = "AZ",
    download = FALSE,
    remove_command = FALSE
  ))

  testthat::expect_equal(state_result$n_files, 1L)
  testthat::expect_match(state_result$urls, "2024_AZ/csv$")
  testthat::expect_match(basename(state_result$destfiles), "^tri_raw_2024_AZ\\.csv$")

  tribal_result <- suppressWarnings(download_tri(
    year = 2024L,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE,
    jurisdiction = "TBL",
    download = FALSE
  ))

  testthat::expect_equal(tribal_result$n_files, 1L)
  testthat::expect_match(tribal_result$urls, "2024_tbl/csv$")
  testthat::expect_match(
    basename(tribal_result$destfiles),
    "^tri_raw_2024_tbl\\.csv$"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_tri validates jurisdiction input", {
  withr::with_tempdir({
    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        jurisdiction = "northcarolina",
        download = FALSE
      ),
      regexp = "jurisdiction"
    )
  })
})

testthat::test_that("download_tri (LIVE - state and tribal)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  withr::with_tempdir({
    state_result <- download_tri(
      year = 2024L,
      directory_to_save = ".",
      acknowledgement = TRUE,
      jurisdiction = "AZ",
      download = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )
    tribal_result <- download_tri(
      year = 2024L,
      directory_to_save = ".",
      acknowledgement = TRUE,
      jurisdiction = "tbl",
      download = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_type(state_result, "list")
    testthat::expect_type(tribal_result, "list")

    state_file <- "tri_raw_2024_AZ.csv"
    tribal_file <- "tri_raw_2024_tbl.csv"

    testthat::expect_true(file.exists(state_file))
    testthat::expect_true(file.exists(tribal_file))
    testthat::expect_gt(file.size(state_file), 1000)
    testthat::expect_gt(file.size(tribal_file), 1000)
  })
})

testthat::test_that("download_tri deprecation warnings", {
  directory_to_save <- paste0(tempdir(), "/tri_dep/")

  testthat::expect_warning(
    download_tri(
      year = c(2020L, 2020L),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    download_tri(
      year = c(2020L, 2020L),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    ),
    regexp = "remove_command.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})


testthat::test_that("download_tri mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_tri(
          year = c(2020L, 2020L),
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
##### calculate_tri
testthat::test_that("calculate_tri", {
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
    terra::vect(ncp, geom = c("lon", "lat"), keepgeom = TRUE, crs = "EPSG:4326")
  ncpt$time <- 2018L
  path_tri <- testthat::test_path("..", "testdata", "tri")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  testthat::expect_s4_class(tri_r, "SpatVector")

  testthat::expect_no_error(
    tri_c <- calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L)
    )
  )
  testthat::expect_true(is.data.frame(tri_c))

  # with geometry terra
  testthat::expect_no_error(
    tri_c_terra <- calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L),
      geom = "terra"
    )
  )
  testthat::expect_s4_class(tri_c_terra, "SpatVector")

  # with geometry sf
  testthat::expect_no_error(
    tri_c_sf <- calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L),
      geom = "sf"
    )
  )
  testthat::expect_true("sf" %in% class(tri_c_sf))

  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L),
      geom = TRUE
    )
  )

  testthat::expect_no_error(
    calculate_tri(
      from = tri_r,
      locs = sf::st_as_sf(ncpt),
      radius = 50000L
    )
  )
  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt,
      radius = 50000L
    )
  )
  testthat::expect_error(
    calculate_tri(
      from = paste0(tdir, "/tri/"),
      locs = ncpt[, 1:2],
      radius = 50000L
    )
  )
  testthat::expect_error(
    calculate_tri(
      from = paste0(tdir, "/tri/"),
      locs = ncpt,
      radius = "As far as the Earth's radius"
    )
  )
})

################################################################################
##### download_tri hash=FALSE branch

testthat::test_that("download_tri mock download hash=FALSE", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_tri(
          year = c(2018, 2018),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = FALSE
        )
      )
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
  })
})
