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
  testthat::expect_match(
    basename(state_result$destfiles),
    "^tri_raw_2024_AZ\\.csv$"
  )

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

testthat::test_that("download_tri rejects empty, NA, and non-scalar jurisdictions", {
  withr::with_tempdir({
    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        jurisdiction = "",
        download = FALSE
      ),
      regexp = "jurisdiction"
    )
    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        jurisdiction = NA_character_,
        download = FALSE
      ),
      regexp = "jurisdiction"
    )
    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        jurisdiction = c("US", "AZ"),
        download = FALSE
      ),
      regexp = "jurisdiction"
    )
    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = ".",
        acknowledgement = TRUE,
        jurisdiction = 42,
        download = FALSE
      ),
      regexp = "jurisdiction"
    )
  })
})

testthat::test_that("download_tri normalizes jurisdiction and filters existing files", {
  download_urls_seen <- NULL
  destfiles_seen <- NULL

  testthat::local_mocked_bindings(
    check_destfile = function(path) !grepl("2021_NC", path),
    download_run_method = function(urls, destfiles, ...) {
      download_urls_seen <<- urls
      destfiles_seen <<- destfiles
      list(success = length(urls), failed = 0)
    },
    .package = "amadeus"
  )

  withr::with_tempdir({
    result <- suppressMessages(download_tri(
      year = c(2020L, 2021L),
      directory_to_save = ".",
      acknowledgement = TRUE,
      jurisdiction = " nc ",
      download = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    ))

    testthat::expect_equal(result$success, 1L)
    testthat::expect_length(download_urls_seen, 1L)
    testthat::expect_match(download_urls_seen, "2020_NC/csv$")
    testthat::expect_match(
      basename(destfiles_seen),
      "^tri_raw_2020_NC\\.csv$"
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
##### get_tri_info helper
testthat::test_that("get_tri_info returns TRI lookup tables", {
  path_tri <- testthat::test_path("..", "testdata", "tri_small")

  chems <- get_tri_info(path = path_tri)
  testthat::expect_s3_class(chems, "data.frame")
  testthat::expect_true(
    all(c("TRI_CHEMICAL_COMPOUND_ID", "CHEMICAL", "CASN") %in% names(chems))
  )
  testthat::expect_true(any(chems$CHEMICAL == "BENZENE"))
  testthat::expect_true(any(chems$CASN == "108-88-3"))

  chems_2019 <- get_tri_info(path = path_tri, year = 2019)
  testthat::expect_true(all(chems_2019$CHEMICAL == "BENZENE"))

  inds <- get_tri_info(path = path_tri, type = "industries")
  testthat::expect_s3_class(inds, "data.frame")
  testthat::expect_true(
    all(c("INDUSTRY_SECTOR_CODE", "INDUSTRY_SECTOR") %in% names(inds))
  )
  testthat::expect_true(any(inds$INDUSTRY_SECTOR_CODE == "324"))
  testthat::expect_true(any(inds$INDUSTRY_SECTOR == "CHEMICAL MANUFACTURING"))

  testthat::expect_error(
    get_tri_info(path = path_tri, year = c(2018, 2019)),
    regexp = "`year`"
  )
  testthat::expect_error(
    get_tri_info(path = path_tri, type = "industries", year = NA_real_),
    regexp = "`year`"
  )
})

################################################################################
##### process_tri
testthat::test_that("process_tri", {
  withr::local_package("terra")
  path_tri <- testthat::test_path("..", "testdata", "tri_small")

  testthat::expect_no_error(
    tri_r <- process_tri(path = path_tri, year = 2018)
  )
  testthat::expect_s4_class(tri_r, "SpatVector")
  testthat::expect_true("STACK_AIR_100" %in% names(tri_r))
  testthat::expect_true("STACK_AIR_200" %in% names(tri_r))
  testthat::expect_false(any(grepl("^FUGITIVE_AIR_", names(tri_r))))
  testthat::expect_equal(attr(tri_r, "tri_variables"), "STACK_AIR")
  testthat::expect_true(
    identical(attr(tri_r, "tri_target_fields"), c("STACK_AIR_100", "STACK_AIR_200"))
  )

  testthat::expect_no_error(
    tri_multi <- process_tri(
      path = path_tri,
      year = 2018,
      variables = c("STACK_AIR", "FUGITIVE_AIR")
    )
  )
  testthat::expect_true(any(grepl("^STACK_AIR_", names(tri_multi))))
  testthat::expect_true(any(grepl("^FUGITIVE_AIR_", names(tri_multi))))

  testthat::expect_no_error(
    tri_benzene <- process_tri(
      path = path_tri,
      year = 2018,
      variables = "STACK_AIR",
      chemical = "benzene"
    )
  )
  testthat::expect_true(all(grepl("_100$", attr(tri_benzene, "tri_target_fields"))))

  testthat::expect_no_error(
    tri_cas <- process_tri(
      path = path_tri,
      year = 2018,
      variables = "STACK_AIR",
      chemical = "108-88-3"
    )
  )
  testthat::expect_true(all(grepl("_200$", attr(tri_cas, "tri_target_fields"))))

  testthat::expect_no_error(
    tri_sector <- process_tri(
      path = path_tri,
      year = 2018,
      variables = "STACK_AIR",
      industry_group = "industry_sector_code"
    )
  )
  testthat::expect_true("STACK_AIR_325_100" %in% names(tri_sector))
  testthat::expect_true("STACK_AIR_324_200" %in% names(tri_sector))

  testthat::expect_no_error(
    tri_sector_both <- process_tri(
      path = path_tri,
      year = 2018,
      variables = "STACK_AIR",
      industry_group = "both"
    )
  )
  testthat::expect_true(
    "STACK_AIR_325_CHEMICAL_MANUFACTURING_100" %in% names(tri_sector_both)
  )

  # test with cropping extent
  testthat::expect_no_error(
    tri_r_ext <- process_tri(
      path = path_tri,
      year = 2018,
      extent = terra::ext(tri_r)
    )
  )
  testthat::expect_s4_class(tri_r, "SpatVector")

  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = 49
    ),
    regexp = "`variables`"
  )
  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = "STACK_AIR",
      chemical = "this-will-not-match"
    ),
    regexp = "`chemical` did not match"
  )
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

  ncp <- data.frame(lon = c(-78.8277, -79.0000), lat = c(35.95013, 36.10000))
  ncp$site_id <- c("3799900018810101", "3799900018819999")
  ncp$time <- 2018L
  ncpt <-
    terra::vect(ncp, geom = c("lon", "lat"), keepgeom = TRUE, crs = "EPSG:4326")
  ncpt$time <- 2018L
  path_tri <- testthat::test_path("..", "testdata", "tri_small")

  testthat::expect_no_error(
    tri_r <- process_tri(
      path = path_tri,
      year = 2018,
      variables = c("STACK_AIR", "WATER"),
      chemical = "benzene|toluene"
    )
  )
  testthat::expect_s4_class(tri_r, "SpatVector")
  testthat::expect_true(any(grepl("^WATER_", names(tri_r))))

  testthat::expect_no_error(
    tri_c <- calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = c(1500L, 50000L)
    )
  )
  testthat::expect_true(is.data.frame(tri_c))
  testthat::expect_true(any(grepl("STACK_AIR_", names(tri_c))))
  testthat::expect_true(any(grepl("WATER_", names(tri_c))))

  attr(tri_r, "tri_target_fields") <- NULL
  testthat::expect_no_error(
    tri_c_fallback <- calculate_tri(
      from = tri_r,
      locs = ncpt,
      radius = 50000L
    )
  )
  testthat::expect_true(any(grepl("WATER_", names(tri_c_fallback))))

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
      from = tempdir(),
      locs = ncpt[, 1:2],
      radius = 50000L
    )
  )
  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
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
