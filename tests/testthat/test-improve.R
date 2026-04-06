################################################################################
##### unit and integration tests for IMPROVE (FLMA) functions
# nolint start

improve_path <- testthat::test_path("..", "testdata", "improve")

################################################################################
##### process_improve

testthat::test_that("process_improve raw returns data.table", {
  withr::local_package("data.table")
  result <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "data.table"
  )
  testthat::expect_s3_class(result, "data.table")
  testthat::expect_true("SiteCode" %in% names(result))
  testthat::expect_true("FactDate" %in% names(result))
  testthat::expect_true("ParamCode" %in% names(result))
  testthat::expect_true("FactValue" %in% names(result))
  testthat::expect_true(nrow(result) > 0L)
})

testthat::test_that("process_improve rhr2 returns data.table with bext", {
  withr::local_package("data.table")
  result <- process_improve(
    path = improve_path,
    product = "rhr2",
    return_format = "data.table"
  )
  testthat::expect_s3_class(result, "data.table")
  testthat::expect_true("ParamCode" %in% names(result))
  bext_rows <- result[result$ParamCode == "bext", ]
  testthat::expect_true(nrow(bext_rows) > 0L)
})

testthat::test_that("process_improve rhr3 returns data.table with dv", {
  withr::local_package("data.table")
  result <- process_improve(
    path = improve_path,
    product = "rhr3",
    return_format = "data.table"
  )
  testthat::expect_s3_class(result, "data.table")
  dv_rows <- result[result$ParamCode == "dv", ]
  testthat::expect_true(nrow(dv_rows) > 0L)
})

testthat::test_that("process_improve returns terra SpatVector with coords", {
  withr::local_package("terra")
  withr::local_package("data.table")
  result <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  testthat::expect_s4_class(result, "SpatVector")
  testthat::expect_true(nrow(result) > 0L)
  testthat::expect_true("SiteCode" %in% names(result))
})

testthat::test_that("process_improve returns sf object", {
  withr::local_package("sf")
  withr::local_package("data.table")
  result <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "sf"
  )
  testthat::expect_s3_class(result, "sf")
  testthat::expect_true(nrow(result) > 0L)
})

testthat::test_that("process_improve date filter works", {
  withr::local_package("data.table")
  result_full <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "data.table"
  )
  result_filt <- process_improve(
    path = improve_path,
    product = "raw",
    date = c("2022-01-02", "2022-01-02"),
    return_format = "data.table"
  )
  testthat::expect_true(nrow(result_filt) < nrow(result_full))
  testthat::expect_true(all(result_filt$FactDate == as.Date("2022-01-02")))
})

testthat::test_that("process_improve errors on invalid path", {
  testthat::expect_error(
    process_improve(path = "/nonexistent_dir_xyz"),
    regexp = "valid directory"
  )
})

testthat::test_that("process_improve errors when no matching files", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(
    process_improve(path = tmp, product = "raw"),
    regexp = "No IMPAER_YYYY.txt files"
  )
})

################################################################################
##### process_covariates dispatch for improve

testthat::test_that("process_covariates dispatches to process_improve", {
  withr::local_package("data.table")
  result <- process_covariates(
    covariate = "improve",
    path = improve_path,
    product = "raw",
    return_format = "data.table"
  )
  testthat::expect_s3_class(result, "data.table")
})

testthat::test_that("process_covariates dispatches IMPROVE uppercase", {
  withr::local_package("data.table")
  result <- process_covariates(
    covariate = "IMPROVE",
    path = improve_path,
    product = "rhr2",
    return_format = "data.table"
  )
  testthat::expect_s3_class(result, "data.table")
})

################################################################################
##### calculate_improve

testthat::test_that("calculate_improve raw returns joined data.frame", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = c("NEAR_ACAD", "NEAR_BIBE"),
    lon = c(-68.26,  -103.18),
    lat = c( 44.38,    29.30)
  )
  result <- calculate_improve(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 100000
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("site_id" %in% names(result))
  testthat::expect_true("SiteCode" %in% names(result))
  testthat::expect_true(nrow(result) > 0L)
})

testthat::test_that("calculate_improve rhr2 works", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "rhr2",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "LOC1",
    lon = -68.26,
    lat =  44.38
  )
  result <- calculate_improve(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 200000
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0L)
  testthat::expect_true("ParamCode" %in% names(result))
  testthat::expect_true(any(result$ParamCode == "bext"))
})

testthat::test_that("calculate_improve rhr3 works", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "rhr3",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "LOC1",
    lon = -68.26,
    lat =  44.38
  )
  result <- calculate_improve(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 200000
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0L)
  testthat::expect_true("ParamCode" %in% names(result))
  testthat::expect_true(any(result$ParamCode == "dv"))
})

testthat::test_that("calculate_improve warns when no monitors in radius", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "FAR",
    lon = 10.0,
    lat = 50.0
  )
  testthat::expect_warning(
    result <- calculate_improve(
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 1000
    ),
    regexp = "No IMPROVE monitors found"
  )
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("calculate_improve errors on bad locs_id", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "S1", lon = -68.26, lat = 44.38
  )
  testthat::expect_error(
    calculate_improve(from = from, locs = locs, locs_id = "bad_col"),
    regexp = "not found in"
  )
})

testthat::test_that("calculate_improve errors on NULL from", {
  locs <- data.frame(site_id = "S1", lon = -68.26, lat = 44.38)
  testthat::expect_error(
    calculate_improve(from = NULL, locs = locs),
    regexp = "SpatVector"
  )
})

testthat::test_that("calculate_improve errors on non-SpatVector from", {
  locs <- data.frame(site_id = "S1", lon = -68.26, lat = 44.38)
  testthat::expect_error(
    calculate_improve(from = data.frame(a = 1), locs = locs),
    regexp = "must be a SpatVector"
  )
})

testthat::test_that("calculate_improve accepts sf locs", {
  withr::local_package("sf")
  withr::local_package("terra")
  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs_sf <- sf::st_as_sf(
    data.frame(site_id = "S1", lon = -68.26, lat = 44.38),
    coords = c("lon", "lat"),
    crs = 4326
  )
  result <- calculate_improve(
    from = from,
    locs = locs_sf,
    locs_id = "site_id",
    radius = 200000
  )
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0L)
})

testthat::test_that("calculate_improve errors when data.frame locs lacks lon/lat", {
  withr::local_package("terra")
  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  bad_locs <- data.frame(site_id = "S1", note = "not_a_point")
  testthat::expect_error(
    calculate_improve(from = from, locs = bad_locs, locs_id = "site_id"),
    regexp = "could not be converted"
  )
})

testthat::test_that("calculate_improve errors for unsupported locs class", {
  withr::local_package("terra")
  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  testthat::expect_error(
    calculate_improve(from = from, locs = list(site_id = "S1"), locs_id = "site_id"),
    regexp = "must be a data.frame, SpatVector, or sf"
  )
})

testthat::test_that("calculate_improve returns geometry objects when requested", {
  withr::local_package("terra")
  withr::local_package("sf")
  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "S1",
    lon = -68.26,
    lat = 44.38
  )
  terra_res <- calculate_improve(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 200000,
    geom = "terra"
  )
  sf_res <- calculate_improve(
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 200000,
    geom = "sf"
  )
  testthat::expect_s4_class(terra_res, "SpatVector")
  testthat::expect_true("sf" %in% class(sf_res))
})

testthat::test_that("calculate_improve nearest_only=FALSE returns more rows", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "MID",
    lon = -85.0,
    lat =  37.0
  )
  r_near <- calculate_improve(
    from = from, locs = locs, locs_id = "site_id",
    radius = 5000000, nearest_only = TRUE
  )
  r_all <- calculate_improve(
    from = from, locs = locs, locs_id = "site_id",
    radius = 5000000, nearest_only = FALSE
  )
  testthat::expect_true(nrow(r_all) >= nrow(r_near))
})

testthat::test_that("calculate_covariates dispatches to calculate_improve", {
  withr::local_package("terra")
  withr::local_package("data.table")

  from <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  locs <- data.frame(
    site_id = "S1",
    lon = -68.26,
    lat =  44.38
  )
  result <- calculate_covariates(
    covariate = "improve",
    from = from,
    locs = locs,
    locs_id = "site_id",
    radius = 200000
  )
  testthat::expect_s3_class(result, "data.frame")
})

################################################################################
##### download_improve (arg-validation only — no network)

testthat::test_that("download_improve requires acknowledgement", {
  testthat::expect_error(
    download_improve(
      year = 2022,
      product = "raw",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = FALSE
    )
  )
})

testthat::test_that("download_improve errors on invalid product", {
  testthat::expect_error(
    download_improve(
      year = 2022,
      product = "invalid",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = "should be one of"
  )
})

testthat::test_that("download_improve errors on null directory", {
  testthat::expect_error(
    download_improve(
      year = 2022,
      product = "raw",
      directory_to_save = NULL,
      acknowledgement = TRUE
    )
  )
})
# nolint end

################################################################################
##### Additional branch coverage tests

testthat::test_that("process_improve warns on empty date range", {
  withr::local_package("data.table")
  testthat::expect_warning(
    result <- process_improve(
      path = improve_path,
      product = "raw",
      date = c("1900-01-01", "1900-01-01"),
      return_format = "data.table"
    ),
    regexp = "No IMPROVE measurements"
  )
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("process_improve extent crop reduces rows", {
  withr::local_package("terra")
  withr::local_package("data.table")
  full <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra"
  )
  small_extent <- terra::ext(-70, -67, 43, 46)
  cropped <- process_improve(
    path = improve_path,
    product = "raw",
    return_format = "terra",
    extent = small_extent
  )
  testthat::expect_true(terra::nrow(cropped) <= terra::nrow(full))
})

testthat::test_that("process_improve warns when sites file missing coords", {
  withr::local_package("data.table")
  tmp <- withr::local_tempdir()
  # Copy measurement file into tmp
  file.copy(
    file.path(improve_path, "IMPAER_2022.txt"),
    file.path(tmp, "IMPAER_2022.txt")
  )
  # Create a sites file missing Latitude/Longitude
  writeLines("SiteCode|Name\nMEF|Moosehorn", file.path(tmp, "bad_sites.txt"))
  testthat::expect_warning(
    result <- process_improve(
      path = tmp,
      product = "raw",
      sites_file = file.path(tmp, "bad_sites.txt"),
      return_format = "data.table"
    ),
    regexp = "Latitude"
  )
  testthat::expect_s3_class(result, "data.table")
})

testthat::test_that("process_improve warns when no coords and format != data.table", {
  withr::local_package("data.table")
  tmp <- withr::local_tempdir()
  # measurement file without sites (so no coords)
  file.copy(
    file.path(improve_path, "IMPAER_2022.txt"),
    file.path(tmp, "IMPAER_2022.txt")
  )
  testthat::expect_warning(
    result <- process_improve(
      path = tmp,
      product = "raw",
      return_format = "terra"
    ),
    regexp = "No site coordinates"
  )
  testthat::expect_s3_class(result, "data.table")
})

testthat::test_that("download_improve deprecated params warn", {
  testthat::expect_warning(
    tryCatch(
      download_improve(
        year = 2022,
        product = "raw",
        directory_to_save = withr::local_tempdir(),
        acknowledgement = TRUE,
        download = FALSE
      ),
      error = function(e) NULL
    ),
    regexp = "deprecated"
  )
  testthat::expect_warning(
    tryCatch(
      download_improve(
        year = 2022,
        product = "raw",
        directory_to_save = withr::local_tempdir(),
        acknowledgement = TRUE,
        remove_command = TRUE
      ),
      error = function(e) NULL
    ),
    regexp = "deprecated"
  )
})

testthat::test_that("download_improve returns early when files present", {
  tmp <- withr::local_tempdir()
  # pre-create the expected file so check_destfile returns FALSE
  writeLines("x", file.path(tmp, "IMPAER_2022.txt"))
  writeLines("x", file.path(tmp, "improve_sites.txt"))
  result <- download_improve(
    year = 2022,
    product = "raw",
    directory_to_save = tmp,
    acknowledgement = TRUE,
    include_sites = TRUE
  )
  testthat::expect_true(is.list(result) || is.null(result))
})
