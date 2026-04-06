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
