################################################################################
##### unit and integration tests for IMPROVE (FLMA) functions
# nolint start

improve_path <- testthat::test_path("..", "testdata", "improve")

################################################################################
##### process_improve

testthat::test_that("process_improve_sites_builtin returns expected metadata table", {
  withr::local_package("data.table")
  sites <- process_improve_sites_builtin()

  testthat::expect_s3_class(sites, "data.table")
  testthat::expect_true(nrow(sites) > 200L)
  testthat::expect_true(all(c("SiteCode", "Latitude", "Longitude", "ProgramKey") %in% names(sites)))
  testthat::expect_equal(sum(duplicated(sites$SiteCode)), 0L)
  testthat::expect_true("IMPROVE" %in% unique(stats::na.omit(sites$ProgramKey)))
  testthat::expect_true(all(c("ACAD1", "BIBE1", "YOSE1") %in% sites$SiteCode))
})

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

testthat::test_that("process_improve falls back to data.table when coords unavailable", {
  withr::local_package("data.table")
  tmp <- withr::local_tempdir()
  file.copy(
    file.path(improve_path, "IMPAER_2022.txt"),
    file.path(tmp, "IMPAER_2022.txt")
  )
  writeLines("SiteCode|Name\nMEF|Moosehorn", file.path(tmp, "bad_sites.txt"))
  testthat::expect_warning(
    result <- process_improve(
      path = tmp,
      product = "raw",
      sites_file = file.path(tmp, "bad_sites.txt"),
      return_format = "terra"
    ),
    regexp = "No site coordinates available"
  )
  testthat::expect_s3_class(result, "data.table")
})

testthat::test_that("process_improve uses embedded metadata when sites file missing", {
  withr::local_package("data.table")
  withr::local_package("terra")
  tmp <- withr::local_tempdir()
  # measurement file without local sites file should still gain coords
  file.copy(
    file.path(improve_path, "IMPAER_2022.txt"),
    file.path(tmp, "IMPAER_2022.txt")
  )
  result <- process_improve(
    path = tmp,
    product = "raw",
    return_format = "terra"
  )
  testthat::expect_s4_class(result, "SpatVector")
  testthat::expect_true(terra::nrow(result) > 0L)
})

testthat::test_that("download_improve deprecated params warn", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0, skipped = 0),
    .package = "amadeus"
  )
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
  result <- download_improve(
    year = 2022,
    product = "raw",
    directory_to_save = tmp,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result) || is.null(result))
})

testthat::test_that("process_improve single-date string expands correctly", {
  withr::local_package("data.table")
  result_single <- process_improve(
    path = improve_path,
    product = "raw",
    date = "2022-01-02",
    return_format = "data.table"
  )
  result_pair <- process_improve(
    path = improve_path,
    product = "raw",
    date = c("2022-01-02", "2022-01-02"),
    return_format = "data.table"
  )
  testthat::expect_equal(nrow(result_single), nrow(result_pair))
})

testthat::test_that("download_improve returns hash when files present and hash=TRUE", {
  tmp <- withr::local_tempdir()
  writeLines("x", file.path(tmp, "IMPAER_2022.txt"))
  testthat::local_mocked_bindings(
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  result <- download_improve(
    year = 2022,
    product = "raw",
    directory_to_save = tmp,
    acknowledgement = TRUE,
    hash = TRUE
  )
  testthat::expect_equal(result, "fakehash")
})

testthat::test_that("download_improve hash=TRUE returns hash after download", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) list(success = 1, failed = 0),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- download_improve(
      year = 2022,
      product = "raw",
      directory_to_save = ".",
      acknowledgement = TRUE,
      hash = TRUE
    )
    testthat::expect_equal(result, "fakehash")
  })
})

testthat::test_that("download_improve hash=FALSE returns download_result", {
  captured <- NULL
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, ...) {
      captured <<- list(urls = urls, destfiles = destfiles)
      list(success = 1, failed = 0, skipped = 0)
    },
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- download_improve(
      year = 2022,
      product = "raw",
      directory_to_save = ".",
      acknowledgement = TRUE,
      hash = FALSE
    )
    testthat::expect_type(result, "list")
    testthat::expect_equal(result$success, 1)
    testthat::expect_true(grepl(
      "^https://vibe\\.cira\\.colostate\\.edu/data/export/IMPAER/IMPAER_2022\\.txt\\.zip$",
      captured$urls[1]
    ))
    testthat::expect_true(grepl("IMPAER_2022\\.txt\\.zip$", captured$destfiles[1]))
  })
})

testthat::test_that(
  "calculate_improve(site_code=monitor): preserves observations and target order",
  {
    source <- process_improve(improve_path, return_format = "data.table")
    before <- data.table::copy(source)
    targets <- data.frame(id = c("009", "001", "005", "003"),
                          monitor = c("BIBE1", "ACAD1", "NONE", "BIBE1"))
    result <- calculate_improve(source, targets, "id", "monitor")
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_identical(unique(result$id), targets$id)
    testthat::expect_equal(result$FactValue[result$id == "009"],
                          source$FactValue[source$SiteCode == "BIBE1"])
    testthat::expect_equal(result$FactValue[result$id == "003"],
                          source$FactValue[source$SiteCode == "BIBE1"])
    testthat::expect_equal(result$FactValue[result$id == "001" &
                            result$ParamCode == "FPM" &
                            !is.na(result$ParamCode)], c(2.85, 3.12))
    testthat::expect_equal(sum(result$id == "005"), 1L)
    testthat::expect_identical(result$SiteCode[result$id == "005"], "NONE")
    testthat::expect_identical(result$FactValue[result$id == "005"], NA_real_)
    testthat::expect_s3_class(result$FactDate, "Date")
    testthat::expect_identical(source, before)
  }
)

testthat::test_that(
  "calculate_covariates(covariate=IMPROVE): accepts every processed format/product",
  {
    targets <- data.frame(site_id = "001", SiteCode = "ACAD1")
    for (product in c("raw", "rhr2", "rhr3")) {
      for (format in c("data.table", "sf", "terra")) {
        source <- process_covariates("improve", path = improve_path,
                                    product = product, return_format = format)
        result <- calculate_covariates("IMPROVE", source, targets)
        expected <- switch(product, raw = c(0.00044, 0.03816, 2.85,
                                           0.00062, 0.04201, 3.12),
                           rhr2 = c(12.3, 14.7), rhr3 = c(1.52, 1.73))
        testthat::expect_equal(result$FactValue, expected)
        testthat::expect_identical(unique(result$site_id), "001")
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(geom=sf/terra): repeats target geometry and preserves CRS",
  {
    source <- process_improve(improve_path, return_format = "sf")
    targets <- sf::st_as_sf(
      data.frame(id = c("007", "002"), SiteCode = c("BIBE1", "ACAD1"),
                 x = c(-100, -70), y = c(30, 40)),
      coords = c("x", "y"), crs = 4326
    )
    targets <- sf::st_transform(targets, 3857)
    result <- calculate_improve(source, targets, "id", geom = "sf")
    testthat::expect_s3_class(result, "sf")
    testthat::expect_identical(sf::st_crs(result), sf::st_crs(targets))
    testthat::expect_identical(sf::st_geometry(result),
                              sf::st_geometry(targets)[match(result$id,
                                                            targets$id)])
    vector <- calculate_improve(source, terra::vect(targets), "id",
                                geom = "terra")
    testthat::expect_s4_class(vector, "SpatVector")
    testthat::expect_equal(as.data.frame(vector)$id, result$id)
  }
)

testthat::test_that(
  "calculate_improve(from=duplicates/missing): retains flags, zeros and rows",
  {
    source <- data.frame(SiteCode = c("A", "A", "A", NA),
                         FactDate = as.Date("2022-01-02"), ParamCode = "x",
                         FactValue = c(0, NA, 0, 99), Units = "u",
                         Status = c("V0", "invalid", "V0", "V0"))
    targets <- data.frame(site_id = c("01", "02"), SiteCode = c("A", NA))
    result <- calculate_improve(source, targets)
    testthat::expect_equal(result$FactValue, c(0, NA, 0, NA))
    testthat::expect_equal(result$Status, c("V0", "invalid", "V0", NA))
    testthat::expect_equal(nrow(result), 4L)
    testthat::expect_equal(nrow(calculate_improve(source[FALSE, ], targets)), 2L)
    testthat::expect_equal(nrow(calculate_improve(source, targets[FALSE, ])), 0L)
    codes <- data.frame(SiteCode = "A")
    testthat::expect_equal(nrow(calculate_improve(source, codes, "SiteCode")), 3L)
  }
)

testthat::test_that(
  "calculate_improve(invalid inputs): rejects ambiguity and unsupported options",
  {
    source <- process_improve(improve_path, return_format = "data.table")
    targets <- data.frame(site_id = "001", SiteCode = "ACAD1")
    testthat::expect_error(calculate_improve(source, rbind(targets, targets)),
                           "unique and nonmissing")
    testthat::expect_error(calculate_improve(source, targets, "missing"),
                           "must contain")
    testthat::expect_error(calculate_improve(source, targets, geom = "sf"),
                           "Spatial")
    testthat::expect_error(calculate_improve(source, targets, radius = 100),
                           "Unused")
    testthat::expect_error(calculate_improve(source, targets, .by_time = "month"),
                           "requires")
    testthat::expect_error(calculate_covariates("improve", source, targets,
                                               weights = 1), "requires")
    testthat::expect_error(calculate_improve(data.frame(x = 1), targets),
                           "must contain")
    targets$FactValue <- targets$site_id
    testthat::expect_error(calculate_improve(source, targets, "FactValue"),
                           "conflicts")
  }
)
