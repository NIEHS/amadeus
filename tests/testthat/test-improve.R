################################################################################
##### unit and integration tests for IMPROVE (FLMA) functions
# nolint start

improve_path <- testthat::test_path("..", "testdata", "improve")

improve_calculate_fixture <- function() {
  measurements <- data.frame(
    SiteCode = c("MON_A", "MON_A", "MON_A", "MON_B", "MON_B"),
    POC = 1L,
    FactDate = as.Date(c(
      "2022-01-02",
      "2022-01-05",
      "2022-01-02",
      "2022-01-02",
      "2022-01-05"
    )),
    ParamCode = c("FPM", "FPM", "ECf", "FPM", "FPM"),
    MethodID = c(5017L, 5017L, 917L, 5017L, 5017L),
    Units = "ug/m^3",
    FactValue = c(2, 4, 0.5, 10, 12),
    Status = "V0",
    ProviderStatus = "NM",
    lon = c(0, 0, 0, 1, 1),
    lat = c(0, 0, 0, 0, 0)
  )
  terra::vect(
    measurements,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
}

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
##### calculate_improve

testthat::test_that(
  "calculate_improve(nearest_only=TRUE): returns nearest monitor records",
  {
    locs <- data.frame(
      site_id = c("near_a", "near_b"),
      lon = c(0.02, 0.98),
      lat = c(0, 0)
    )

    result <- calculate_improve(
      from = improve_calculate_fixture(),
      locs = locs,
      locs_id = "site_id",
      radius = 200000
    )

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_setequal(unique(result$site_id), locs$site_id)
    testthat::expect_setequal(
      unique(result$SiteCode[result$site_id == "near_a"]),
      "MON_A"
    )
    testthat::expect_setequal(
      unique(result$SiteCode[result$site_id == "near_b"]),
      "MON_B"
    )
    testthat::expect_equal(sum(result$site_id == "near_a"), 3L)
    testthat::expect_gt(min(result$distance_m), 2000)
    testthat::expect_lt(max(result$distance_m), 2300)
    testthat::expect_s3_class(result$FactDate, "Date")
    testthat::expect_s3_class(result$time, "POSIXct")
  }
)

testthat::test_that(
  "calculate_covariates(covariate='IMPROVE'): completes processed workflow",
  {
    from <- process_covariates(
      covariate = "improve",
      path = improve_path,
      product = "raw",
      return_format = "terra"
    )
    locs <- data.frame(
      site_id = c("near_acad", "near_bibe"),
      lon = c(-68.26, -103.18),
      lat = c(44.38, 29.30)
    )

    result <- calculate_covariates(
      covariate = "IMPROVE",
      from = from,
      locs = locs,
      locs_id = "site_id",
      radius = 100000
    )

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_setequal(unique(result$site_id), locs$site_id)
    testthat::expect_setequal(unique(result$SiteCode), c("ACAD1", "BIBE1"))
    testthat::expect_setequal(
      unique(result$ParamCode),
      c("ALf", "ECf", "FPM")
    )
    testthat::expect_lt(max(result$distance_m), 1000)
  }
)

testthat::test_that(
  "calculate_improve(product=rhr2/rhr3): preserves product parameters",
  {
    expected_parameters <- c(rhr2 = "bext", rhr3 = "dv")

    for (product in names(expected_parameters)) {
      from <- process_improve(
        path = improve_path,
        product = product,
        return_format = "terra"
      )
      result <- calculate_improve(
        from = from,
        locs = data.frame(
          site_id = "near_acad",
          lon = -68.26,
          lat = 44.38
        ),
        locs_id = "site_id",
        radius = 100000
      )

      testthat::expect_setequal(
        unique(result$ParamCode),
        unname(expected_parameters[[product]])
      )
      testthat::expect_lt(max(result$distance_m), 1000)
    }
  }
)

testthat::test_that(
  "calculate_improve(nearest_only=FALSE): returns all nearby monitors",
  {
    result <- calculate_improve(
      from = improve_calculate_fixture(),
      locs = data.frame(site_id = "mid", lon = 0.5, lat = 0),
      locs_id = "site_id",
      radius = 60000,
      nearest_only = FALSE
    )

    testthat::expect_setequal(unique(result$SiteCode), c("MON_A", "MON_B"))
    testthat::expect_false(anyNA(result$distance_m))
    testthat::expect_gt(min(result$distance_m), 55000)
    testthat::expect_lt(max(result$distance_m), 56000)
  }
)

testthat::test_that(
  "calculate_improve(radius=0): matches only co-located monitors",
  {
    testthat::expect_warning(
      result <- calculate_improve(
        from = improve_calculate_fixture(),
        locs = data.frame(
          site_id = c("exact", "offset"),
          lon = c(0, 0.01),
          lat = c(0, 0)
        ),
        locs_id = "site_id",
        radius = 0,
        .by_time = "month"
      ),
      regexp = "1 of 2"
    )

    exact <- result[result$site_id == "exact", , drop = FALSE]
    offset <- result[result$site_id == "offset", , drop = FALSE]
    testthat::expect_setequal(unique(exact$SiteCode), "MON_A")
    testthat::expect_equal(unique(exact$distance_m), 0)
    testthat::expect_equal(nrow(offset), 1L)
    testthat::expect_true(is.na(offset$SiteCode))
  }
)

testthat::test_that(
  "calculate_improve(.by_time='month'): averages values by parameter",
  {
    result <- calculate_improve(
      from = improve_calculate_fixture(),
      locs = data.frame(site_id = "query", lon = 0.01, lat = 0),
      locs_id = "site_id",
      radius = 5000,
      .by_time = "month"
    )

    fpm <- result[result$ParamCode == "FPM", , drop = FALSE]
    ecf <- result[result$ParamCode == "ECf", , drop = FALSE]
    testthat::expect_equal(nrow(fpm), 1L)
    testthat::expect_equal(fpm$FactValue, 3)
    testthat::expect_equal(ecf$FactValue, 0.5)
    testthat::expect_identical(fpm$FactDate, as.Date("2022-01-01"))
  }
)

testthat::test_that(
  "calculate_improve(radius=no matches): retains query identifiers",
  {
    testthat::expect_warning(
      result <- calculate_improve(
        from = improve_calculate_fixture(),
        locs = data.frame(site_id = "far", lon = 10, lat = 10),
        locs_id = "site_id",
        radius = 100,
        .by_time = "month"
      ),
      regexp = "1 of 1"
    )

    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_identical(result$site_id, "far")
    testthat::expect_true(is.na(result$SiteCode))
    testthat::expect_true(is.na(result$FactValue))
    testthat::expect_s3_class(result$time, "POSIXct")
  }
)

testthat::test_that(
  "calculate_improve(geom='terra'): returns query-location geometry",
  {
    result <- calculate_improve(
      from = improve_calculate_fixture(),
      locs = data.frame(site_id = "query", lon = 0.1, lat = 0.2),
      locs_id = "site_id",
      radius = 50000,
      geom = "terra"
    )

    testthat::expect_s4_class(result, "SpatVector")
    result_coords <- unique(terra::crds(result))
    testthat::expect_equal(unname(result_coords[, 1]), 0.1, tolerance = 1e-8)
    testthat::expect_equal(unname(result_coords[, 2]), 0.2, tolerance = 1e-8)
    testthat::expect_equal(unique(result$Longitude), 0)
    testthat::expect_equal(unique(result$Latitude), 0)
  }
)

testthat::test_that(
  "calculate_improve(arguments=invalid): reports actionable errors",
  {
    from <- improve_calculate_fixture()
    locs <- data.frame(site_id = "query", lon = 0.01, lat = 0)

    testthat::expect_error(
      calculate_improve(from = data.frame(), locs = locs),
      regexp = "SpatVector"
    )
    testthat::expect_error(
      calculate_improve(from = from, locs = locs, radius = -1),
      regexp = "non-negative"
    )
    testthat::expect_error(
      calculate_improve(from = from, locs = locs, nearest_only = NA),
      regexp = "TRUE or FALSE"
    )
    testthat::expect_error(
      calculate_improve(from = from, locs = locs, weights = 1),
      regexp = "not supported"
    )
    testthat::expect_error(
      calculate_improve(
        from = from,
        locs = data.frame(
          site_id = c("duplicate", "duplicate"),
          lon = c(0, 1),
          lat = c(0, 0)
        )
      ),
      regexp = "unique"
    )
  }
)

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
