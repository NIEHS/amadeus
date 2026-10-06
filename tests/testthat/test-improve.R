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

################################################################################
# Calculate observations using the real processor and existing fixtures.

testthat::test_that(
  "calculate_improve(from=processed): preserves all products and formats",
  {
    sites <- data.frame(id = c("002", "001"), SiteCode = c("BIBE1", "ACAD1"))
    for (product in c("raw", "rhr2", "rhr3")) {
      for (format in c("data.table", "sf", "terra")) {
        processed <- process_covariates(
          "improve", path = improve_path, product = product,
          return_format = format
        )
        source <- if (inherits(processed, "sf")) {
          as.data.frame(sf::st_drop_geometry(processed))
        } else {
          as.data.frame(processed)
        }
        expected <- rbind(
          source[source$SiteCode == "BIBE1", , drop = FALSE],
          source[source$SiteCode == "ACAD1", , drop = FALSE]
        )
        rownames(expected) <- NULL
        out <- calculate_covariates(
          "IMPROVE", from = processed, locs = sites, locs_id = "id"
        )
        testthat::expect_identical(class(out), "data.frame")
        testthat::expect_equal(out[names(expected)], expected)
        testthat::expect_identical(
          out$id, sites$id[match(out$SiteCode, sites$SiteCode)]
        )
        testthat::expect_equal(as.Date(out$time), as.Date(out$FactDate))
        testthat::expect_s3_class(out$time, "POSIXct")
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(locs_id='SiteCode'): retains repeated observations",
  {
    source <- data.frame(
      SiteCode = c("001", "001", "002"),
      FactDate = as.Date(c("2022-01-01", "2022-01-01", "2022-02-01")),
      ParamCode = c("FPM", "ECf", "FPM"),
      FactValue = c(0, NA_real_, -999), Units = "ug/m^3"
    )
    sites <- data.frame(SiteCode = c("002", "001"))
    out <- calculate_improve(source, sites, locs_id = "SiteCode")
    testthat::expect_identical(out$SiteCode, c("002", "001", "001"))
    testthat::expect_identical(out$FactValue, c(-999, 0, NA_real_))
    testthat::expect_identical(out$ParamCode, c("FPM", "FPM", "ECf"))
    testthat::expect_identical(out$Units, rep("ug/m^3", 3))
  }
)

testthat::test_that(
  "calculate_improve(geom='sf'/'terra'): aligns geometry and custom IDs",
  {
    source <- process_improve(improve_path, return_format = "data.table")
    sites <- data.frame(
      id = c("002", "001"), SiteCode = c("ACAD1", "ACAD1"),
      lon = c(-70, -71), lat = c(42, 43)
    )
    projected <- sf::st_transform(
      sf::st_as_sf(sites, coords = c("lon", "lat"), crs = 4326), 3857
    )
    for (locs in list(sites, projected, terra::vect(projected))) {
      for (geom in c("sf", "terra")) {
        out <- calculate_improve(source, locs, locs_id = "id", geom = geom)
        if (geom == "sf") {
          testthat::expect_s3_class(out, "sf")
        } else {
          testthat::expect_s4_class(out, "SpatVector")
        }
        out_sf <- sf::st_as_sf(out)
        expected_crs <- if (is.data.frame(locs) && !inherits(locs, "sf")) {
          sf::st_crs(4326)
        } else {
          sf::st_crs(3857)
        }
        testthat::expect_identical(sf::st_crs(out_sf) == expected_crs, TRUE)
        coords <- sf::st_coordinates(sf::st_transform(out_sf, 4326))
        testthat::expect_equal(
          unname(coords[, 1]), sites$lon[match(out_sf$id, sites$id)]
        )
        testthat::expect_equal(
          unname(coords[, 2]), sites$lat[match(out_sf$id, sites$id)]
        )
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(locs=unmatched/empty): returns a typed empty result",
  {
    source <- process_improve(improve_path, return_format = "data.table")
    sites <- data.frame(site_id = "missing", lon = -70, lat = 42)
    for (geom in list(FALSE, "sf", "terra")) {
      testthat::expect_warning(
        out <- calculate_improve(source, sites, geom = geom), "no matching"
      )
      testthat::expect_equal(nrow(out), 0L)
      empty <- calculate_improve(source, sites[FALSE, ], geom = geom)
      testthat::expect_equal(nrow(empty), 0L)
    }
    testthat::expect_warning(
      out <- calculate_improve(source[0, ], sites), "no matching"
    )
    testthat::expect_s3_class(out$time, "POSIXct")
  }
)

testthat::test_that(
  "calculate_improve(from=invalid, locs=invalid): validates the contract",
  {
    source <- data.frame(SiteCode = "ACAD1", FactDate = as.Date("2022-01-01"))
    sites <- data.frame(site_id = "001", SiteCode = "ACAD1")
    testthat::expect_error(calculate_improve(NULL, sites), "must be data.frame")
    testthat::expect_error(
      calculate_improve(source["SiteCode"], sites), "must contain"
    )
    testthat::expect_error(
      calculate_improve(source, rbind(sites, sites)), "unique, nonmissing"
    )
    testthat::expect_error(
      calculate_improve(source, sites, locs_id = "missing"), "unique, nonmissing"
    )
    testthat::expect_error(
      calculate_improve(source, sites, geom = TRUE), "geom"
    )
    testthat::expect_error(
      calculate_improve(source, sites, geom = "sf"), "Geometry requires"
    )
    testthat::expect_error(
      calculate_improve(source, sites, radius = 100), "Additional arguments"
    )
    testthat::expect_error(
      calculate_covariates("improve", source, sites, weights = 1), "weights"
    )
    testthat::expect_error(
      calculate_covariates("improve", source, sites, .by_time = "month"),
      "observation matching"
    )
    source$FactDate <- "invalid"
    testthat::expect_error(calculate_improve(source, sites), "valid dates")
    source$FactDate <- as.Date("2022-01-01")
    source$time <- 1
    testthat::expect_error(calculate_improve(source, sites), "conflict")
  }
)


testthat::test_that(
  "calculate_covariates(covariate='improve'): completes the offline workflow",
  {
    path <- withr::local_tempdir()
    fixture <- normalizePath(file.path(improve_path, "IMPAER_2022.txt"))
    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        withr::with_dir(dirname(fixture), {
          utils::zip(destfiles, files = basename(fixture), flags = "-q")
        })
        list(success = 1L, failed = 0L)
      }
    )
    download_data(
      "improve", year = 2022, directory_to_save = path,
      acknowledgement = TRUE
    )
    testthat::expect_gt(file.info(file.path(path, basename(fixture)))$size, 0)
    processed <- process_covariates(
      "improve", path = path, return_format = "data.table"
    )
    out <- calculate_covariates(
      "improve", from = processed, locs = data.frame(site_id = "ACAD1")
    )
    testthat::expect_equal(out$FactValue[out$ParamCode == "FPM"], c(2.85, 3.12))
    testthat::expect_identical(unique(out$site_id), "ACAD1")
  }
)
