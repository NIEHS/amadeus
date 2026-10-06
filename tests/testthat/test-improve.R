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
  "calculate_improve(from=<processed>): preserves product values and dates",
  {
    locs <- data.frame(id = c("Acadia", "unmatched"),
                       lon = c(-68.2608, 0), lat = c(44.3771, 0))
    expected <- list(raw = c(2.85, 3.12), rhr2 = c(12.3, 14.7),
                     rhr3 = c(1.52, 1.73))
    columns <- c(raw = "improve_FPM", rhr2 = "improve_bext",
                 rhr3 = "improve_dv")
    for (product in names(expected)) {
      for (format in c("terra", "sf", "data.table")) {
        from <- amadeus::process_improve(
          improve_path, product = product, return_format = format
        )
        out <- amadeus::calculate_covariates(
          "IMPROVE", from, locs, locs_id = "id"
        )
        testthat::expect_s3_class(out, "data.frame")
        testthat::expect_identical(out$id,
                                   c("Acadia", "Acadia", "unmatched",
                                     "unmatched"))
        testthat::expect_equal(out[[columns[[product]]]],
                              c(expected[[product]], NA, NA))
        testthat::expect_s3_class(out$time, "POSIXct")
        testthat::expect_equal(as.Date(out$time),
                              rep(as.Date(c("2022-01-02", "2022-01-05")), 2))
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(locs=<polygon>, .by_time='month'): averages observations",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_aoi()
    locs$site_id <- "region"
    out <- amadeus::calculate_improve(from, locs)
    testthat::expect_equal(out$improve_FPM, c((2.85 + 1.98) / 2,
                                           (3.12 + 2.05) / 2))
    monthly <- amadeus::calculate_covariates(
      "improve", from, locs, .by_time = "month"
    )
    testthat::expect_equal(monthly$improve_FPM, mean(c(2.85, 1.98, 3.12, 2.05)))
    testthat::expect_equal(nrow(monthly), 1L)
  }
)

testthat::test_that(
  paste0("calculate_improve(radius=1000, geom=<format>): ",
         "aligns CRS and keeps geometry"),
  {
    from <- amadeus::process_improve(improve_path)
    locs <- terra::vect(
      data.frame(site_id = "nearby", lon = -68.261, lat = 44.377),
      geom = c("lon", "lat"), crs = "EPSG:4326"
    )
    projected <- terra::project(locs, "EPSG:3857")
    for (geom in list(TRUE, "terra", "sf")) {
      out <- amadeus::calculate_improve(
        from, projected, radius = 1000, geom = geom
      )
      if (identical(geom, "sf")) {
        testthat::expect_s3_class(out, "sf")
        values <- sf::st_drop_geometry(out)
      } else {
        testthat::expect_s4_class(out, "SpatVector")
        values <- as.data.frame(out)
      }
      testthat::expect_equal(values$improve_FPM, c(2.85, 3.12))
      testthat::expect_equal(sf::st_crs(sf::st_as_sf(out))$epsg, 4326L)
    }
    direct <- amadeus::calculate_improve(from, locs)
    testthat::expect_equal(direct$improve_FPM, c(NA_real_, NA_real_))
  }
)

testthat::test_that(
  paste0("calculate_improve(FactValue=NA, from=<empty>): ",
         "retains missingness and schema"),
  {
    from <- amadeus::process_improve(improve_path, return_format = "data.table")
    locs <- fixture_aoi()
    locs$site_id <- "region"
    from$FactValue[from$ParamCode == "FPM"] <- NA_real_
    out <- amadeus::calculate_improve(from, locs, .by_time = "month")
    testthat::expect_identical(out$improve_FPM, NA_real_)
    empty <- amadeus::calculate_improve(from[0, ], locs)
    testthat::expect_equal(nrow(empty), 0L)
    testthat::expect_named(empty, c("site_id", "time"))
    empty_locs <- amadeus::calculate_improve(from, locs[0, ])
    testthat::expect_equal(nrow(empty_locs), 0L)
    testthat::expect_setequal(names(empty_locs),
                             c("site_id", "time", "improve_ALf",
                               "improve_ECf", "improve_FPM"))
  }
)

testthat::test_that(
  "calculate_improve(from=<invalid>, locs_id=<invalid>): reports input errors",
  {
    from <- amadeus::process_improve(improve_path, return_format = "data.table")
    locs <- fixture_points(2)
    testthat::expect_error(amadeus::calculate_improve(from, locs, radius = -1),
                           "radius")
    testthat::expect_error(amadeus::calculate_improve(from, locs, geom = NA),
                           "geom")
    testthat::expect_error(
      amadeus::calculate_improve(from, locs, locs_id = "x"), "identifier"
    )
    locs$site_id <- c("same", "same")
    testthat::expect_error(amadeus::calculate_improve(from, locs), "unique")
    locs$site_id <- c("a", "b")
    bad <- as.data.frame(from)
    bad$FactValue <- NULL
    testthat::expect_error(amadeus::calculate_improve(bad, locs), "FactValue")
    bad <- as.data.frame(from)
    bad$Longitude <- NULL
    testthat::expect_error(amadeus::calculate_improve(bad, locs), "Longitude")
    bad <- as.data.frame(from)
    bad$FactDate[1] <- NA
    testthat::expect_error(amadeus::calculate_improve(bad, locs), "FactDate")
    bad <- as.data.frame(from)
    bad$Units[1] <- "different"
    testthat::expect_error(amadeus::calculate_improve(bad, locs), "Units")
    testthat::expect_error(
      amadeus::calculate_improve(from, locs, .by = "month"), "by"
    )
  }
)

testthat::test_that(
  paste0("calculate_covariates(covariate='improve'): ",
         "completes mocked download workflow"),
  {
    path <- withr::local_tempdir()
    local_download_mocks(download_run_method = function(...) {
      file.copy(list.files(improve_path, full.names = TRUE), path)
      list(success = 1L, failed = 0L)
    })
    amadeus::download_data(
      "improve", directory_to_save = path, acknowledgement = TRUE,
      year = 2022, product = "raw"
    )
    from <- amadeus::process_covariates("improve", path = path)
    locs <- fixture_aoi()
    locs$site_id <- "region"
    out <- amadeus::calculate_covariates("improve", from, locs)
    testthat::expect_equal(out$improve_FPM, c(2.415, 2.585))
  }
)

testthat::test_that(
  "calculate_improve(from=<duplicates>): averages available values per date",
  {
    from <- as.data.frame(amadeus::process_improve(
      improve_path, return_format = "data.table"
    ))
    from <- from[from$SiteCode == "ACAD1" & from$ParamCode == "FPM", ]
    from <- from[c(1, 1, 2), ]
    from$FactValue <- c(2, 4, NA_real_)
    locs <- data.frame(site_id = 42L, lon = -68.2608, lat = 44.3771)
    out <- amadeus::calculate_improve(from, locs)
    testthat::expect_identical(out$site_id, c(42L, 42L))
    testthat::expect_equal(out$improve_FPM, c(3, NA_real_))
    monthly <- amadeus::calculate_improve(from, locs, .by_time = "month")
    testthat::expect_equal(monthly$improve_FPM, 3)
    testthat::expect_equal(as.Date(monthly$time), as.Date("2022-01-01"))
  }
)
