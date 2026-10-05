################################################################################
##### unit and integration tests for IMPROVE (FLMA) functions
# nolint start

improve_path <- testthat::test_path("..", "testdata", "improve")

# Fixed monitor coordinates and shuffled character IDs expose alignment errors.
improve_test_locs <- function() {
  data.frame(
    id = c("009", "001", "007"),
    lon = c(-103.1774, -68.2608, 0),
    lat = c(29.3025, 44.3771, 0)
  )
}

for (format in c("terra", "sf", "data.table")) {
  testthat::test_that(paste0(
    "calculate_improve(from=", format, ", radius=0): preserves observations and IDs"
  ), {
    from <- process_improve(improve_path, return_format = format)
    out <- calculate_improve(from, improve_test_locs(), locs_id = "id")
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 13L)
    testthat::expect_identical(unique(out$id), c("009", "001", "007"))
    testthat::expect_equal(out$FactValue[out$id == "009" &
      !is.na(out$ParamCode) & out$ParamCode == "FPM"], c(1.98, 2.05))
    testthat::expect_equal(out$FactValue[out$id == "001" &
      !is.na(out$ParamCode) & out$ParamCode == "FPM"], c(2.85, 3.12))
    testthat::expect_s3_class(out$FactDate, "Date")
    testthat::expect_equal(sort(unique(stats::na.omit(out$FactDate))),
      as.Date(c("2022-01-02", "2022-01-05")))
    testthat::expect_equal(out$Status[out$id == "009" &
      !is.na(out$ParamCode) & out$ParamCode == "FPM"], c("V0", "M1"))
    testthat::expect_equal(out$FactValue[out$id == "007"], NA_real_)
    testthat::expect_equal(unique(stats::na.omit(out$Units)), "ug/m^3")
  })
}

for (product in c("rhr2", "rhr3")) {
  testthat::test_that(paste0(
    "calculate_covariates(covariate='IMPROVE', product=", product,
    ", .by_time='month'): summarizes only measurement values"
  ), {
    from <- process_covariates("improve", path = improve_path, product = product)
    out <- calculate_covariates("IMPROVE", from, improve_test_locs(),
      locs_id = "id", .by_time = "month")
    expected <- if (product == "rhr2") c(9.05, 13.5) else c(1.015, 1.625)
    testthat::expect_equal(out$FactValue[match(c("009", "001"), out$id)], expected)
    testthat::expect_equal(nrow(out), 3L)
    testthat::expect_equal(unique(stats::na.omit(out$FactDate)), as.Date("2022-01-01"))
    testthat::expect_equal(out$FactValue[out$id == "007"], NA_real_)
    testthat::expect_equal(intersect(c("Elevation", "good_year", "n_dv"), names(out)), character())
    testthat::expect_equal(unique(stats::na.omit(out$Units)),
      if (product == "rhr2") "1/Mm" else "dv")
  })
}

testthat::test_that("calculate_improve(locs=polygon, .by_time='month'): keeps monitors and parameters separate", {
  from <- process_improve(improve_path)
  locs <- fixture_aoi()
  locs$id <- "001"
  out <- calculate_improve(from, locs, locs_id = "id", .by_time = "month")
  testthat::expect_equal(nrow(out), 7L)
  testthat::expect_equal(out$FactValue[out$SiteCode == "ACAD1" & out$ParamCode == "FPM"], 2.985)
  testthat::expect_equal(out$FactValue[out$SiteCode == "BIBE1" & out$ParamCode == "ALf"], 0.00109)
  testthat::expect_equal(sort(out$FactValue[out$SiteCode == "BIBE1" & out$ParamCode == "FPM"]), c(1.98, 2.05))
})

for (geom in c("sf", "terra")) {
  testthat::test_that(paste0(
    "calculate_improve(geom=", geom, ", radius=1000): aligns CRS and buffered geometry"
  ), {
    from <- process_improve(improve_path, product = "rhr2")
    locs <- sf::st_as_sf(improve_test_locs(), coords = c("lon", "lat"), crs = 4326)
    locs <- sf::st_transform(locs, 3857)
    out <- calculate_improve(from, locs, locs_id = "id", radius = 1000,
      geom = geom, .by_time = "month")
    if (geom == "sf") testthat::expect_s3_class(out, "sf") else
      testthat::expect_s4_class(out, "SpatVector")
    out <- sf::st_as_sf(out)
    testthat::expect_identical(sf::st_crs(out)$epsg, 4326L)
    testthat::expect_equal(as.character(sf::st_geometry_type(out)), rep("POLYGON", 3L))
    testthat::expect_equal(out$FactValue[match(c("009", "001"), out$id)], c(9.05, 13.5))
    centers <- sf::st_transform(locs[match(out$id, locs$id), ], 4326)
    testthat::expect_equal(diag(sf::st_intersects(out, centers, sparse = FALSE)), rep(TRUE, 3L))
  })
}

testthat::test_that("calculate_improve(FactValue=NA/0, .by_time='month'): preserves missingness and units", {
  from <- process_improve(improve_path, product = "rhr2", return_format = "data.table")
  from$FactValue <- c(0, NA_real_, NA_real_, NA_real_)
  out <- calculate_improve(from, improve_test_locs(), locs_id = "id", .by_time = "month")
  testthat::expect_equal(out$FactValue[match(c("001", "009", "007"), out$id)], c(0, NA_real_, NA_real_))
  from$Units[2] <- "other"
  out <- calculate_improve(from, improve_test_locs(), locs_id = "id", .by_time = "month")
  testthat::expect_equal(nrow(out[out$id == "001", ]), 2L)
})

testthat::test_that("calculate_improve(locs=outside): retains unmatched locations with optional summaries", {
  from <- process_improve(improve_path)
  for (unit in list(NULL, "month")) {
    out <- calculate_improve(from, improve_test_locs()[3, ], locs_id = "id", .by_time = unit)
    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_identical(out$id, "007")
    testthat::expect_equal(out$FactValue, NA_real_)
  }
})

testthat::test_that("calculate_improve(radius=10/1000): uses meter buffers without nearest matching", {
  from <- process_improve(improve_path, product = "rhr2")
  locs <- improve_test_locs()[2, ]
  locs$lon <- locs$lon + 0.005
  small <- calculate_improve(from, locs, "id", radius = 10)
  large <- calculate_improve(from, locs, "id", radius = 1000)
  testthat::expect_equal(small$FactValue, NA_real_)
  testthat::expect_equal(large$FactValue, c(12.3, 14.7))
  testthat::expect_equal(large$SiteCode, rep("ACAD1", 2L))
})

testthat::test_that("calculate_improve(.by_time='month', POC/MethodID=distinct): preserves full summary keys", {
  from <- process_improve(improve_path, product = "rhr2", return_format = "data.table")
  from <- as.data.frame(from)[rep(1, 4), ]
  from$FactValue <- c(10, 20, 30, 40)
  from$POC <- c(1L, 2L, 1L, 1L)
  from$MethodID <- c(3002L, 3002L, 3003L, 3002L)
  from$FactDate <- as.Date(c("2022-01-31", "2022-01-31", "2022-01-31", "2022-02-01"))
  out <- calculate_improve(from, improve_test_locs()[2, ], "id", .by_time = "month")
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_equal(out$FactValue[out$POC == 2L], 20)
  testthat::expect_equal(out$FactValue[out$MethodID == 3003L], 30)
  testthat::expect_equal(out$FactValue[out$FactDate == as.Date("2022-02-01")], 40)
  testthat::expect_equal(sort(out$FactValue), c(10, 20, 30, 40))
})

testthat::test_that("calculate_improve(from=invalid, locs_id=invalid): rejects ambiguous inputs", {
  from <- process_improve(improve_path, return_format = "data.table")
  locs <- improve_test_locs()
  testthat::expect_error(calculate_improve(from, locs), "locs_id")
  testthat::expect_error(calculate_improve(from, locs[c(1, 1), ], locs_id = "id"), "unique")
  locs$id[1] <- NA_character_
  testthat::expect_error(calculate_improve(from, locs, locs_id = "id"), "nonmissing")
  locs <- improve_test_locs()
  locs$SiteCode <- locs$id
  testthat::expect_error(calculate_improve(from, locs, locs_id = "SiteCode"), "duplicate")
  for (radius in list(-1, NA_real_, Inf, c(0, 1))) {
    testthat::expect_error(calculate_improve(from, locs, "id", radius = radius), "radius")
  }
  testthat::expect_error(calculate_covariates("improve", from, locs, "id", weights = 1), "weights")
  testthat::expect_error(calculate_improve(from, locs, "id", .by_time = "bogus"), "by_time")
  testthat::expect_error(calculate_improve(from, locs, "id", .by = "id"), "by")
  testthat::expect_error(calculate_improve(from, locs, "id", geom = TRUE), "geom")
  bad <- as.data.frame(from)
  bad$Longitude <- NULL
  testthat::expect_error(calculate_improve(bad, locs, "id"), "Longitude")
  bad <- as.data.frame(from)
  bad$FactValue <- NULL
  testthat::expect_error(calculate_improve(bad, locs, "id"), "FactValue")
  bad <- as.data.frame(from)
  bad$FactDate[1] <- NA
  testthat::expect_error(calculate_improve(bad, locs, "id"), "FactDate")
})

testthat::test_that("download_data(dataset_name='improve'): fixture archive flows through process and calculate", {
  destination <- withr::local_tempdir()
  local_download_mocks(download_run_method = function(urls, destfiles, ...) {
    testthat::expect_match(urls, "IMPAER_2022.txt.zip", fixed = TRUE)
    withr::with_dir(improve_path, {
      utils::zip(destfiles, files = "IMPAER_2022.txt", flags = "-q")
    })
    list(success = 1L, failed = 0L)
  })
  download_data("improve", year = 2022, directory_to_save = destination,
    acknowledgement = TRUE)
  file.copy(file.path(improve_path, "improve_sites.txt"), destination)
  from <- process_covariates("improve", path = destination)
  out <- calculate_covariates("improve", from, improve_test_locs(), locs_id = "id")
  testthat::expect_equal(nrow(out), 13L)
  testthat::expect_equal(out$FactValue[out$id == "001" &
    !is.na(out$ParamCode) & out$ParamCode == "FPM"], c(2.85, 3.12))
})

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
