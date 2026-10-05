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
  "calculate_improve(radius=0): retains values, dates, units and custom IDs",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_improve_locs()
    out <- amadeus::calculate_improve(from, locs, locs_id = "station")
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_named(
      out, c("station", "time", "ParamCode", "Units", "FactValue")
    )
    testthat::expect_equal(nrow(out), 18L)
    testthat::expect_identical(out$station, rep(locs$station, each = 6))
    testthat::expect_s3_class(out$time, "POSIXct")
    testthat::expect_identical(attr(out$time, "tzone"), "UTC")
    testthat::expect_equal(
      unique(as.Date(out$time)), as.Date(c("2022-01-02", "2022-01-05"))
    )
    testthat::expect_identical(unique(out$Units), "ug/m^3")
    testthat::expect_equal(
      out$FactValue[out$ParamCode == "FPM"],
      c(1.98, 2.05, 2.85, 3.12, NA, NA)
    )
    testthat::expect_equal(
      out$FactValue[out$ParamCode == "ALf"],
      c(0.00120, 0.00098, 0.00044, 0.00062, NA, NA)
    )
    reordered <- amadeus::calculate_improve(
      from, locs[3:1, ], locs_id = "station"
    )
    testthat::expect_identical(reordered$station, rep(rev(locs$station), each = 6))
    testthat::expect_equal(
      reordered$FactValue[reordered$ParamCode == "FPM"],
      c(NA, NA, 2.85, 3.12, 1.98, 2.05)
    )
  }
)

testthat::test_that(
  "calculate_improve(from=processed formats): accepts terra, sf and data.table",
  {
    locs <- fixture_improve_locs()
    for (format in c("terra", "sf", "data.table")) {
      from <- amadeus::process_improve(improve_path, return_format = format)
      out <- amadeus::calculate_improve(
        from, locs, locs_id = "station", variable = "FPM"
      )
      testthat::expect_equal(nrow(out), 6L, info = format)
      testthat::expect_equal(out$FactValue, c(1.98, 2.05, 2.85, 3.12, NA, NA))
      testthat::expect_identical(unique(out$ParamCode), "FPM")
    }
  }
)

testthat::test_that(
  "calculate_improve(radius=1000): buffers locations in meters",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_improve_locs()[2, ]
    locs$lon <- locs$lon + 0.005
    for (radius in c(0, 50, 1000)) {
      out <- amadeus::calculate_improve(
        from, locs, locs_id = "station", radius = radius, variable = "FPM"
      )
      expected <- if (radius == 1000) c(2.85, 3.12) else c(NA_real_, NA_real_)
      testthat::expect_equal(out$FactValue, expected, info = radius)
    }
  }
)

testthat::test_that(
  "calculate_improve(locs=polygon): summarizes intersecting monitors",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_aoi()
    locs$station <- "002"
    out <- amadeus::calculate_improve(
      from, locs, locs_id = "station", variable = "FPM"
    )
    testthat::expect_equal(out$FactValue, c(2.415, 2.585))
    monthly <- amadeus::calculate_improve(
      from, locs, locs_id = "station", variable = "FPM", .by_time = "month"
    )
    testthat::expect_equal(monthly$FactValue, 2.5)
    summed <- amadeus::calculate_improve(
      from, locs, locs_id = "station", variable = "FPM", fun_summary = "sum"
    )
    testthat::expect_equal(summed$FactValue, c(4.83, 5.17))
    boundary <- terra::vect(terra::ext(-68.2608, -68, 44, 45), crs = "EPSG:4326")
    boundary$station <- "003"
    out_boundary <- amadeus::calculate_improve(
      from, boundary, locs_id = "station", variable = "FPM"
    )
    testthat::expect_equal(out_boundary$FactValue, c(2.85, 3.12))
  }
)

testthat::test_that(
  "calculate_improve(geom='sf'/'terra'): aligns projected IDs and geometry",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- sf::st_as_sf(fixture_improve_locs(), coords = c("lon", "lat"), crs = 4326)
    projected <- sf::st_transform(locs, 3857)
    for (geom in c("sf", "terra")) {
      input <- if (geom == "sf") projected else terra::vect(projected)
      out <- amadeus::calculate_improve(
        from, input, locs_id = "station", variable = "FPM",
        radius = 1000, .by_time = "month", geom = geom
      )
      if (geom == "sf") {
        testthat::expect_s3_class(out, "sf")
      } else {
        testthat::expect_s4_class(out, "SpatVector")
      }
      out_sf <- sf::st_as_sf(out)
      testthat::expect_identical(out_sf$station, c("020", "003", "001"))
      testthat::expect_equal(out_sf$FactValue, c(2.015, 2.985, NA))
      testthat::expect_identical(sf::st_crs(out_sf)$epsg, 4326L)
      testthat::expect_identical(as.character(sf::st_geometry_type(out_sf)), rep("POLYGON", 3))
      testthat::expect_identical(
        lapply(sf::st_intersects(out_sf, locs), identity), list(1L, 2L, 3L)
      )
    }
  }
)

testthat::test_that(
  "calculate_covariates(covariate='IMPROVE', .by_time='month'): forwards options",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_improve_locs()
    for (alias in c("improve", "IMPROVE")) {
      out <- amadeus::calculate_covariates(
        alias, from, locs, locs_id = "station", variable = "FPM",
        .by_time = "month", fun_summary = "max"
      )
      testthat::expect_equal(out$FactValue, c(2.05, 3.12, NA))
      testthat::expect_equal(as.Date(out$time), rep(as.Date("2022-01-01"), 3))
      testthat::expect_identical(out$station, locs$station)
    }
    testthat::expect_error(
      amadeus::calculate_covariates("improve", from, locs, weights = 1),
      "IMPROVE supports unweighted"
    )
  }
)

testthat::test_that(
  "calculate_improve(.by_time='month'): separates units and averages dates equally",
  {
    from <- data.frame(
      FactDate = as.Date(c("2022-01-02", "2022-01-02", "2022-01-05", "2022-01-02")),
      ParamCode = "FPM", Units = c("ug/m^3", "ug/m^3", "ug/m^3", "ng/m^3"),
      FactValue = c(10, 20, 40, 1000), POC = c(1, 2, 1, 1),
      MethodID = 5017, Latitude = 44.3771, Longitude = -68.2608
    )
    out <- amadeus::calculate_improve(
      from, fixture_improve_locs()[2, ], locs_id = "station", .by_time = "month"
    )
    testthat::expect_equal(nrow(out), 2L)
    testthat::expect_equal(out$FactValue[out$Units == "ug/m^3"], 27.5)
    testthat::expect_equal(out$FactValue[out$Units == "ng/m^3"], 1000)
    testthat::expect_named(out, c("station", "time", "ParamCode", "Units", "FactValue"))
  }
)

testthat::test_that(
  "calculate_improve(FactValue=NA/0): distinguishes missing values and valid zero",
  {
    from <- amadeus::process_improve(improve_path, return_format = "data.table")
    from$FactValue[from$ParamCode == "ALf"] <- NA_real_
    from$FactValue[from$ParamCode == "ECf"] <- 0
    from$FactValue[from$ParamCode == "FPM" & from$SiteCode == "BIBE1"] <- NA_real_
    locs <- fixture_aoi()
    locs$site_id <- "003"
    out <- amadeus::calculate_improve(from, locs, .by_time = "month")
    testthat::expect_identical(out$FactValue[out$ParamCode == "ALf"], NA_real_)
    testthat::expect_equal(out$FactValue[out$ParamCode == "ECf"], 0)
    testthat::expect_equal(out$FactValue[out$ParamCode == "FPM"], 2.985)
    out_sum <- amadeus::calculate_improve(from, locs, fun_summary = "sum")
    testthat::expect_identical(out_sum$FactValue[out_sum$ParamCode == "ALf"], c(NA_real_, NA_real_))
  }
)

testthat::test_that(
  "calculate_improve(from=<empty>, locs=<empty>): returns typed empty results",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_improve_locs()
    for (geom in list(FALSE, "sf", "terra")) {
      for (empty_source in c(TRUE, FALSE)) {
        out <- amadeus::calculate_improve(
          if (empty_source) from[FALSE, ] else from,
          if (empty_source) locs else locs[FALSE, ],
          locs_id = "station", geom = geom, .by_time = "month"
        )
        testthat::expect_equal(nrow(out), 0L)
        if (identical(geom, FALSE)) testthat::expect_s3_class(out, "data.frame")
        if (identical(geom, "sf")) testthat::expect_s3_class(out, "sf")
        if (identical(geom, "terra")) testthat::expect_s4_class(out, "SpatVector")
        testthat::expect_type(out$FactValue, "double")
      }
    }
    table <- amadeus::process_improve(improve_path, return_format = "data.table")
    empty_table <- amadeus::calculate_improve(
      table[FALSE, ], locs, locs_id = "station"
    )
    testthat::expect_equal(nrow(empty_table), 0L)
    testthat::expect_named(
      empty_table, c("station", "time", "ParamCode", "Units", "FactValue")
    )
    testthat::expect_s3_class(empty_table$time, "POSIXct")
  }
)

testthat::test_that(
  "calculate_improve(inputs=<invalid>): rejects ambiguous or unsupported inputs",
  {
    from <- amadeus::process_improve(improve_path)
    locs <- fixture_improve_locs()
    calculate <- function(...) amadeus::calculate_improve(from, locs, locs_id = "station", ...)
    for (radius in list(-1, NA_real_, Inf, c(1, 2), "1000")) {
      testthat::expect_error(calculate(radius = radius), "`radius`")
    }
    testthat::expect_error(calculate(variable = "unknown"), "not found in `ParamCode`")
    testthat::expect_error(calculate(variable = NA_character_), "`variable`")
    testthat::expect_error(calculate(.by_time = "invalid"), "`.by_time`")
    testthat::expect_error(
      amadeus::calculate_improve(from, locs, locs_id = "station", .by = "year"),
      "no longer supported"
    )
    testthat::expect_error(calculate(geom = TRUE), "`geom`")
    testthat::expect_error(calculate(weights = 1), "`weights`")
    testthat::expect_error(calculate(fun_summary = function(x, ...) c(1, 2)), "one numeric value")
    testthat::expect_error(amadeus::calculate_improve(from, locs), "not found in `locs`")
    testthat::expect_error(amadeus::calculate_improve(from, locs, locs_id = "time"), "output field")
    locs$station[2] <- locs$station[1]
    testthat::expect_error(calculate(), "unique and nonmissing")
    locs$station[2] <- NA_character_
    testthat::expect_error(calculate(), "unique and nonmissing")
  }
)

testthat::test_that(
  "calculate_improve(from=<invalid>): validates measurement and spatial schema",
  {
    from <- amadeus::process_improve(improve_path, return_format = "data.table")
    from <- as.data.frame(from)
    calculate <- function(x) amadeus::calculate_improve(x, fixture_improve_locs(), locs_id = "station")
    testthat::expect_error(calculate(from[, setdiff(names(from), "Longitude")]), "Longitude and Latitude")
    testthat::expect_error(calculate(from[, setdiff(names(from), "FactValue")]), "must contain FactDate")
    bad <- from
    bad$FactDate[1] <- NA
    testthat::expect_error(calculate(bad), "valid, nonmissing dates")
    bad <- from
    bad$Units[1] <- NA_character_
    testthat::expect_error(calculate(bad), "must be nonmissing")
    bad <- from
    bad$FactValue <- as.character(bad$FactValue)
    testthat::expect_error(calculate(bad), "must be numeric")
    testthat::expect_error(calculate(fixture_spatraster()), "points with a CRS")
    bad <- amadeus::process_improve(improve_path)
    terra::crs(bad) <- ""
    testthat::expect_error(calculate(bad), "points with a CRS")
  }
)

testthat::test_that(
  "download_data(dataset_name='improve'): all products reach calculation offline",
  {
    tmp <- withr::local_tempdir()
    local_download_mocks(download_run_method = function(urls, destfiles, ...) {
      fixture <- sub("\\.zip$", "", basename(destfiles))
      utils::zip(destfiles, file.path(improve_path, fixture), flags = "-jq")
      list(success = 1L, failed = 0L, skipped = 0L)
    })
    expected <- list(
      raw = list(parameter = "FPM", units = "ug/m^3", values = c(1.98, 2.05, 2.85, 3.12)),
      rhr2 = list(parameter = "bext", units = "1/Mm", values = c(8.9, 9.2, 12.3, 14.7)),
      rhr3 = list(parameter = "dv", units = "dv", values = c(0.98, 1.05, 1.52, 1.73))
    )
    for (product in names(expected)) {
      download <- amadeus::download_data(
        "improve", tmp, acknowledgement = TRUE, year = 2022, product = product
      )
      testthat::expect_equal(download$success, 1L)
      processed <- amadeus::process_covariates("IMPROVE", path = tmp, product = product)
      # Use embedded site metadata, as a real download does.
      sites <- processed[match(c("BIBE1", "ACAD1"), processed$SiteCode), "SiteCode"]
      sites$station <- c("020", "003")
      out <- amadeus::calculate_covariates(
        "IMPROVE", processed, sites, locs_id = "station",
        variable = expected[[product]]$parameter
      )
      testthat::expect_equal(out$FactValue, expected[[product]]$values)
      testthat::expect_identical(unique(out$Units), expected[[product]]$units)
      testthat::expect_identical(out$station, rep(c("020", "003"), each = 2))
      testthat::expect_length(list.files(tmp, pattern = "\\.zip$"), 0L)
    }
  }
)
