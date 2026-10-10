################################################################################
##### unit and integration tests for IMPROVE (FLMA) functions
# nolint start

improve_path <- testthat::test_path("..", "testdata", "improve")

improve_test_locs <- function() {
  sites <- data.table::fread(file.path(improve_path, "improve_sites.txt"))
  locs <- data.frame(
    site_id = c(sites$SiteCode, "unmatched"),
    lon = c(sites$Longitude, -78),
    lat = c(sites$Latitude, 36)
  )
  terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
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
##### calculate_improve

for (format in c("terra", "sf", "data.table")) {
  testthat::test_that(
    paste0("calculate_improve(from='", format, "'): preserves dates and values"),
    {
      from <- process_improve(path = improve_path, return_format = format)
      result <- calculate_improve(from = from, locs = improve_test_locs())
      testthat::expect_s3_class(result, "data.frame")
      testthat::expect_named(
        result,
        c("site_id", "time", "improve_ALf_0", "improve_ECf_0", "improve_FPM_0")
      )
      testthat::expect_s3_class(result$time, "POSIXct")
      testthat::expect_equal(nrow(result), 6L)
      acadia <- result[result$site_id == "ACAD1", ]
      testthat::expect_equal(
        as.Date(acadia$time), as.Date(c("2022-01-02", "2022-01-05"))
      )
      testthat::expect_equal(acadia$improve_FPM_0, c(2.85, 3.12))
      testthat::expect_equal(acadia$improve_ALf_0, c(0.00044, 0.00062))
      testthat::expect_equal(acadia$improve_ECf_0, c(0.03816, 0.04201))
      testthat::expect_equal(
        result$improve_FPM_0[result$site_id == "unmatched"],
        c(NA_real_, NA_real_)
      )
    }
  )
}

for (product in c("raw", "rhr2", "rhr3")) {
  testthat::test_that(
    paste0("calculate_covariates(covariate='IMPROVE', product='", product,
           "'): completes mocked download-process-calculate workflow"),
    {
      directory <- withr::local_tempdir()
      prefix <- c(raw = "IMPAER", rhr2 = "IMPRHR2", rhr3 = "IMPRHR3")[[product]]
      filename <- paste0(prefix, "_2022.txt")
      local_download_mocks(
        download_run_method = function(urls, destfiles, ...) {
          testthat::expect_match(urls, paste0(filename, ".zip"), fixed = TRUE)
          withr::with_dir(improve_path, {
            utils::zip(destfiles, files = filename, flags = "-q")
          })
          list(success = 1L, failed = 0L, skipped = 0L)
        }
      )
      downloaded <- download_data(
        dataset_name = "improve", year = 2022, product = product,
        directory_to_save = directory, acknowledgement = TRUE
      )
      testthat::expect_equal(downloaded$success, 1L)
      testthat::expect_gt(file.size(file.path(directory, filename)), 0)
      from <- process_covariates(
        covariate = "improve", path = directory, product = product,
        sites_file = file.path(improve_path, "improve_sites.txt")
      )
      testthat::expect_s4_class(from, "SpatVector")
      result <- calculate_covariates(
        covariate = "IMPROVE", from = from, locs = improve_test_locs(),
        .by_time = "month"
      )
      parameter <- c(raw = "FPM", rhr2 = "bext", rhr3 = "dv")[[product]]
      expected <- list(raw = 2.985, rhr2 = 13.5, rhr3 = 1.625)[[product]]
      column <- paste0("improve_", parameter, "_0")
      testthat::expect_equal(
        result[[column]][result$site_id == "ACAD1"], expected
      )
      testthat::expect_equal(nrow(result), 3L)
      testthat::expect_equal(
        unique(as.Date(result$time)), as.Date("2022-01-01")
      )
    }
  )
}

testthat::test_that(
  "calculate_improve(status='V0', param_code='FPM'): filters explicitly",
  {
    from <- process_improve(path = improve_path)
    locs <- improve_test_locs()
    unfiltered <- calculate_improve(from, locs, param_code = "FPM")
    filtered <- calculate_improve(from, locs, param_code = "FPM", status = "V0")
    testthat::expect_named(filtered, c("site_id", "time", "improve_FPM_0"))
    testthat::expect_equal(
      unfiltered$improve_FPM_0[unfiltered$site_id == "BIBE1"], c(1.98, 2.05)
    )
    testthat::expect_equal(
      filtered$improve_FPM_0[filtered$site_id == "BIBE1"], c(1.98, NA_real_)
    )
    monthly <- calculate_improve(
      from, locs, param_code = "FPM", status = "V0", .by_time = "month"
    )
    testthat::expect_equal(
      monthly$improve_FPM_0[monthly$site_id == "BIBE1"], 1.98
    )
  }
)

testthat::test_that(
  "calculate_improve(radius=1000, locs_id='subject'): aligns CRS and buffers",
  {
    from <- process_improve(path = improve_path)
    locs <- data.frame(subject = c("near", "far"), lon = -68.255,
                       lat = c(44.3771, 45))
    locs <- sf::st_as_sf(locs, coords = c("lon", "lat"), crs = 4326)
    locs <- sf::st_transform(locs, 3857)
    exact <- calculate_improve(
      from, locs, locs_id = "subject", param_code = "FPM"
    )
    buffered <- calculate_improve(
      from, locs, locs_id = "subject", param_code = "FPM", radius = 1000
    )
    testthat::expect_equal(exact$improve_FPM_0, rep(NA_real_, 4))
    testthat::expect_equal(
      buffered$improve_FPM_1000[buffered$subject == "near"], c(2.85, 3.12)
    )
    testthat::expect_equal(
      buffered$improve_FPM_1000[buffered$subject == "far"], rep(NA_real_, 2)
    )
  }
)

testthat::test_that(
  "calculate_improve(locs=polygons): summarizes stations within each date",
  {
    from <- process_improve(path = improve_path)
    locs <- fixture_aoi()
    locs$site_id <- "US"
    result <- calculate_improve(from, locs)
    testthat::expect_equal(result$improve_FPM_0, c(2.415, 2.585))
    testthat::expect_equal(result$improve_ECf_0, c(0.02958, 0.030255))
    summed <- calculate_improve(from, locs, fun_summary = "sum")
    testthat::expect_equal(summed$improve_FPM_0, c(4.83, 5.17))
  }
)

for (geom in c("sf", "terra")) {
  testthat::test_that(
    paste0("calculate_improve(geom='", geom, "'): retains location geometry"),
    {
      from <- process_improve(path = improve_path)
      locs <- improve_test_locs()[c(3, 1, 2), ]
      locs$subject <- c(30L, 10L, 20L)
      result <- calculate_improve(
        from, locs, locs_id = "subject", geom = geom, .by_time = "month"
      )
      if (geom == "sf") {
        testthat::expect_s3_class(result, "sf")
      } else {
        testthat::expect_s4_class(result, "SpatVector")
        result <- sf::st_as_sf(result)
      }
      testthat::expect_equal(sf::st_crs(result)$epsg, 4326)
      testthat::expect_equal(nrow(result), 3L)
      expected <- sf::st_as_sf(locs)
      expected <- expected[match(result$subject, expected$subject), ]
      testthat::expect_equal(
        unname(sf::st_coordinates(result)), unname(sf::st_coordinates(expected))
      )
      testthat::expect_equal(result$improve_FPM_0[result$subject == 10], 2.985)
    }
  )
}

testthat::test_that(
  "calculate_improve(.by_time='month'): summarizes dates with equal weight",
  {
    from <- process_improve(path = improve_path, return_format = "sf")
    from <- from[!(from$SiteCode == "BIBE1" &
                     from$FactDate == as.Date("2022-01-05")), ]
    locs <- fixture_aoi()
    locs$site_id <- "US"
    result <- calculate_improve(
      from, locs, param_code = "FPM", .by_time = "month"
    )
    testthat::expect_equal(result$improve_FPM_0, mean(c(2.415, 3.12)))
    testthat::expect_s3_class(result$time, "POSIXct")
  }
)

for (fun in c("mean", "median", "sum", "min", "max")) {
  testthat::test_that(
    paste0("calculate_improve(fun_summary='", fun, "'): keeps missing NA"),
    {
      from <- process_improve(path = improve_path)
      from$FactValue <- NA_real_
      result <- calculate_improve(
        from, improve_test_locs(), fun_summary = fun, .by_time = "month"
      )
      testthat::expect_equal(result$improve_FPM_0, rep(NA_real_, 3))
      testthat::expect_equal(result$improve_ALf_0, rep(NA_real_, 3))
    }
  )
}

testthat::test_that(
  "calculate_improve(status='absent'): returns a typed empty result",
  {
    from <- process_improve(path = improve_path)
    for (geom in list(FALSE, "sf", "terra")) {
      result <- calculate_improve(
        from, improve_test_locs(), status = "absent", geom = geom,
        .by_time = "month"
      )
      testthat::expect_equal(nrow(result), 0L)
      testthat::expect_contains(
        names(result), c("site_id", "time", "improve_FPM_0")
      )
      if (identical(geom, FALSE)) {
        testthat::expect_s3_class(result$time, "POSIXct")
      } else if (geom == "sf") {
        testthat::expect_s3_class(result, "sf")
      } else {
        testthat::expect_s4_class(result, "SpatVector")
      }
    }
  }
)

testthat::test_that(
  "calculate_improve(locs=empty): retains the output schema",
  {
    from <- process_improve(path = improve_path)
    result <- calculate_improve(from, improve_test_locs()[0, ])
    testthat::expect_equal(nrow(result), 0L)
    testthat::expect_named(
      result,
      c("site_id", "time", "improve_ALf_0", "improve_ECf_0", "improve_FPM_0")
    )
  }
)

testthat::test_that(
  "calculate_improve(from=empty): returns IDs and time without parameters",
  {
    from <- process_improve(path = improve_path)[0, ]
    testthat::expect_equal(nrow(from), 0L)
    result <- calculate_improve(from, improve_test_locs(), .by_time = "month")
    testthat::expect_equal(nrow(result), 0L)
    testthat::expect_named(result, c("site_id", "time"))
    testthat::expect_s3_class(result$time, "POSIXct")
  }
)

testthat::test_that(
  "calculate_improve(from=invalid): validates measurement columns and units",
  {
    from <- process_improve(path = improve_path, return_format = "sf")
    locs <- improve_test_locs()
    missing <- from
    missing$FactValue <- NULL
    testthat::expect_error(calculate_improve(missing, locs), "required IMPROVE")
    nonnumeric <- from
    nonnumeric$FactValue <- as.character(nonnumeric$FactValue)
    testthat::expect_error(
      calculate_improve(nonnumeric, locs), "must be numeric"
    )
    from$Units[1] <- "different unit"
    testthat::expect_error(calculate_improve(from, locs), "single unit")
    testthat::expect_error(calculate_improve(data.frame(), locs), "Longitude")
    testthat::expect_error(
      calculate_improve(fixture_spatraster(), locs), "point"
    )
  }
)

testthat::test_that(
  "calculate_improve(locs_id=invalid): rejects invalid IDs",
  {
    from <- process_improve(path = improve_path)
    locs <- improve_test_locs()
    testthat::expect_error(
      calculate_improve(from, locs, locs_id = "absent"), "locs_id"
    )
    locs$site_id <- c("same", "same", "other")
    testthat::expect_error(calculate_improve(from, locs), "unique, nonmissing")
    locs$site_id <- c("one", "two", NA_character_)
    testthat::expect_error(calculate_improve(from, locs), "unique, nonmissing")
  }
)

testthat::test_that(
  "calculate_improve(radius=invalid, .by=legacy): validates arguments",
  {
    from <- process_improve(path = improve_path)
    locs <- improve_test_locs()
    for (radius in list(-1, NA_real_, Inf, c(0, 1000), "1000")) {
      testthat::expect_error(
        calculate_improve(from, locs, radius = radius), "radius"
      )
    }
    testthat::expect_error(calculate_improve(from, locs, geom = TRUE), "geom")
    testthat::expect_error(
      calculate_improve(from, locs, .by = "site_id"), "no longer"
    )
    testthat::expect_error(
      calculate_improve(from, locs, .by_time = "bad"), "by_time"
    )
    testthat::expect_error(
      calculate_improve(from, locs, weights = 1), "weights"
    )
    testthat::expect_error(
      calculate_improve(from, locs, fun_summary = "bad"), "arg"
    )
    testthat::expect_error(
      calculate_improve(from, locs, param_code = "bad"), "absent"
    )
    testthat::expect_error(
      calculate_improve(from, locs, status = NA_character_), "status"
    )
  }
)
