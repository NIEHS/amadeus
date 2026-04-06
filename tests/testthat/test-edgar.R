################################################################################
##### unit and integration tests for EDGAR functions

# Helper: call download_edgar with download=FALSE and suppress the deprecation
# warning, returning the result list.
edgar_discover <- function(...) {
  suppressWarnings(
    amadeus::download_edgar(..., download = FALSE, unzip = FALSE)
  )
}

write_edgar_fixture <- function(path) {
  raster <- terra::rast(
    ncols = 3,
    nrows = 2,
    xmin = -80,
    xmax = -77,
    ymin = 35,
    ymax = 37,
    crs = "EPSG:4326"
  )
  terra::values(raster) <- seq_len(terra::ncell(raster))
  names(raster) <- "emi_nox"
  terra::writeRaster(raster, path, overwrite = TRUE)
  invisible(path)
}

write_edgar_fixtures <- function(destfiles, data_dir) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  fixture_paths <- vapply(
    seq_along(destfiles),
    function(i) {
      fixture_path <- file.path(
        data_dir,
        paste0(tools::file_path_sans_ext(basename(destfiles[i])), ".tif")
      )
      raster <- terra::rast(
        ncols = 3,
        nrows = 2,
        xmin = -80,
        xmax = -77,
        ymin = 35,
        ymax = 37,
        crs = "EPSG:4326"
      )
      terra::values(raster) <- seq_len(terra::ncell(raster)) + i
      names(raster) <- paste0("emi_", sprintf("%02d", i))
      terra::writeRaster(raster, fixture_path, overwrite = TRUE)
      fixture_path
    },
    character(1)
  )
  invisible(fixture_paths)
}

run_live_edgar_chain <- function(
  ...,
  extent = c(-80, -77, 35, 37),
  radius = 1000
) {
  locs <- data.frame(
    site_id = c("a", "b"),
    lon = c(-79.5, -77.5),
    lat = c(36.5, 35.5)
  )

  suppressMessages(
    amadeus::download_edgar(
      ...,
      directory_to_save = ".",
      acknowledgement = TRUE,
      download = TRUE,
      unzip = TRUE,
      remove_zip = FALSE,
      show_progress = TRUE
    )
  )

  zip_files <- list.files(
    "zip_files",
    pattern = "\\.zip$",
    recursive = TRUE,
    full.names = TRUE
  )
  data_files <- list.files(
    "data_files",
    recursive = TRUE,
    full.names = TRUE
  )
  raster_files <- grep(
    "\\.(nc4?|tif|tiff|grd|img)$",
    data_files,
    ignore.case = TRUE,
    value = TRUE
  )

  processed <- process_edgar(
    path = raster_files,
    extent = extent
  )
  calc_zero <- calculate_edgar(
    from = processed,
    locs = locs,
    locs_id = "site_id",
    radius = 0
  )
  calc_buf <- calculate_edgar(
    from = processed,
    locs = locs,
    locs_id = "site_id",
    radius = radius
  )

  list(
    zip_files = zip_files,
    data_files = data_files,
    raster_files = raster_files,
    processed = processed,
    calc_zero = calc_zero,
    calc_buf = calc_buf
  )
}

################################################################################
##### download_edgar success tests (URL discovery, no actual download)

testthat::test_that("download_edgar (no errors, yearly with sectors)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "CO",
    temp_res = "yearly",
    sector_yearly = "ENE",
    year_range = c(2021, 2022),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(length(result$urls) > 0)
  testthat::expect_true(all(grepl("^https://", result$urls)))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (monthly, no sector)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "monthly",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(length(result$urls) > 0)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (monthly, w/sector)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "monthly",
    sector_monthly = "BUILDINGS",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(length(result$urls) > 0)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (single year)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "NOx",
    temp_res = "yearly",
    sector_yearly = "AGS",
    year_range = 2022,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC with sector_voc)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    version = "8.1_voc",
    voc = "1",
    sector_voc = "AGRICULTURE",
    year_range = c(2018, 2019),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_true(length(result$urls) > 0)
  testthat::expect_equal(length(result$destfiles), result$n_files)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC w/out year_range)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    version = "8.1_voc",
    voc = "1",
    sector_voc = "AGRICULTURE",
    year_range = NULL,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(length(result$destfiles), result$n_files)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (VOC w/out sector_voc)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    version = "8.1_voc",
    voc = "1",
    sector_voc = NULL,
    year_range = NULL,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(length(result$destfiles), result$n_files)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (default year_range)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "yearly",
    sector_yearly = "AWB",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (NULL sector_yearly)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "yearly",
    sector_yearly = NULL,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (NULL sector_yearly + year_range)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "yearly",
    sector_yearly = NULL,
    year_range = c(2018, 2019),
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 2)
  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar (timeseries)", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar/")
  result <- edgar_discover(
    species = "SO2",
    temp_res = "timeseries",
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### download_edgar deprecation warnings

testthat::test_that("download_edgar deprecation warnings", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/edgar_dep/")

  testthat::expect_warning(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      unzip = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE,
      unzip = FALSE
    ),
    regexp = "remove_command.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})

################################################################################
##### download_edgar error tests

testthat::test_that("download_edgar (invalid year_range length)", {
  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        species = "CO",
        temp_res = "yearly",
        sector_yearly = "ENE",
        year_range = c(2015, 2016, 2017),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), "/e/"),
        unzip = FALSE
      )
    ),
    "year_range must be of length 1 or 2"
  )
})

testthat::test_that("download_edgar (invalid species)", {
  skip_if_offline()
  testthat::expect_error(
    amadeus::download_edgar(
      species = "XYZ",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = c(2021, 2022),
      acknowledgement = TRUE,
      download = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "No valid URLs were constructed"
  )
})

testthat::test_that("download_edgar (incompatible output-format)", {
  testthat::expect_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "monthly",
      output = "flx",
      format = "txt",
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "Output 'flux' is only supported for format 'nc'."
  )

  testthat::expect_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "monthly",
      output = "nc",
      format = "txt",
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    )
  )
})

testthat::test_that("download_edgar (missing acknowledgement triggers error)", {
  testthat::expect_error(
    amadeus::download_edgar(
      species = "CO",
      temp_res = "monthly",
      directory_to_save = paste0(tempdir(), "/e/"),
      unzip = FALSE
    ),
    regexp = "acknowledge"
  )
})

testthat::test_that("download_edgar (bad version)", {
  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        version = "unacceptable version",
        voc = "1",
        sector_voc = "AGRICULTURE",
        year_range = c(2018, 2019),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), "/e/"),
        unzip = FALSE
      )
    )
  )
})

testthat::test_that("download_edgar (bad year)", {
  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        species = "SO2",
        temp_res = "yearly",
        sector_yearly = NULL,
        year_range = c(2018, 2019, 2022),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), "/e/"),
        unzip = FALSE
      )
    )
  )
})

testthat::test_that("download_edgar (bad temp_res)", {
  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        species = "SO2",
        temp_res = "not_recognized",
        sector_yearly = NULL,
        year_range = c(2018, 2019),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), "/e/"),
        unzip = FALSE
      )
    )
  )
})


testthat::test_that("download_edgar mock download with hash", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) TRUE,
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_edgar(
          species = "CO",
          temp_res = "yearly",
          sector_yearly = "ENE",
          year_range = c(2021, 2021),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})

################################################################################
##### download_edgar missing URL warning branch (some URLs return FALSE)

testthat::test_that("download_edgar missing URL warning path", {
  call_idx <- 0L
  testthat::local_mocked_bindings(
    check_url_status = function(u, ...) {
      call_idx <<- call_idx + 1L
      call_idx > 1L # First URL is invalid (FALSE), rest are valid (TRUE)
    },
    download_run_method = function(...) list(success = 1, failed = 0),
    download_unzip = function(...) invisible(NULL),
    download_remove_zips = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_warning(
      suppressMessages(
        download_edgar(
          species = c("CO", "CH4"),
          temp_res = "yearly",
          sector_yearly = "ENE",
          year_range = c(2021, 2021),
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          unzip = FALSE,
          hash = FALSE
        )
      ),
      "Some URLs could not be accessed"
    )
  })
})

################################################################################
##### download_edgar all-URLs-invalid error (covers line 4057)

testthat::test_that("download_edgar stops when all URLs invalid", {
  testthat::local_mocked_bindings(
    check_url_status = function(...) FALSE,
    .package = "amadeus"
  )
  withr::with_tempdir({
    testthat::expect_error(
      suppressMessages(
        download_edgar(
          species = "CO",
          temp_res = "yearly",
          sector_yearly = "ENE",
          year_range = c(2021, 2022),
          acknowledgement = TRUE,
          download = TRUE,
          directory_to_save = ".",
          unzip = FALSE
        )
      ),
      "No valid URLs were constructed"
    )
  })
})

################################################################################
##### process_edgar and calculate_edgar

testthat::test_that("process_edgar reads gridded EDGAR rasters", {
  withr::local_package("terra")

  withr::with_tempdir({
    raster_path <- file.path(".", "edgar_2021_total_emi.tif")
    write_edgar_fixture(raster_path)

    testthat::expect_no_error(
      edgar <- process_edgar(path = raster_path)
    )
    testthat::expect_s4_class(edgar, "SpatRaster")
    testthat::expect_equal(terra::nlyr(edgar), 1)
    testthat::expect_match(names(edgar), "^edgar_")
    testthat::expect_equal(as.character(terra::time(edgar)[1]), "2021-01-01")

    testthat::expect_no_error(
      edgar_dir <- process_edgar(path = ".")
    )
    testthat::expect_s4_class(edgar_dir, "SpatRaster")
  })
})

testthat::test_that("process_edgar rejects unsupported text-only inputs", {
  withr::with_tempdir({
    txt_path <- file.path(".", "edgar_totals.txt")
    writeLines("1 2 3", txt_path)

    testthat::expect_error(
      process_edgar(path = txt_path),
      "supports gridded raster files only"
    )
  })
})

testthat::test_that("process_edgar validates empty and unsupported non-raster paths", {
  testthat::expect_error(
    process_edgar(path = character(0)),
    "path does not contain files"
  )

  withr::with_tempdir({
    csv_path <- file.path(".", "edgar_totals.csv")
    write.csv(data.frame(x = 1), csv_path, row.names = FALSE)
    testthat::expect_error(
      process_edgar(path = csv_path),
      "supported EDGAR raster files"
    )
  })
})

testthat::test_that("process_edgar prefixes informative multi-layer names", {
  withr::local_package("terra")

  withr::with_tempdir({
    r1 <- terra::rast(ncols = 2, nrows = 2, xmin = -80, xmax = -78, ymin = 35, ymax = 37, crs = "EPSG:4326")
    r2 <- terra::rast(ncols = 2, nrows = 2, xmin = -80, xmax = -78, ymin = 35, ymax = 37, crs = "EPSG:4326")
    terra::values(r1) <- 1:4
    terra::values(r2) <- 5:8
    rr <- c(r1, r2)
    names(rr) <- c("nox_total", "so2_total")
    raster_path <- file.path(".", "edgar_named_layers_2021.tif")
    terra::writeRaster(rr, raster_path, overwrite = TRUE)

    edgar <- process_edgar(path = raster_path)
    testthat::expect_true(all(grepl("^edgar_", names(edgar))))
    testthat::expect_true(all(names(edgar) == c("edgar_nox_total", "edgar_so2_total")))
  })
})

testthat::test_that("calculate_edgar extracts EDGAR raster values", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    raster_path <- file.path(".", "edgar_2021_total_emi.tif")
    write_edgar_fixture(raster_path)
    edgar <- process_edgar(path = raster_path)
    locs <- data.frame(
      site_id = c("a", "b"),
      lon = c(-79.5, -77.5),
      lat = c(36.5, 35.5)
    )

    testthat::expect_no_error(
      edgar_vals <- calculate_edgar(
        from = edgar,
        locs = locs,
        locs_id = "site_id",
        radius = 0
      )
    )
    testthat::expect_true(is.data.frame(edgar_vals))
    testthat::expect_true("site_id" %in% names(edgar_vals))
    testthat::expect_true(any(grepl("^edgar_", names(edgar_vals))))

    testthat::expect_no_warning(
      edgar_buf <- calculate_edgar(
        from = edgar,
        locs = locs,
        locs_id = "site_id",
        radius = 1000
      )
    )
    testthat::expect_true(is.data.frame(edgar_buf))
    testthat::expect_true(any(grepl("_1000$", names(edgar_buf))))

    testthat::expect_no_error(
      edgar_geom <- calculate_edgar(
        from = edgar,
        locs = locs,
        locs_id = "site_id",
        radius = 0,
        geom = "terra"
      )
    )
    testthat::expect_s4_class(edgar_geom, "SpatVector")
    testthat::expect_equal(terra::nrow(edgar_geom), nrow(locs))
  })
})

testthat::test_that("calculate_edgar retains locs_id for sf inputs", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    raster_path <- file.path(".", "edgar_2021_total_emi.tif")
    write_edgar_fixture(raster_path)
    edgar <- process_edgar(path = raster_path)

    locs_df <- data.frame(
      site_id = c("a", "b"),
      lon = c(-79.5, -77.5),
      lat = c(36.5, 35.5)
    )
    locs_sf <- sf::st_as_sf(locs_df, coords = c("lon", "lat"), crs = 4326)

    testthat::expect_no_error(
      out <- calculate_edgar(
        from = edgar,
        locs = locs_sf,
        locs_id = "site_id",
        radius = 0,
        geom = "sf"
      )
    )
    testthat::expect_true("site_id" %in% names(out))
    testthat::expect_equal(nrow(out), 2)
  })
})

testthat::test_that("calculate_edgar handles empty sf locations", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    raster_path <- file.path(".", "edgar_2021_total_emi.tif")
    write_edgar_fixture(raster_path)
    edgar <- process_edgar(path = raster_path)

    locs_empty <- data.frame(
      site_id = character(),
      lon = numeric(),
      lat = numeric()
    )
    locs_empty <- sf::st_as_sf(locs_empty, coords = c("lon", "lat"), crs = 4326)

    testthat::expect_no_error(
      out <- calculate_edgar(
        from = edgar,
        locs = locs_empty,
        locs_id = "site_id",
        radius = 0,
        geom = "sf"
      )
    )
    testthat::expect_s3_class(out, "sf")
    testthat::expect_equal(nrow(out), 0)
  })
})

################################################################################
##### comprehensive EDGAR integration coverage

testthat::test_that("download_edgar builds the full 25-VOC matrix", {
  directory_to_save <- paste0(tempdir(), "/edgar_voc_matrix/")
  voc_values <- 1:25

  result <- edgar_discover(
    version = "8.1_voc",
    voc = voc_values,
    sector_voc = "AGRICULTURE",
    year_range = 2021,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )

  expected_urls <- paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/",
    "v81_FT2022_VOC_spec/voc",
    voc_values,
    "/bkl_AGRICULTURE/emi_nc/",
    "v8.1_FT2022_VOC_spec_voc",
    voc_values,
    "_2021_bkl_AGRICULTURE_emi_nc.zip"
  )

  testthat::expect_equal(result$n_files, 25)
  testthat::expect_length(result$urls, 25)
  testthat::expect_length(result$destfiles, 25)
  testthat::expect_setequal(result$urls, expected_urls)
  testthat::expect_true(all(grepl(
    "^.+/zip_files/edgar_voc_",
    result$destfiles
  )))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar validates live VOC URLs for all 25 groups", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  directory_to_save <- paste0(tempdir(), "/edgar_voc_live/")
  voc_values <- 1:25

  result <- edgar_discover(
    version = "8.1_voc",
    voc = voc_values,
    sector_voc = "AGRICULTURE",
    year_range = 2021,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )
  result_totals <- edgar_discover(
    version = "8.1_voc",
    voc = voc_values,
    sector_voc = NULL,
    year_range = NULL,
    directory_to_save = directory_to_save,
    acknowledgement = TRUE
  )

  status_sector <- vapply(result$urls, amadeus::check_url_status, logical(1))
  status_totals <- vapply(
    result_totals$urls,
    amadeus::check_url_status,
    logical(1)
  )

  testthat::expect_true(
    all(status_sector),
    info = paste(
      "Unavailable sector URLs for VOC:",
      paste(voc_values[!status_sector], collapse = ", ")
    )
  )
  testthat::expect_true(
    all(status_totals),
    info = paste(
      "Unavailable totals URLs for VOC:",
      paste(voc_values[!status_totals], collapse = ", ")
    )
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_edgar discovery feeds process_edgar and calculate_edgar", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    discovery_cases <- list(
      yearly_sector = list(
        species = "CO",
        temp_res = "yearly",
        sector_yearly = "ENE",
        year_range = 2021
      ),
      yearly_totals = list(
        species = "SO2",
        temp_res = "yearly",
        sector_yearly = NULL,
        year_range = 2021
      ),
      monthly_sector = list(
        species = "SO2",
        temp_res = "monthly",
        sector_monthly = "BUILDINGS"
      ),
      monthly_totals = list(
        species = "PM2.5",
        temp_res = "monthly"
      ),
      timeseries = list(
        species = "NOx",
        temp_res = "timeseries"
      )
    )

    locs <- data.frame(
      site_id = c("a", "b"),
      lon = c(-79.5, -77.5),
      lat = c(36.5, 35.5)
    )

    for (case_name in names(discovery_cases)) {
      discovery <- do.call(
        edgar_discover,
        c(
          discovery_cases[[case_name]],
          list(directory_to_save = ".", acknowledgement = TRUE)
        )
      )
      data_dir <- file.path(".", case_name)
      write_edgar_fixtures(discovery$destfiles, data_dir)
      processed <- process_edgar(path = data_dir)

      testthat::expect_s4_class(processed, "SpatRaster")
      testthat::expect_equal(terra::nlyr(processed), discovery$n_files)

      calc_zero <- calculate_edgar(
        from = processed,
        locs = locs,
        locs_id = "site_id",
        radius = 0
      )
      calc_buf <- calculate_edgar(
        from = processed,
        locs = locs,
        locs_id = "site_id",
        radius = 1000
      )

      testthat::expect_true(is.data.frame(calc_zero))
      testthat::expect_true(is.data.frame(calc_buf))
      testthat::expect_equal(ncol(calc_zero) - 1, discovery$n_files)
      testthat::expect_equal(ncol(calc_buf) - 1, discovery$n_files)
      testthat::expect_true(all(grepl("_0$", names(calc_zero)[-1])))
      testthat::expect_true(all(grepl("_1000$", names(calc_buf)[-1])))
    }
  })
})

testthat::test_that("all 25 VOC groups feed through process_edgar and calculate_edgar", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    voc_values <- 1:25
    discovery <- edgar_discover(
      version = "8.1_voc",
      voc = voc_values,
      sector_voc = "AGRICULTURE",
      year_range = 2021,
      directory_to_save = ".",
      acknowledgement = TRUE
    )

    write_edgar_fixtures(discovery$destfiles, "./voc_all")
    processed <- process_edgar(path = "./voc_all")
    locs <- data.frame(
      site_id = c("a", "b"),
      lon = c(-79.5, -77.5),
      lat = c(36.5, 35.5)
    )

    calc_zero <- calculate_edgar(
      from = processed,
      locs = locs,
      locs_id = "site_id",
      radius = 0
    )
    calc_buf <- calculate_edgar(
      from = processed,
      locs = locs,
      locs_id = "site_id",
      radius = 1000
    )

    testthat::expect_equal(discovery$n_files, 25)
    testthat::expect_equal(terra::nlyr(processed), 25)
    testthat::expect_equal(ncol(calc_zero) - 1, 25)
    testthat::expect_equal(ncol(calc_buf) - 1, 25)
    testthat::expect_true(all(grepl("voc", names(calc_zero)[-1], fixed = TRUE)))
    testthat::expect_true(all(grepl("_1000$", names(calc_buf)[-1])))
  })
})

################################################################################
##### live EDGAR download integration coverage

testthat::test_that("live yearly EDGAR download feeds process_edgar and calculate_edgar", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    live <- run_live_edgar_chain(
      species = "CO",
      temp_res = "yearly",
      sector_yearly = "ENE",
      year_range = 2021
    )

    testthat::expect_gte(length(live$zip_files), 1)
    testthat::expect_gte(length(live$raster_files), 1)
    testthat::expect_s4_class(live$processed, "SpatRaster")
    testthat::expect_true(is.data.frame(live$calc_zero))
    testthat::expect_true(is.data.frame(live$calc_buf))
    testthat::expect_gte(ncol(live$calc_zero) - 1, 1)
    testthat::expect_gte(ncol(live$calc_buf) - 1, 1)
  })
})

testthat::test_that("live monthly EDGAR download feeds process_edgar and calculate_edgar", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    live <- run_live_edgar_chain(
      species = "SO2",
      temp_res = "monthly",
      sector_monthly = "BUILDINGS"
    )

    testthat::expect_gte(length(live$zip_files), 1)
    testthat::expect_gte(length(live$raster_files), 1)
    testthat::expect_s4_class(live$processed, "SpatRaster")
    testthat::expect_true(is.data.frame(live$calc_zero))
    testthat::expect_true(is.data.frame(live$calc_buf))
    testthat::expect_gte(ncol(live$calc_zero) - 1, 1)
    testthat::expect_gte(ncol(live$calc_buf) - 1, 1)
  })
})

testthat::test_that("live timeseries EDGAR download feeds process_edgar and calculate_edgar", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    live <- run_live_edgar_chain(
      species = "NOx",
      temp_res = "timeseries"
    )

    testthat::expect_gte(length(live$zip_files), 1)
    testthat::expect_gte(length(live$raster_files), 1)
    testthat::expect_s4_class(live$processed, "SpatRaster")
    testthat::expect_true(is.data.frame(live$calc_zero))
    testthat::expect_true(is.data.frame(live$calc_buf))
    testthat::expect_gte(ncol(live$calc_zero) - 1, 1)
    testthat::expect_gte(ncol(live$calc_buf) - 1, 1)
  })
})

testthat::test_that("live all-25 VOC EDGAR downloads feed process_edgar and calculate_edgar", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  withr::with_tempdir({
    live <- run_live_edgar_chain(
      version = "8.1_voc",
      voc = 1:25,
      sector_voc = "AGRICULTURE",
      year_range = 2021
    )

    testthat::expect_equal(length(live$zip_files), 25)
    testthat::expect_gte(length(live$raster_files), 25)
    testthat::expect_s4_class(live$processed, "SpatRaster")
    testthat::expect_gte(terra::nlyr(live$processed), 25)
    testthat::expect_gte(ncol(live$calc_zero) - 1, 25)
    testthat::expect_gte(ncol(live$calc_buf) - 1, 25)
    for (voc_idx in 1:25) {
      testthat::expect_true(
        any(grepl(paste0("voc", voc_idx, "_"), names(live$processed))),
        info = sprintf("Missing processed layer for VOC group %d", voc_idx)
      )
    }
  })
})
