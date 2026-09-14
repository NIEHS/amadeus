################################################################################
##### unit and integration tests for CropScape functions

################################################################################
##### download_cropscape
testthat::test_that("download_cropscape (no errors - GMU)", {
  withr::local_package("httr2")
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  result <- suppressWarnings(
    download_cropscape(
      year = year,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  testthat::expect_true(grepl("^https://", result$urls))
  testthat::expect_true(grepl(as.character(year), result$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (no errors - USDA)", {
  withr::local_package("httr2")
  year <- 2010
  directory_to_save <- paste0(tempdir(), "/cps/")

  result <- suppressWarnings(
    download_cropscape(
      year = year,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$n_files, 1)
  testthat::expect_true(grepl("^https://", result$urls))
  testthat::expect_true(grepl(as.character(year), result$urls))

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape deprecation warnings", {
  withr::local_package("httr2")
  directory_to_save <- paste0(tempdir(), "/cps_dep/")

  testthat::expect_warning(
    download_cropscape(
      year = 2010,
      source = "GMU",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    regexp = "download=FALSE is deprecated"
  )

  testthat::expect_warning(
    download_cropscape(
      year = 2010,
      source = "USDA",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = TRUE
    ),
    regexp = "remove_command.*deprecated"
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_cropscape (expected errors)", {
  # invalid source
  testthat::expect_error(download_cropscape(year = 2020, source = "CMU"))
  # GMU year too early
  testthat::expect_error(
    download_cropscape(year = 1996, source = "GMU")
  )
  # USDA year too early
  testthat::expect_error(
    download_cropscape(year = 2000, source = "USDA")
  )
})


testthat::test_that("download_cropscape mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_unzip = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_cropscape(
          year = 2020,
          source = "GMU",
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

testthat::test_that("download_cropscape extracts archives into per-file directories", {
  extracted <- list()
  testthat::local_mocked_bindings(
    archive_extract = function(archive, file = NULL, dir = ".", ...) {
      extracted[[length(extracted) + 1]] <<- list(archive = archive, dir = dir)
      invisible(NULL)
    },
    .package = "archive"
  )
  testthat::local_mocked_bindings(
    download_run_method = function(urls, destfiles, ...) {
      vapply(destfiles, file.create, logical(1))
      invisible(NULL)
    },
    download_hash = function(hash, dir) NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    suppressWarnings(
      suppressMessages(
        download_cropscape(
          year = 2020,
          source = "GMU",
          directory_to_save = ".",
          acknowledgement = TRUE,
          unzip = TRUE,
          show_progress = FALSE
        )
      )
    )
    testthat::expect_length(extracted, 1)
    testthat::expect_true(dir.exists(extracted[[1]]$dir))
    testthat::expect_match(
      basename(extracted[[1]]$archive),
      "2020_cdls\\.tar\\.gz$"
    )
    testthat::expect_match(basename(extracted[[1]]$dir), "2020_cdls$")
  })
})

################################################################################
##### process_cropscape
testthat::test_that("process_cropscape", {
  # Set up test data
  withr::local_package("terra")
  filepath <-
    testthat::test_path("..", "testdata/cropscape/cdl_30m_r_nc_2019_sub.tif")
  dirpath <- testthat::test_path("..", "testdata/cropscape")
  year <- 2019

  # Call the function
  testthat::expect_no_error(result <- process_cropscape(filepath, year))
  testthat::expect_no_error(process_cropscape(dirpath, year))

  # test with cropping extent
  testthat::expect_no_error(
    result_ext <- process_cropscape(
      filepath,
      year,
      extent = terra::ext(result)
    )
  )

  # Check the return type
  testthat::expect_true(inherits(result, "SpatRaster"))

  # Check the metadata
  result_meta <- terra::metags(result)
  testthat::expect_equal(
    unname(result_meta[result_meta$name == "year", "value"]),
    as.character(year)
  )

  # error cases
  testthat::expect_error(process_cropscape(path = 0, year = "MILLENNIUM"))
  testthat::expect_error(
    process_cropscape(path = "/home/some/path", year = "MILLENNIUM")
  )
})

## calculate_cropscape
testthat::test_that("calculate_cropscape", {
  # Set up test data
  withr::local_package("terra")
  withr::local_package("sf")

  filepath <-
    testthat::test_path("..", "testdata/cropscape/cdl_30m_r_nc_2019_sub.tif")
  dirpath <- testthat::test_path("..", "testdata/cropscape")
  year <- 2019

  # Call the function
  testthat::expect_no_error(crop_rast <- process_cropscape(filepath, year))

  # Check the return type
  testthat::expect_true(inherits(crop_rast, "SpatRaster"))

  # Calculation
  # make a faux location
  locs <- data.frame(site_id = "001", lon = -78.90, lat = 35.97)
  locs_v <- terra::vect(locs, geom = c("lon", "lat"), crs = "epsg:4326")
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_v,
      locs_id = "site_id",
      radius = 300
    )
  )

  # zero radius
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_v,
      locs_id = "site_id",
      radius = 0
    )
  )

  # sf input
  locs_s <- sf::st_as_sf(locs_v)
  testthat::expect_no_error(
    crop_df <- calculate_cropscape(
      crop_rast,
      locs = locs_s,
      locs_id = "site_id",
      radius = 300
    )
  )

  # Check the return type
  testthat::expect_true(inherits(crop_df, "data.frame"))

  # error cases
  testthat::expect_error(
    calculate_cropscape(locs_v, locs = locs_v, locs_id = "site_id"),
    "`from` must be a SpatRaster object."
  )
})

################################################################################
##### Recommended linked mock workflows

testthat::test_that(
  "CropScape linked mock workflows: GMU and USDA archives produce calculated fractions",
  {
    withr::local_package("terra")
    withr::local_package("sf")

    fixture <- normalizePath(
      testthat::test_path(
        "..",
        "testdata",
        "cropscape",
        "cdl_30m_r_nc_2019_sub.tif"
      ),
      mustWork = TRUE
    )
    state <- new.env(parent = emptyenv())
    state$downloaded_archives <- character()
    state$extraction_directories <- character()

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        testthat::expect_match(
          urls,
          "(2019_cdls\\.tar\\.gz|2019_30m_cdls\\.zip)$"
        )
        state$downloaded_archives <- c(
          state$downloaded_archives,
          destfiles
        )
        vapply(
          destfiles,
          function(destination) {
            writeBin(charToRaw("mock archive"), destination)
            TRUE
          },
          logical(1)
        )
        list(
          success = length(destfiles),
          failed = 0L,
          skipped = 0L
        )
      },
      download_hash = function(...) invisible(NULL)
    )
    testthat::local_mocked_bindings(
      archive_extract = function(archive, file = NULL, dir = ".", ...) {
        state$extraction_directories <- c(
          state$extraction_directories,
          dir
        )
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        destination <- file.path(dir, basename(fixture))
        copied <- file.copy(fixture, destination, overwrite = TRUE)
        testthat::expect_true(copied)
        invisible(NULL)
      },
      .package = "archive"
    )

    for (source in c("GMU", "USDA")) {
      download_directory <- withr::local_tempdir(
        pattern = paste0("cropscape-", tolower(source), "-")
      )
      downloads_before <- length(state$downloaded_archives)
      extractions_before <- length(state$extraction_directories)

      suppressMessages(download_data(
        dataset_name = "cropscape",
        directory_to_save = download_directory,
        acknowledgement = TRUE,
        year = 2019,
        source = source,
        show_progress = FALSE,
        rate_limit = 0,
        unzip = TRUE
      ))

      testthat::expect_equal(
        length(state$downloaded_archives),
        downloads_before + 1L
      )
      testthat::expect_equal(
        length(state$extraction_directories),
        extractions_before + 1L
      )
      archive <- state$downloaded_archives[[downloads_before + 1L]]
      extraction_directory <-
        state$extraction_directories[[extractions_before + 1L]]
      expected_archive <- if (source == "GMU") {
        "2019_cdls.tar.gz"
      } else {
        "2019_30m_cdls.zip"
      }
      expected_extraction_directory <- if (source == "GMU") {
        "2019_cdls"
      } else {
        "2019_30m_cdls"
      }

      testthat::expect_identical(basename(archive), expected_archive)
      testthat::expect_true(file.exists(archive))
      testthat::expect_gt(file.size(archive), 0)
      testthat::expect_identical(
        basename(extraction_directory),
        expected_extraction_directory
      )
      extracted_tiff <- file.path(extraction_directory, basename(fixture))
      testthat::expect_true(file.exists(extracted_tiff))
      testthat::expect_gt(file.size(extracted_tiff), 0)

      processed <- suppressMessages(process_covariates(
        covariate = "cropscape",
        path = extraction_directory,
        year = 2019
      ))

      metadata <- terra::metags(processed)
      testthat::expect_s4_class(processed, "SpatRaster")
      testthat::expect_equal(terra::nlyr(processed), 1L)
      testthat::expect_gt(terra::ncell(processed), 0L)
      testthat::expect_true(terra::hasValues(processed))
      testthat::expect_true(nzchar(terra::crs(processed)))
      testthat::expect_identical(
        unname(metadata[metadata$name == "year", "value"]),
        "2019"
      )
      testthat::expect_true(any(!is.na(terra::values(processed, mat = FALSE))))

      locations <- terra::vect(
        data.frame(site_id = "site_01", lon = -78.90, lat = 35.97),
        geom = c("lon", "lat"),
        crs = "EPSG:4326"
      )
      result <- suppressMessages(calculate_covariates(
        covariate = "cropscape",
        from = processed,
        locs = locations,
        locs_id = "site_id",
        radius = 300
      ))

      fraction_columns <- grep(
        "^cropscape_300_",
        names(result),
        value = TRUE
      )
      fractions <- unlist(result[fraction_columns], use.names = FALSE)
      testthat::expect_s3_class(result, "data.frame")
      testthat::expect_equal(nrow(result), 1L)
      testthat::expect_identical(result$site_id, "site_01")
      testthat::expect_gt(length(fraction_columns), 0L)
      testthat::expect_type(fractions, "double")
      testthat::expect_false(anyNA(fractions))
      testthat::expect_true(all(fractions >= 0 & fractions <= 1))
      testthat::expect_equal(sum(fractions), 1, tolerance = 1e-6)
    }
  }
)
