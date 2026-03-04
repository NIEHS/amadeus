################################################################################
##### unit and integration tests for EDGAR functions

# Helper: call download_edgar with download=FALSE and suppress the deprecation
# warning, returning the result list.
edgar_discover <- function(...) {
  suppressWarnings(
    amadeus::download_edgar(..., download = FALSE, unzip = FALSE)
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
      call_idx > 1L  # First URL is invalid (FALSE), rest are valid (TRUE)
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
