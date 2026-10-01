################################################################################
##### unit and integration tests for NOAA HRRR functions

################################################################################
testthat::test_that("download_hrrr (single date)", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/hrrr/")

  # Expect deprecation warning
  testthat::expect_warning(
    download_data(
      dataset_name = "hrrr",
      date = "2021-05-04",
      product = "surface",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    ),
    "Setting download=FALSE is deprecated"
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_hrrr (expected errors)", {
  testthat::expect_error(
    download_data(
      dataset_name = "hrrr",
      product = "surface",
      date = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    )
  )
})

testthat::test_that("hrrr_variable (expected errors)", {
  # expected error due to unrecognized variable name
  testthat::expect_error(
    hrrr_variable("uNrEcOgNiZed")
  )
})

testthat::test_that("download_hrrr without download=FALSE", {
  skip_on_cran()
  skip_if_offline()

  withr::local_package("httr2")
  withr::local_package("stringr")
  directory_to_save <- paste0(tempdir(), "/hrrr_new/")

  # Test without download=FALSE (new httr2 method, no deprecation warning)
  testthat::expect_no_error(
    download_data(
      dataset_name = "hrrr",
      date = "2021-05-04",
      product = "surface",
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    )
  )

  # Check that directory was created
  testthat::expect_true(
    dir.exists(directory_to_save)
  )

  unlink(directory_to_save, recursive = TRUE)
})

testthat::test_that("download_hrrr remove_command deprecation warning", {
  withr::with_tempdir({
    testthat::expect_warning(
      download_hrrr(
        date = "2021-05-04",
        product = "surface",
        directory_to_save = ".",
        acknowledgement = TRUE,
        download = FALSE,
        remove_command = TRUE
      ),
      regexp = "remove_command.*deprecated"
    )
  })
})

testthat::test_that("download_hrrr mock download with hash", {
  testthat::local_mocked_bindings(
    download_run_method = function(...) invisible(NULL),
    download_hash = function(hash, dir) if (isTRUE(hash)) "fakehash" else NULL,
    .package = "amadeus"
  )
  withr::with_tempdir({
    result <- suppressWarnings(
      suppressMessages(
        download_hrrr(
          date = "2021-05-04",
          product = "surface",
          directory_to_save = ".",
          acknowledgement = TRUE,
          download = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})
