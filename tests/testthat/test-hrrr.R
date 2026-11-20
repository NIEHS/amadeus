################################################################################
##### unit and integration tests for NOAA HRRR functions

################################################################################
testthat::test_that("download_hrrr (expected errors)", {
  # input year instead of date
  testthat::expect_error(
    download_hrrr(
      date = "2021",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 0:4L,
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_dates.*exactly 10 characters"
  )
  # invalid cycle runtime (out of bounds)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 25L,
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "cycle_runtime.*<= 23"
  )
  # invalid cycle runtime (character)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = "four",
      forecast_hour = 1:5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "cycle_runtime.*type 'integerish'"
  )

  # invalid forecast hour (out of bounds)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = 500L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "forecast_hour.*<= 48"
  )

  # invalid forecast hour (character)
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = "five",
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "forecast_hour.*type 'integerish'"
  )

  # unknown product
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "4d spacetime",
      sector = "conus",
      cycle_runtime = 4L,
      forecast_hour = 5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_product.*element of set"
  )

  # unknown sector
  testthat::expect_error(
    download_hrrr(
      date = "2021-05-04",
      product = "2d surface",
      sector = "europe",
      cycle_runtime = 4L,
      forecast_hour = 5L,
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path("..", "testdata/", "")
    ),
    regexp = "chr_sector.*element of set"
  )
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
          product = "2d surface",
          sector = "conus",
          cycle_runtime = 0L,
          forecast_hour = 1L,
          directory_to_save = ".",
          acknowledgement = TRUE,
          hash = TRUE
        )
      )
    )
    testthat::expect_equal(result, "fakehash")
  })
})
