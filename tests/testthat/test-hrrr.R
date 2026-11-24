################################################################################
##### unit and integration tests for NOAA HRRR functions

################################################################################
testthat::test_that(
  "process_hrrr(path = positional, accumulation = 1): preserves argument binding",
  {
    testthat::local_mocked_bindings(
      stack_hourly = function(paths, extent, variable, short) {
        list(paths = paths, extent = extent, variable = variable, short = short)
      },
      .package = "amadeus"
    )
    path <- withr::local_tempdir()
    files <- file.path(
      path,
      c(
        "hrrr.20210504.t00z.wrfsfcf00.grib2",
        "hrrr.20210504.t00z.wrfsfcf02.grib2"
      )
    )
    file.create(files)
    extent <- c(-100, -90, 30, 40)
    reference <- "SFC=Ground or water surface; Total precipitation [kg/(m^2)]"
    hourly <- "SFC=Ground or water surface; 01 hr Total precipitation [kg/(m^2)]"

    result <- process_hrrr("2021-05-04", reference, 0, path, extent)
    testthat::expect_identical(result$paths, files[1])
    testthat::expect_identical(result$extent, extent)
    testthat::expect_identical(result$variable, reference)
    testthat::expect_identical(result$short, "APCP_SFC")

    result <- process_hrrr(
      "2021-05-04", hourly, 2, path, extent, accumulation = 1
    )
    testthat::expect_identical(result$paths, files[2])
    testthat::expect_identical(result$extent, extent)
    testthat::expect_identical(result$variable, hourly)
    testthat::expect_identical(result$short, "APCP_SFC")
  }
)

################################################################################
testthat::test_that("download_hrrr(download = FALSE): warns about deprecation", {
  testthat::local_mocked_bindings(
    download_run = function(...) stop("Unexpected download in preview mode"),
    .package = "amadeus"
  )
  directory_to_save <- withr::local_tempdir()
  testthat::expect_warning(
    suppressMessages(
      download_hrrr(
        date = "2021-05-04",
        product = "2d surface",
        cycle_runtime = 0L,
        forecast_hour = 0L,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE,
        download = FALSE
      )
    ),
    regexp = "download=FALSE is deprecated"
  )
})

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
