testthat::test_that(
  "download_data(dataset_name=drought aliases): dispatches with the correct source",
  {
    calls <- list()
    testthat::local_mocked_bindings(
      download_drought = function(source = "spei", ...) {
        calls[[length(calls) + 1L]] <<- source
        source
      },
      .package = "amadeus"
    )

    for (source in c("spei", "eddi", "usdm")) {
      result <- download_data(
        dataset_name = source,
        directory_to_save = withr::local_tempdir(),
        acknowledgement = TRUE
      )
      testthat::expect_identical(result, source)
    }

    result <- download_data(
      dataset_name = "drought",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      source = "usdm"
    )

    testthat::expect_identical(result, "usdm")
    testthat::expect_identical(
      unlist(calls, use.names = FALSE),
      c("spei", "eddi", "usdm", "usdm")
    )
  }
)

testthat::test_that(
  "download_data(dataset_name=cropscape aliases): dispatches to download_cropscape",
  {
    calls <- list()
    testthat::local_mocked_bindings(
      download_cropscape = function(year, source, ...) {
        calls[[length(calls) + 1L]] <<- list(year = year, source = source)
        paste(year, source, sep = ":")
      },
      .package = "amadeus"
    )

    cropscape_result <- download_data(
      dataset_name = "cropscape",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      year = 2019,
      source = "GMU"
    )
    cdl_result <- download_data(
      dataset_name = "cdl",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      year = 2020,
      source = "USDA"
    )

    testthat::expect_identical(cropscape_result, "2019:GMU")
    testthat::expect_identical(cdl_result, "2020:USDA")
    testthat::expect_length(calls, 2L)
    testthat::expect_identical(calls[[1L]], list(year = 2019, source = "GMU"))
    testthat::expect_identical(calls[[2L]], list(year = 2020, source = "USDA"))
  }
)
