################################################################################
# Live network tests for download_drought(). Mocked tests: test-drought.R.
################################################################################

expect_live_download_files <- function(download_expr, dir) {
  result <- withCallingHandlers(
    tryCatch(
      download_expr,
      error = function(e) {
        skip_if_transient_live_issue(e)
        stop(e)
      }
    ),
    warning = function(w) {
      skip_if_transient_live_issue(w)
    }
  )

  if (
    is.list(result) &&
      is.numeric(result$success) &&
      is.numeric(result$failed) &&
      result$success == 0 &&
      result$failed > 0
  ) {
    testthat::skip(sprintf(
      "Transient upstream/live failure: %s file(s) failed to download.",
      result$failed
    ))
  }

  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  testthat::expect_gt(length(files), 0)
  testthat::expect_gt(sum(file.info(files)$size > 0), 0)
}

testthat::test_that(
  paste0(
    "download_drought(source='spei', timescale=1, date=<month>): ",
    "downloads SPEI file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_live_download_files(
      amadeus::download_drought(
        source = "spei",
        date = c("2022-01-01", "2022-01-31"),
        timescale = 1L,
        directory_to_save = dir,
        acknowledgement = TRUE,
        unzip = FALSE
      ),
      dir
    )
  }
)

testthat::test_that(
  paste0(
    "download_drought(source='eddi', timescale=1, date=<Tuesday>): ",
    "downloads EDDI file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_live_download_files(
      amadeus::download_drought(
        source = "eddi",
        date = c("2022-01-04", "2022-01-04"),
        timescale = 1L,
        directory_to_save = dir,
        acknowledgement = TRUE,
        unzip = FALSE
      ),
      dir
    )
  }
)

testthat::test_that(
  paste0(
    "download_drought(source='usdm', date=<Tuesday>): ",
    "downloads USDM file"
  ),
  {
    skip_if_no_live_tests()
    dir <- withr::local_tempdir()
    expect_live_download_files(
      amadeus::download_drought(
        source = "usdm",
        date = c("2022-01-04", "2022-01-04"),
        directory_to_save = dir,
        acknowledgement = TRUE,
        unzip = FALSE
      ),
      dir
    )
  }
)
