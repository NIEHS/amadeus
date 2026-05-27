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
    "download_drought(source='spei', timescale=1): ",
    "upstream SPEI mirrors are reachable"
  ),
  {
    skip_if_no_live_tests()
    # SPEI ships a single multi-gigabyte netCDF per timescale from
    # spei.csic.es. Pulling the full file from a GitHub Actions runner
    # is unreliable (frequent partial transfers / connection resets)
    # and a single test run can consume tens of minutes plus several GB
    # of bandwidth. Treat this as a connectivity smoke test against the
    # candidate-URL list compiled in download_drought() instead.
    ts_str <- sprintf("%02d", 1L)
    spei_file <- paste0("spei", ts_str, ".nc")
    candidates <- c(
      paste0("https://spei.csic.es/spei_database_2_11/nc/", spei_file),
      paste0("https://spei.csic.es/spei_database_2_10/nc/", spei_file),
      paste0("https://spei.csic.es/files/", spei_file)
    )
    reachable <- tryCatch(
      vapply(candidates, amadeus::check_url_status, logical(1)),
      error = function(e) {
        skip_if_transient_live_issue(e)
        stop(e)
      }
    )
    if (!any(reachable)) {
      testthat::skip(
        "All SPEI upstream mirrors unreachable (transient/upstream issue)."
      )
    }
    testthat::expect_true(any(reachable))
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
