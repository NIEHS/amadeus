################################################################################
##### Fixtures and mocked tests for mirai-enabled downloads.

################################################################################
##### Local summary fixtures and daemon mocks.
mirai_summary_fixture <- function(
  success = 0,
  failed = 0,
  skipped = 0L,
  failed_urls = character(0),
  failed_files = character(0)
) {
  # ----------------------------------------------------------------------------
  # Match the sequential runner's count types and empty failure vectors.
  list(
    success = success,
    failed = failed,
    skipped = skipped,
    failed_urls = failed_urls,
    failed_files = failed_files
  )
}

local_mirai_download_mocks <- function(
  results,
  daemons = 2L,
  .env = parent.frame()
) {
  # ----------------------------------------------------------------------------
  # Capture dispatch arguments without launching processes or running requests.
  calls <- new.env(parent = emptyenv())
  calls$jobs <- vector("list", length(results))
  testthat::local_mocked_bindings(
    daemons_set = function(.compute = NULL) {
      calls$detection_profile <- .compute
      daemons > 0L
    },
    require_daemons = function(.compute = NULL) {
      calls$required_profile <- .compute
      invisible(TRUE)
    },
    nextget = function(x, .compute = "default") {
      calls$count_argument <- x
      calls$count_profile <- .compute
      daemons
    },
    mirai_map = function(.x, .f, ..., .args = list(), .compute = NULL) {
      calls$map_input <- .x
      calls$map_function <- .f
      calls$map_args <- .args
      calls$map_profile <- .compute
      calls$map_dots <- list(...)
      calls$jobs
    },
    collect_mirai = function(x, options = NULL) {
      calls$collected_jobs <- x
      calls$collect_options <- options
      results
    },
    daemons = function(...) stop("Unexpected daemon creation or shutdown."),
    .package = "mirai",
    .env = .env
  )
  calls
}

################################################################################
##### download_run: validation before daemon detection.
testthat::test_that("download_run(urls/destfiles=invalid): rejects inputs before dispatch", {
  # ----------------------------------------------------------------------------
  # Validation must finish before consulting either execution backend.
  testthat::local_mocked_bindings(
    daemons_set = function(...) stop("Unexpected daemon lookup."),
    .package = "mirai"
  )
  cases <- list(
    list(urls = NULL, destfiles = "file.nc", argument = "urls"),
    list(urls = character(0), destfiles = character(0), argument = "urls"),
    list(urls = NA_character_, destfiles = "file.nc", argument = "urls"),
    list(urls = 1, destfiles = "file.nc", argument = "urls"),
    list(
      urls = "https://example.com/a.nc",
      destfiles = NULL,
      argument = "destfiles"
    ),
    list(
      urls = "https://example.com/a.nc",
      destfiles = NA_character_,
      argument = "destfiles"
    ),
    list(
      urls = "https://example.com/a.nc",
      destfiles = c("a.nc", "b.nc"),
      argument = "destfiles"
    )
  )
  # ----------------------------------------------------------------------------
  # Each invalid argument should produce its own assertion diagnostic.
  for (case in cases) {
    testthat::expect_error(
      amadeus::download_run(urls = case$urls, destfiles = case$destfiles),
      regexp = case$argument
    )
  }
})

################################################################################
##### download_run: sequential fallback and argument forwarding.
testthat::test_that("download_run(show_progress=FALSE): forwards options without daemons", {
  # ----------------------------------------------------------------------------
  # Give every optional argument a non-default value to detect lost options.
  args <- list(
    urls = "https://example.com/a.nc",
    destfiles = file.path(withr::local_tempdir(), "a.nc"),
    token = "fixture-token",
    show_progress = FALSE,
    max_tries = 3L,
    rate_limit = 0.5,
    timeout = 17,
    http_version = 2L
  )
  expected <- mirai_summary_fixture(success = 1)
  calls <- local_mirai_download_mocks(results = list(), daemons = 0L)
  testthat::local_mocked_bindings(
    download_run_method = function(...) {
      calls$sequential_args <- list(...)
      invisible(expected)
    },
    download_run_mirai = function(...) stop("Unexpected concurrent runner."),
    .package = "amadeus"
  )
  # ----------------------------------------------------------------------------
  # Preserve the sequential summary, its visibility, and every argument.
  result <- withVisible(do.call(amadeus::download_run, args))
  testthat::expect_identical(calls$detection_profile, "default")
  testthat::expect_identical(calls$sequential_args, args)
  testthat::expect_identical(result$value, expected)
  testthat::expect_false(result$visible)
})

################################################################################
##### download_run: concurrent dispatch, profiles, and fixed request options.
testthat::test_that("download_run(show_progress=TRUE): maps pairs and scales the worker interval", {
  # ----------------------------------------------------------------------------
  # Different names must not reorder URL/destination pairs in the map input.
  urls <- c(
    second = "https://example.com/b.nc",
    first = "https://example.com/a.nc"
  )
  destfiles <- stats::setNames(
    file.path(withr::local_tempdir(), c("b.nc", "a.nc")),
    c("destination_b", "destination_a")
  )
  calls <- local_mirai_download_mocks(list(
    mirai_summary_fixture(success = 1),
    mirai_summary_fixture(success = 1)
  ))
  # ----------------------------------------------------------------------------
  # Exercise the real dispatcher and concurrent runner against daemon mocks.
  result <- withVisible(suppressMessages(amadeus::download_run(
    urls = urls,
    destfiles = destfiles,
    token = "fixture-token",
    show_progress = TRUE,
    max_tries = 3L,
    rate_limit = 0.5,
    timeout = 17,
    http_version = 2L
  )))
  # ----------------------------------------------------------------------------
  # All daemon operations must target the same explicit profile.
  testthat::expect_identical(calls$detection_profile, "default")
  testthat::expect_identical(calls$required_profile, "default")
  testthat::expect_identical(calls$count_profile, "default")
  testthat::expect_identical(calls$map_profile, "default")
  testthat::expect_identical(calls$count_argument, "n")
  # ----------------------------------------------------------------------------
  # Only paired URLs and paths vary; request options remain fixed per worker.
  testthat::expect_identical(
    calls$map_input,
    data.frame(
      urls = unname(urls),
      destfiles = unname(destfiles)
    )
  )
  testthat::expect_identical(calls$map_function, amadeus::download_run_method)
  testthat::expect_identical(
    calls$map_args,
    list(
      token = "fixture-token",
      show_progress = FALSE,
      max_tries = 3L,
      rate_limit = 1,
      timeout = 17,
      http_version = 2L
    )
  )
  testthat::expect_length(calls$map_dots, 0L)
  # ----------------------------------------------------------------------------
  # Collect every job without early stopping and preserve the summary types.
  testthat::expect_identical(calls$collected_jobs, calls$jobs)
  testthat::expect_null(calls$collect_options)
  testthat::expect_identical(result$value, mirai_summary_fixture(success = 2))
  testthat::expect_false(result$visible)
})

################################################################################
##### download_run: summaries for existing files.
testthat::test_that("download_run(destfiles=existing): matches sequential skipped-file types", {
  # ----------------------------------------------------------------------------
  # Existing non-empty files give a real sequential summary without requests.
  destfiles <- file.path(withr::local_tempdir(), c("a.nc", "b.nc"))
  urls <- paste0("https://example.com/", basename(destfiles))
  for (destfile in destfiles) {
    writeLines("existing fixture", destfile)
  }
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      stop("Unexpected request for an existing file.")
    },
    .package = "httr2"
  )
  sequential <- withVisible(suppressMessages(amadeus::download_run_method(
    urls = urls,
    destfiles = destfiles,
    show_progress = FALSE
  )))
  # ----------------------------------------------------------------------------
  # Aggregate the per-file skipped summaries returned by the mocked workers.
  local_mirai_download_mocks(list(
    mirai_summary_fixture(skipped = 1L),
    mirai_summary_fixture(skipped = 1L)
  ))
  concurrent <- withVisible(suppressMessages(amadeus::download_run(
    urls = urls,
    destfiles = destfiles
  )))
  # ----------------------------------------------------------------------------
  # Check exact identity, including integer counts and character(0) failures.
  testthat::expect_identical(
    sequential$value,
    mirai_summary_fixture(skipped = 2L)
  )
  testthat::expect_identical(concurrent, sequential)
})

################################################################################
##### download_run_mirai: handled failures remain in the combined summary.
testthat::test_that("download_run_mirai(daemons=2): combines success, failure, and skipped counts", {
  # ----------------------------------------------------------------------------
  # Keep failure URLs and basenames in input order when combining workers.
  destfiles <- file.path(
    withr::local_tempdir(),
    c("success.nc", "failed_b.nc", "skipped.nc", "failed_a.nc")
  )
  urls <- paste0("https://example.com/", basename(destfiles))
  local_mirai_download_mocks(list(
    mirai_summary_fixture(success = 1),
    mirai_summary_fixture(
      failed = 1,
      failed_urls = urls[2],
      failed_files = basename(destfiles[2])
    ),
    mirai_summary_fixture(skipped = 1L),
    mirai_summary_fixture(
      failed = 1,
      failed_urls = urls[4],
      failed_files = basename(destfiles[4])
    )
  ))
  # ----------------------------------------------------------------------------
  # Handled request failures warn normally and do not trigger a worker error.
  testthat::expect_warning(
    result <- withVisible(suppressMessages(amadeus:::download_run_mirai(
      urls = urls,
      destfiles = destfiles,
      timeout = 60,
      http_version = NULL,
      daemons = 2L
    ))),
    regexp = "2 file(s) failed to download:\n  failed_b.nc\n  failed_a.nc",
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Preserve all counts, failure details, and invisible return behavior.
  testthat::expect_identical(
    result$value,
    mirai_summary_fixture(
      success = 1,
      failed = 2,
      skipped = 1L,
      failed_urls = urls[c(2, 4)],
      failed_files = basename(destfiles[c(2, 4)])
    )
  )
  testthat::expect_false(result$visible)
})

################################################################################
##### download_run_mirai: unexpected worker errors after collection.
testthat::test_that("download_run_mirai(daemons=2): reports each worker error and destination", {
  # ----------------------------------------------------------------------------
  # Mix a valid summary with an R exception and a daemon connection failure.
  destfiles <- file.path(
    withr::local_tempdir(),
    c("success.nc", "exception.nc", "disconnected.nc")
  )
  urls <- paste0("https://example.com/", basename(destfiles))
  calls <- local_mirai_download_mocks(list(
    mirai_summary_fixture(success = 1),
    structure(
      "Worker helper unavailable",
      class = c("miraiError", "errorValue")
    ),
    structure(19L, class = "errorValue")
  ))
  # ----------------------------------------------------------------------------
  # Use the real mirai error classifier and require both original diagnostics.
  testthat::expect_error(
    suppressMessages(amadeus:::download_run_mirai(
      urls = urls,
      destfiles = destfiles,
      timeout = 60,
      http_version = NULL,
      daemons = 2L
    )),
    regexp = paste(
      "Download worker errors:",
      paste0("  ", destfiles[2], ": Worker helper unavailable"),
      paste0("  ", destfiles[3], ": 19"),
      sep = "\n"
    ),
    fixed = TRUE
  )
  # ----------------------------------------------------------------------------
  # Collect without the .stop option before reporting worker errors.
  testthat::expect_identical(calls$collected_jobs, calls$jobs)
  testthat::expect_null(calls$collect_options)
})
