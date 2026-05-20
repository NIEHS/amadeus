# Skip helpers for amadeus tests
#
# Centralizes all conditional-skip logic so individual test files don't have
# to remember the right environment variable or credential name.
#
# Usage:
#   testthat::test_that("...", {
#     skip_if_no_live_tests()
#     skip_if_no_credentials("EARTHDATA_TOKEN")
#     ...
#   })

#' Skip unless AMADEUS_LIVE_TESTS env var is set truthy.
#'
#' Live tests perform real network calls against upstream APIs. They are
#' opt-in: set `AMADEUS_LIVE_TESTS=true` (or any non-empty value) in the
#' environment to run them. The scheduled CI workflow
#' `.github/workflows/test-live.yaml` sets this automatically.
#' @keywords internal
skip_if_no_live_tests <- function() {
  if (!nzchar(Sys.getenv("AMADEUS_LIVE_TESTS"))) {
    testthat::skip("Live tests disabled (set AMADEUS_LIVE_TESTS=true to enable).")
  }
  invisible(TRUE)
}

#' Skip if a required credential env var is not set.
#'
#' @param var character(1) Environment variable name (e.g. "EARTHDATA_TOKEN").
#' @keywords internal
skip_if_no_credentials <- function(var) {
  stopifnot(length(var) == 1L, is.character(var))
  if (!nzchar(Sys.getenv(var))) {
    testthat::skip(sprintf("Credential not set: %s.", var))
  }
  invisible(TRUE)
}

#' Skip when a suggested package is not installed.
#'
#' Thin wrapper for consistency with the rest of the skip helpers.
#' @param pkg character(1) Package name.
#' @keywords internal
skip_if_pkg_missing <- function(pkg) {
  testthat::skip_if_not_installed(pkg)
  invisible(TRUE)
}
