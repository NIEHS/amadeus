# Reusable mock factories for amadeus download_* tests
#
# These functions return *named lists* of bindings suitable for passing into
# `testthat::local_mocked_bindings(..., .package = "amadeus")` via
# `rlang::inject` or `do.call`.
#
# Pattern at call site:
#
#   testthat::test_that("download_aqs(hash=TRUE): returns hash string", {
#     do.call(
#       testthat::local_mocked_bindings,
#       c(mocks_download_stack(), .package = "amadeus")
#     )
#     ...
#   })
#
# Or, for the most common case, use `local_download_mocks()` below which wraps
# the boilerplate.

#' Common download stack bindings.
#'
#' Provides the network/IO-touching internals used by virtually every
#' `download_*` function. Override any binding by passing a replacement.
#'
#' @param success,failed,skipped integer counts returned by
#'   `download_run_method()`.
#' @param hash_value character(1) value returned by `download_hash()`.
#' @param url_ok logical(1) returned by `check_url_status()`.
#' @param destfile_ok logical(1) returned by `check_destfile()`.
#' @param ... Additional bindings that override or extend the defaults.
#' @return Named list of mock bindings.
#' @keywords internal
mocks_download_stack <- function(success = 1L,
                                 failed = 0L,
                                 skipped = 0L,
                                 hash_value = "hash-ok",
                                 url_ok = TRUE,
                                 destfile_ok = TRUE,
                                 ...) {
  defaults <- list(
    check_url_status     = function(...) url_ok,
    check_destfile       = function(...) destfile_ok,
    download_run_method  = function(...) {
      list(success = success, failed = failed, skipped = skipped)
    },
    download_unzip       = function(...) NULL,
    download_remove_zips = function(...) NULL,
    download_hash        = function(hash, directory) hash_value
  )
  overrides <- list(...)
  defaults[names(overrides)] <- overrides
  defaults
}

#' Bindings for credential/token-based downloads.
#'
#' Returns a token mock plus interactive-prompt mocks used by
#' Earthdata / NASA-style downloads.
#'
#' @param token character(1) returned by `get_token()`.
#' @keywords internal
mocks_token_stack <- function(token = "test-token") {
  list(
    get_token   = function(...) token,
    interactive = function() FALSE,
    readline    = function(prompt = "") token
  )
}

#' Apply the download stack as a `local_mocked_bindings()` call.
#'
#' Convenience wrapper. The bindings are scoped to the calling frame, just
#' like a direct `testthat::local_mocked_bindings()` call.
#'
#' @inheritParams mocks_download_stack
#' @param envir Calling frame; do not set manually.
#' @keywords internal
local_download_mocks <- function(...,
                                 envir = parent.frame()) {
  bindings <- mocks_download_stack(...)
  do.call(
    testthat::local_mocked_bindings,
    c(bindings, list(.package = "amadeus", .env = envir))
  )
  invisible(bindings)
}

#' Apply the token stack as a `local_mocked_bindings()` call.
#' @inheritParams mocks_token_stack
#' @keywords internal
local_token_mocks <- function(token = "test-token",
                              envir = parent.frame()) {
  bindings <- mocks_token_stack(token = token)
  do.call(
    testthat::local_mocked_bindings,
    c(bindings, list(.package = "amadeus", .env = envir))
  )
  invisible(bindings)
}
