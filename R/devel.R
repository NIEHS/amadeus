################################################################################
#' Download files using configured mirai daemons
#' @description
#' Internal concurrent runner used by [download_run()]. Distribute
#' URL/destination pairs across the configured `"default"` compute profile,
#' calling [download_run_method()] once per pair, then collect and combine
#' the results.
#' @param urls character vector. Non-empty vector of URLs to download.
#' @param destfiles character vector. Distinct absolute destination paths,
#'   in the same order and of the same length as `urls`. Paths must be
#'   accessible to both the calling process and all workers.
#' @param token character(1) or NULL. Resolved bearer token added to every
#'   request when supplied. Default is `NULL`.
#' @param max_tries integer(1). Maximum attempts per file request, including
#'   the initial attempt. Must be at least 1. Default is 20.
#' @param rate_limit numeric(1). Request-throttling interval in seconds.
#'   Default is 2. Multiplied by `as.integer(daemons)` before being passed
#'   to each worker.
#' @param timeout numeric(1). Timeout in seconds for each request attempt.
#'   Required; [download_run()] supplies 3600 by default.
#' @param http_version integer(1) or NULL. HTTP version passed to curl via
#'   CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
#'   Required; supply `NULL` to allow automatic negotiation, as
#'   [download_run()] does by default.
#' @param daemons integer(1). Positive number of workers in the fixed
#'   `"default"` pool. Although the default is `NULL`, a count must be supplied.
#'   [download_run()] obtains it with
#'   `mirai::nextget("n", .compute = "default")`.
#' @details
#' The caller manages daemon creation and shutdown. Configure a fixed local
#' pool with `mirai::daemons(n, .compute = "default")` before use and supply
#' the same count as `daemons`. Detection and dispatch always use `"default"`,
#' regardless of the active compute profile. Rate scaling assumes that the
#' pool size remains fixed and matches the supplied count; each process has
#' its own throttle state, and token buckets can still allow bursts.
#'
#' [download_run()] validates URLs and destinations before calling this helper.
#' Direct callers must supply valid inputs. Each task receives one URL and
#' destination, with the remaining request arguments held constant and
#' `show_progress = FALSE`. Workers need compatible `amadeus` code and access
#' to the destination storage; see [download_run()] for development setup.
#'
#' Results are collected without early stopping. Handled request failures
#' remain part of the combined summary. After collection, unexpected worker
#' errors, worker interrupts, or connection failures returned as mirai error
#' values cause an error headed `"Download worker errors:"`, listing each
#' affected destination and its error message or code. This happens before
#' aggregation, so no combined summary is printed or returned in that case.
#' Otherwise, [download_report()] prints the combined totals and warns about
#' failed files in the calling process.
#' @return An invisible list containing numeric counts `success`, `failed`,
#'   and `skipped`, plus character vectors `failed_urls` and `failed_files`.
#'   The latter contains destination basenames. For valid non-empty inputs,
#'   both failure vectors are `character(0)` when no downloads fail.
#' @author Mitchell Manware
#' @seealso [download_run()], [download_run_method()], [download_report()],
#'   [mirai::mirai_map()], [mirai::collect_mirai()]
#' @keywords internal
download_run_mirai <- function(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  max_tries = 20,
  rate_limit = 2,
  timeout,
  http_version,
  daemons = NULL
) {
  # ----------------------------------------------------------------------------
  # Ensure `mirai` daemons are set.
  mirai::require_daemons(.compute = "default")
  # ----------------------------------------------------------------------------
  # Number of daemons as integer.
  int_daemons <- as.integer(daemons)
  # ----------------------------------------------------------------------------
  # Message about `mirai`-enabled download.
  message(sprintf(
    "Concurrent download using %d configrured {mirai} daemons.",
    int_daemons
  ))
  # ----------------------------------------------------------------------------
  # Store URLs and destination files as `data.frame`.
  stopifnot(length(urls) == length(destfiles))
  df_mirai <- data.frame(urls = unname(urls), destfiles = unname(destfiles))
  # ----------------------------------------------------------------------------
  # Dispatch `download_run_method` across available daemons.
  mirai_jobs <- mirai::mirai_map(
    df_mirai,
    amadeus::download_run_method,
    .compute = "default",
    .args = list(
      token = token,
      show_progress = FALSE,
      max_tries = max_tries,
      rate_limit = rate_limit * int_daemons,
      timeout = timeout,
      http_version = http_version
    )
  )
  # ----------------------------------------------------------------------------
  # Collect `mirai` results.
  mirai_results <- mirai::collect_mirai(mirai_jobs)
  # ----------------------------------------------------------------------------
  # Check for and handle `mirai` errors, which disturb the list collection.
  idx_error <- which(vapply(
    mirai_results,
    mirai::is_error_value,
    logical(1)
  ))
  if (length(idx_error) > 0L) {
    chr_errors <- vapply(
      idx_error,
      function(i) {
        sprintf("  %s: %s", destfiles[i], as.character(mirai_results[[i]]))
      },
      character(1)
    )
    stop(
      paste(c("Download worker errors:", chr_errors), collapse = "\n"),
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # Collapse `mirai` results to match `download_run_method` output.
  list_summary <- list(
    success = sum(vapply(mirai_results, `[[`, numeric(1), "success")),
    failed = sum(vapply(mirai_results, `[[`, numeric(1), "failed")),
    skipped = sum(vapply(mirai_results, `[[`, numeric(1), "skipped")),
    failed_urls = unlist(
      lapply(mirai_results, `[[`, "failed_urls"),
      use.names = FALSE
    ),
    failed_files = unlist(
      lapply(mirai_results, `[[`, "failed_files"),
      use.names = FALSE
    )
  )
  # ----------------------------------------------------------------------------
  # Report the combined download totals.
  download_report(list_summary)
  # ----------------------------------------------------------------------------
  # Return the counts and failed file details invisibly.
  invisible(list_summary)
}

################################################################################
#' Report download totals and failed filenames
#' @description
#' Print the numbers of successful, failed, and skipped downloads. When any
#' downloads failed, emit a warning listing their destination basenames.
#' Used by the sequential downloader and by the parent process after
#' concurrent download results have been combined.
#' @param list list. Download summary containing numeric(1) fields `success`,
#'   `failed`, and `skipped`, and a character vector `failed_files` containing
#'   the basenames of failed destinations. Other fields, such as `failed_urls`,
#'   are not used by this helper.
#' @details
#' The summary message is always emitted when this helper is called. A warning
#' is emitted only when `failed > 0`, with `call. = FALSE`. This helper does
#' not validate the summary or inspect downloaded files.
#' @return Used for its messaging side effects; callers should ignore the
#'   return value. The calling downloader returns the download summary.
#' @author Mitchell Manware
#' @seealso [download_run()], [download_run_method()]
#' @examples
#' amadeus:::download_report(list(
#'   success = 2,
#'   failed = 0,
#'   skipped = 1,
#'   failed_files = character(0)
#' ))
#' @keywords internal
download_report <- function(list) {
  # ----------------------------------------------------------------------------
  # Report the combined download totals.
  message(sprintf(
    "\n=== Download Summary ===\n%d succeeded, %d failed, %d skipped\n",
    list$success,
    list$failed,
    list$skipped
  ))
  # ----------------------------------------------------------------------------
  # Warn with the names of files that failed to download.
  if (list$failed > 0) {
    warning(
      sprintf(
        "%d file(s) failed to download:\n  %s\n",
        list$failed,
        paste(list$failed_files, collapse = "\n  ")
      ),
      call. = FALSE
    )
  }
}

################################################################################
#' Download files sequentially or using configured mirai daemons
#' @description
#' Download files using [download_run_method()]. When [mirai::daemons_set()]
#' detects configured daemons in the `"default"` compute profile, distribute
#' URL/destination pairs across those workers and combine their results.
#' Otherwise, download sequentially in the calling process.
#' @param urls character vector. Non-empty vector of URLs to download.
#' @param destfiles character vector. Destination file paths, in the same order
#'   and of the same length as `urls`. Use distinct absolute paths accessible
#'   to both the calling process and all workers for concurrent downloads.
#' @param token character(1) or NULL. Bearer token added to every request when
#'   supplied. Default is `NULL`. Resolve token files or environment variables
#'   with [get_token()] before calling this function.
#' @param show_progress logical(1). Show per-file messages and download progress
#'   bars during sequential downloads. Default is `TRUE`. Ignored during
#'   concurrent downloads, where per-file progress is disabled. Status and
#'   summary messages may still be printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file request, including the
#'   initial attempt. Must be at least 1. Default is 20.
#' @param rate_limit numeric(1). Request-throttling interval in seconds. Default
#'   is 2. Passed unchanged to [download_run_method()] for sequential downloads.
#'   For concurrent downloads, multiplied by the configured daemon count for
#'   the `"default"` compute profile before being passed to each worker.
#' @param timeout numeric(1). Timeout in seconds for each request attempt.
#'   Default is 3600 (one hour). This does not limit the total time across
#'   multiple files or retries.
#' @param http_version integer(1) or NULL. Force HTTP version via curl's
#'   CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
#'   `NULL` (default) lets curl negotiate automatically. Passed unchanged to
#'   [download_run_method()] in both execution modes.
#' @details
#' Daemon creation and shutdown are managed by the caller using
#' [mirai::daemons()]. For concurrent downloads, configure a fixed local pool
#' with `mirai::daemons(n, .compute = "default")` before calling this function.
#' Daemon detection, counting, and task dispatch always use the `"default"`
#' compute profile, regardless of the active profile. Daemon counting and rate
#' scaling assume that the pool still contains the `n` workers configured by
#' that call, with no workers added separately or pool resizing during downloads.
#' Each worker receives one URL/destination pair per task; the remaining request
#' arguments are constant across tasks.
#'
#' Workers must have access to compatible `amadeus` code and its dependencies,
#' including internal helpers such as [download_report()]. Loading development
#' code with `devtools::load_all()` in the calling process does not load it on
#' workers. During development, use [mirai::everywhere()] to load the same
#' package source on workers before submitting downloads. Remote workers need
#' access to the same destination storage as the calling process.
#'
#' Existing non-empty destination files are skipped. Missing or empty files
#' are downloaded, with missing parent directories created as needed. Request
#' handling, retries, and file validation are delegated to
#' [download_run_method()]. Throttling is maintained separately in each
#' process. Scaling the interval by a fixed daemon count targets the combined
#' average request rate; token buckets can still allow bursts.
#'
#' Handled request failures are included in the returned summary. Concurrent
#' results are collected without early stopping. After collection, mirai error
#' values representing unexpected worker errors, worker interrupts, or connection
#' failures are checked before aggregation. If any are present, the function
#' stops with `"Download worker errors:"`, listing each affected destination path
#' and its error message or code. No combined summary is printed or returned in
#' this case. Otherwise, the results are combined and reported by
#' [download_report()] in the calling process, then returned invisibly.
#' @return
#' An invisible list containing:
#' * `success`: number of files downloaded and found to be non-empty.
#' * `failed`: number of files whose download or non-empty-file check failed.
#' * `skipped`: number of existing non-empty destination files.
#' * `failed_urls`: character vector of URLs whose downloads failed.
#' * `failed_files`: character vector of the corresponding destination
#'   basenames.
#'
#' For non-empty requests that return normally, all five fields are present;
#' `failed_urls` and `failed_files` are `character(0)` when no downloads fail.
#' A warning lists failed filenames when `failed > 0`.
#' @author Mitchell Manware
#' @seealso [download_run_method()], [download_report()],
#'   [download_setup_dir()], [mirai::daemons()], [mirai::mirai_map()]
#' @examples
#' \dontrun{
#' download_run(
#'   urls = "https://example.com/data.nc4",
#'   destfiles = file.path(tempdir(), "data.nc4"),
#'   max_tries = 3
#' )
#' }
#' @keywords internal
#' @export
download_run <- function(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2,
  timeout = 3600,
  http_version = NULL
) {
  # ----------------------------------------------------------------------------
  # Check URLs and destination files.
  checkmate::assert_character(urls, min.len = 1L, any.missing = FALSE)
  checkmate::assert_character(
    destfiles,
    len = length(urls),
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  if (!mirai::daemons_set(.compute = "default")) {
    # --------------------------------------------------------------------------
    # Sequential download using vanilla downloader.
    amadeus::download_run_method(
      urls = urls,
      destfiles = destfiles,
      token = token,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit,
      timeout = timeout,
      http_version = http_version
    )
  } else {
    # --------------------------------------------------------------------------
    # Concurrent download using pre-set number of `mirai` daemons.
    download_run_mirai(
      urls = urls,
      destfiles = destfiles,
      token = token,
      max_tries = max_tries,
      rate_limit = rate_limit,
      timeout = timeout,
      http_version = http_version,
      daemons = mirai::nextget("n", .compute = "default")
    )
  }
}
