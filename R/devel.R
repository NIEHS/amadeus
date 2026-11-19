################################################################################
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
  mirai::require_daemons()
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
  jobs <- mirai::mirai_map(
    df_mirai,
    amadeus::download_run_method,
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
  results <- mirai::collect_mirai(jobs, options = ".stop")
  # ----------------------------------------------------------------------------
  # Collapse `mirai` results to match `download_run_method` output.
  list(
    success = sum(vapply(results, `[[`, numeric(1), "success")),
    failed = sum(vapply(results, `[[`, numeric(1), "failed")),
    skipped = sum(vapply(results, `[[`, numeric(1), "skipped")),
    failed_urls = unlist(
      lapply(results, `[[`, "failed_urls"),
      use.names = FALSE
    ),
    failed_files = unlist(
      lapply(results, `[[`, "failed_files"),
      use.names = FALSE
    )
  )
}

################################################################################
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
  if (!mirai::daemons_set()) {
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
      daemons = mirai::nextget("n")
    )
  }
}
