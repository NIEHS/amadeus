################################################################################
# Development work for {mirai} backended functions.
# 08 October 2026
# Mitchell Manware

################################################################################
# {mirai} enabled download dispatcher.
download_run_mirai <- function(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  max_tries = 20,
  rate_limit = 2,
  daemons = NULL,
  ...
) {
  mirai::require_daemons()

  # Parallel download using available mirai daemons.
  int_daemons <- as.integer(daemons)

  message(sprintf(
    "Concurrent download using %d configrured {mirai} daemons.",
    int_daemons
  ))

  df_mirai <- data.frame(
    urls = unname(urls),
    destfiles = unname(destfiles)
  )

  jobs <- mirai::mirai_map(
    df_mirai,
    function(
      urls,
      destfiles,
      max_tries,
      rate_limit
    ) {
      amadeus::download_run_method(
        urls = urls,
        destfiles = destfiles,
        token = NULL,
        show_progress = FALSE,
        max_tries = max_tries,
        rate_limit = rate_limit,
        ...
      )
    },
    .args = list(max_tries = max_tries, rate_limit = rate_limit)
  )

  results <- mirai::collect_mirai(jobs, options = ".stop")

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
# {download_narr} updated with the mirai optional dispatcher.
download_narr_map <- function(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### 3. Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### 4. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 5. Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 6. Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  stopifnot(
    all(
      seq(year[1], year[2], 1) %in%
        seq(1979, as.numeric(substr(Sys.Date(), 1, 4)), 1)
    )
  )
  years <- seq(year[1], year[2], 1)

  #### 7. Define variables
  variables_list <- as.list(unique(variables))

  #### 8. Collect all URLs and destination files
  list_map <- lapply(
    variables_list,
    function(x) {
      base <- amadeus::narr_variable(x)[[1]]
      month <- amadeus::narr_variable(x)[[2]]
      download_grid <- expand.grid(
        base = base,
        variable = x,
        year = years,
        month = month,
        dir = directory_to_save
      )
      download_grid$url <- with(
        download_grid,
        paste0(
          base,
          variable,
          ".",
          year,
          month,
          ".nc"
        )
      )
      download_grid$destfile <- with(
        download_grid,
        paste0(
          dir,
          variable,
          "/",
          variable,
          ".",
          year,
          month,
          ".nc"
        )
      )
      needs_download <- vapply(
        download_grid$destfile,
        amadeus::check_destfile,
        FUN.VALUE = logical(1)
      )
      var_urls <- download_grid$url[needs_download]
      var_destfiles <- download_grid$destfile[needs_download]
      stopifnot(length(var_urls) == length(var_destfiles))
      list(urls = var_urls, destfiles = var_destfiles)
    }
  )
  all_urls <- unlist(lapply(list_map, function(x) x[[1]]))
  all_destfiles <- unlist(lapply(list_map, function(x) x[[2]]))

  #### 9. Exit early if download=FALSE (deprecated behavior)
  if (!isTRUE(download)) {
    message(
      sprintf(
        "Skipping download. Found %d files available for download.\n",
        length(all_urls)
      )
    )
    return(
      invisible(
        list(
          urls = all_urls,
          destfiles = all_destfiles,
          n_files = length(all_urls)
        )
      )
    )
  }

  #### 10. Download files using httr2 (sequential or concurrent with mirai)
  if (length(all_urls) == 0L) {
    download_result <- list(success = 0L, failed = 0L, skipped = 0L)
  } else if (!mirai::daemons_set()) {
    # Sequential download if mirai daemons are not set.
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # NARR doesn't use token authentication
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    download_result <- download_run_mirai(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # NARR doesn't use token authentication
      max_tries = max_tries,
      rate_limit = rate_limit,
      daemons = mirai::nextget("n")
    )
  }

  #### 11. Return hash if requested
  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}
