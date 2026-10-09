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

################################################################################
# {download_hms} updated with the mirai optional dispatcher.
download_hms_map <- function(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  if (as.Date(date[1]) < as.Date("2005-08-05")) {
    stop("NOAA HMS wildfire smoke data begins at August 05, 2005.")
  }

  #### Directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  #### Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Check for unzip/remove_zip conflict
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    ))
  }

  #### Define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )

  #### Define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"

  if (tolower(data_format) == "shapefile") {
    data_format <- "Shapefile"
    suffix <- ".zip"
    directory_to_cat <- directory_to_download
  } else if (tolower(data_format) == "kml") {
    data_format <- "KML"
    suffix <- ".kml"
    directory_to_cat <- directory_to_save
  }

  #### Define all URLs and destination files
  urls <- paste0(
    base,
    data_format,
    "/",
    substr(date_sequence, 1, 4),
    "/",
    substr(date_sequence, 5, 6),
    "/hms_smoke",
    date_sequence,
    suffix
  )
  destfiles <- paste0(
    directory_to_cat,
    "hms_smoke_",
    data_format,
    "_",
    date_sequence,
    suffix
  )
  needs_download <- vapply(
    destfiles,
    amadeus::check_destfile,
    FUN.VALUE = logical(1)
  )

  #### Validate first URL only
  if (!amadeus::check_url_status(urls[1])) {
    stop(paste0(
      "Invalid date returns HTTP code 404. ",
      "Check `date` parameter.\n"
    ))
  }

  #### Retain URLs and destfiles to be downloaded
  all_urls <- urls[needs_download]
  all_destfiles <- destfiles[needs_download]
  stopifnot(length(all_urls) == length(all_destfiles))

  #### Exit early if download = FALSE
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

  #### Download files using httr2 (sequential or concurrent with mirai)
  if (length(all_urls) == 0L) {
    download_result <- list(success = 0L, failed = 0L, skipped = 0L)
  } else if (!mirai::daemons_set()) {
    # Sequential download if mirai daemons are not set.
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # HMS doesn't use token authentication
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    download_result <- download_run_mirai(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # HMS doesn't use token authentication
      max_tries = max_tries,
      rate_limit = rate_limit,
      daemons = mirai::nextget("n")
    )
  }

  #### Handle KML (no unzipping needed)
  if (data_format == "KML") {
    unlink(directory_to_download, recursive = TRUE)
    message("KML files cannot be unzipped.\n")
    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    } else {
      return(invisible(download_result))
    }
  }

  #### Unzip downloaded zip files if unzip = TRUE
  invisible(lapply(
    all_destfiles,
    function(x) {
      amadeus::download_unzip(x, directory_to_save, unzip)
    }
  ))

  #### Remove zip files if remove_zip = TRUE
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = all_destfiles
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}
