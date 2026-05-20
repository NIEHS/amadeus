# Download auxiliary and internal functions

#' Setup directory
#' @description
#' Create \code{directory} if it does not already exist.
#' @param directory character(1) directory path
#' @param zip logical(1). Should sub-directories be created for zip files and
#' data files? If `TRUE`, a vector of sub-directoy names will be returned.
#' @description If directory does not exist, the directory
#' will be created.
#' @return NULL; if `zip = TRUE` a vector of directories for zip files and
#' data files
#' @keywords internal
#' @export
download_setup_dir <-
  function(directory, zip = FALSE) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    if (zip) {
      directory_zip <- amadeus::download_sanitize_path(
        paste0(
          amadeus::download_sanitize_path(directory),
          "zip_files"
        )
      )
      if (!dir.exists(directory_zip)) {
        dir.create(directory_zip, recursive = TRUE)
      }
      directory_data <- amadeus::download_sanitize_path(
        paste0(
          amadeus::download_sanitize_path(directory),
          "data_files"
        )
      )
      if (!dir.exists(directory_data)) {
        dir.create(directory_data, recursive = TRUE)
      }
      return(c(directory_zip, directory_data))
    }
  }


#' Sanitize directory
#' @description
#' Append forward slash to end of \code{directory} if it does not already
#' end with one.
#' @param directory character(1). Path
#' @return character ending with a forward slash.
#' @keywords internal
#' @export
download_sanitize_path <-
  function(directory) {
    #### 1. directory setup
    chars_dir <- nchar(directory)
    if (
      substr(
        directory,
        chars_dir,
        chars_dir
      ) !=
        "/"
    ) {
      directory <-
        paste(directory, "/", sep = "")
    }
    return(directory)
  }


modis_extract_temporal_key <- function(path) {
  filename <- basename(path)

  key_daily <- regmatches(
    filename,
    regexpr("A20\\d{5}", filename)
  )
  if (length(key_daily) > 0 && nzchar(key_daily)) {
    return(sub("^A", "", key_daily))
  }

  key_txt <- regmatches(
    filename,
    regexpr("_[12][0-9]{6}", filename)
  )
  if (length(key_txt) > 0 && nzchar(key_txt)) {
    return(sub("^_", "", key_txt))
  }

  key_monthly <- regmatches(
    filename,
    regexpr("\\.20[0-9]{4}\\.", filename)
  )
  if (length(key_monthly) > 0 && nzchar(key_monthly)) {
    return(gsub("\\.", "", key_monthly))
  }

  return(NA_character_)
}


modis_extract_temporal_scale <- function(path) {
  filename <- basename(path)

  if (grepl("A20\\d{5}", filename)) {
    return("daily")
  }
  if (grepl("_[12][0-9]{6}", filename)) {
    return("daily")
  }
  if (grepl("\\.20[0-9]{4}\\.", filename)) {
    return("monthly")
  }

  return(NA_character_)
}


modis_key_to_date <- function(
  key,
  scale
) {
  if (length(scale) == 1L) {
    scale <- rep(scale, length(key))
  }
  stopifnot(length(key) == length(scale))

  as_date <- mapply(
    function(key_i, scale_i) {
      if (is.na(key_i) || is.na(scale_i)) {
        return(as.Date(NA))
      }
      if (scale_i == "daily") {
        return(as.Date(key_i, format = "%Y%j"))
      }
      if (scale_i == "monthly") {
        return(as.Date(paste0(key_i, "01"), format = "%Y%m%d"))
      }
      stop("Unsupported MODIS temporal scale.\n")
    },
    key,
    scale,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  return(as.Date(as_date, origin = "1970-01-01"))
}


modis_filter_paths_by_date <- function(
  paths,
  date
) {
  if (length(paths) == 0) {
    return(paths)
  }
  if (length(date) == 1L) {
    date <- rep(date, 2L)
  }

  keys <- vapply(paths, modis_extract_temporal_key, character(1))
  scales <- vapply(paths, modis_extract_temporal_scale, character(1))

  if (all(is.na(keys)) || all(is.na(scales))) {
    return(character(0))
  }

  scales_unique <- unique(stats::na.omit(scales))
  if (length(scales_unique) != 1L) {
    stop("MODIS paths contain mixed or unsupported temporal patterns.\n")
  }

  scale_target <- scales_unique[[1]]
  if (scale_target == "monthly") {
    month_start <- as.Date(format(as.Date(date[1]), "%Y-%m-01"))
    month_end <- as.Date(format(as.Date(date[2]), "%Y-%m-01"))
    month_sequence <- seq(month_start, month_end, by = "month")
    target_keys <- format(month_sequence, "%Y%m")
    keep <- keys %in% target_keys
  } else {
    parsed_dates <- modis_key_to_date(keys, scales)
    keep <- parsed_dates >= as.Date(date[1]) & parsed_dates <= as.Date(date[2])
  }

  return(paths[keep])
}


#' Check data download acknowledgement
#' @description
#' Return an error if the \code{acknowledgement = FALSE}.
#' @param acknowledgement logical(1). Whether to
#' start downloading
#' @note
#' The \code{acknowledgement} parameter is designed to help users avoid
#' accidentally initiating a very large data download that may take a very long
#' time to run or exceed machine capabilities.
#' @return NULL; returns a stop error if the acknowledgement is FALSE
#' @keywords internal
#' @export
download_permit <-
  function(acknowledgement) {
    if (!acknowledgement) {
      stop(paste0(
        "Data download acknowledgement is set to FALSE. ",
        "Please acknowledge that the data downloaded using this ",
        "function may be very large and use lots of machine storage ",
        "and memory.\n"
      ))
    }
  }

#' Get authentication token from various sources
#' @description
#' Retrieves authentication token from environment variable, file, or direct
#' input.
#' Priority order: 1) Environment variable, 2) File path, 3) Direct token
#' string.
#' This function helps prevent accidental token exposure in code or logs.
#' @param token character(1) or NULL. Can be:
#'   - NULL: reads from environment variable (recommended)
#'   - File path: reads token from file
#'   - Token string: uses directly (not recommended for scripts)
#' @param env_var character(1). Name of environment variable containing token.
#'   Default is "NASA_EARTHDATA_TOKEN"
#' @return character(1). The authentication token
#' @keywords internal
#' @export
get_token <- function(token = NULL, env_var = "NASA_EARTHDATA_TOKEN") {
  # Priority 1: Check environment variable
  token_env <- Sys.getenv(env_var, unset = NA)
  if (!is.na(token_env) && nzchar(token_env)) {
    message(sprintf("Using token from environment variable: %s\n", env_var))
    return(trimws(token_env))
  }

  # Priority 2: If token provided, check if it's a file path
  if (!is.null(token)) {
    if (length(token) == 1 && file.exists(token)) {
      message(sprintf("Reading token from file: %s\n", token))
      token_file <- trimws(readLines(token, n = 1, warn = FALSE))
      if (length(token_file) == 0 || !nzchar(token_file)) {
        stop(sprintf("Token file '%s' is empty.\n", token))
      }
      return(token_file)
    }

    # Priority 3: Use token string directly
    # (warn if looks like it's being hard-coded)
    if (length(token) == 1 && nzchar(token)) {
      # Don't show the actual token in any messages!
      message("Using provided token string.\n")
      return(trimws(token))
    }
  }

  # No token found
  stop(
    "No authentication token found. Please provide a token using one of:\n",
    sprintf(
      "  1. Set environment variable: Sys.setenv(%s = 'your_token')\n",
      env_var
    ),
    "  2. Create ~/.nasa_earthdata_token file with your token\n",
    paste0(
      "  3. Pass token file path:",
      " nasa_earth_data_token = '~/.nasa_earthdata_token'\n"
    ),
    "  4. Pass token directly:",
    " nasa_earth_data_token = 'your_token' (not recommended)\n",
    sprintf(
      paste0(
        "\nTo set up for all R sessions,",
        " add to ~/.Renviron:\n  %s=your_token_here\n"
      ),
      env_var
    ),
    call. = FALSE
  )
}


#' Download files using httr2
#' @description
#' Execute downloads using httr2 with robust retry logic and rate limiting.
#' This function handles authentication, retries, progress tracking, and
#' streams files directly to disk.
#' HTTP-status retries use exponential backoff capped at 30 s to avoid
#' long hangs from DNS timeouts (each attempt takes ~10 s). Transport-level
#' failures (SSL drops, connection resets) are also retried up to
#' \code{max_tries} times.
#' @param urls character vector. URLs to download
#' @param destfiles character vector. Destination file paths (same length as
#' urls)
#' @param token character(1). Authentication token (optional, e.g., for NASA
#' EarthData)
#' @param show_progress logical(1). Show download progress bars (default TRUE)
#' @param max_tries integer(1). Maximum number of retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @param timeout numeric(1). Timeout in seconds for each request (default 3600
#' = 1 hour)
#' @param http_version integer(1). Force HTTP version via curl's
#' CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
#' NULL (default) lets curl negotiate automatically. Pass 2L for servers
#' that drop HTTP/2 connections (e.g., www.mrlc.gov for NLCD).
#' @return invisible list with success and failure counts
#' @importFrom httr2 request req_headers req_options req_perform req_retry
#' @importFrom httr2 req_throttle req_error req_progress req_timeout resp_status
#' @importFrom stats runif
#' @keywords internal
#' @export
download_run_method <- function(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2,
  timeout = 3600,
  http_version = NULL
) {
  # Validate inputs
  if (is.null(urls) || length(urls) == 0) {
    stop("No URLs provided for download.\n")
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop("destfiles must have same length as urls.\n")
  }

  # Filter to only files that need downloading
  needs_download <- sapply(destfiles, amadeus::check_destfile)
  urls_filtered <- urls[needs_download]
  destfiles_filtered <- destfiles[needs_download]

  if (length(urls_filtered) == 0) {
    message("All files already exist. Nothing to download.\n")
    return(invisible(list(success = 0, failed = 0, skipped = length(urls))))
  }

  message(sprintf(
    "Downloading %d files using httr2 (skipped %d existing files)...\n",
    length(urls_filtered),
    sum(!needs_download)
  ))

  n_files <- length(urls_filtered)
  n_success <- 0
  n_failed <- 0
  failed_urls <- character(0)
  failed_files <- character(0)

  for (i in seq_along(urls_filtered)) {
    url <- urls_filtered[i]
    destfile <- destfiles_filtered[i]

    # Create directory if needed
    destdir <- dirname(destfile)
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }

    # Progress message: only show file number when progress is on
    progress_prefix <- sprintf("[%d/%d] ", i, n_files)
    if (show_progress) {
      message(sprintf(
        "%sDownloading: %s",
        progress_prefix,
        basename(destfile)
      ))
    }

    tryCatch(
      {
        # Build request
        req <- httr2::request(url)

        # Add authentication if token provided
        if (!is.null(token)) {
          req <- req |>
            httr2::req_headers(Authorization = paste("Bearer", token))
        }

        # Configure retry, throttle, timeout, and error handling.
        # Note: 500 is excluded from is_transient because some APIs
        # (e.g. EPA TRI)
        # return HTTP 500 with valid response bodies on every request. Retrying
        # would make redundant requests. 502/503/504 are gateway errors that
        # are genuinely transient.
        # retry_on_failure retries transport-level errors (SSL drops, etc.)
        # but is capped at min(max_tries, 3L) to avoid long hangs on DNS
        # timeouts (each timeout attempt takes ~10s).
        req <- req |>
          httr2::req_retry(
            max_tries = max_tries,
            is_transient = \(resp) {
              httr2::resp_status(resp) %in% c(429, 502, 503, 504)
            },
            retry_on_failure = TRUE,
            backoff = \(i) stats::runif(1) * pmin(i ^ 2, 30)
          ) |>
          httr2::req_timeout(timeout) |>
          httr2::req_options(connecttimeout = 30L) |>
          httr2::req_throttle(rate = 1 / rate_limit) |>
          httr2::req_error(is_error = \(resp) {
            status <- httr2::resp_status(resp)
            status >= 400 && !(status %in% c(429, 500, 502, 503, 504))
          })

        if (!is.null(http_version)) {
          req <- req |> httr2::req_options(http_version = http_version)
        }

        # Add progress only if requested
        if (show_progress) {
          req <- req |> httr2::req_progress(type = "down")
        }

        # Perform the request
        req |> httr2::req_perform(path = destfile)

        # Verify file was created and has content
        if (file.exists(destfile) && file.size(destfile) > 0) {
          n_success <- n_success + 1
          # Success message: format with or without progress prefix
          success_msg <- sprintf(
            "Success: %s (%s)",
            basename(destfile),
            format_file_size(file.size(destfile))
          )
          message(sprintf(
            "%s%s\n",
            if (show_progress) " OK " else "",
            success_msg
          ))
        } else {
          # File failed validation
          n_failed <- n_failed + 1
          failed_urls <- c(failed_urls, url)
          failed_files <- c(failed_files, basename(destfile))

          if (file.exists(destfile)) {
            file.remove(destfile)
          }

          # Failure message: always show, format based on progress mode
          failure_msg <- sprintf(
            "Failed: %s (0 bytes)",
            basename(destfile)
          )
          message(sprintf(
            "%s%s\n",
            if (show_progress) " FAIL " else "",
            failure_msg
          ))
        }

        # Brief pause between downloads
        if (i < n_files) {
          Sys.sleep(runif(1, 0.5, 1.5))
        }
      },
      error = function(e) {
        n_failed <<- n_failed + 1
        failed_urls <<- c(failed_urls, url)
        failed_files <<- c(failed_files, basename(destfile))

        # Error message: always show
        error_msg <- sprintf(
          "Failed: %s - %s",
          basename(destfile),
          conditionMessage(e)
        )
        message(sprintf(
          "%s%s\n",
          if (show_progress) " FAIL " else "",
          error_msg
        ))

        # Clean up failed download
        if (file.exists(destfile)) {
          file.remove(destfile)
        }
      }
    )
  }

  # Summary message
  message(sprintf(
    "\n=== Download Summary ===\n%d succeeded, %d failed, %d skipped\n",
    n_success,
    n_failed,
    sum(!needs_download)
  ))

  if (n_failed > 0) {
    warning(
      sprintf(
        "%d file(s) failed to download:\n  %s\n",
        n_failed,
        paste(failed_files, collapse = "\n  ")
      ),
      call. = FALSE
    )
  }

  invisible(list(
    success = n_success,
    failed = n_failed,
    skipped = sum(!needs_download),
    failed_urls = failed_urls,
    failed_files = failed_files
  ))
}


#' Format file size for display
#' @keywords internal
#' @noRd
format_file_size <- function(bytes) {
  if (bytes < 1024) {
    return(sprintf("%d B", bytes))
  } else if (bytes < 1024^2) {
    return(sprintf("%.1f KB", bytes / 1024))
  } else if (bytes < 1024^3) {
    return(sprintf("%.1f MB", bytes / 1024^2))
  } else {
    return(sprintf("%.1f GB", bytes / 1024^3))
  }
}


#' Legacy download_run function for backwards compatibility
#' @description
#' **DEPRECATED**: This function is maintained for backwards compatibility.
#' New code should use `download_run_method()` directly.
#'
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (\code{TRUE}) or skip (\code{FALSE})
#' download.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' command.
#' @return NULL; runs download commands with shell (Unix/Linux) or
#' command prompt (Windows) and removes \code{commands_txt} file if
#' \code{remove = TRUE}.
#' @keywords internal
#' @export
download_run <- function(
  download = FALSE,
  commands_txt = NULL,
  remove = FALSE
) {
  # Show deprecation warning once per session
  if (!isTRUE(getOption("amadeus.download_run.warned"))) {
    warning(
      "download_run() is deprecated. Use download_run_method() instead.\n",
      "  Old: download_modis(..., download = TRUE)\n",
      "  New: download_modis(...) uses httr2 by default\n",
      call. = FALSE
    )
    options(amadeus.download_run.warned = TRUE)
  }

  if (tolower(.Platform$OS.type) == "windows") {
    # nocov start
    runner <- ""
    commands_bat <- gsub(".txt", ".bat", commands_txt)
    file.rename(commands_txt, commands_bat)
    commands_txt <- commands_bat
    # nocov end
  } else {
    runner <- ". "
    if (!grepl("(^/|^\\./|^\\.\\./)", commands_txt)) {
      commands_txt <- paste0("./", commands_txt)
    }
  }
  system_command <- paste0(runner, shQuote(commands_txt))
  if (download == TRUE) {
    message(paste0("Downloading requested files...\n"))
    system(command = system_command, intern = TRUE)
    message(paste0("Requested files have been downloaded.\n"))
  } else {
    message(paste0("Skipping data download.\n"))
  }
  amadeus::download_remove_command(
    commands_txt = commands_txt,
    remove = remove
  )
}

#' Remove download commands
#' @description
#' Remove or retain the .txt file storing all download commands.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or
#'  keep (\code{FALSE}) commands
#' @return NULL; removes .txt/.bat file storing all download commands.
#' @keywords internal
#' @export
download_remove_command <-
  function(commands_txt = NULL, remove = FALSE) {
    if (remove) {
      file.remove(commands_txt)
    }
  }


#' Sink download commands
#' @description
#' Open connection to \code{command_txt} file to store download commands.
#' @param command_txt character(1). file path to export commands.
#' @return NULL; creates and opens connection to text file to store
#' download commands
#' @keywords internal
#' @export
download_sink <-
  function(command_txt) {
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    sink(file = command_txt, append = FALSE)
  }


#' Extract downloaded archives
#' @description
#' Extract downloaded ".zip" or ".7z" files.
#' @param file_name character(1). Full archive file path
#' @param directory_to_unzip character(1). Directory to extract
#' data
#' @param unzip logical(1). Extract (\code{TRUE}) or not.
#' @return NULL; extracts downloaded archive files
#' @keywords internal
#' @export
download_unzip <-
  function(file_name, directory_to_unzip, unzip = TRUE) {
    if (!unzip) {
      message(paste0("Downloaded files will not be unzipped.\n"))
      return(NULL)
    }
    ext <- tolower(tools::file_ext(file_name))
    message(paste0("Unzipping files...\n"))
    if (ext == "zip") {
      unzip(file_name, exdir = directory_to_unzip)
    } else if (ext == "7z") {
      archive::archive_extract(file_name, dir = directory_to_unzip)
    } else {
      stop("Unsupported archive format. Expected .zip or .7z.")
    }
    message(paste0(
      "Files unzipped and saved in ",
      directory_to_unzip,
      ".\n"
    ))
  }


#' Normalize AQS unzip layout
#' @description
#' Move files from an AQS archive's nested year directory into the main
#' `data_files` directory when the archive unpacks into a single folder.
#' @param directory_to_unzip character(1). Directory where files were unzipped.
#' @param resolution_temporal character(1). Temporal resolution used in the
#'   AQS archive name.
#' @param parameter_code integer(1). AQS parameter code used in the archive
#'   name.
#' @param year integer(1). Year represented by the archive.
#' @return NULL; normalizes the AQS data file layout in place
#' @keywords internal
#' @noRd
download_normalize_aqs_unzip <- function(
  directory_to_unzip,
  resolution_temporal,
  parameter_code,
  year
) {
  nested_dir <- file.path(
    directory_to_unzip,
    sprintf("%s_%s_%s", resolution_temporal, parameter_code, year)
  )

  if (!dir.exists(nested_dir)) {
    return(invisible(NULL))
  }

  nested_files <- list.files(
    nested_dir,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(nested_files) == 0 || any(dir.exists(nested_files))) {
    return(invisible(NULL))
  }

  for (nested_file in nested_files) {
    target_file <- file.path(directory_to_unzip, basename(nested_file))
    if (!file.exists(target_file)) {
      moved <- file.rename(nested_file, target_file)
      if (!isTRUE(moved)) {
        # nocov start
        stop(sprintf(
          "Failed to normalize AQS unzip layout for %s.",
          basename(nested_file)
        ))
        # nocov end
      }
    } else {
      unlink(nested_file)
    }
  }

  unlink(nested_dir, recursive = TRUE)
  invisible(NULL)
}


#' Remove zip files
#' @description
#' Remove downloaded ".zip" files.
#' @param remove logical(1). Confirm removal. Default is FALSE.
#' @param download_name character. Full zip file path
#' @note
#' !!! USE THE FUNCTION WITH CAUTION !!!
#' If \code{remove = TRUE}, ensure that \code{unzip = TRUE}. Choosing to remove
#' ".zip" files without unzipping will retain none of the downloaded data.
#' then it will remove all files in the second higher level directory.
#' @return NULL; removes downloaded zip files after they are unzipped
#' @keywords internal
#' @export
download_remove_zips <-
  function(remove = FALSE, download_name) {
    #### remove zip files
    if (remove) {
      message(paste0("Removing download files...\n"))
      file.remove(download_name)
      # oftentimes zipfiles are stored in zip_files under
      # directory_to_save in download functions.
      unlink(dirname(download_name), recursive = TRUE)
      message(paste0("Download files removed.\n"))
    }
  }


#' Check parameters
#' @description
#' Check that all parameters have been assigned a value.
#' @param parameters parameters passed to function (called by
#' \code{mget(ls())}.)
#' @return NULL; returns a stop error if one or more function
#' parameters other than 'extent' are NULL
#' @keywords internal
#' @export
check_for_null_parameters <-
  function(
    parameters
  ) {
    if ("extent" %in% names(parameters)) {
      parameters <- parameters[-grep("extent", names(parameters))]
    }
    parameters_status <- any(unlist(lapply(parameters, is.null)))
    if (parameters_status) {
      stop(paste0("One or more parameters are NULL\n"))
    }
  }

#' Generate date sequence
#' @description
#' Generate a sequence of dates from \code{date_start} to \code{date_end}.
#' @param date_start character(1). Beginning of date sequence.
#' @param date_end character(1). End of date sequence.
#' @param sub_hyphen logical(1). Substitute hyphen in dates. If `TRUE`, returns
#' date sequence as "YYYYMMDD". If `FALSE`, returns date sequence as
#' "YYYY-MM-DD".
#' @return vector
#' @keywords internal auxiliary
#' @export
generate_date_sequence <-
  function(
    date_start,
    date_end,
    sub_hyphen = TRUE
  ) {
    dates_original <- seq(
      as.Date(date_start, format = "%Y-%m-%d"),
      as.Date(date_end, format = "%Y-%m-%d"),
      "day"
    )
    if (sub_hyphen) {
      dates_sub_hyphen <- gsub("-", "", as.character(dates_original))
      return(dates_sub_hyphen)
    } else {
      return(dates_original)
    }
  }


#' Generate time sequence
#' @description
#' Generate a sequence of time values based on the GEOS-CF collection.
#' @param collection character(1). GEOS-CF data collection
#' @return vector
#' @note
#' GEOS-CF hourly values are observed on the hour (ie. 0000 = 12:00:00 AM, 0100
#' = 01:00:00 AM) or the half hour (ie. 0030 = 12:30:00 AM, 0130 = 01:30:00 AM).
#' Typically, 2-dimensional collections (latitude and longitude only) utilize
#' half hour, and 3-dimensional collections (latitude, longitude, and time)
#' utilize hour.
#' @keywords internal auxiliary
#' @export
generate_time_sequence <-
  function(
    collection
  ) {
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      ts <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      ts <- seq(from = 0, to = 2300, by = 100)
    }
    time_sequence <- sprintf("%04d", ts)
    return(time_sequence)
  }

#' Check HTTP status
#' @description
#' Check if provided URL returns HTTP status 200 or 206.
#' @param url Download URL to be checked.
#' @param max_tries integer(1). Maximum number of retry attempts for
#'   transient failures (SSL drops, connection resets). Default 3L.
#' @author Insang Song; Mitchell Manware; Kyle Messier
#' @importFrom httr2 request req_perform resp_status
#' @importFrom httr2 req_method req_error req_retry
#' @return logical object
#' @keywords internal auxiliary
#' @export
check_url_status <- function(
  url,
  max_tries = 3L
) {
  http_status_ok <- c(200, 206)

  tryCatch(
    {
      status <- url |>
        httr2::request() |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_retry(
          max_tries = max_tries,
          retry_on_failure = TRUE
        ) |>
        httr2::req_perform() |>
        httr2::resp_status()

      Sys.sleep(1)
      return(status %in% http_status_ok)
    },
    error = function(e) {
      # Return FALSE for any errors (network, DNS, SSL, etc.)
      return(FALSE)
    }
  )
}

#' Import download commands
#' @description
#' Read download commands from .txt file and convert to character vector.
#' @param commands_path file path with wget/curl commands
#' @return character vector containing download commands
#' @importFrom utils read.csv
#' @keywords internal
#' @export
read_commands <- function(
  commands_path = commands_path
) {
  commands <- utils::read.csv(commands_path, header = FALSE)
  commands <- commands[seq_len(nrow(commands)), ]
  return(commands)
}

#' Extract download URLs
#' @description
#' Extract download URLs from multi-argument download commands.
#' @param commands character vector containing download commands
#' @param position URL position in the vector
#' @return character vector containing download URLs
#' @keywords internal
#' @export
extract_urls <- function(
  commands = commands,
  position = NULL
) {
  if (is.null(position)) {
    message("URL position in command is not defined.")
    return(NULL)
  }
  urls <- sapply(
    strsplit(
      trimws(commands),
      " "
    ),
    function(x, l) x[l],
    l = position
  )
  return(urls)
}

#' Implement \code{check_url_status}
#' @description
#' Apply \code{check_url_status()} function to a sample of download URLs.
#' @param urls character vector of URLs
#' @param size number of observations to be sampled from \code{urls}
#' @param method If set to `"SKIP"`, the HTTP status will not be checked and
#' returned.
#' @return logical vector for URL status = 200
#' @keywords internal auxiliary
#' @export
check_urls <- function(
  urls = urls,
  size = NULL,
  method = NULL
) {
  if (is.null(size)) {
    message(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  if (length(urls) < size) {
    size <- length(urls)
  }
  if (!is.null(method) && toupper(method) == "SKIP") {
    message(paste0("Skipping HTTP status check...\n"))
    return(NULL)
  } else {
    url_sample <- sample(urls, size, replace = FALSE)
    url_status <- sapply(
      url_sample,
      check_url_status
    )
    return(url_status)
  }
}

#' Sort NOAA NARR variables
#' @description
#' Determine whether a NOAA NARR variable selected for download is a
#' monolevel or pressure level variable. Monolevel variables are derived
#' from https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/,
#' and pressure level variables are derived from
#' https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/.
#' @param variable character(1). User-selected NARR variable
#' @return list with URL base and vector of months (blank for monolevel)
#' @keywords internal auxiliary
#' @export
narr_variable <- function(variable) {
  stopifnot(length(variable) == 1)
  mono <- c(
    "acpcp",
    "air.2m",
    "air.sfc",
    "albedo",
    "apcp",
    "bgrun",
    "bmixl.hl1",
    "cape",
    "ccond",
    "cdcon",
    "cdlyr",
    "cfrzr",
    "cicep",
    "cin",
    "cnwat",
    "crain",
    "csnow",
    "dlwrf",
    "dpt.2m",
    "dswrf",
    "evap",
    "gflux",
    "hcdc",
    "hgt.tropo",
    "hlcy",
    "hpbl",
    "lcdc",
    "lftx4",
    "lhtfl",
    "mcdc",
    "mconv.hl1",
    "mslet",
    "mstav",
    "pevap",
    "pottmp.hl1",
    "pottmp.sfc",
    "prate",
    "pres.sfc",
    "pres.tropo",
    "prmsl",
    "pr_wtr",
    "rcq",
    "rcs",
    "rcsol",
    "rct",
    "rhum.2m",
    "shtfl",
    "shum.2m",
    "snod",
    "snohf",
    "snom",
    "snowc",
    "soilm",
    "ssrun",
    "tcdc",
    "tke.hl1",
    "ulwrf.ntat",
    "ulwrf.sfc",
    "ustm",
    "uswrf.ntat",
    "uswrf.sfc",
    "uwnd.10m",
    "veg",
    "vis",
    "vstm",
    "vvel.hl1",
    "vwnd.10m",
    "vwsh.tropo",
    "wcconv",
    "wcinc",
    "wcuflx",
    "wcvflx",
    "weasd",
    "wvconv",
    "wvinc",
    "wvuflx",
    "wvvflx"
  )
  pressure <- c("air", "hgt", "omega", "shum", "tke", "uwnd", "vwnd")
  soil <- c("soill", "soilw", "tsoil")
  base <- "https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/"
  if (variable %in% mono) {
    base <- paste0(base, "monolevel/")
    months <- ""
  } else {
    months <- sprintf("%02d", seq(1, 12, by = 1))
    if (variable %in% pressure) {
      base <- paste0(base, "pressure/")
    } else if (variable %in% soil) {
      base <- paste0(base, "subsurface/")
    } else {
      stop(
        paste0(
          "Selected variable \"",
          variable,
          "\" is not available.\n"
        )
      )
    }
  }
  return(list(base, months))
}

#' Create hash of downloaded files.
#' @description
#' Create a combined md5sum hash based on the files in a specified directory.
#' @param hash logical(1). Create hash of downloaded files.
#' @param dir character(1). Directory path.
#' @return character(1) Combined 128-bit md5sum of download files.
#' @keywords internal auxiliary
#' @export
download_hash <- function(
  hash = TRUE,
  dir = NULL
) {
  if (hash) {
    h_command <- paste0(
      "find ",
      shQuote(dir),
      " -type f -exec md5sum {} + | awk '{print $1}' | sort -k 2 | md5sum"
    )
    h <- system(h_command, intern = TRUE)
    h_clean <- sub("  -$", "", h)
    return(h_clean)
  }
}

#' Check if destination file exists or is 0 bytes.
#' @description
#' Check if destination file exists or is 0 bytes. If either condition is
#' met, the function returns `TRUE` to allow the download to proceed.
#' @param destfile character(1). Destination file path.
#' @return logical(1)
#' @keywords internal auxiliary
#' @export
check_destfile <- function(destfile) {
  if (!file.exists(destfile) || file.size(destfile) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Set up NASA EarthData authentication
#' @description
#' Interactive helper to securely set up NASA EarthData authentication.
#' This function guides users through setting up their token in a secure way
#' that won't be exposed in scripts or version control.
#' @param method character(1). Setup method:
#'   - "renviron": Add to ~/.Renviron (recommended, persists across sessions)
#'   - "file": Save to ~/.nasa_earthdata_token file
#'   - "session": Set for current R session only
#' @param token character(1). Your NASA EarthData token. If NULL, will prompt.
#' @return invisible(NULL). Sets up authentication.
#' @examples
#' \dontrun{
#' # Interactive setup (recommended)
#' setup_nasa_token()
#'
#' # Save to .Renviron for permanent setup
#' setup_nasa_token(method = "renviron", token = "your_token_here")
#'
#' # Save to file
#' setup_nasa_token(method = "file", token = "your_token_here")
#'
#' # Current session only
#' setup_nasa_token(method = "session", token = "your_token_here")
#' }
#' @export
setup_nasa_token <- function(
  method = c("renviron", "file", "session"),
  token = NULL
) {
  method <- match.arg(method)

  # Get token if not provided
  if (is.null(token)) {
    if (interactive()) {
      cat("Enter your NASA EarthData token: ")
      token <- readline()
      token <- trimws(token)
    } else {
      stop("Token must be provided in non-interactive mode.\n", call. = FALSE)
    }
  }

  if (!nzchar(token)) {
    stop("Token cannot be empty.\n", call. = FALSE)
  }

  switch(
    method,
    renviron = {
      renviron_path <- path.expand("~/.Renviron")

      # Read existing .Renviron
      if (file.exists(renviron_path)) {
        renviron_lines <- readLines(renviron_path)
        # Remove any existing NASA_EARTHDATA_TOKEN lines
        renviron_lines <- renviron_lines[
          !grepl("^NASA_EARTHDATA_TOKEN=", renviron_lines)
        ]
      } else {
        renviron_lines <- character(0)
      }

      # Add new token
      renviron_lines <- c(
        renviron_lines,
        paste0("NASA_EARTHDATA_TOKEN=", token)
      )
      writeLines(renviron_lines, renviron_path)

      message(sprintf(
        "Token saved to %s\n",
        renviron_path
      ))
      message(
        "  Restart R for changes to take effect,",
        " or run: readRenviron('~/.Renviron')\n"
      )
    },

    file = {
      token_path <- path.expand("~/.nasa_earthdata_token")
      writeLines(token, token_path)

      # Set file permissions to user-only (Unix-like systems)
      if (.Platform$OS.type != "windows") {
        Sys.chmod(token_path, mode = "0600")
      }

      message(sprintf("Token saved to %s\n", token_path))
      message(
        "  Use in functions:",
        " nasa_earth_data_token = '~/.nasa_earthdata_token'\n"
      )
    },

    session = {
      Sys.setenv(NASA_EARTHDATA_TOKEN = token)
      message("Token set for current R session\n")
      message(
        "  This will be lost when you close R.",
        " Use method='renviron' for permanent setup.\n"
      )
    }
  )

  invisible(NULL)
}


#' Convert spatial extent to MODIS sinusoidal tile codes
#' @description
#' Returns the set of MODIS sinusoidal grid tile codes (e.g. \code{"h08v04"})
#' whose geographic footprint overlaps the supplied bounding box.
#' @details
#' The MODIS sinusoidal grid divides the globe into 18 x 36 tiles, each
#' nominally covering 10 degrees of latitude. Because the sinusoidal projection
#' compresses longitude at high latitudes, the geographic lon/lat bounding
#' boxes of tiles are \emph{not} simple 10-degree squares — they can be
#' significantly wider in geographic longitude near the poles.
#'
#' This function uses the official NASA MODLAND sinusoidal tile bounding
#' coordinates table (\code{sn_bound_10deg.txt},
#' \url{https://modis-land.gsfc.nasa.gov/pdf/sn_bound_10deg.txt}) bundled in
#' \code{inst/extdata/}. It returns every non-fill tile whose geographic
#' bounding box overlaps the requested extent.
#'
#' Horizontal tile numbers (h) range from 0 to 35 (west to east); vertical
#' tile numbers (v) range from 0 to 17 (north to south).
#' @param extent numeric(4). Bounding box \code{c(xmin, ymin, xmax, ymax)}
#'   in decimal degrees (EPSG:4326).
#' @return character vector of tile codes in \code{"hXXvYY"} format, ordered
#'   by increasing v then h.
#' @author Kyle Messier
#' @seealso \code{\link{download_modis}}
#' @examples
#' extent_to_modis_tiles(c(-125, 22, -64, 50))
#' @keywords internal auxiliary
#' @export
extent_to_modis_tiles <- function(extent) {
  stopifnot(
    "extent must be numeric(4)" = is.numeric(extent) && length(extent) == 4,
    "extent[1] (xmin) must be >= -180" = extent[1] >= -180,
    "extent[3] (xmax) must be <= 180" = extent[3] <= 180,
    "extent[2] (ymin) must be >= -90" = extent[2] >= -90,
    "extent[4] (ymax) must be <= 90" = extent[4] <= 90,
    "xmin must be < xmax" = extent[1] < extent[3],
    "ymin must be < ymax" = extent[2] < extent[4]
  )

  bounds_file <- system.file(
    "extdata", "sn_bound_10deg.txt", package = "amadeus"
  )
  stopifnot(
    "sn_bound_10deg.txt not found in inst/extdata/" = nzchar(bounds_file)
  )

  lines <- readLines(bounds_file, warn = FALSE)
  data_lines <- grep("^ *[0-9]", lines, value = TRUE)
  tiles_df <- utils::read.table(
    text    = paste(data_lines, collapse = "\n"),
    col.names = c("iv", "ih", "lon_min", "lon_max", "lat_min", "lat_max")
  )

  # Drop fill tiles (lon_min == -999)
  tiles_df <- tiles_df[tiles_df$lon_min > -900, ]

  xmin <- extent[1]
  ymin <- extent[2]
  xmax <- extent[3]
  ymax <- extent[4]

  # Bounding-box overlap: tile overlaps query if
  #   tile_lon_max >= xmin  AND  tile_lon_min <= xmax
  #   tile_lat_max >= ymin  AND  tile_lat_min <= ymax
  hits <- tiles_df[
    tiles_df$lon_max >= xmin & tiles_df$lon_min <= xmax &
      tiles_df$lat_max >= ymin & tiles_df$lat_min <= ymax,
  ]

  sprintf("h%02dv%02d", hits$ih, hits$iv)
}


#' Generate weekly Tuesday dates for drought products
#' @description
#' Return a character vector of YYYYMMDD strings for each Tuesday falling
#' within \code{[date_start, date_end]}.  EDDI and USDM are both released on
#' Tuesdays; this helper centralises the logic.
#' @param date_start character(1). Start date, \code{"YYYY-MM-DD"}.
#' @param date_end character(1). End date, \code{"YYYY-MM-DD"}.
#' @return character vector of \code{"YYYYMMDD"} strings (may be length 0).
#' @keywords internal
#' @export
drought_weekly_dates <- function(date_start, date_end) {
  all_dates <- seq(
    as.Date(date_start, format = "%Y-%m-%d"),
    as.Date(date_end, format = "%Y-%m-%d"),
    by = "day"
  )
  tuesdays <- all_dates[weekdays(all_dates) == "Tuesday"]
  format(tuesdays, "%Y%m%d")
}
