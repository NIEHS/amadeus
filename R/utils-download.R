################################################################################
# Support and internal {download} functions.
################################################################################

################################################################################
#' Sanitize directory
#' @description
#' Append forward slash to end of \code{directory} if it does not already
#' end with one.
#' @param directory character(1). Path
#' @return character ending with a forward slash.
#' @keywords internal
#' @export
download_sanitize_path <- function(directory) {
  # ----------------------------------------------------------------------------
  # Append a forward slash if the directory path does not end with one.
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
  # ----------------------------------------------------------------------------
  # Return the directory path.
  return(directory)
}

################################################################################
#' Set up download directories
#' @description
#' Create missing download directories, including parent directories, and
#' return their paths with trailing forward slashes.
#' @param directory character(1). Directory path for downloads.
#' @param zip logical(1). If `TRUE`, also create `zip_files` and `data_files`
#' subdirectories within `directory`. Default is `FALSE`.
#' @return If `zip = FALSE`, a character scalar containing `directory` with
#' a trailing forward slash. If `zip = TRUE`, a character vector containing
#' the paths to `zip_files` and `data_files`, in that order, each with a
#' trailing forward slash. Paths are returned whether the directories
#' already existed or were newly created.
#' @seealso [download_sanitize_path()]
#' @examples
#' directory <- file.path(tempdir(), "amadeus_downloads")
#' download_setup_dir(directory)
#' download_setup_dir(directory, zip = TRUE)
#' @keywords internal
#' @export
download_setup_dir <- function(directory, zip = FALSE) {
  # ----------------------------------------------------------------------------
  # Sanitize the download path and create missing parent directories.
  chr_dir_sanitized <- amadeus:::download_sanitize_path(directory)
  if (!dir.exists(chr_dir_sanitized)) {
    dir.create(chr_dir_sanitized, recursive = TRUE)
  }
  # ----------------------------------------------------------------------------
  # Set up separate archive and data directories when requested.
  if (zip) {
    # --------------------------------------------------------------------------
    # Create the archive directory.
    directory_zip <- amadeus:::download_sanitize_path(
      paste0(
        chr_dir_sanitized,
        "zip_files"
      )
    )
    if (!dir.exists(directory_zip)) {
      dir.create(directory_zip, recursive = TRUE)
    }
    # --------------------------------------------------------------------------
    # Create the extracted data directory.
    directory_data <- amadeus:::download_sanitize_path(
      paste0(
        chr_dir_sanitized,
        "data_files"
      )
    )
    if (!dir.exists(directory_data)) {
      dir.create(directory_data, recursive = TRUE)
    }
    # --------------------------------------------------------------------------
    # Return the archive and data paths, in that order.
    return(c(directory_zip, directory_data))
  } else {
    # --------------------------------------------------------------------------
    # Return the main download path.
    chr_dir_sanitized
  }
}

################################################################################
#' Check if destination file exists or is 0 bytes.
#' @description
#' Check if destination file exists or is 0 bytes. If either condition is
#' met, the function returns `TRUE` to allow the download to proceed.
#' @param destfile character(1). Destination file path.
#' @return logical(1)
#' @keywords internal auxiliary
#' @export
check_destfile <- function(destfile) {
  # ----------------------------------------------------------------------------
  # Request a download when the destination is missing or empty.
  if (!file.exists(destfile) || file.size(destfile) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

################################################################################
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
  # ----------------------------------------------------------------------------
  # Define successful HTTP status codes.
  http_status_ok <- c(200, 206)
  # ----------------------------------------------------------------------------
  # Probe the URL with a HEAD request and retry transient failures.
  status_head <- tryCatch(
    {
      url |>
        httr2::request() |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_retry(
          max_tries = max_tries,
          retry_on_failure = TRUE
        ) |>
        httr2::req_perform() |>
        httr2::resp_status()
    },
    error = function(e) NA_integer_
  )
  # ----------------------------------------------------------------------------
  # Pause and return early when the HEAD request succeeds.
  if (!is.na(status_head) && status_head %in% http_status_ok) {
    Sys.sleep(1)
    return(TRUE)
  }
  # ----------------------------------------------------------------------------
  # Fall back to a ranged GET when the HEAD request fails.
  status_get <- tryCatch(
    {
      url |>
        httr2::request() |>
        httr2::req_headers(Range = "bytes=0-0") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_retry(
          max_tries = max_tries,
          retry_on_failure = TRUE
        ) |>
        httr2::req_perform() |>
        httr2::resp_status()
    },
    error = function(e) NA_integer_
  )
  # ----------------------------------------------------------------------------
  # Pause and return whether the GET request succeeded.
  Sys.sleep(1)
  !is.na(status_get) && status_get %in% http_status_ok
}

################################################################################
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
  # ----------------------------------------------------------------------------
  # Return early when no sample size is supplied.
  if (is.null(size)) {
    message(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  # ----------------------------------------------------------------------------
  # Limit the sample size to the number of available URLs.
  if (length(urls) < size) {
    size <- length(urls)
  }
  # ----------------------------------------------------------------------------
  # Skip the status checks when requested.
  if (!is.null(method) && toupper(method) == "SKIP") {
    message(paste0("Skipping HTTP status check...\n"))
    return(NULL)
  } else {
    # --------------------------------------------------------------------------
    # Sample URLs without replacement and return their status checks.
    url_sample <- sample(urls, size, replace = FALSE)
    url_status <- sapply(
      url_sample,
      amadeus:::check_url_status
    )
    return(url_status)
  }
}

################################################################################
#' Check a download URL before downloading
#' @description
#' Check a single download URL with [check_url_status()] and warn if the check
#' fails, allowing the calling download function to continue.
#' @param url character(1). Download URL to check.
#' @details
#' HTTP status codes 200 and 206 are treated as successful by
#' [check_url_status()], which tries a HEAD request followed by a ranged GET
#' when necessary. A failed check emits a warning; the caller handles any
#' subsequent download attempt.
#' @return NULL, invisibly. Called for its warning side effect.
#' @seealso [check_url_status()]
#' @keywords internal
#' @noRd
check_preflight <- function(url) {
  # ----------------------------------------------------------------------------
  # Warn if the URL check fails so the caller can continue downloading.
  if (!amadeus:::check_url_status(url)) {
    warning(
      paste0(
        "Preflight URL check failed; attempting direct download ",
        "for year validation.\n"
      ),
      call. = FALSE
    )
  }
}

################################################################################
#' Preview planned downloads
#' @description
#' Print a message reporting the number of planned downloads and return their
#' URLs and destination paths without downloading files.
#' @param urls character vector. Planned download URLs.
#' @param destfiles character vector. Corresponding destination file paths,
#'   in the same order and of the same length as `urls`.
#' @details
#' The file count is based on the supplied URLs; remote availability and
#' existing local files are not checked. The calling download function handles
#' the `download = FALSE` condition and exits using
#' `return(invisible(download_preview(urls, destfiles)))`.
#' @return A list, returned visibly, containing:
#' * `urls`: the supplied download URLs.
#' * `destfiles`: the supplied destination file paths.
#' * `n_files`: the number of supplied URLs, calculated as `length(urls)`.
#' @author Mitchell Manware
#' @seealso [download_run_method()]
#' @keywords internal
#' @examples
#' download_preview(
#'   urls = "https://example.com/data.zip",
#'   destfiles = file.path(tempdir(), "data.zip")
#' )
#' @export
download_preview <- function(urls, destfiles) {
  # ----------------------------------------------------------------------------
  # Report the number of planned downloads.
  message(sprintf(
    "Skipping download. Found %d files available for download.\n",
    length(urls)
  ))
  # ----------------------------------------------------------------------------
  # Return the planned URLs, destination paths, and file count.
  list(
    urls = urls,
    destfiles = destfiles,
    n_files = length(urls)
  )
}

################################################################################
#' Download files using httr2
#' @description
#' Download files sequentially with httr2, streaming each response to disk.
#' Handles bearer-token authentication, retries, request throttling, progress
#' reporting, and a summary of successful, failed, and skipped files.
#' @param urls character vector. Non-empty vector of URLs to download.
#' @param destfiles character vector. Destination file paths, in the same order
#'   and of the same length as `urls`.
#' @param token character(1) or NULL. Bearer token added to every request when
#'   supplied. Default is `NULL`. Resolve token files or environment variables
#'   with [get_token()] before calling this function.
#' @param show_progress logical(1). Show per-file progress messages and download
#'   progress bars. Default is `TRUE`; status and summary messages may still
#'   be printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file request, including the
#'   initial attempt. Must be at least 1. Default is 20.
#' @param rate_limit numeric(1). Request-throttling interval in seconds. Default
#'   is 2. Passed to [httr2::req_throttle()] as `rate = 1 / rate_limit`; the
#'   host-based token bucket controls the average rate and can allow bursts.
#' @param timeout numeric(1). Timeout in seconds for each request attempt.
#'   Default is 3600 (one hour). This does not limit the total time across
#'   multiple files or retries. Connection establishment also has a 30-second
#'   timeout, subject to the overall per-attempt timeout.
#' @param http_version integer(1) or NULL. Force HTTP version via curl's
#' CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
#' NULL (default) lets curl negotiate automatically. Pass 2L for servers
#' that drop HTTP/2 connections (e.g., www.mrlc.gov for NLCD).
#' @details
#' Existing non-empty destination files are skipped. Missing or empty files
#' are downloaded, with missing parent directories created as needed.
#'
#' HTTP 429, 502, 503, and 504 responses and transport failures are eligible
#' for retries. When a server retry delay is unavailable, the configured
#' backoff is a random delay between 0 and `min(i^2, 30)` seconds for retry
#' index `i`. Server-provided retry delays may exceed 30 seconds. HTTP 500 is
#' explicitly allowed by the error policy and is not included in the retry
#' status set. Download validation checks that the saved file is non-empty;
#' it does not validate its contents or format.
#'
#' Individual request errors are recorded and remaining files are attempted.
#' Files left by a failed request or empty download are removed. A warning
#' reports failed filenames after the download loop finishes.
#' @return
#' An invisible list containing:
#' * `success`: number of files downloaded and found to be non-empty.
#' * `failed`: number of files whose download or non-empty-file check failed.
#' * `skipped`: number of existing non-empty destination files.
#' * `failed_urls`: character vector of URLs whose downloads failed.
#' * `failed_files`: character vector of the corresponding destination
#'    basenames.
#'
#' All five fields are always present. When no downloads fail, `failed_urls`
#' and `failed_files` are `character(0)`, including when every file is skipped.
#' @seealso [get_token()], [check_destfile()], [download_preview()]
#' @examples
#' \dontrun{
#' download_run_method(
#'   urls = "https://example.com/data.nc4",
#'   destfiles = file.path(tempdir(), "data.nc4"),
#'   max_tries = 3
#' )
#' }
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
  # ----------------------------------------------------------------------------
  # Check that URLs and matching destination paths are supplied.
  if (is.null(urls) || length(urls) == 0) {
    stop("No URLs provided for download.\n")
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop("destfiles must have same length as urls.\n")
  }
  # ----------------------------------------------------------------------------
  # Retain destinations that are missing or empty.
  idx_download <- sapply(destfiles, amadeus:::check_destfile)
  chr_urls_filtered <- urls[idx_download]
  chr_destfiles_filtered <- destfiles[idx_download]
  # ----------------------------------------------------------------------------
  # Return early when all destination files already contain data.
  if (length(chr_urls_filtered) == 0) {
    message("All files already exist. Nothing to download.\n")
    return(invisible(list(
      success = 0,
      failed = 0,
      skipped = length(urls),
      failed_urls = character(0),
      failed_files = character(0)
    )))
  }
  # ----------------------------------------------------------------------------
  # Report the download count and initialize result tracking.
  message(sprintf(
    "Downloading %d files using httr2 (skipped %d existing files)...\n",
    length(chr_urls_filtered),
    sum(!idx_download)
  ))
  int_nfiles <- length(chr_urls_filtered)
  num_success <- 0
  num_failed <- 0
  chr_failed_urls <- character(0)
  chr_failed_files <- character(0)
  # ----------------------------------------------------------------------------
  # Download each file and record successes and failures.
  for (int_i in seq_along(chr_urls_filtered)) {
    # --------------------------------------------------------------------------
    # Select the URL and destination for this download.
    chr_url <- chr_urls_filtered[int_i]
    chr_destfile <- chr_destfiles_filtered[int_i]
    # --------------------------------------------------------------------------
    # Create the destination directory if needed.
    chr_destdir <- dirname(chr_destfile)
    if (!dir.exists(chr_destdir)) {
      dir.create(chr_destdir, recursive = TRUE)
    }
    # --------------------------------------------------------------------------
    # Report the current file number when progress is enabled.
    chr_progress_prefix <- sprintf("[%d/%d] ", int_i, int_nfiles)
    if (show_progress) {
      message(sprintf(
        "%sDownloading: %s",
        chr_progress_prefix,
        basename(chr_destfile)
      ))
    }
    # --------------------------------------------------------------------------
    # Handle request failures without interrupting the remaining downloads.
    tryCatch(
      {
        # ----------------------------------------------------------------------
        # Build the request and add authentication when a token is supplied.
        list_req <- httr2::request(chr_url)
        if (!is.null(token)) {
          list_req <- list_req |>
            httr2::req_headers(Authorization = paste("Bearer", token))
        }
        # ----------------------------------------------------------------------
        # Configure retries, rate limiting, timeouts, and HTTP error handling.
        # Allow HTTP 500 because some APIs return it with valid response bodies.
        # Retry HTTP 429/502/503/504 and transport failures up to `max_tries`.
        list_req <- list_req |>
          httr2::req_retry(
            max_tries = max_tries,
            is_transient = \(list_resp) {
              httr2::resp_status(list_resp) %in% c(429, 502, 503, 504)
            },
            retry_on_failure = TRUE,
            backoff = \(int_i) stats::runif(1) * pmin(int_i^2, 30)
          ) |>
          httr2::req_timeout(timeout) |>
          httr2::req_options(connecttimeout = 30L) |>
          httr2::req_throttle(rate = 1 / rate_limit) |>
          httr2::req_error(is_error = \(list_resp) {
            int_status <- httr2::resp_status(list_resp)
            int_status >= 400 && !(int_status %in% c(429, 500, 502, 503, 504))
          })
        # ----------------------------------------------------------------------
        # Apply an HTTP version override when supplied.
        if (!is.null(http_version)) {
          list_req <- list_req |>
            httr2::req_options(http_version = http_version)
        }
        # ----------------------------------------------------------------------
        # Enable the download progress bar when requested.
        if (show_progress) {
          list_req <- list_req |> httr2::req_progress(type = "down")
        }
        # ----------------------------------------------------------------------
        # Stream the response to the destination file.
        list_req |> httr2::req_perform(path = chr_destfile)
        # ----------------------------------------------------------------------
        # Verify the downloaded file exists and contains data.
        if (file.exists(chr_destfile) && file.size(chr_destfile) > 0) {
          # --------------------------------------------------------------------
          # Record the successful download and report its size.
          num_success <- num_success + 1
          chr_success_msg <- sprintf(
            "Success: %s (%s)",
            basename(chr_destfile),
            amadeus:::format_file_size(file.size(chr_destfile))
          )
          message(sprintf(
            "%s%s\n",
            if (show_progress) " OK " else "",
            chr_success_msg
          ))
        } else {
          # --------------------------------------------------------------------
          # Record the failed validation and remove any empty destination.
          num_failed <- num_failed + 1
          chr_failed_urls <- c(chr_failed_urls, chr_url)
          chr_failed_files <- c(chr_failed_files, basename(chr_destfile))
          if (file.exists(chr_destfile)) {
            file.remove(chr_destfile)
          }
          # --------------------------------------------------------------------
          # Report the failed validation using the selected progress format.
          chr_failure_msg <- sprintf(
            "Failed: %s (0 bytes)",
            basename(chr_destfile)
          )
          message(sprintf(
            "%s%s\n",
            if (show_progress) " FAIL " else "",
            chr_failure_msg
          ))
        }
        # ----------------------------------------------------------------------
        # Pause briefly before the next download.
        if (int_i < int_nfiles) {
          Sys.sleep(runif(1, 0.5, 1.5))
        }
      },
      error = function(list_error) {
        # ----------------------------------------------------------------------
        # Record the failed request.
        num_failed <<- num_failed + 1
        chr_failed_urls <<- c(chr_failed_urls, chr_url)
        chr_failed_files <<- c(chr_failed_files, basename(chr_destfile))
        # ----------------------------------------------------------------------
        # Report the request error.
        chr_error_msg <- sprintf(
          "Failed: %s - %s",
          basename(chr_destfile),
          conditionMessage(list_error)
        )
        message(sprintf(
          "%s%s\n",
          if (show_progress) " FAIL " else "",
          chr_error_msg
        ))
        # ----------------------------------------------------------------------
        # Remove any file left by the failed download.
        if (file.exists(chr_destfile)) {
          file.remove(chr_destfile)
        }
      }
    )
  }
  # ----------------------------------------------------------------------------
  # Report the download totals.
  message(sprintf(
    "\n=== Download Summary ===\n%d succeeded, %d failed, %d skipped\n",
    num_success,
    num_failed,
    sum(!idx_download)
  ))
  # ----------------------------------------------------------------------------
  # Warn with the names of files that failed to download.
  if (num_failed > 0) {
    warning(
      sprintf(
        "%d file(s) failed to download:\n  %s\n",
        num_failed,
        paste(chr_failed_files, collapse = "\n  ")
      ),
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # Return the counts and failed file details invisibly.
  invisible(list(
    success = num_success,
    failed = num_failed,
    skipped = sum(!idx_download),
    failed_urls = chr_failed_urls,
    failed_files = chr_failed_files
  ))
}

################################################################################
#' Format file size for display
#' @keywords internal
#' @noRd
format_file_size <- function(bytes) {
  # ----------------------------------------------------------------------------
  # Format the byte count in B, KB, MB, or GB according to its size.
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

################################################################################
#' Validate ZIP archive listings
#' @description
#' Check that each supplied file can be listed as a ZIP archive, without
#' extracting its contents. This detects responses such as text or HTML
#' notices saved under a ZIP filename.
#' @param files character. Paths to ZIP archives to check, in order.
#' @details
#' Calls [utils::unzip()] with `list = TRUE` for each file. A warning or error
#' while listing is treated as a validation failure. Validation stops at the
#' first failure and identifies the archive by its basename.
#'
#' When possible, the error includes the first three lines read from the
#' failed file as text. This preview can expose a provider's error or service
#' notice. Files are neither extracted nor removed, and callers may validate
#' archives even when extraction is disabled.
#'
#' A successful listing does not fully verify archive-member integrity or
#' the type, contents, or scientific validity of the extracted data.
#' @return `NULL`, invisibly, when all supplied archives can be listed
#' without warnings or errors, including when `files` is empty. Otherwise,
#' raises an error; no per-file logical result is returned.
#' @seealso [download_prism()], [download_unzip()], [utils::unzip()]
#' @keywords internal
#' @noRd
validate_zip <- function(files) {
  # ----------------------------------------------------------------------------
  # Loop over all files.
  for (f in files) {
    # --------------------------------------------------------------------------
    # Return `TRUE` if unzip to list successful; `FALSE` if not.
    lgl_zip <- tryCatch(
      {
        withCallingHandlers(
          {
            utils::unzip(f, list = TRUE)
            TRUE
          },
          warning = function(w) {
            stop(conditionMessage(w), call. = FALSE)
          }
        )
      },
      error = function(e) FALSE
    )
    # --------------------------------------------------------------------------
    # If unzip to list successful, read and report first lines.
    if (!lgl_zip) {
      chr_preview <- tryCatch(
        {
          paste0(readLines(f, n = 3, warn = FALSE), collapse = " ")
        },
        error = function(e) ""
      )
      stop(
        sprintf(
          "Downloaded archive '%s' is not a valid zip file.%s",
          basename(f),
          if (nzchar(chr_preview)) {
            paste0("Response preview: ", chr_preview)
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
  }
}

################################################################################
#' Extract downloaded archives
#' @description
#' Extract one or more downloaded `.zip`, `.7z`, `.tar`, or `.gz` archives,
#' including `.tar.gz` files, into shared or separate directories.
#' @param file_name character vector. One or more non-missing, non-empty
#'   archive paths, in extraction order.
#' @param directory_to_unzip character vector. One shared extraction directory
#'   or one directory per archive, matched by position to `file_name`.
#'   Paths must be non-missing and non-empty; other vector lengths are rejected.
#' @param unzip logical(1). Extract the supplied archives. Default is `TRUE`.
#'   `FALSE` skips path validation, extraction, and removal.
#' @param remove logical(1). Run [download_remove_zips()] once for the complete
#'   archive vector after the extraction loop finishes. Default is `FALSE`;
#'   ignored when `unzip = FALSE`.
#' @details
#' Archives are extracted sequentially. ZIP files use [utils::unzip()]; `.7z`,
#' `.tar`, and `.gz` files use [archive::archive_extract()]. Paths stored inside
#' each archive are preserved beneath its corresponding extraction directory.
#' An extraction error stops the remaining extractions and prevents cleanup;
#' files already extracted remain on disk. Warnings from the extraction
#' functions are not converted to errors.
#' @note When `remove = TRUE`, the existing cleanup helper removes the supplied
#'   archives and recursively deletes their immediate parent directories,
#'   including any remaining contents. Removal occurs after the extraction
#'   loop, not between individual archives.
#' @return Invisible `NULL`, for both single and multiple archives and when
#'   `unzip = FALSE`. Extracted paths and per-file statuses are not returned.
#' @seealso [download_remove_zips()], [utils::unzip()],
#'   [archive::archive_extract()]
#' @keywords internal
#' @export
download_unzip <- function(
  file_name,
  directory_to_unzip,
  unzip = TRUE,
  remove = FALSE
) {
  # ----------------------------------------------------------------------------
  # Skip extraction and removal when unzip = FALSE.
  if (!unzip) {
    message(paste0("Downloaded files will not be unzipped.\n"))
    return(invisible(NULL))
  }
  # ----------------------------------------------------------------------------
  # Accept one or more archives and extraction directories.
  checkmate::assert_character(
    file_name,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE
  )
  checkmate::assert_character(
    directory_to_unzip,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Allow one shared directory or exactly one directory per archive.
  int_nfile <- length(file_name)
  checkmate::assert_subset(length(directory_to_unzip), c(1L, int_nfile))
  # ----------------------------------------------------------------------------
  # Expand a shared directory while preserving explicit archive/directory pairs.
  chr_directories <- if (length(directory_to_unzip) == 1L) {
    rep(directory_to_unzip, int_nfile)
  } else {
    directory_to_unzip
  }
  # ----------------------------------------------------------------------------
  # Extract each archive into its corresponding directory.
  lapply(
    seq_along(file_name),
    function(f) {
      chr_file <- file_name[f]
      chr_directory <- chr_directories[f]
      chr_extension <- tolower(tools::file_ext(chr_file))
      # ------------------------------------------------------------------------
      # Select the extraction method separately for each archive.
      message("Unzipping ", chr_file, "...")
      if (chr_extension == "zip") {
        utils::unzip(chr_file, exdir = chr_directory)
      } else if (chr_extension %in% c("7z", "gz", "tar")) {
        archive::archive_extract(chr_file, dir = chr_directory)
      } else {
        stop(
          "Unsupported archive format for '",
          chr_file,
          "'. Expected .zip, .7z, .tar, or .gz.",
          call. = FALSE
        )
      }
      # ------------------------------------------------------------------------
      # Report the extraction directory.
      message("Files unzipped and saved in ", chr_directory, ".")
      invisible(NULL)
    }
  )
  # ----------------------------------------------------------------------------
  # Remove downloaded archives and their parent directories when requested.
  amadeus:::download_remove_zips(
    remove = remove,
    download_name = file_name
  )
  # ----------------------------------------------------------------------------
  # Return NULL.
  invisible(NULL)
}

################################################################################
#' Remove zip files
#' @description
#' Remove supplied archive files and recursively delete their parent
#' directories.
#' @param remove logical(1). Confirm removal. Default is FALSE.
#' @param download_name character vector. Archive file paths to remove.
#' @note
#' With `remove = TRUE`, each immediate parent directory returned by
#' `dirname(download_name)` is deleted recursively, including files that were
#' not listed in `download_name`. This helper does not check whether extraction
#' occurred. Call it only after all required extractions from those directories
#' have finished, with extracted data stored outside the directories removed.
#' @return Invisible `NULL`; optionally removes archives and their directories.
#' @keywords internal
#' @export
download_remove_zips <- function(
  remove = FALSE,
  download_name
) {
  # ----------------------------------------------------------------------------
  # Remove the downloaded files when requested.
  if (remove) {
    message(paste0("Removing download files...\n"))
    file.remove(download_name)
    # --------------------------------------------------------------------------
    # Remove their parent directories recursively, including remaining contents.
    unlink(dirname(download_name), recursive = TRUE)
    message(paste0("Download files removed.\n"))
  }
}

################################################################################
#' Create hash of downloaded files.
#' @description
#' Create a combined MD5 hash from file contents recursively under a specified
#' directory, including files in nested subdirectories and previously saved
#' files.
#' @param hash logical(1). Create hash of downloaded files.
#' @param dir character(1). Root directory to hash. Sibling directories are not
#'   included; hashing `data_files/` therefore excludes sibling `zip_files/`.
#' @return With `hash = TRUE`, a character(1) combined MD5 hash. With
#'   `hash = FALSE`, invisible `NULL`.
#' @keywords internal auxiliary
#' @export
download_hash <- function(
  hash = TRUE,
  dir = NULL
) {
  # ----------------------------------------------------------------------------
  # Calculate a combined file hash when requested.
  if (hash) {
    # --------------------------------------------------------------------------
    # Build the command to hash all files within the directory.
    h_command <- paste0(
      "find ",
      shQuote(dir),
      " -type f -exec md5sum {} + | awk '{print $1}' | sort -k 2 | md5sum"
    )
    # --------------------------------------------------------------------------
    # Run the command and return the hash without the trailing filename marker.
    h <- system(h_command, intern = TRUE)
    h_clean <- sub("  -$", "", h)
    return(h_clean)
  }
}

################################################################################
#' Return download results or a directory hash
#' @description
#' Return the download summary unchanged, or calculate a combined MD5 hash of
#' the files in the output directory.
#' @param results list. Download summary returned by [download_run_method()].
#'   Contains `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`.
#'   The two failure vectors are `character(0)` when no downloads failed,
#'   including when every file was skipped. Used only when `hash = FALSE`.
#' @param hash logical(1). Return a directory hash instead of the download
#'   summary.
#' @param directory_to_save character(1). Directory containing downloaded or
#'   extracted files. All files recursively under this directory are included,
#'   including existing files and files in per-archive subdirectories. Used only
#'   when `hash = TRUE`; the caller selects the directory to hash.
#' @return `results`, returned invisibly when `hash = FALSE`; otherwise a
#'   character scalar containing the combined MD5 hash from [download_hash()].
#' @seealso [download_run_method()], [download_hash()]
#' @keywords internal
#' @noRd
download_return <- function(
  results,
  hash,
  directory_to_save
) {
  # ----------------------------------------------------------------------------
  # Return a directory hash or an invisible download summary.
  if (hash) {
    amadeus:::download_hash(hash = hash, directory_to_save)
  } else {
    invisible(results)
  }
}

################################################################################
