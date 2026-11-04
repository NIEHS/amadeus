################################################################################
# Primary {download_data} function.
################################################################################
#' Download raw data wrapper function
# nolint start
#' @description
#' The \code{download_data()} function accesses and downloads atmospheric,
#' meteorological, and environmental data from various open-access data
#' sources.
# nolint end
#' @param dataset_name character(1). Dataset to download.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token. Required for NASA EarthData datasets: `"geos"`,
#' `"merra2"`, `"modis"`, `"sedac_groads"` / `"groads"`, and
#' `"sedac_population"` / `"population"`. Can be a token string, a path to a
#' file containing the token, or \code{NULL} to read from the
#' \code{NASA_EARTHDATA_TOKEN} environment variable. Ignored for datasets that
#' do not use NASA EarthData authentication.
#' @param rate_limit numeric(1). Minimum seconds between HTTP requests
#' (default 2). Passed to the underlying download function.
#' @param ... Additional arguments passed to each download function.
#' @note
#' - All download function names are in \code{download_*} formats
#' @author Insang Song
#' @seealso
#' For details of each download function per dataset,
#' Please refer to:
#' * \code{\link{download_aqs}}: `"aqs"`, `"AQS"`
#' * \code{\link{download_ecoregion}}: `"ecoregions"`, `"ecoregion"`
#' * \code{\link{download_geos}}: `"geos"`
#' * \code{\link{download_goes}}: `"goes"`, `"goes_adp"`, `"GOES"`
#' * \code{\link{download_gmted}}: `"gmted"`, `"GMTED"`
#' * \code{\link{download_koppen_geiger}}: `"koppen"`, `"koppengeiger"`
#' * \code{\link{download_merra2}}: "merra2", `"merra"`, `"MERRA"`, `"MERRA2"`
#' * \code{\link{download_narr}}: `"narr"`
#' * \code{\link{download_nlcd}}: `"nlcd"`, `"NLCD"`
#' * \code{\link{download_hms}}: `"noaa"`, `"smoke"`, `"hms"`
#' * \code{\link{download_groads}}: `"sedac_groads"`, `"groads"`
#' * \code{\link{download_population}}: `"sedac_population"`,
#'   `"population"`
#' * \code{\link{download_modis}}: `"modis"`, `"MODIS"`
#' * \code{\link{download_tri}}: `"tri"`, `"TRI"`
#' * \code{\link{download_nei}}: `"nei"`, `"NEI"`
#' * \code{\link{download_gridmet}}: `"gridMET"`, `"gridmet"`
#' * \code{\link{download_terraclimate}}: `"TerraClimate"`, `"terraclimate"`
#' * \code{\link{download_huc}}: `"huc"`
#' * \code{\link{download_cropscape}}: `"cropscape"`, `"cdl"`
#' * \code{\link{download_prism}}: `"prism"`
#' * \code{\link{download_edgar}}: `"edgar"`
#' * \code{\link{download_improve}}: `"improve"`, `"IMPROVE"`
#' * \code{\link{download_drought}}: `"drought"`, `"spei"`, `"eddi"`, `"usdm"`
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Data files will be downloaded and stored in respective
#' sub-directories within \code{directory_to_save}. File format and
#' sub-directory names depend on data source and dataset of interest.
#' @examples
#' \dontrun{
#' download_data(
#'   dataset_name = "narr",
#'   variables = "weasd",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_data <-
  function(
    dataset_name = c(
      "aqs",
      "ecoregion",
      "ecoregions",
      "geos",
      "goes",
      "goes_adp",
      "GOES",
      "gmted",
      "koppen",
      "koppengeiger",
      "merra2",
      "merra",
      "modis",
      "narr",
      "nlcd",
      "noaa",
      "sedac_groads",
      "sedac_population",
      "groads",
      "population",
      "hms",
      "smoke",
      "tri",
      "nei",
      "gridmet",
      "terraclimate",
      "huc",
      "cropscape",
      "cdl",
      "prism",
      "edgar",
      "improve",
      "IMPROVE",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    hash = FALSE,
    nasa_earth_data_token = NULL,
    rate_limit = 2,
    ...
  ) {
    dataset_name <- tolower(dataset_name)
    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    what_to_run <- switch(
      dataset_name,
      aqs = download_aqs,
      ecoregion = download_ecoregion,
      ecoregions = download_ecoregion,
      geos = download_geos,
      goes = download_goes,
      goes_adp = download_goes,
      gmted = download_gmted,
      koppen = download_koppen_geiger,
      koppengeiger = download_koppen_geiger,
      merra2 = download_merra2,
      merra = download_merra2,
      narr = download_narr,
      nlcd = download_nlcd,
      noaa = download_hms,
      smoke = download_hms,
      hms = download_hms,
      sedac_groads = download_groads,
      groads = download_groads,
      sedac_population = download_population,
      population = download_population,
      modis = download_modis,
      tri = download_tri,
      nei = download_nei,
      gridmet = download_gridmet,
      terraclimate = download_terraclimate,
      huc = download_huc,
      cropscape = download_cropscape,
      cdl = download_cropscape,
      prism = download_prism,
      edgar = download_edgar,
      improve = download_improve,
      drought = download_drought,
      spei = function(...) download_drought(source = "spei", ...),
      eddi = function(...) download_drought(source = "eddi", ...),
      usdm = function(...) download_drought(source = "usdm", ...)
    )

    call_args <- c(
      list(
        directory_to_save = directory_to_save,
        acknowledgement = acknowledgement,
        hash = hash,
        rate_limit = rate_limit
      ),
      list(...)
    )

    # Only pass nasa_earth_data_token to functions that accept it
    if ("nasa_earth_data_token" %in% names(formals(what_to_run))) {
      call_args$nasa_earth_data_token <- nasa_earth_data_token
    }

    return <- tryCatch(
      {
        do.call(what_to_run, call_args)
      },
      error = function(e) {
        stop(
          paste0(
            e,
            "\n",
            paste0(deparse(args(what_to_run)), collapse = "\n"),
            "\n",
            "Please refer to the argument list and ",
            "the error message above to rectify the error.\n"
          )
        )
      }
    )

    return(return)
  }

################################################################################
# Support and internal {download} functions.
################################################################################
#' Generate an inclusive sequence of years
#' @description
#' Validate a single year or two range endpoints, order the endpoints, and
#' generate all years in the range.
#' @param year numeric(1 or 2). Non-missing, non-negative, integer-valued years.
#'   Supply a single year or two range endpoints in either order.
#' @details Source-specific data availability limits are not checked.
#' @return Numeric vector of consecutive years in ascending order, including
#'   both endpoints. A single input year returns one value.
#' @keywords internal
#' @noRd
sequence_years <- function(year) {
  # ----------------------------------------------------------------------------
  # Repeat year if only one.
  int_years <- if (length(year) == 1) rep(year, 2) else year
  # ----------------------------------------------------------------------------
  # Ensure `year` is length 2.
  checkmate::assert_integerish(
    int_years,
    len = 2L,
    lower = 0,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Order years.
  int_years <- int_years[order(int_years)]
  # ----------------------------------------------------------------------------
  # Generate sequence.
  seq(int_years[1], int_years[2], 1)
}

################################################################################
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

################################################################################
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

################################################################################
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

################################################################################
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

################################################################################
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
  http_status_ok <- c(200, 206)

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

  if (!is.na(status_head) && status_head %in% http_status_ok) {
    Sys.sleep(1)
    return(TRUE)
  }

  # Some hosts reject/flake on HEAD; probe with a tiny ranged GET.
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

################################################################################
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
            backoff = \(i) stats::runif(1) * pmin(i^2, 30)
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

################################################################################
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

################################################################################
#' Extract downloaded archives
#' @description
#' Extract downloaded ".zip" or ".7z" files.
#' @param file_name character(1). Full archive file path
#' @param directory_to_unzip character(1). Directory to extract
#' data
#' @param unzip logical(1). Extract (\code{TRUE}) or not.
#' @param remove logical(1). Confirm removal. Default is FALSE.
#' @return NULL; extracts downloaded archive files
#' @keywords internal
#' @export
download_unzip <- function(
  file_name,
  directory_to_unzip,
  unzip = TRUE,
  remove = FALSE
) {
  # ----------------------------------------------------------------------------
  # Return NULL if `unzip = FALSE`
  if (!unzip) {
    message(paste0("Downloaded files will not be unzipped.\n"))
    return(NULL)
  }
  # ----------------------------------------------------------------------------
  # Unzip according to compression type.
  ext <- tolower(tools::file_ext(file_name))
  message(paste0("Unzipping files...\n"))
  if (ext == "zip") {
    unzip(file_name, exdir = directory_to_unzip)
  } else if (ext == "7z") {
    archive::archive_extract(file_name, dir = directory_to_unzip)
  } else {
    stop("Unsupported archive format. Expected .zip or .7z.")
  }
  # ----------------------------------------------------------------------------
  message(paste0(
    "Files unzipped and saved in ",
    directory_to_unzip,
    ".\n"
  ))
  # ----------------------------------------------------------------------------
  # Remove zip files if `remove = TRUE`
  amadeus::download_remove_zips(
    remove = remove,
    download_name = file_name
  )
}

################################################################################
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
download_remove_zips <- function(
  remove = FALSE,
  download_name
) {
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

################################################################################
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

################################################################################
#' Return download results or a directory hash
#' @description
#' Return the download summary unchanged, or calculate a combined MD5 hash of
#' the files in the output directory.
#' @param results list. Download summary returned by [download_run_method()].
#'   Used only when `hash = FALSE`.
#' @param hash logical(1). Return a directory hash instead of the download
#'   summary.
#' @param directory_to_save character(1). Directory containing downloaded or
#'   extracted files. Used only when `hash = TRUE`.
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
  if (hash) {
    amadeus::download_hash(hash = hash, directory_to_save)
  } else {
    invisible(results)
  }
}

################################################################################
