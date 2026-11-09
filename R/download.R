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
#' Validate all supplied years and generate every year from the minimum
#' through the maximum, including both endpoints.
#' @param year numeric(1 or more). A non-empty vector of non-missing,
#'   integer-valued years between 1900 and the current calendar year.
#'   A single year, two endpoints, or a longer vector may be supplied in
#'   any order; repeated years are accepted.
#' @details Gaps are filled automatically: `c(2018, 2023)` and
#'   `c(2018, 2020, 2023)` both return `2018:2023`.
#'   Source-specific data availability limits are checked by the caller.
#' @return Numeric vector of consecutive years in ascending order, including
#'   both endpoints. A single input year returns one value.
#' @keywords internal
#' @noRd
sequence_years <- function(year) {
  # ----------------------------------------------------------------------------
  # Validate every supplied year before selecting the range.
  checkmate::assert_integerish(
    year,
    min.len = 1L,
    lower = 1900,
    upper = as.integer(format(Sys.Date(), "%Y")),
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Find the earliest and latest years.
  int_years <- range(year)
  # ----------------------------------------------------------------------------
  # Generate the inclusive sequence.
  seq(int_years[1L], int_years[2L], by = 1)
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
    # --------------------------------------------------------------------------
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
    # --------------------------------------------------------------------------
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
download_setup_dir <-
  function(directory, zip = FALSE) {
    # --------------------------------------------------------------------------
    # Sanitize the download path and create missing parent directories.
    chr_dir_sanitized <- amadeus::download_sanitize_path(directory)
    if (!dir.exists(chr_dir_sanitized)) {
      dir.create(chr_dir_sanitized, recursive = TRUE)
    }
    # --------------------------------------------------------------------------
    # Set up separate archive and data directories when requested.
    if (zip) {
      # ------------------------------------------------------------------------
      # Create the archive directory.
      directory_zip <- amadeus::download_sanitize_path(
        paste0(
          chr_dir_sanitized,
          "zip_files"
        )
      )
      if (!dir.exists(directory_zip)) {
        dir.create(directory_zip, recursive = TRUE)
      }
      # ------------------------------------------------------------------------
      # Create the extracted data directory.
      directory_data <- amadeus::download_sanitize_path(
        paste0(
          chr_dir_sanitized,
          "data_files"
        )
      )
      if (!dir.exists(directory_data)) {
        dir.create(directory_data, recursive = TRUE)
      }
      # ------------------------------------------------------------------------
      # Return the archive and data paths, in that order.
      return(c(directory_zip, directory_data))
    } else {
      # ------------------------------------------------------------------------
      # Return the main download path.
      chr_dir_sanitized
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
  # ----------------------------------------------------------------------------
  # Check the token storage method.
  method <- match.arg(method)

  # ----------------------------------------------------------------------------
  # Prompt for a token when none is supplied.
  if (is.null(token)) {
    if (interactive()) {
      cat("Enter your NASA EarthData token: ")
      token <- readline()
      token <- trimws(token)
    } else {
      stop("Token must be provided in non-interactive mode.\n", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # Reject an empty token.
  if (!nzchar(token)) {
    stop("Token cannot be empty.\n", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Store the token using the selected method.
  switch(
    method,
    renviron = {
      # ------------------------------------------------------------------------
      # Locate the user environment file.
      renviron_path <- path.expand("~/.Renviron")

      # ------------------------------------------------------------------------
      # Read existing settings and remove previous token entries.
      if (file.exists(renviron_path)) {
        renviron_lines <- readLines(renviron_path)
        renviron_lines <- renviron_lines[
          !grepl("^NASA_EARTHDATA_TOKEN=", renviron_lines)
        ]
      } else {
        renviron_lines <- character(0)
      }

      # ------------------------------------------------------------------------
      # Save the token alongside the existing environment settings.
      renviron_lines <- c(
        renviron_lines,
        paste0("NASA_EARTHDATA_TOKEN=", token)
      )
      writeLines(renviron_lines, renviron_path)

      # ------------------------------------------------------------------------
      # Report where the token was saved and how to load it.
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
      # ------------------------------------------------------------------------
      # Save the token in the user token file.
      token_path <- path.expand("~/.nasa_earthdata_token")
      writeLines(token, token_path)

      # ------------------------------------------------------------------------
      # Restrict file access to the owner on Unix-like systems.
      if (.Platform$OS.type != "windows") {
        Sys.chmod(token_path, mode = "0600")
      }

      # ------------------------------------------------------------------------
      # Report where the token was saved and how to use it.
      message(sprintf("Token saved to %s\n", token_path))
      message(
        "  Use in functions:",
        " nasa_earth_data_token = '~/.nasa_earthdata_token'\n"
      )
    },

    session = {
      # ------------------------------------------------------------------------
      # Set the token for the current session and report its lifetime.
      Sys.setenv(NASA_EARTHDATA_TOKEN = token)
      message("Token set for current R session\n")
      message(
        "  This will be lost when you close R.",
        " Use method='renviron' for permanent setup.\n"
      )
    }
  )

  # ----------------------------------------------------------------------------
  # Return invisibly after configuring authentication.
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
  # ----------------------------------------------------------------------------
  # Use the environment variable first when it contains a token.
  token_env <- Sys.getenv(env_var, unset = NA)
  if (!is.na(token_env) && nzchar(token_env)) {
    message(sprintf("Using token from environment variable: %s\n", env_var))
    return(trimws(token_env))
  }

  # ----------------------------------------------------------------------------
  # Check the supplied value when no environment token is available.
  if (!is.null(token)) {
    # --------------------------------------------------------------------------
    # Read and validate the token when the supplied value is a file path.
    if (length(token) == 1 && file.exists(token)) {
      message(sprintf("Reading token from file: %s\n", token))
      token_file <- trimws(readLines(token, n = 1, warn = FALSE))
      if (length(token_file) == 0 || !nzchar(token_file)) {
        stop(sprintf("Token file '%s' is empty.\n", token))
      }
      return(token_file)
    }

    # --------------------------------------------------------------------------
    # Use a supplied token string without printing its contents.
    if (length(token) == 1 && nzchar(token)) {
      message("Using provided token string.\n")
      return(trimws(token))
    }
  }

  # ----------------------------------------------------------------------------
  # Explain how to provide a token when no source contains one.
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
#' Generate an inclusive sequence of dates
#' @description
#' Generate consecutive daily dates from a single date or two range endpoints.
#' Range endpoints are ordered before generating the sequence.
#' @param date character(1 or 2). A single date or two range endpoints in
#' `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.
#' @param hyphen logical(1). Include hyphens in the returned date strings.
#' If `FALSE` (the default), return `"YYYYMMDD"`; if `TRUE`, return
#' `"YYYY-MM-DD"`.
#' @return A character vector of daily dates in ascending order, including
#' both endpoints. A single input date returns one date string. The output
#' format is controlled by `hyphen`.
#' @author Mitchell Manware
#' @seealso [base::seq.Date()], [base::as.Date()]
#' @examples
#' sequence_dates("2020-05-05")
#' sequence_dates(c("2020-05-07", "2020-05-05"))
#' sequence_dates(c("2020-05-05", "2020-05-07"), hyphen = TRUE)
#' @keywords internal auxiliary
#' @export
sequence_dates <- function(date, hyphen = FALSE) {
  # ----------------------------------------------------------------------------
  # Ensure `hyphen` is TRUE/FALSE.
  checkmate::assert_flag(hyphen)
  # ----------------------------------------------------------------------------
  # Repeat date if only one.
  chr_dates <- if (length(date) == 1) rep(date, 2) else date
  # ----------------------------------------------------------------------------
  # Ensure `date` is length 2.
  checkmate::assert_character(chr_dates, n.chars = 10, len = 2)
  # ----------------------------------------------------------------------------
  # Order dates.
  chr_dates <- sort(chr_dates)
  # ----------------------------------------------------------------------------
  # Ensure valid date range.
  checkmate::assert_date(
    as.Date(chr_dates, "%Y-%m-%d"),
    lower = as.Date("1950-01-01"),
    upper = Sys.Date()
  )
  # ----------------------------------------------------------------------------
  # Generate sequence.
  date_seq <- as.character(
    seq(
      as.Date(chr_dates[1], format = "%Y-%m-%d"),
      as.Date(chr_dates[2], format = "%Y-%m-%d"),
      "day"
    )
  )
  # ----------------------------------------------------------------------------
  # Format dates according to `hyphen`.
  if (hyphen) date_seq else gsub("-", "", as.character(date_seq))
}

################################################################################
#' Generate a daily sequence of clock times
#' @description
#' Generate equally spaced clock times throughout a day, starting at an
#' offset from midnight and ending before the next midnight, then retain
#' only the requested hours.
#' @param by integer(1). Positive interval in minutes. Default is 60.
#' @param offset integer(1). Minutes after midnight at which the sequence
#' starts, from 0 through 1439. Default is 0.
#' @param colon logical(1). Include a colon in returned time strings.
#' If `FALSE` (the default), return `"HHMM"`; if `TRUE`, return `"HH:MM"`.
#' @param hours integer vector. Non-empty selection of hours from 0 through
#' 23 to retain. Default is `0:23`, which retains all hours. Input order and
#' repeated hours do not affect the output.
#' @details The interval and offset define the daily schedule before filtering
#' by `hours`; selecting hours does not restart the interval at each hour.
#' @return A character vector of retained times in ascending order. Times never
#' extend past `"23:59"` or wrap into the next day. Returns `character(0)` when
#' no scheduled times fall within the requested hours.
#' @examples
#' sequence_times()
#' sequence_times(by = 60, offset = 30)
#' sequence_times(by = 90, offset = 45, colon = TRUE)
#' sequence_times(offset = 30, hours = c(0, 6, 12, 18))
#' @keywords internal
#' @noRd
sequence_times <- function(
  by = 60L,
  offset = 0L,
  colon = FALSE,
  hours = 0:23
) {
  # ----------------------------------------------------------------------------
  # Check output format, interval, starting offset, and selected hours.
  checkmate::assert_flag(colon)
  checkmate::assert_int(by, lower = 1L)
  checkmate::assert_int(offset, lower = 0L, upper = 1439L)
  checkmate::assert_integerish(
    hours,
    lower = 0L,
    upper = 23L,
    min.len = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate the sequence in minutes and retain the requested hours.
  int_seq <- seq.int(from = offset, to = 1439L, by = by)
  int_seq <- int_seq[int_seq %/% 60L %in% hours]
  # ----------------------------------------------------------------------------
  # Format clock times.
  sprintf(
    if (colon) "%02d:%02d" else "%02d%02d",
    int_seq %/% 60L,
    int_seq %% 60L
  )
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
      check_url_status
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
  if (!amadeus::check_url_status(url)) {
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
#' * `failed_files`: character vector of the corresponding destination basenames.
#'
#' When every file is skipped, only `success`, `failed`, and `skipped` are
#' returned. Otherwise the two failure vectors are present, and are empty
#' when no downloads failed.
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
  needs_download <- sapply(destfiles, amadeus::check_destfile)
  urls_filtered <- urls[needs_download]
  destfiles_filtered <- destfiles[needs_download]

  # ----------------------------------------------------------------------------
  # Return early when all destination files already contain data.
  if (length(urls_filtered) == 0) {
    message("All files already exist. Nothing to download.\n")
    return(invisible(list(success = 0, failed = 0, skipped = length(urls))))
  }

  # ----------------------------------------------------------------------------
  # Report the download count and initialize result tracking.
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

  # ----------------------------------------------------------------------------
  # Download each file and record successes and failures.
  for (i in seq_along(urls_filtered)) {
    # --------------------------------------------------------------------------
    # Select the URL and destination for this download.
    url <- urls_filtered[i]
    destfile <- destfiles_filtered[i]

    # --------------------------------------------------------------------------
    # Create the destination directory if needed.
    destdir <- dirname(destfile)
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }

    # --------------------------------------------------------------------------
    # Report the current file number when progress is enabled.
    progress_prefix <- sprintf("[%d/%d] ", i, n_files)
    if (show_progress) {
      message(sprintf(
        "%sDownloading: %s",
        progress_prefix,
        basename(destfile)
      ))
    }

    # --------------------------------------------------------------------------
    # Handle request failures without interrupting the remaining downloads.
    tryCatch(
      {
        # ----------------------------------------------------------------------
        # Build the request and add authentication when a token is supplied.
        req <- httr2::request(url)

        if (!is.null(token)) {
          req <- req |>
            httr2::req_headers(Authorization = paste("Bearer", token))
        }

        # ----------------------------------------------------------------------
        # Configure retries, rate limiting, timeouts, and HTTP error handling.
        # Allow HTTP 500 because some APIs return it with valid response bodies.
        # Retry HTTP 429/502/503/504 and transport failures up to `max_tries`.
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

        # ----------------------------------------------------------------------
        # Apply an HTTP version override when supplied.
        if (!is.null(http_version)) {
          req <- req |> httr2::req_options(http_version = http_version)
        }

        # ----------------------------------------------------------------------
        # Enable the download progress bar when requested.
        if (show_progress) {
          req <- req |> httr2::req_progress(type = "down")
        }

        # ----------------------------------------------------------------------
        # Stream the response to the destination file.
        req |> httr2::req_perform(path = destfile)

        # ----------------------------------------------------------------------
        # Verify the downloaded file exists and contains data.
        if (file.exists(destfile) && file.size(destfile) > 0) {
          # --------------------------------------------------------------------
          # Record the successful download and report its size.
          n_success <- n_success + 1
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
          # --------------------------------------------------------------------
          # Record the failed validation and remove any empty destination.
          n_failed <- n_failed + 1
          failed_urls <- c(failed_urls, url)
          failed_files <- c(failed_files, basename(destfile))

          if (file.exists(destfile)) {
            file.remove(destfile)
          }

          # --------------------------------------------------------------------
          # Report the failed validation using the selected progress format.
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

        # ----------------------------------------------------------------------
        # Pause briefly before the next download.
        if (i < n_files) {
          Sys.sleep(runif(1, 0.5, 1.5))
        }
      },
      error = function(e) {
        # ----------------------------------------------------------------------
        # Record the failed request.
        n_failed <<- n_failed + 1
        failed_urls <<- c(failed_urls, url)
        failed_files <<- c(failed_files, basename(destfile))

        # ----------------------------------------------------------------------
        # Report the request error.
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

        # ----------------------------------------------------------------------
        # Remove any file left by the failed download.
        if (file.exists(destfile)) {
          file.remove(destfile)
        }
      }
    )
  }

  # ----------------------------------------------------------------------------
  # Report the download totals.
  message(sprintf(
    "\n=== Download Summary ===\n%d succeeded, %d failed, %d skipped\n",
    n_success,
    n_failed,
    sum(!needs_download)
  ))

  # ----------------------------------------------------------------------------
  # Warn with the names of files that failed to download.
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

  # ----------------------------------------------------------------------------
  # Return the counts and failed file details invisibly.
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
  # Return NULL if `unzip = FALSE`.
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
  } else if (ext %in% c("7z", "gz", "tar")) {
    archive::archive_extract(file_name, dir = directory_to_unzip)
  } else {
    stop("Unsupported archive format. Expected .zip, .7z., .tar, or .gz")
  }
  # ----------------------------------------------------------------------------
  # Report where the extracted files were saved.
  message(paste0(
    "Files unzipped and saved in ",
    directory_to_unzip,
    ".\n"
  ))
  # ----------------------------------------------------------------------------
  # Remove downloaded archives and their parent directories when requested.
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
  # ----------------------------------------------------------------------------
  # Return a directory hash or an invisible download summary.
  if (hash) {
    amadeus::download_hash(hash = hash, directory_to_save)
  } else {
    invisible(results)
  }
}

################################################################################
