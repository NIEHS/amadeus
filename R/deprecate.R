################################################################################
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
    # --------------------------------------------------------------------------
    # Stop when the download acknowledgement has not been given.
    if (!acknowledgement) {
      stop(paste0(
        "Data download acknowledgement is set to FALSE. ",
        "Please acknowledge that the data downloaded using this ",
        "function may be very large and use lots of machine storage ",
        "and memory.\n"
      ))
    }
  }

################################################################################
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
    # --------------------------------------------------------------------------
    # Exclude the optional spatial extent from required parameters.
    if ("extent" %in% names(parameters)) {
      parameters <- parameters[-grep("extent", names(parameters))]
    }
    # --------------------------------------------------------------------------
    # Stop if any remaining parameter is NULL.
    parameters_status <- any(unlist(lapply(parameters, is.null)))
    if (parameters_status) {
      stop(paste0("One or more parameters are NULL\n"))
    }
  }

################################################################################
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
    # --------------------------------------------------------------------------
    # Remove an existing download command file.
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    # --------------------------------------------------------------------------
    # Redirect subsequent output to a new command file.
    sink(file = command_txt, append = FALSE)
  }

################################################################################
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
  # ----------------------------------------------------------------------------
  # Read the command file and return its rows as a vector.
  commands <- utils::read.csv(commands_path, header = FALSE)
  commands <- commands[seq_len(nrow(commands)), ]
  return(commands)
}

################################################################################
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
  # ----------------------------------------------------------------------------
  # Return early when the URL position is unspecified.
  if (is.null(position)) {
    message("URL position in command is not defined.")
    return(NULL)
  }
  # ----------------------------------------------------------------------------
  # Extract the requested space-delimited field from each command.
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

################################################################################
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
  # ----------------------------------------------------------------------------
  # Warn about the legacy command runner once per session.
  if (!isTRUE(getOption("amadeus.download_run.warned"))) {
    warning(
      "download_run() is deprecated. Use download_run_method() instead.\n",
      "  Old: download_modis(..., download = TRUE)\n",
      "  New: download_modis(...) uses httr2 by default\n",
      call. = FALSE
    )
    options(amadeus.download_run.warned = TRUE)
  }

  # ----------------------------------------------------------------------------
  # Prepare the command file and runner for the current operating system.
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
  # ----------------------------------------------------------------------------
  # Execute or skip the saved download commands.
  system_command <- paste0(runner, shQuote(commands_txt))
  if (download == TRUE) {
    message(paste0("Downloading requested files...\n"))
    system(command = system_command, intern = TRUE)
    message(paste0("Requested files have been downloaded.\n"))
  } else {
    message(paste0("Skipping data download.\n"))
  }
  # ----------------------------------------------------------------------------
  # Remove the command file when requested.
  amadeus::download_remove_command(
    commands_txt = commands_txt,
    remove = remove
  )
}

################################################################################
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
    # --------------------------------------------------------------------------
    # Remove the command file when requested.
    if (remove) {
      file.remove(commands_txt)
    }
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
    # --------------------------------------------------------------------------
    # Generate daily dates between the supplied endpoints.
    dates_original <- seq(
      as.Date(date_start, format = "%Y-%m-%d"),
      as.Date(date_end, format = "%Y-%m-%d"),
      "day"
    )
    # --------------------------------------------------------------------------
    # Return compact date strings or retain the Date vector.
    if (sub_hyphen) {
      dates_sub_hyphen <- gsub("-", "", as.character(dates_original))
      return(dates_sub_hyphen)
    } else {
      return(dates_original)
    }
  }

################################################################################
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
    # --------------------------------------------------------------------------
    # Select whole-hour or half-hour values using the collection suffix.
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      ts <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      ts <- seq(from = 0, to = 2300, by = 100)
    }
    # --------------------------------------------------------------------------
    # Format the sequence as four-digit clock times.
    time_sequence <- sprintf("%04d", ts)
    return(time_sequence)
  }
