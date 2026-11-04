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
    if ("extent" %in% names(parameters)) {
      parameters <- parameters[-grep("extent", names(parameters))]
    }
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
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
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
    if (remove) {
      file.remove(commands_txt)
    }
  }
