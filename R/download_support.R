################################################################################
# Date created: 2023-12-06
# Packages required: None
################################################################################


#' Check if input directory exists
#' @param directory character(1) directory path
#' @description If directory does not exist, the directory
#' will be created.
#' @returns NULL
#' @export
download_setup_dir <-
  function(directory) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }


#' Sanitize path to end with a forward slash
#' @param directory character(1). Path
#' @returns character ending with a forward slash.
#' @export
download_sanitize_path <-
  function(directory) {
    #### 1. directory setup
    chars_dir <- nchar(directory)
    if (substr(
      directory,
      chars_dir,
      chars_dir
    ) != "/") {
      directory <-
        paste(directory,
          "/",
          sep = ""
        )
    }
    return(directory)
  }


#' Check for data download acknowledgement
#' @param data_download_acknowledgement logical(1). Whether to
#' start downloading
#' @returns NULL
#' @export
download_permit <-
  function(data_download_acknowledgement) {
    if (!data_download_acknowledgement) {
      stop(paste0(
        "Data download acknowledgement is set to FALSE. ",
        "Please acknowledge that the data downloaded using this ",
        "function may be very large and use lots of machine storage ",
        "and memory.\n"
      ))
    }
  }


#' download_run: execute or skip \code{system_command}
#' in data download function.
#'
#' @description
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (\code{TRUE}) or
#'  skip (\code{FALSE}) download.
#' @param system_command character(1). Linux command to execute downloads.
#' Inherited from data download function.
#' @returns NULL
#' @export
download_run <- function(
    download = FALSE,
    system_command = NULL) {
  if (download == TRUE) {
    cat(paste0("Downloading requested files...\n"))
    system(command = system_command)
    cat(paste0("Requested files have been downloaded.\n"))
  } else {
    cat(paste0("Skipping data download.\n"))
    return(NULL)
  }
}


#' Remove or keep wget command file
#'
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or
#'  keep (\code{FALSE}) commands
#' @returns NULL
#' @export
download_remove_command <-
  function(commands_txt = NULL,
           remove = FALSE) {
    if (remove) {
      file.remove(commands_txt)
    }
  }


#' Start sink download commands into a text file
#' @param command_txt character(1). file path to export commands.
#' @returns NULL
#' @export
download_sink <-
  function(command_txt) {
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    sink(file = command_txt, append = FALSE)
  }


#' Unzip downloaded data
#' @param file_name character(1). Full zip file path
#' @param directory_to_unzip character(1). Directory to unzip
#' data
#' @param unzip logical(1). Unzip (\code{TRUE}) or not.
#' @returns NULL
#' @export
download_unzip <-
  function(file_name,
           directory_to_unzip,
           unzip = TRUE) {
    if (!unzip) {
      cat(paste0("Downloaded files will not be unzipped.\n"))
      return(NULL)
    }

    cat(paste0("Unzipping files...\n"))
    unzip(file_name,
      exdir = directory_to_unzip
    )
    cat(paste0(
      "Files unzipped and saved in ",
      directory_to_unzip,
      ".\n"
    ))
  }


#' Remove downloaded zip files
#' @param remove logical(1). Confirm removal. Default is FALSE.
#' @param download_name character. Full zip file path
#' @returns NULL
#' @export
download_remove_zips <-
  function(remove = FALSE,
           download_name) {
    #### remove zip files
    if (remove) {
      cat(paste0("Removing download files...\n"))
      file.remove(download_name)
      cat(paste0("Download files removed.\n"))
    }
  }


#' Check for null arguments
#' @param parameters parameters passed to function (called by
#' \code{mget(ls())}.)
#' @returns NULL
#' @export
check_for_null_parameters <-
  function(
      parameters) {
    parameters_status <- any(unlist(lapply(parameters, is.null)))
    if (parameters_status) {
      stop(paste0("One or more parameters are NULL\n"))
    }
  }

#' Generate sequence of dates based on `date_start` and `date_end`.
#' @param date_start character(1). Beginning of date sequence.
#' @param date_end character(1). End of date sequence.
#' @param sub_hyphen logical(1). Substitute hyphen in dates. If `TRUE`, returns
#' date sequence as "YYYYMMDD". If `FALSE`, returns date sequence as
#' "YYYY-MM-DD".
#' @returns vector
#' @export
generate_date_sequence <-
  function(
      date_start,
      date_end,
      sub_hyphen = TRUE) {
    dates_original <- seq(
      as.Date(date_start, format = "%Y-%m-%d"),
      as.Date(date_end, format = "%Y-%m-%d"),
      "day"
    )
    if (sub_hyphen == TRUE) {
      dates_sub_hyphen <- gsub("-", "", as.character(dates_original))
      return(dates_sub_hyphen)
    } else {
      return(dates_original)
    }
  }
