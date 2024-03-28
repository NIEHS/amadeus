# Download auxiliary and internal functions

#' Setup directory
#' @description
#' Create \code{directory} if it does not already exist.
#' @param directory character(1) directory path
#' @description If directory does not exist, the directory
#' will be created.
#' @returns NULL
#' @keywords internal
#' @export
download_setup_dir <-
  function(directory) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }


#' Sanitize directory
#' @description
#' Append forward slash to end of \code{directory} if it does not already
#' end with one.
#' @param directory character(1). Path
#' @returns character ending with a forward slash.
#' @keywords internal
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


#' Check data download acknowledgement
#' @description
#' Return an error if the \code{acknowledgement = FALSE}.
#' @param acknowledgement logical(1). Whether to
#' start downloading
#' @note
#' The \code{acknowledgement} parameter is designed to help users avoid
#' accidentally initiating a very large data download that may take a very long
#' time to run or exceed machine capabilities.
#' @returns NULL
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


#' Run download commands
#' @description
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (\code{TRUE}) or
#'  skip (\code{FALSE}) download.
#' @param system_command character(1). Linux command to execute downloads.
#' Inherited from data download function.
#' @returns NULL
#' @keywords internal
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


#' Remove download commands
#' @description
#' Remove or retain the .txt file storing all download commands.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or
#'  keep (\code{FALSE}) commands
#' @returns NULL
#' @keywords internal
#' @export
download_remove_command <-
  function(commands_txt = NULL,
           remove = FALSE) {
    if (remove) {
      file.remove(commands_txt)
    }
  }


#' Sink download commands
#' @description
#' Open connection to \code{command_txt} file to store download commands.
#' @param command_txt character(1). file path to export commands.
#' @returns NULL
#' @keywords internal
#' @export
download_sink <-
  function(command_txt) {
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    sink(file = command_txt, append = FALSE)
  }


#' Unzip zip files
#' @description
#' Unzip (inflate) downloaded ".zip" files.
#' @param file_name character(1). Full zip file path
#' @param directory_to_unzip character(1). Directory to unzip
#' data
#' @param unzip logical(1). Unzip (\code{TRUE}) or not.
#' @returns NULL
#' @keywords internal
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


#' Remove zip files
#' @description
#' Remove downloaded ".zip" files.
#' @param remove logical(1). Confirm removal. Default is FALSE.
#' @param download_name character. Full zip file path
#' @note
#' If \code{remove = TRUE}, ensure that \code{unzip = TRUE}. Choosing to remove
#' ".zip" files without unzipping will retain none of the downloaded data.
#' @returns NULL
#' @keywords internal
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


#' Check parameters
#' @description
#' Check that all parameters have been assigned a value.
#' @param parameters parameters passed to function (called by
#' \code{mget(ls())}.)
#' @returns NULL
#' @keywords internal
#' @export
check_for_null_parameters <-
  function(
      parameters) {
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
#' @returns vector
#' @keywords auxiliary
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


#' Check EPA certificate
#' @param epa_certificate_path character(1).
#' Full path of a converted certificate of EPA.
#' Should end with `.pem`
#' @param certificate_url character(1).
#' URL of the original certificate.
#' @returns A file designated in `epa_certificate_path`
#' @author Insang Song
#' @importFrom utils download.file
#' @keywords internal
#' @export
download_epa_certificate <-
  function(
    epa_certificate_path = "cacert_gaftp_epa.pem",
    certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ) {
    if (!endsWith(epa_certificate_path, ".pem")) {
      stop("Path should end with .pem .\n")
    }
    if (!file.exists(epa_certificate_path)) {
      download_crt_target <- gsub("pem", "crt", epa_certificate_path)
      utils::download.file(certificate_url, download_crt_target)
      system(paste(
        "openssl x509",
        "-inform DER",
        "-outform PEM",
        "-in",
        download_crt_target,
        "-out",
        epa_certificate_path
      ))
      message("Certificate conversion completed.\n")
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
#' @keywords auxiliary
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
#' Check if provided URL returns HTTP status 200.
#' @param url Download URL to be checked.
#' @param method httr method to obtain URL (`"HEAD"` or `"GET"`)
#' @author Insang Song; Mitchell Manware
#' @importFrom httr HEAD
#' @importFrom httr GET
#' @return logical object
#' @keywords auxiliary
#' @export
check_url_status <- function(
    url,
    method = c("HEAD", "GET")) {
  method <- match.arg(method)
  http_status_ok <- 200
  if (method == "HEAD") {
    hd <- httr::HEAD(url)
  } else if (method == "GET") {
    hd <- httr::GET(url)
  }
  
  status <- hd$status_code
  Sys.sleep(1.5)
  return(status == http_status_ok)
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
    commands_path = commands_path) {
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
    position = NULL) {
  if (is.null(position)) {
    cat(paste0("URL position in command is not defined.\n"))
    return(NULL)
  }
  urls <- sapply(
    strsplit(
      commands,
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
#' @param method httr method to obtain URL (`"HEAD"` or `"GET"`). If set to
#' `"SKIP"`, the HTTP status will not be checked and returned.
#' @return logical vector for URL status = 200
#' @keywords auxiliary
#' @export
check_urls <- function(
    urls = urls,
    size = NULL,
    method = c("HEAD", "GET", "SKIP")) {
  if (is.null(size)) {
    cat(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  if (length(urls) < size) {
    size <- length(urls)
  }
  method <- match.arg(method)
  if (method == "SKIP") {
    cat(paste0("Skipping HTTP status check...\n"))
    return(NULL)
  } else {
    url_sample <- sample(urls, size, replace = FALSE)
    url_status <- sapply(url_sample,
                         check_url_status,
                         method = method
    )
    return(url_status)
  }
}

#' Download unit tests
#' @description
#' Implement directory, file, and download URL unit tests.
#' @param directory_to_save directory to test saving
#' @param directory_to_download directory to test download
#' @param commands_path file path with download commands
#' @param url_status logical vector for URL status = 200
#' @importFrom testthat expect_true
#' @return NULL
#' @keywords internal
#' @export
test_download_functions <- function(
    directory_to_download = NULL,
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status) {
  # test that directory_to_download exists
  # skip test if directory_to_download is default (NULL)
  if (!(is.null(directory_to_download))) {
    testthat::expect_true(dir.exists(directory_to_download))
  }
  # test that directory_to_save exists
  testthat::expect_true(dir.exists(directory_to_save))
  # test that commands_path exists
  testthat::expect_true(file.exists(commands_path))
  if (!(is.null(url_status))) {
    # test that sample of download URLs all have HTTP status 200
    testthat::expect_true(all(url_status))
  }
}
