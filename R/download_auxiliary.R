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
      directory_zip <- download_sanitize_path(
        paste0(
          download_sanitize_path(directory),
          "zip_files"
        )
      )
      if (!dir.exists(directory_zip)) {
        dir.create(directory_zip, recursive = TRUE)
      }
      directory_data <- download_sanitize_path(
        paste0(
          download_sanitize_path(directory),
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


#' Run download commands
#' @description
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (\code{TRUE}) or
#'  skip (\code{FALSE}) download.
#' @param commands_txt character(1). Path of download commands
#' @return NULL; runs download commands with shell (Unix/Linux) or
#' command prompt (Windows)
#' @keywords internal
#' @export
download_run <- function(
    download = FALSE,
    commands_txt = NULL) {
  if (.Platform$OS.type == "windows") {
    runner <- ""
    commands_txt <- gsub(".txt", "bat", commands_txt)
  } else {
    runner <- ". "
  }
  system_command <- paste0(runner, commands_txt, "\n")
  if (download == TRUE) {
    message(paste0("Downloading requested files...\n"))
    system(command = system_command)
    message(paste0("Requested files have been downloaded.\n"))
  } else {
    message(paste0("Skipping data download.\n"))
    return(NULL)
  }
}


#' Remove download commands
#' @description
#' Remove or retain the .txt file storing all download commands.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or
#'  keep (\code{FALSE}) commands
#' @return NULL; removes .txt file storing all download commands.
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


#' Unzip zip files
#' @description
#' Unzip (inflate) downloaded ".zip" files.
#' @param file_name character(1). Full zip file path
#' @param directory_to_unzip character(1). Directory to unzip
#' data
#' @param unzip logical(1). Unzip (\code{TRUE}) or not.
#' @return NULL; unzips downloaded zip files
#' @keywords internal
#' @export
download_unzip <-
  function(file_name,
           directory_to_unzip,
           unzip = TRUE) {
    if (!unzip) {
      message(paste0("Downloaded files will not be unzipped.\n"))
      return(NULL)
    }

    message(paste0("Unzipping files...\n"))
    unzip(file_name,
      exdir = directory_to_unzip
    )
    message(paste0(
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
#' !!! USE THE FUNCTION WITH CAUTION !!!
#' If \code{remove = TRUE}, ensure that \code{unzip = TRUE}. Choosing to remove
#' ".zip" files without unzipping will retain none of the downloaded data.
#' then it will remove all files in the second higher level directory.
#' @return NULL; removes downloaded zip files after they are unzipped
#' @keywords internal
#' @export
download_remove_zips <-
  function(remove = FALSE,
           download_name) {
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
      parameters) {
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
#' @return A file designated in `epa_certificate_path`
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
#' Check if provided URL returns HTTP status 200 or 206.
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
  http_status_ok <- c(200, 206)
  if (method == "HEAD") {
    hd <- httr::HEAD(url)
  } else if (method == "GET") {
    hd <- httr::GET(url)
  }
  status <- hd$status_code
  Sys.sleep(1)
  return(status %in% http_status_ok)
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
    message(paste0("URL position in command is not defined.\n"))
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
    message(paste0("URL sample size is not defined.\n"))
    return(NULL)
  }
  if (length(urls) < size) {
    size <- length(urls)
  }
  method <- match.arg(method)
  if (method == "SKIP") {
    message(paste0("Skipping HTTP status check...\n"))
    return(NULL)
  } else {
    url_sample <- sample(urls, size, replace = FALSE)
    url_status <- sapply(
      url_sample,
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
#' @param commands_path file path with download commands
#' @param url_status logical vector for URL status = 200
#' @importFrom testthat expect_true
#' @return NULL; returns stop error if one or more tests fail
#' @keywords internal
#' @export
test_download_functions <- function(
    directory_to_save = directory_to_save,
    commands_path = commands_path,
    url_status = url_status) {
  # test that directory_to_save exists
  testthat::expect_true(dir.exists(directory_to_save))
  # test that commands_path exists
  testthat::expect_true(file.exists(commands_path))
  if (!(is.null(url_status))) {
    # test that sample of download URLs all have HTTP status 200
    testthat::expect_true(all(url_status))
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
#' @keywords auxiliary
#' @export
narr_variable <- function(variable) {
  stopifnot(length(variable) == 1)
  mono <- c(
    "acpcp", "air.2m", "air.sfc", "albedo", "apcp",
    "bgrun", "bmixl.hl1", "cape", "ccond", "cdcon",
    "cdlyr", "cfrzr", "cicep", "cin", "cnwat",
    "crain", "csnow", "dlwrf", "dpt.2m", "dswrf",
    "evap", "gflux", "hcdc", "hgt.tropo", "hlcy",
    "hpbl", "lcdc", "lftx4", "lhtfl", "mcdc",
    "mconv.hl1", "mslet", "mstav", "pevap", "pottmp.hl1",
    "pottmp.sfc", "prate", "pres.sfc", "pres.tropo", "prmsl",
    "pr_wtr", "rcq", "rcs", "rcsol", "rct",
    "rhum.2m", "shtfl", "shum.2m", "snod", "snohf",
    "snom", "snowc", "soilm", "ssrun", "tcdc",
    "tke.hl1", "ulwrf.ntat", "ulwrf.sfc", "ustm", "uswrf.ntat",
    "uswrf.sfc", "uwnd.10m", "veg", "vis", "vstm",
    "vvel.hl1", "vwnd.10m", "vwsh.tropo", "wcconv", "wcinc",
    "wcuflx", "wcvflx", "weasd", "wvconv", "wvinc",
    "wvuflx", "wvvflx"
  )
  pressure <- c("air", "hgt", "omega", "shum", "tke", "uwnd", "vwnd")
  soil <- c("soill", "soilw", "tsoil")
  base <- "https://psl.noaa.gov/thredds/fileServer/Datasets/NARR/Dailies/"
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
