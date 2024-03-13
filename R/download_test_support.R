# Functions used to simplify, implement and support download unit tests

#' Check HTTP status
#' @description
#' Check if provided URL returns HTTP status 200.
#' @param url Download URL to be checked.
#' @param method httr method to obtain URL (`"HEAD"` or `"GET"`)
#' @author Insang Song; Mitchell Manware
#' @importFrom httr HEAD
#' @importFrom httr GET
#' @return logical object
#' @keywords auxillary
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
#' @keywords auxillary
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
