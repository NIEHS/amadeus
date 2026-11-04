#' Download road emissions data
#' @description
#' The \code{download_nei()} function accesses and downloads road emissions
#' data from the U.S Environmental Protection Agency's (EPA) National
#' Emissions Inventory (NEI).
#' @note NEI data does not require authentication.
#' @param epa_certificate_path TO BE DEPRECATED. Certificate path.
#' @param certificate_url TO BE DEPRECATED. Certificate URL.
#' @param year integer(1). Available years of NEI data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip zip files (default TRUE).
#' @param remove_zip logical(1). Remove zip files after unzipping (default
#' FALSE).
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Kyle Messier, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024nei}{amadeus}
#' @examples
#' \dontrun{
#' download_nei(
#'   year = c(2017L, 2020L),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_nei <- function(
  epa_certificate_path = NULL,
  certificate_url = paste0(
    # nolint: line_length_linter.
    "http://cacerts.digicert.com/",
    "DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ),
  year = c(2017L, 2020L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  #### Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  if (
    !is.null(epa_certificate_path) ||
      certificate_url !=
        "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ) {
    warning(
      "Parameters 'epa_certificate_path' and 'certificate_url'",
      " are deprecated.\n",
      "SSL certificates are now handled automatically by httr2.\n",
      call. = FALSE
    )
  }

  #### Define measurement data paths
  # Each NEI year has a distinct URL path suffix; use a named lookup so the
  # function works correctly whether `year` is a scalar (as dispatched from
  # the targets pipeline via pattern = map()) or a full c(2017, 2020) vector.
  url_download_base <- "https://gaftp.epa.gov/air/nei/%s/data_summaries/"
  nei_url_map <- c(
    "2017" = "2017v1/2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  nei_file_map <- c(
    "2017" = "2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  year_chr <- as.character(year)
  unknown <- setdiff(year_chr, names(nei_url_map))
  if (length(unknown) > 0) {
    stop(paste0(
      "NEI data is not available for year(s): ",
      paste(unknown, collapse = ", "),
      ". Available years: ",
      paste(names(nei_url_map), collapse = ", "),
      ".\n"
    ))
  }
  download_urls <- vapply(
    year_chr,
    function(y) paste0(sprintf(url_download_base, y), nei_url_map[y]),
    character(1)
  )
  download_names_file <- unname(nei_file_map[year_chr])
  download_names <- paste0(directory_to_download, download_names_file)

  #### Filter to files that need downloading
  needs_download <- sapply(download_names, amadeus::check_destfile)
  download_urls_filtered <- download_urls[needs_download]
  download_names_filtered <- download_names[needs_download]

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(download_urls_filtered)
    ))
    return(invisible(list(
      urls = download_urls_filtered,
      destfiles = download_names_filtered,
      n_files = length(download_urls_filtered)
    )))
  }

  #### Download files using httr2
  if (length(download_urls_filtered) > 0) {
    download_result <- amadeus::download_run_method(
      urls = download_urls_filtered,
      destfiles = download_names_filtered,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("All files already exist. Nothing to download.\n")
    download_result <- list(
      success = 0,
      failed = 0,
      skipped = length(download_names)
    )
  }

  #### Unzip data (custom unzipping to separate directories per file)
  if (unzip) {
    dir_unzip <- paste0(
      directory_to_save,
      sub(".zip", "", download_names_file)
    )
    for (fn in seq_along(dir_unzip)) {
      if (file.exists(download_names[fn])) {
        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }
    }
  }

  #### Remove zip files
  if (remove_zip) {
    for (fn in download_names) {
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }
  }

  message("Requests were processed.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

