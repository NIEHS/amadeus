#' Download toxic release data
#' @description
#' The \code{download_tri()} function accesses and downloads toxic release
#' data from the U.S. Environmental Protection Agency's (EPA) Toxic Release
#' Inventory (TRI) Program. The EPA TRI basic data files contain annual,
#' facility-reported toxic chemical release and waste management information.
#' EPA publishes TRI basic files in multiple annual variants under the same
#' service endpoint: a nationwide file (\code{"US"}), state-specific files
#' identified by two-letter postal abbreviations (for example \code{"AZ"} or
#' \code{"NC"}), and a tribal file (\code{"tbl"}).
#' @note TRI data does not require authentication. State and tribal downloads
#'   are saved with jurisdiction-specific file names, while the U.S.-wide
#'   download keeps the historical \code{tri_raw_<year>.csv} naming pattern.
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param jurisdiction character(1). TRI file variant to download. Use
#'   \code{"US"} for the nationwide file, a two-letter state or territory code
#'   such as \code{"AZ"} or \code{"NC"} for a jurisdiction-specific file, or
#'   \code{"tbl"} for the tribal file. Default is \code{"US"}.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mariana Kassien, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024tri}{amadeus}
#' @examples
#' \dontrun{
#' download_tri(
#'   year = 2021L,
#'   directory_to_save = tempdir(),
#'   jurisdiction = "NC",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_tri <- function(
  year = c(2018L, 2022L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  jurisdiction = "US",
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Check years
  year <- if (length(year) == 1) rep(year, 2) else year
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Check jurisdiction
  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      "`jurisdiction` must be a single character value such as ",
      "\"US\", \"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction <- trimws(jurisdiction)
  if (!nzchar(jurisdiction)) {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code, or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction_upper <- toupper(jurisdiction)
  if (identical(jurisdiction_upper, "TBL")) {
    jurisdiction_url <- "tbl"
    jurisdiction_suffix <- "_tbl"
  } else if (
    identical(jurisdiction_upper, "US") ||
      grepl("^[A-Z]{2}$", jurisdiction_upper)
  ) {
    jurisdiction_url <- jurisdiction_upper
    jurisdiction_suffix <- if (identical(jurisdiction_upper, "US")) {
      ""
    } else {
      paste0("_", jurisdiction_upper)
    }
  } else {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code such as ",
      "\"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
  }

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

  #### Define measurement data paths
  url_download <- paste0(
    # nolint: line_length_linter.
    "https://data.epa.gov/efservice/downloads/tri/",
    "mv_tri_basic_download/"
  )
  year_sequence <- seq(year[1], year[2], 1)
  download_urls <- paste0(
    url_download,
    year_sequence,
    "_",
    jurisdiction_url,
    "/csv"
  )
  download_names <- paste0(
    directory_to_save,
    "tri_raw_",
    year_sequence,
    jurisdiction_suffix,
    ".csv"
  )

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
  download_result <- amadeus::download_run_method(
    urls = download_urls_filtered,
    destfiles = download_names_filtered,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  message("Requests were processed.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

