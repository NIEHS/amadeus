#' Download National Land Cover Database (NLCD) data
#' @description
#' Downloads NLCD data products from the Multi-Resolution Land Characteristics
#' (MRLC) Consortium. NLCD provides nationwide land cover and land cover change
#' information for the United States at a 30m resolution.
#' @param product character(1). NLCD product type. One of:
#' \itemize{
#'   \item "Land Cover" (default)
#'   \item "Land Cover Change"
#'   \item "Land Cover Confidence"
#'   \item "Fractional Impervious Surface"
#'   \item "Impervious Descriptor"
#'   \item "Spectral Change Day of Year"
#' }
#' @param year integer(1). Year of NLCD data (1985-2024). Default is 2021.
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed with
#' download.
#' @param download logical(1). DEPRECATED. Downloads now happen automatically.
#'   Set to FALSE to skip downloading (generates file list only).
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip downloaded files? Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files after extraction? Default is
#' \code{FALSE}.
#' @param show_progress logical(1). Show download progress? Default is
#' \code{TRUE}.
#' @param hash logical(1). Return hash of downloaded files? Default is
#' \code{FALSE}.
#' @param max_tries integer(1). Maximum download retry attempts. Default is 20.
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return invisible NULL; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{dewitz_national_2023}{amadeus}
#' @examples
#' \dontrun{
#' # Download 2021 Land Cover
#' download_nlcd(
#'   product = "Land Cover",
#'   year = 2021,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # Download Land Cover Change for 2019
#' download_nlcd(
#'   product = "Land Cover Change",
#'   year = 2019,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   unzip = TRUE,
#'   remove_zip = TRUE
#' )
#' }
#' @export
download_nlcd <- function(
  product = "Land Cover",
  year = 2021,
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

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

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

  #### Check for valid years
  valid_years <- 1985:2024L
  if (!(as.integer(year) %in% valid_years)) {
    stop(paste0("Requested year is not recognized.\n"))
  }

  #### Define URL base
  base <- paste0(
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/",
    "data-bundles/Annual_NLCD_"
  )

  #### Define collection code
  collection_code <- switch(
    tolower(product),
    "land cover" = "LndCov",
    "land cover change" = "LndChg",
    "land cover confidence" = "LndCnf",
    "fractional impervious surface" = "FctImp",
    "impervious descriptor" = "ImpDsc",
    "spectral change day of year" = "SpcChg"
  )

  #### Build URL
  download_url <- paste0(
    base,
    collection_code,
    "_",
    year,
    "_CU_C1V1.zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "Annual_NLCD_",
    collection_code,
    "_",
    year,
    "_CU_C1V1.zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  # http_version = 2L forces HTTP/1.1 to avoid nghttp2 PROTOCOL_ERRORs from
  # www.mrlc.gov, which drops HTTP/2 connections mid-transfer.
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit,
      http_version = 2L
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}

