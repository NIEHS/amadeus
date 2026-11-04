#' Download climate classification data
#' @description
#' The \code{download_koppen_geiger()} function accesses and downloads
#' climate classification data.
#' @note Köppen-Geiger data does not require authentication.
#' @param data_resolution character(1). Available resolutions.
#' @param time_period character(1). "Present" (1980-2016) or "Future"
#' (2071-2100).
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
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_beck2023koppen}{amadeus}
#' \insertRef{article_beck2018present}{amadeus}
#' @examples
#' \dontrun{
#' download_koppen_geiger(
#'   data_resolution = "0.0083",
#'   time_period = "Present",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_koppen_geiger <- function(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
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
      "Setting download=FALSE is deprecated.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Check for data resolution
  data_resolution <- match.arg(data_resolution)

  #### Check for valid time period
  time_period <- match.arg(time_period)

  #### Define time period
  period <- tolower(time_period)

  #### Define data resolution
  data_resolution <- gsub("\\.", "p", data_resolution)

  #### Define download URL
  download_url <- paste0(
    "https://s3-eu-west-1.amazonaws.com/",
    "pfigshare-u-files/12407516/Beck_KG_V1.zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "koppen_geiger_",
    period,
    "_",
    data_resolution,
    ".zip"
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
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
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
