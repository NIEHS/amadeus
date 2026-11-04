#' Download roads data
#' @description
#' The \code{download_groads()} function accesses and downloads
#' roads data from NASA's Global Roads Open Access Data Set (gROADS).
#' @note gROADS data is hosted on NASA EarthData and requires a valid
#' NASA EarthData token for authentication. Set the
#' \code{NASA_EARTHDATA_TOKEN} environment variable or pass the token
#' directly via \code{nasa_earth_data_token}.
#' Use \code{setup_nasa_token()} for setup.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#'   authentication token. Can be a token string, a path to a file
#'   containing the token, or \code{NULL} to read from the
#'   \code{NASA_EARTHDATA_TOKEN} environment variable.
#' @param data_region character(1). Data region.
#' @param data_format character(1). "Shapefile" or "Geodatabase".
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
#' \insertRef{data_ciesin2013groads}{amadeus}
#' @examples
#' \dontrun{
#' download_groads(
#'   data_region = "Americas",
#'   data_format = "Shapefile",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_groads <- function(
  data_region = c(
    "Americas",
    "Global",
    "Africa",
    "Asia",
    "Europe",
    "Oceania East",
    "Oceania West"
  ),
  data_format = c("Shapefile", "Geodatabase"),
  nasa_earth_data_token = NULL,
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

  #### Retrieve NASA EarthData token (before null parameter check)
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

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

  #### Check if region is valid
  data_format <- match.arg(data_format)
  data_region <- match.arg(data_region)

  #### Define URL base
  base <- paste0(
    "https://data.earthdata.nasa.gov/nasa-earth/",
    "human-dimensions/sedac-root/downloads/data/groads/",
    "groads-global-roads-open-access-v1/",
    "groads-v1-"
  )

  #### Define data format
  if (data_format == "Shapefile" && data_region == "Global") {
    message("Geodatabase format utilized for 'Global' dataset.\n")
    format <- "gdb"
  } else if (data_format == "Shapefile" && !(data_region == "Global")) {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gdb"
  }

  #### Coerce region to lower case
  region <- tolower(data_region)

  #### Build download URL
  download_url <- paste0(
    base,
    gsub(" ", "-", region),
    "-",
    format,
    ".zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "groads_v1_",
    gsub(" ", "-", region),
    "_",
    format,
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
      token = nasa_earth_data_token,
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

