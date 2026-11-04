################################################################################
# {amadeus} functions for NASA SEDAC population density data.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download population density data
#' @description
#' The \code{download_population()} function accesses and downloads
#' population density data from NASA's UN WPP-Adjusted Population Density.
#' @note Population data may require NASA EarthData authentication depending on
#' access method.
#' @param data_resolution character(1). Available resolutions.
#' @param data_format character(1). "ASCII", "GeoTIFF", or "netCDF".
#' @param year character(1). Available years or "all".
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
#' @param nasa_earth_data_token character(1). NASA EarthData bearer token.
#' If NULL (default), reads from the \code{NASA_EARTHDATA_TOKEN} environment
#' variable via \code{get_token()}.
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2017gpwv4}{amadeus}
#' @examples
#' \dontrun{
#' # RECOMMENDED: Set up token once (persists across sessions)
#' setup_nasa_token()
#'
#' download_population(
#'   data_resolution = "30 second",
#'   data_format = "GeoTIFF",
#'   year = "2020",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_population <- function(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2,
  nasa_earth_data_token = NULL
) {
  #### Retrieve NASA EarthData token
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

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

  #### Define URL base
  base <- paste0(
    paste0(
      "https://data.earthdata.nasa.gov/nasa-earth/human-dimensions/",
      "sedac-root/downloads/data/gpw-v4/"
    )
  )

  #### Define year
  year <- ifelse(year == "all", "totpop", as.character(year))

  #### Define data resolution
  resolution <- amadeus::process_sedac_codes(data_resolution)

  #### 30 second resolution not available for all years
  if (year == "totpop" && resolution == "30_sec") {
    resolution <- "2pt5_min"
    message(paste0(
      "30 second resolution not available for all years. Returning",
      " highest (2.5 minute) resolution.\n"
    ))
  }

  data_format <- match.arg(data_format)

  #### Define data format
  if (data_format == "GeoTIFF") {
    if (year != "totpop") {
      format <- "tif"
    } else {
      format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (data_format == "ASCII") {
    if (year != "totpop") {
      format <- "asc"
    } else {
      format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (data_format == "netCDF") {
    format <- "nc"
  }

  #### Build download URL
  download_url <- paste0(
    base,
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11/",
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11_",
    year,
    "_",
    resolution,
    "_",
    format,
    ".zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "gpw_v4_population_density_adjusted_to_2015_unwpp_",
    "country_totals_rev11_",
    year,
    "_",
    resolution,
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

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process population density data
#' @description
#' The \code{process_secac_population()} function imports and cleans raw
#' population density data, returning a single `SpatRaster` object.
#' @param path character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' pop <- process_population(
#'   path = "./data/sedac_population_example.tif"
#' )
#' }
#' @export
# nolint end
process_population <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  if (substr(path, nchar(path) - 2, nchar(path)) == ".nc") {
    message(
      paste0(
        "netCDF functionality for SEDAC data is under construction.\n"
      )
    )
    return()
  }
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### import data
  data <- terra::rast(path, win = extent)
  #### identify names
  names_raw <- names(data)
  #### create new names
  for (r in seq_along(names_raw)) {
    split1 <- strsplit(
      names_raw[r],
      "_rev11_",
    )[[1]][[2]]
    split2 <- strsplit(
      split1,
      "_"
    )[[1]]
    names(data[[r]]) <- paste0(
      "gpw_v4_population_",
      split2[1],
      "_",
      split2[2],
      "_",
      split2[3]
    )
    message(paste0(
      "Cleaning ",
      amadeus::process_sedac_codes(
        paste0(
          split2[2],
          "_",
          split2[3]
        ),
        invert = TRUE
      ),
      " population data for year ",
      split2[1],
      "...\n"
    ))
    #### year
    terra::metags(data) <- paste0("year=", split2[1])
  }
  return(data)
}

################################################################################
#' Process population resolution code
#' @description
#' Convert full length resolution name to/from resolution code.
#' @param string character(1). Resolution name or code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides resolution code, and returns full length resolution.
#' @keywords internal auxiliary
#' @return character
#' @export
process_sedac_codes <-
  function(
    string,
    invert = FALSE
  ) {
    resolution_namecodes <- cbind(
      c(
        "60 minute",
        "30 second",
        "2.5 minute",
        "15 minute",
        "30 minute"
      ),
      c(
        "1_deg",
        "30_sec",
        "2pt5_min",
        "15_min",
        "30_min"
      )
    )
    if (invert == FALSE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 1] == string][2]
    } else if (invert == TRUE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 2] == string][1]
    }
    return(resolution)
  }
################################################################################
