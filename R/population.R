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
#' @param data_resolution character(1). One of `"60 minute"` (default),
#'   `"30 second"`, `"2.5 minute"`, `"15 minute"`, or `"30 minute"`.
#' @param data_format character(1). Requested format: `"GeoTIFF"` (default),
#'   `"ASCII"`, or `"netCDF"`.
#' @param year character(1). One year: `"2020"` (default), `"2000"`, `"2005"`,
#'   `"2010"`, or `"2015"`; use `"all"` for all available years.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#'   Set to FALSE to skip downloading and return the planned file list.
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
#' @details
#' `data_resolution`, `data_format`, and `year` each select one value.
#' Matching is case-sensitive, and unambiguous abbreviations are accepted.
#' If omitted, each argument uses its first listed choice.
#' @author Mitchell Manware, Insang Song
#' @return
#' When `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts. If the requested archive
#' already exists and is nonempty, no file is downloaded; the returned list
#' contains `success = 0`, `failed = 0`, and `skipped = 1`. When a download is
#' attempted, the list also contains `failed_urls` and `failed_files` character
#' vectors, empty if the download succeeded.
#'
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files = 1` is returned without downloading, regardless of `hash`.
#'
#' When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5 hash
#' of the files in the `data_files` directory is returned instead of a list.
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
  data_resolution = c(
    "60 minute",
    "30 second",
    "2.5 minute",
    "15 minute",
    "30 minute"
  ),
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = c("2020", "2000", "2005", "2010", "2015", "all"),
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
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "groads",
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = unzip,
    remove_zip = remove_zip,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  chr_resolution <- match.arg(data_resolution)
  chr_format <- match.arg(data_format)
  chr_year <- match.arg(year)
  # ----------------------------------------------------------------------------
  # Check NASA EarthData token.
  chr_nasatoken <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )
  checkmate::assert_string(chr_nasatoken, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define year.
  chr_year <- ifelse(chr_year == "all", "totpop", as.character(chr_year))
  # ----------------------------------------------------------------------------
  # Query resolution code.
  chr_resolution <- amadeus::process_sedac_codes(chr_resolution)
  # ----------------------------------------------------------------------------
  # Fallback for total population and 30-second resolution mismatch.
  if (chr_year == "totpop" && chr_resolution == "30_sec") {
    chr_resolution <- "2pt5_min"
    message(paste0(
      "30 second resolution not available for all years. Returning",
      " highest (2.5 minute) resolution.\n"
    ))
  }
  # ----------------------------------------------------------------------------
  # Define data format.
  if (chr_format == "GeoTIFF") {
    if (chr_year != "totpop") {
      chr_format <- "tif"
    } else {
      chr_format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (chr_format == "ASCII") {
    if (chr_year != "totpop") {
      chr_format <- "asc"
    } else {
      chr_format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (chr_format == "netCDF") {
    chr_format <- "nc"
  }
  # ----------------------------------------------------------------------------
  # Generate download URL.
  chr_url <- paste0(
    paste0(
      "https://data.earthdata.nasa.gov/nasa-earth/human-dimensions/",
      "sedac-root/downloads/data/gpw-v4/"
    ),
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11/",
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11_",
    chr_year,
    "_",
    chr_resolution,
    "_",
    chr_format,
    ".zip"
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filename <- paste0(
    chr_dir_to_download,
    "gpw_v4_population_density_adjusted_to_2015_unwpp_",
    "country_totals_rev11_",
    chr_year,
    "_",
    chr_resolution,
    "_",
    chr_format,
    ".zip"
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus::download_preview(chr_url, chr_filename)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run_method(
    urls = chr_url,
    destfiles = chr_filename,
    token = chr_nasatoken,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files and remove if requested.
  amadeus::download_unzip(
    file_name = chr_filename,
    directory_to_unzip = chr_dir_to_save,
    unzip = unzip,
    remove = remove_zip
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
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
# {calculate} tier
################################################################################
#' Calculate population density covariates
#' @description
#' Extract population density values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, year, and population
#' density variable. Population density variable column name reflects
#' spatial resolution of \code{from} and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_population()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [process_population()]
#' @return a data.frame or SpatVector object
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_population(
#'   from = pop, # derived from process_population() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_population <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### message information
  name_split <- strsplit(
    names(from),
    "_"
  )[[1]]
  message(
    paste0(
      "Calculating population covariates for ",
      name_split[4],
      " at ",
      amadeus::process_sedac_codes(
        paste0(
          name_split[5],
          "_",
          name_split[6]
        ),
        invert = TRUE
      ),
      " resolution...\n"
    )
  )
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "skip",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 3,
    time = 4,
    time_type = "year",
    weights = weights,
    ...
  )
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}
################################################################################
