################################################################################
# {amadeus} functions for Global Multi-resolution Terrain Elevation Data
# (GMTED2010).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download elevation data
#' @description
#' The \code{download_gmted()} function accesses and downloads Global
#' Multi-resolution Terrain Elevation Data (GMTED2010) from U.S. Geological
#' Survey.
#' @note GMTED data does not require authentication.
#' @param statistic character(1). Available statistics.
#' @param resolution character(1). Available resolutions.
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
#' @author Mitchell Manware, Insang Song
#' @return
#' When `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts. If all requested archives
#' already exist and are nonempty, no files are downloaded; the returned list
#' contains `success = 0`, `failed = 0`, and `skipped` equal to the number of
#' requested files. When downloads are attempted, the list also contains
#' `failed_urls` and `failed_files` character vectors, empty if none failed.
#'
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files` is returned without downloading, regardless of `hash`.
#'
#' When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5 hash
#' of the files in the `data_files` directory is returned instead of a list.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{danielson_global_2011}{amadeus}
#' @examples
#' \dontrun{
#' download_gmted(
#'   statistic = "Breakline Emphasis",
#'   resolution = "7.5 arc-seconds",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_gmted <- function(
  statistic = c(
    "Breakline Emphasis",
    "Systematic Subsample",
    "Median Statistic",
    "Minimum Statistic",
    "Mean Statistic",
    "Maximum Statistic",
    "Standard Deviation Statistic"
  ),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
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
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeus:::download_check(
    dataset_name = "gmted",
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
  checkmate::assert_choice(
    statistic,
    c(
      "Breakline Emphasis",
      "Systematic Subsample",
      "Median Statistic",
      "Minimum Statistic",
      "Mean Statistic",
      "Maximum Statistic",
      "Standard Deviation Statistic"
    )
  )
  checkmate::assert_choice(
    resolution,
    c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
  )
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(
    amadeus::download_sanitize_path(directory_to_save),
    zip = TRUE
  )
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define URL base
  chr_base <- paste0(
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo",
    "/downloads/GMTED/Grid_ZipFiles/"
  )
  # ----------------------------------------------------------------------------
  # Define GMTED-specific statistic and resolution.
  chr_statistic <- amadeus::process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  chr_resolution <- amadeus::process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_url <- paste0(
    chr_base,
    chr_statistic,
    chr_resolution,
    "_grd.zip"
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filename <- paste0(
    chr_dir_to_download,
    "gmted2010_",
    chr_statistic,
    chr_resolution,
    "_grd.zip"
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
    token = NULL,
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
#' Process elevation data
#' @description
#' The \code{process_gmted()} function imports and cleans raw elevation data,
#' returning a single `SpatRaster` object.
#' @param variable vector(1). Vector containing the GMTED statistic first and
#' the resolution second. (Example: variable = c("Breakline Emphasis",
#' "7.5 arc-seconds")).
#' * Statistic options: "Breakline Emphasis", "Systematic Subsample",
#'   "Median Statistic", "Minimum Statistic", "Mean Statistic",
#'   "Maximum Statistic", "Standard Deviation Statistic"
#' * Resolution options: "30 arc-seconds", "15 arc-seconds", "7.5 arc-seconds"
#' @param path character(1). Directory with downloaded GMTED  "*_grd"
#' folder containing .adf files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @note
#' `SpatRaster` layer name indicates selected variable and resolution, and year
#' of release (2010).
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra varnames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' gmted <- process_gmted(
#'   variable = c("Breakline Emphasis", "7.5 arc-seconds"),
#'   path = "./data/be75_grd"
#' )
#' }
#' @export
process_gmted <- function(
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check for length of variable
  if (!(length(variable) == 2)) {
    stop(
      paste0(
        "Please provide a vector with the statistic and resolution.\n"
      )
    )
  }
  #### identify statistic and resolution
  statistic <- variable[1]
  statistic_code <- amadeus::process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  resolution <- variable[2]
  resolution_code <- amadeus::process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  message(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  #### identify file path
  paths <- list.files(
    path,
    full.names = TRUE,
    recursive = TRUE
  )

  #### select only the folder containing data
  data_paths <- unique(
    grep(
      paste0(
        statistic_code,
        resolution_code,
        "_grd",
        collapse = "|"
      ),
      paths,
      value = TRUE
    )
  )
  data_path <- data_paths[grep("(_grd$|w001001.adf)", data_paths)]
  #### import data
  data <- terra::rast(data_path, win = extent)
  #### layer name
  names(data) <- paste0(
    "elevation_",
    gsub(
      "_grd",
      "",
      names(data)
    ),
    "_2010"
  )
  #### varnames
  terra::varnames(data) <- paste0(
    "Elevation: ",
    statistic,
    " (",
    resolution,
    ")"
  )
  #### year
  terra::metags(data) <- "year=2010"
  #### set coordinate reference system
  return(data)
}

################################################################################
#' Process elevation statistic and resolution codes
#' @description
#' Identify the GMTED statistic and resolution based on the file path. Convert
#' statistic and resolution to/from full string to/from statistic and
#' resolution code.
#' @param string character(1). File path to GMTED data file.
#' @param statistic logical(1). Matches statistic to statistic code.
#' @param resolution logical(1). Matches resolution to resolution code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides statistic or resolution code, and returns full length statistic
#' or resolution.
#' @keywords internal auxiliary
#' @return character
#' @export
process_gmted_codes <-
  function(
    string,
    statistic = FALSE,
    resolution = FALSE,
    invert = FALSE
  ) {
    statistics <- c(
      "Breakline Emphasis",
      "Systematic Subsample",
      "Median Statistic",
      "Minimum Statistic",
      "Mean Statistic",
      "Maximum Statistic",
      "Standard Deviation Statistic"
    )
    statistic_codes <- c("be", "ds", "md", "mi", "mn", "mx", "sd")
    statistic_codes <- cbind(statistics, statistic_codes)
    resolutions <- c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds")
    resolution_codes <- c("75", "15", "30")
    resolution_codes <- cbind(resolutions, resolution_codes)
    if (statistic == TRUE && invert == FALSE) {
      code <- statistic_codes[statistic_codes[, 1] == string][2]
    } else if (statistic == TRUE && invert == TRUE) {
      code <- statistic_codes[statistic_codes[, 2] == string][1]
    }
    if (resolution == TRUE && invert == FALSE) {
      code <- resolution_codes[resolution_codes[, 1] == string][2]
    } else if (resolution == TRUE && invert == TRUE) {
      code <- resolution_codes[resolution_codes[, 2] == string][1]
    }
    return(code)
  }

################################################################################
# {calculate} tier
################################################################################
#' Calculate elevation covariates
#' @description
#' Extract elevation values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, year of release, and elevation variable.
#' Elevation variable column name follows the pattern
#' \code{gmted_<radius>} (for example, \code{gmted_0} or \code{gmted_100}).
#' @param from SpatRaster(1). Output from \code{process_gmted()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
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
#' @seealso [`process_gmted()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_gmted(
#'   from = gmted, # derived from process_gmted() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_gmted <- function(
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
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "gmted",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 2,
    time = 3,
    time_type = "year",
    weights = weights
  )
  #### variable column name
  variable_name <- paste0("gmted_", as.integer(radius))
  if (geom %in% c("sf", "terra")) {
    #### convert integer to numeric
    sites_extracted[, 4] <- as.numeric(sites_extracted[, 4])
    names(sites_extracted) <- c(locs_id, "geometry", "time", variable_name)
  } else {
    #### convert integer to numeric
    sites_extracted[, 3] <- as.numeric(sites_extracted[, 3])
    names(sites_extracted) <- c(locs_id, "time", variable_name)
  }
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
