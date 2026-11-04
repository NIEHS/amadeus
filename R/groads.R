################################################################################
# {amadeus} functions for Global Roads Open Access Data Set (gROADS).
################################################################################

################################################################################
# {download} tier
################################################################################
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

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process roads data
#' @description
#' The \code{process_groads()} function imports and cleans raw road data,
#' returning a single `SpatVector` object.
#' @param path character(1). Path to geodatabase or shapefiles.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note U.S. context. The returned `SpatVector` object contains a
#' `$description` column to represent the temporal range covered by the
#' dataset. For more information, see <https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.
#' @author Insang Song
#' @return a `SpatVector` object
#' @importFrom terra vect
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' groads <- process_groads(
#'   path = "./data/groads_example.shp"
#' )
#' }
#' @export
# nolint end
process_groads <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop(
      "`path` must be a single file path to a .shp or .gdb roads file."
    )
  }
  if (!file.exists(path)) {
    stop(
      sprintf(
        "`path` does not exist: %s",
        path
      )
    )
  }
  if (!grepl("\\.(shp|gdb)$", tolower(path))) {
    stop(
      paste0(
        "`path` must point to a .shp or .gdb file. Received: ",
        path
      )
    )
  }
  #### import data
  data <- terra::vect(path, extent = extent)
  #### time period
  data$description <- "1980 - 2010"
  return(data)
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate roads covariates
#' @description Prepared groads data is clipped with the buffer polygons
#' of `radius`. The total length of the roads are calculated.
#' Then the density of the roads is calculated by dividing
#' the total length from the area of the buffer. `terra::linearUnits()`
#' is used to convert the unit of length to meters.
#' @param from SpatVector(1). Output of `process_groads`.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 1000).
#' @param fun function(1). Function used to summarize the length of roads
#' within sites location buffer (Default is `sum`).
#' @param drop logical(1). Should locations with zero roads in the extraction
#' buffer be dropped from results? Default is `FALSE` (retain all locations).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
# nolint start
#' @note Unit is km / sq km. The returned `data.frame` object contains a
#' `$time` column to represent the temporal range covered by the
#' dataset. For more information, see <https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.
# nolint end
#' @author Insang Song
#' @seealso [`process_groads`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom stats aggregate
#' @importFrom stats setNames
#' @importFrom terra as.data.frame
#' @importFrom terra project
#' @importFrom terra intersect
#' @importFrom terra perim
#' @importFrom terra crs
#' @importFrom terra expanse
#' @importFrom terra linearUnits
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_groads(
#'   from = groads, # derived from process_groads() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 1000,
#'   fun = "sum",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_groads <- function(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = 1000,
  fun = "sum",
  drop = FALSE,
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### check for null parameters
  if (radius <= 0) {
    stop("radius should be greater than 0.\n")
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop("`drop` should be a single logical value (TRUE/FALSE).")
  }
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]

  from_re <- terra::project(from, terra::crs(sites_e))
  from_re <- from[terra::ext(sites_e), ]
  from_clip <- terra::intersect(sites_e, from_re)
  area_buffer <- sites_e[1, ]
  area_buffer <- terra::expanse(area_buffer)

  # assign road lengths to rlength field
  from_clip$rlength <- terra::perim(from_clip)
  from_clip <-
    aggregate(
      from_clip$rlength,
      by = from_clip[[locs_id]],
      FUN = fun,
      na.rm = TRUE
    )
  # linear unit conversion
  # if no unit is detected, set to 1
  det_unit <- terra::linearUnits(from_re)
  if (det_unit == 0) {
    det_unit <- 1
  }
  total_name <- sprintf("GRD_TOTAL_%05d", radius)
  density_name <- sprintf("GRD_DENKM_%05d", radius)

  # km / sq km
  if (nrow(from_clip) > 0) {
    from_clip[["x"]] <- (from_clip[["x"]] * det_unit / 1e3)
    from_clip$density <-
      from_clip[["x"]] / (area_buffer * (det_unit^2) / 1e6)
    from_clip <-
      setNames(
        from_clip,
        c(
          locs_id,
          total_name,
          density_name
        )
      )
    from_clip <- data.frame(from_clip)
  } else {
    from_clip <- data.frame(sites_list[[2]])[0, locs_id, drop = FALSE]
    from_clip[[total_name]] <- numeric(0)
    from_clip[[density_name]] <- numeric(0)
  }
  from_clip$description <- "1980 - 2010"

  if (geom %in% c("sf", "terra")) {
    sites_geom <- data.frame(sites_list[[2]])
    from_clip <- merge(
      x = sites_geom,
      y = from_clip[, c(locs_id, "description", total_name, density_name)],
      by = locs_id,
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    sites_id <- data.frame(sites_list[[2]])[, locs_id, drop = FALSE]
    from_clip <- merge(
      x = sites_id,
      y = from_clip[, c(locs_id, "description", total_name, density_name)],
      by = locs_id,
      all.x = TRUE,
      sort = FALSE
    )
  }

  from_clip[[total_name]][is.na(from_clip[[total_name]])] <- 0
  from_clip[[density_name]][is.na(from_clip[[density_name]])] <- 0
  from_clip$description[is.na(from_clip$description)] <- "1980 - 2010"

  if (drop) {
    from_clip <- from_clip[from_clip[[total_name]] > 0, , drop = FALSE]
  }

  if (geom %in% c("sf", "terra")) {
    from_clip_reorder <- from_clip[, c(
      locs_id, "geometry", "description", total_name, density_name
    )]
  } else {
    #### reorder
    from_clip_reorder <- from_clip[, c(
      locs_id, "description", total_name, density_name
    )]
  }
  sites_return <- amadeus::calc_return_locs(
    covar = from_clip_reorder,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}
################################################################################
