################################################################################
# {amadeus} functions for USDA CropScape Cropland Data Layer.
################################################################################

################################################################################
# {download} tier
################################################################################
# 4. USDA Cropscape
# nolint start
#' Download CropScape data
#' @description
#' Accesses and downloads United States Department of Agriculture
#' CropScape Cropland Data Layer data from
#' the [USDA National Agricultural Statistics Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php) or the
#' [George Mason University website](https://nassgeodata.gmu.edu/CropScape/).
#' @param year integer(1). Year of the data to download.
#' @param source character(1). Data source, one of `c("USDA", "GMU")`.
#' * `"USDA"` will download the national data from the USDA website
#'   (available in 2008-last year).
#' * `"GMU"` will download the data from the George Mason University
#'   website (available in 1997-last year).
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip the downloaded compressed files.
#' Default is \code{FALSE}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @author Insang Song
#' @note JSON files should be found at STAC catalog of OpenLandMap
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Yearly comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @examples
#' \dontrun{
#' download_cropscape(
#'   year = 2020,
#'   source = "USDA",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE,
#'   unzip = FALSE
#' )
#' }
#' @importFrom archive archive_extract
#' @export
download_cropscape <- function(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  source <- match.arg(source)
  if (source == "GMU" && year < 1997) {
    stop("Year should be equal to or greater than 1997.")
  }
  if (source == "USDA" && year < 2008) {
    stop("Year should be equal to or greater than 2008.")
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
  if (!isFALSE(remove_command)) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }
  #### 1. check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. define measurement data paths
  url_download_base <- switch(
    source,
    USDA = "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/datasets/",
    GMU = "https://nassgeodata.gmu.edu/nass_data_cache/tar/"
  )
  filename_template <- switch(
    source,
    USDA = "%d_30m_cdls.zip",
    GMU = "%d_cdls.tar.gz"
  )
  url_download_template <- paste0(url_download_base, filename_template)

  download_urls <-
    sprintf(url_download_template, year)
  download_names_file <- sprintf(filename_template, year)
  download_names <- paste0(directory_to_save, download_names_file)

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_urls,
      destfiles = download_names,
      n_files = length(download_urls)
    )))
  }

  #### Download using httr2
  amadeus::download_run_method(
    urls = download_urls,
    destfiles = download_names,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  #### Unzip data
  # nocov start
  if (unzip) {
    extension <- ifelse(source == "USDA", "\\.zip", "(\\.tar|\\.tar\\.gz)")
    dir_unzip <- gsub(extension, "", download_names)
    for (fn in seq_along(dir_unzip)) {
      if (!dir.exists(dir_unzip[fn])) {
        dir.create(dir_unzip[fn], recursive = TRUE)
      }
      archive::archive_extract(download_names[fn], dir = dir_unzip[fn])
    }
  }
  # nocov end
  message("Requests were processed.\n")
  return(amadeus::download_hash(hash, directory_to_save))
}
# nolint end

################################################################################
# {process} tier
################################################################################
#' Process CropScape data
#' @description
#' This function imports and cleans raw CropScape data,
#' returning a single `SpatRaster` object.
#' @param path character giving CropScape data path
#' @param year numeric giving the year of CropScape data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads CropScape file of selected `year`.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' cropscape <- process_cropscape(
#'   path = "./data/cropscape_example.tif",
#'   year = 2020
#' )
#' }
#' @export
process_cropscape <-
  function(
    path = NULL,
    year = 2021,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    # open cdl file corresponding to the year
    if (dir.exists(path)) {
      cdl_file <-
        list.files(
          path,
          pattern = paste0("cdl_30m_*.*", year, "_*.*.tif$"),
          full.names = TRUE
        )
    } else {
      cdl_file <- path
    }
    cdl <- terra::rast(cdl_file, win = extent)
    terra::metags(cdl) <- paste0("year=", year)
    return(cdl)
  }

################################################################################
# {calculate} tier
################################################################################
#' Calculate Cropscape covariates
#' @description
#' Extract Cropscape (CDL) values at point locations.
#' Returns a \code{data.frame}
#' object containing \code{locs_id} and crop specific cell fractions.
#' @param from SpatRaster(1). Output from \code{process_cropscape()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_cropscape()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra extract
#' @importFrom terra crs
#' @importFrom sf st_as_sf st_buffer
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_cropscape(
#'   from = cropscape, # derived from process_cropscape() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_cropscape <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
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

  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
  }

  message(
    sprintf(
      "Calculating Cropscape covariates with %d meters radius...",
      radius
    )
  )

  # extract
  is_polygon_locs <- inherits(sites_e, "SpatVector") &&
    !all(tolower(terra::geomtype(sites_e)) %in% c("points", "point"))
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    # terra::extract for point locations
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
    # rename
    colnames(sites_extracted) <- paste0("cropscape_", radius)
  } else {
    sites_e_buf <- sf::st_as_sf(sites_e)

    # fractions
    extract_args <- c(
      list(
        x = from,
        y = sites_e_buf,
        fun = "frac",
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)

    colnames(sites_extracted) <- gsub(
      "frac_",
      sprintf("cropscape_%d_", radius),
      colnames(sites_extracted)
    )
  }

  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  sites_extracted <- sites_extracted[, c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )]

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}
################################################################################
