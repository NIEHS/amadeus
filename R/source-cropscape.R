################################################################################
# {amadeus} functions for USDA CropScape Cropland Data Layer.
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download CropScape data
#' @description
#' Download annual USDA Cropland Data Layer archives from the
#' [USDA National Agricultural Statistics Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php)
#' or [George Mason University](https://nassgeodata.gmu.edu/CropScape/).
#' Archives and extracted raster data are stored in separate subdirectories
#' using the shared download workflow.
#' @param year integer(1 or more). One year, two endpoints, or a non-empty
#'   vector of years. Every year from the minimum through the maximum is
#'   requested, including gaps: `c(2018, 2023)` requests `2018:2023`.
#'   Default is `1997:2023`. Missing, fractional, and future years are rejected.
#' @param source character(1). Data source, either `"GMU"` (default) or
#'   `"USDA"`. GMU requests use `YYYY_cdls.tar.gz` with a minimum year of
#'   1997. USDA requests use `YYYY_30m_cdls.zip` with a minimum year of 2008;
#'   supply a compatible `year` when choosing USDA. File availability is
#'   determined by the selected provider.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are downloaded into `zip_files/` and, when `unzip = TRUE`,
#'   extracted into separate per-archive directories under `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. CropScape
#'   archives and extracted rasters can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after directory creation and a preflight check of the first URL. No
#'   archives are downloaded or extracted in preview mode. Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   No command file is generated. Default is `FALSE`.
#' @param unzip logical(1). Extract downloaded `.zip` or `.tar.gz` archives
#'   into per-archive directories under `data_files/`. Default is `TRUE`;
#'   `FALSE` skips extraction and creation of the per-archive directories.
#' @param remove_zip logical(1). Remove downloaded archives and their
#'   `zip_files/` directory, including its remaining contents, after the
#'   extraction step. Default is `FALSE`. Ignored when `download = FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of the files under
#'   `data_files/` instead of the download summary. Default is `FALSE`.
#'   Ignored when `download = FALSE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status messages may still be printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file download request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Non-negative throttling interval in seconds,
#'   passed to [download_run_method()]. Default is 2.
#' @details
#' `zip_files/` and `data_files/` are sibling directories created beneath
#' `directory_to_save`, including for previews and when `unzip = FALSE`.
#' Both ZIP and compressed TAR downloads are stored in `zip_files/`.
#' Each archive is extracted into its own directory under `data_files/`,
#' named after the archive with the full `.zip` or `.tar.gz` suffix removed:
#' `data_files/YYYY_30m_cdls/` for USDA and `data_files/YYYY_cdls/` for GMU.
#' This isolates duplicate filenames across archives. Paths stored inside
#' each archive are preserved. Pass the corresponding per-archive directory
#' or an extracted TIFF path to [process_cropscape()].
#'
#' Preview destinations refer to the archive files in `zip_files/`.
#' The preflight check contacts the first URL even in preview mode; a failed
#' check emits a warning. Its request settings are separate from those used
#' for file downloads.
#'
#' Hashing covers all files under `data_files/`, including existing files,
#' and excludes archives in `zip_files/`. With `unzip = FALSE`, hashing still
#' uses `data_files/`, which may be empty or contain previously extracted data.
#' @author Insang Song
#' @return
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`. Otherwise, after downloading and optional
#' extraction, `hash = FALSE` returns the download summary from
#' [download_run_method()] invisibly. With `hash = TRUE`, returns a visible
#' character scalar containing the combined MD5 hash of `data_files/`.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @seealso [download_setup_dir()], [download_unzip()], [process_cropscape()]
#' @examples
#' \dontrun{
#' download_cropscape(
#'   year = 2020,
#'   source = "GMU",
#'   directory_to_save = file.path(tempdir(), "cropscape"),
#'   acknowledgement = TRUE,
#'   download = FALSE # Preview archive URLs and zip_files/ destinations.
#' )
#' }
#' @export
download_cropscape <- function(
  year = seq(1997, 2023),
  source = c("GMU", "USDA"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "cropscape",
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
  chr_source <- match.arg(source)
  checkmate::assert_choice(chr_source, c("GMU", "USDA"))
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Check year and source combinations.
  int_minyear <- switch(chr_source, USDA = 2008, GMU = 1997)
  if (min(int_years) < int_minyear) {
    stop(sprintf("Year should be equal to or greater than %d.", int_minyear))
  }
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define base URL and file name template based on source.
  chr_base <- switch(
    chr_source,
    USDA = paste0(
      "https://www.nass.usda.gov/sites/default/files/",
      "Research_and_Science/Cropland/Release/datasets/"
    ),
    GMU = "https://nassgeodata.gmu.edu/nass_data_cache/tar/"
  )
  chr_filetemplate <- switch(
    chr_source,
    USDA = "%d_30m_cdls.zip",
    GMU = "%d_cdls.tar.gz"
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(paste0(chr_base, chr_filetemplate), int_years)
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- sprintf(
    paste0(chr_dir_to_download, chr_filetemplate),
    int_years
  )
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(chr_urls[1])
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = chr_urls,
    destfiles = chr_filenames,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Per-file directories to avoid filename collisions.
  chr_dirs_to_unzip <- paste0(
    chr_dir_to_save,
    sub("\\.(zip|tar\\.gz)$", "", basename(chr_filenames))
  )
  if (isTRUE(unzip)) {
    invisible(lapply(chr_dirs_to_unzip, amadeus:::download_setup_dir))
  }
  # ----------------------------------------------------------------------------
  # Pair each archive with its own destination. Cleanup follows all extractions.
  mapply(
    amadeus:::download_unzip,
    file_name = chr_filenames,
    directory_to_unzip = chr_dirs_to_unzip,
    MoreArgs = list(unzip = unzip),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  # ----------------------------------------------------------------------------
  # Remove zip files.
  amadeus:::download_remove_zips(
    remove = remove_zip,
    download_name = chr_filenames
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
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
