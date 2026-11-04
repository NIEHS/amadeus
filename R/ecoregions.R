################################################################################
# {amadeus} functions for US EPA Ecoregions.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download ecoregion data
#' @description
#' The \code{download_ecoregion()} function accesses and downloads United
#' States Ecoregions data from the U.S. Environmental Protection Agency's (EPA)
#' Ecoregions.
#' @note Ecoregion data does not require authentication.
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
#' @author Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_omernik2014ecoregions}{amadeus}
#' @examples
#' \dontrun{
#' download_ecoregion(
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_ecoregion <- function(
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

  #### Define download URL
  download_url <- paste0(
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
    "us_eco_l3_state_boundaries.zip"
  )

  #### Build download file name
  download_name <- file.path(
    directory_to_download,
    "us_eco_l3_state_boundaries.zip"
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

  #### Unzip files
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
#' Process ecoregion data
#' @description
#' The [`process_ecoregion`] function imports and cleans raw ecoregion
#' data, returning a `SpatVector` object.
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note The function will fix Tukey's bridge in Portland, ME.
#' This fix will ensure that the EPA air quality monitoring sites
#' will be located within the ecoregion.
#' @author Insang Song
#' @return a `SpatVector` object
#' @importFrom terra vect
#' @importFrom sf st_read st_crs st_as_sfc st_transform st_intersects st_union
#' @importFrom data.table year
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' ecoregion <- process_ecoregion(
#'   path = "./data/epa_ecoregion.gpkg"
#' )
#' }
#' @export
process_ecoregion <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    ecoreg <- sf::st_read(path)
    # fix Tukey's bridge in Portland, ME
    # nolint start
    poly_tukey <-
      "POLYGON ((-70.258 43.68, -70.2555 43.68, -70.255 43.6733, -70.2576 43.6732, -70.258 43.68))"
    poly_tukey <- sf::st_as_sfc(poly_tukey, crs = "EPSG:4326")
    poly_tukey <- sf::st_transform(poly_tukey, sf::st_crs(ecoreg))

    # nolint end
    ecoreg <- ecoreg[,
      grepl(
        "^(L2_KEY|L3_KEY|NA_L2NAME|US_L3NAME|NA_L3NAME)",
        names(ecoreg)
      )
    ]
    ecoreg_edit_idx <- sf::st_intersects(ecoreg, poly_tukey, sparse = FALSE)
    ecoreg_edit_idx <- vapply(ecoreg_edit_idx, function(x) any(x), logical(1))
    if (!all(ecoreg_edit_idx == 0)) {
      ecoreg_else <- ecoreg[!ecoreg_edit_idx, ]
      ecoreg_edit <- sf::st_union(ecoreg[ecoreg_edit_idx, ], poly_tukey)
      ecoreg <- rbind(ecoreg_else, ecoreg_edit)
    }
    ecoreg$time <- paste0(
      "1997 - ",
      format(Sys.Date(), "%Y")
    )
    ecoreg <- terra::vect(ecoreg)
    if (!is.null(extent)) {
      ecoreg_crop <- terra::crop(ecoreg, extent)
      return(ecoreg_crop)
    } else {
      return(ecoreg)
    }
  }
################################################################################
