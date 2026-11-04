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
