################################################################################
# {amadeus} functions for National Hydrography Dataset (NHDPlus) and
# hydrologic units.
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download National Hydrography Dataset Plus (NHDPlus) data
#' @description
#' Download a national NHDPlus Version 2.1 `.7z` archive from Amazon S3.
#' The selected region and data type determine the archive to download.
#' @note
#' For HUC, set `type = "Seamless"`. HUC12 layer presents in the seamless
#' geodatabase. Users can aggregate HUC12 layer to make HUC6, HUC8, HUC10, etc.
#' For whom wants to download a specific region,
#' please visit [Get NHDPlus Data](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#ListofAreas)
#' @param region character(1). Archive region: `"Lower48"` (default) or
#'   `"Islands"`.
#' @param type character(1). Data type: `"Seamless"` (default) or
#'   `"OceanCatchment"`.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   The `.7z` archive is saved in `zip_files/`; when `unzip = TRUE`, its
#'   contents are extracted into `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. Downloaded
#'   archives and extracted data can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` emits a warning and returns
#'   a preview of the planned archive without downloading or extracting it.
#'   Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` emits a
#'   warning. No command file is generated. Default is `FALSE`.
#' @param unzip logical(1). Extract the downloaded `.7z` archive into
#'   `data_files/` using the \pkg{archive} package. Default is `FALSE`, which
#'   retains the archive without extracting its contents.
#' @param remove_zip logical(1). Remove the downloaded `.7z` archive and its
#'   `zip_files/` directory, including remaining contents, after extraction.
#'   Default is `FALSE`. May be `TRUE` only when `unzip = TRUE`.
#' @param hash logical(1). Return a combined MD5 hash of the files under
#'   `data_files/` instead of the download summary when `download = TRUE`.
#'   Ignored when `download = FALSE`. Default is `FALSE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status messages may still be printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file download request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Request-throttling interval in seconds,
#'   passed to [download_run_method()]. Controls the average request rate,
#'   rather than guaranteeing a minimum gap between requests. Default is 2.
#' @details
#' `region` and `type` each select one value. Matching is case-sensitive, and
#' unambiguous abbreviations are accepted. If omitted, each argument uses its
#' first listed choice.
#'
#' `zip_files/` and `data_files/` are sibling directories created beneath
#' `directory_to_save`, including when `unzip = FALSE` or `download = FALSE`.
#' Preview mode returns the planned archive even if it already exists locally
#' and does not check remote availability.
#'
#' During downloading, [download_run_method()] skips an existing non-empty
#' archive in `zip_files/`. That archive is still passed to extraction when
#' `unzip = TRUE`. Existing extracted data alone do not cause a download to
#' be skipped. Archive member names and paths are preserved beneath
#' `data_files/`; pass the extracted geodatabase path to [process_huc()].
#'
#' Hashing includes all files recursively under `data_files/`, including
#' existing files, and excludes archives in `zip_files/`. With `unzip = FALSE`,
#' hashing still uses `data_files/`, which may be empty or contain previously
#' extracted data.
#' @return
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files = 1` is returned regardless of `hash`.
#'
#' When `download = TRUE` and `hash = FALSE`, returns the download summary from
#' [download_run_method()] invisibly, including `success`, `failed`, and
#' `skipped` file counts. When a download is attempted, the summary also
#' contains `failed_urls` and `failed_files` character vectors. Counts describe
#' archive downloads, not extracted files.
#'
#' When `download = TRUE` and `hash = TRUE`, returns a visible character scalar
#' containing the combined MD5 hash of the files under `data_files/`.
#' @author Insang Song
#' @seealso [download_setup_dir()], [download_unzip()], [process_huc()]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_usgs2023nhd}{amadeus}
#' @examples
#' \dontrun{
#' download_huc(
#'   region = "Lower48",
#'   type = "Seamless",
#'   directory_to_save = file.path(tempdir(), "nhdplus"),
#'   acknowledgement = TRUE,
#'   unzip = TRUE
#' )
#' }
# nolint end
#' @export
# @importFrom archive archive_extract
download_huc <- function(
  region = c("Lower48", "Islands"),
  type = c("Seamless", "OceanCatchment"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = FALSE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "huc",
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
  chr_region <- match.arg(region)
  chr_type <- match.arg(type)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(
    amadeus::download_sanitize_path(directory_to_save),
    zip = TRUE
  )
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define template based on region and data type.
  chr_template <- switch(
    paste0(chr_region, "_", chr_type),
    Lower48_Seamless = "Seamless_Geodatabase_Lower48_07.7z",
    Lower48_OceanCatchment = "OceanCatchment_Lower48_fgdb_01.7z",
    Islands_Seamless = "Seamless_Geodatabase_HI_PR_VI_PI_03.7z",
    Islands_OceanCatchment = "OceanCatchment_Islands_fgdb_01.7z"
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_url <- paste0(
    "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/",
    "Data/NationalData/NHDPlusV21_NationalData_",
    chr_template
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filename <- paste0(
    chr_dir_to_download,
    "NHDPlusV21_NationalData_",
    chr_template
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
  # Unzip downloaded .zip files.
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
#' Retrieve Hydrologic Unit Code (HUC) data
#' @author Insang Song
#' @param path character. Path to the file or the directory containing HUC data.
#' @param layer_name character(1). Layer name in the `path`
#' @param huc_level character(1). Field name of HUC level
#' @param huc_header character(1). The upper level HUC code header to extract
#'  lower level HUCs.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Arguments passed to `hydrogeofetch::get_huc()`
#' @return a `SpatVector` object
#' @seealso [`hydrogeofetch::get_huc`]
#' @importFrom terra vect
#' @importFrom terra vector_layers
#' @importFrom rlang inject
#' @examples
#' ## NOTE: Examples are wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' library(terra)
#' getf <- "WBD_National_GDB.gdb"
#' # check the layer name to read
#' terra::vector_layers(getf)
#' test1 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test2 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test3 <- process_huc(
#'   "",
#'   layer_name = NULL,
#'   huc_level = NULL,
#'   huc_header = NULL,
#'   id = "030202",
#'   type = "huc06"
#' )
#' }
#' @export
process_huc <-
  function(
    path,
    layer_name = NULL,
    huc_level = NULL,
    huc_header = NULL,
    extent = NULL,
    ...
  ) {
    # exclude the coverage due to write permission related to memoization
    #nocov start
    if (missing(path) || (!file.exists(path) && !dir.exists(path))) {
      if (!requireNamespace("hydrogeofetch", quietly = TRUE)) {
        stop(
          "Package 'hydrogeofetch' is required when fetching HUC data ",
          "remotely. ",
          "Please install it and try again."
        )
      }
      hucpoly <- try(
        rlang::inject(hydrogeofetch::get_huc(!!!list(...)))
      )
      if (inherits(hucpoly, "try-error")) {
        stop("HUC data was not found.")
      }
      hucpoly <- terra::vect(hucpoly)
    }
    #nocov end
    if (!missing(path) && (file.exists(path) || dir.exists(path))) {
      if (!is.null(huc_header)) {
        querybase <-
          sprintf(
            "SELECT * FROM %s WHERE %s LIKE '%s%%'",
            layer_name,
            huc_level,
            huc_header
          )
      } else {
        querybase <-
          sprintf("SELECT * FROM %s", layer_name)
      }
      if (!layer_name %in% terra::vector_layers(path)) {
        stop(
          paste0(
            "Layer ",
            layer_name,
            " not found in ",
            path
          )
        )
      }

      hucpoly <- try(
        terra::vect(
          path,
          query = querybase,
          extent = extent
        )
      )
    }
    return(hucpoly)
  }

################################################################################
# {calculate} tier
################################################################################
#' Calculate HUC covariates
#' @description
#' Extract HUC IDs at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and HUC IDs.
#' @param from SpatVector(1). Output from \code{process_huc()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_huc()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra intersect
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_huc(
#'   from = huc, # derived from process_huc() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_huc <- function(
  from,
  locs,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  if (!inherits(from, "SpatVector")) {
    stop("`from` must be the output of process_huc().")
  }

  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = 0, # radius irrelevant for point-in-poly usually, or treated as 0
    geom = geom
  )
  sites_e <- sites_list[[1]]

  message("Calculating HUC covariates...")

  # intersect
  sites_extracted <- terra::intersect(sites_e, from)
  sites_df <- terra::as.data.frame(sites_extracted)

  # Remove valid geometry columns if present
  cols_to_keep <- c(locs_id, setdiff(names(sites_df), locs_id))
  sites_df <- sites_df[, cols_to_keep, drop = FALSE]

  sites_return <- amadeus::calc_return_locs(
    covar = sites_df,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}
################################################################################
