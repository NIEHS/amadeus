################################################################################
# {amadeus} functions for National Hydrography Dataset (NHDPlus) and
# hydrologic units.
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download National Hydrography Dataset (NHD) data
#' @description
#' NHDPlus data provides the most comprehensive and high-resolution
#' hydrography data. This function downloads **national** dataset from
#' NHDPlus Version 2.1 on USGS Amazon S3 storage.
#' @note
#' For HUC, set `type = "Seamless"`. HUC12 layer presents in the seamless
#' geodatabase. Users can aggregate HUC12 layer to make HUC6, HUC8, HUC10, etc.
#' For whom wants to download a specific region,
#' please visit [Get NHDPlus Data](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#ListofAreas)
#' @param region character(1). One of `c("Lower48", "Islands")`.
#' When `"Islands"` is selected, the data will be downloaded for Hawaii,
#' Puerto Rico, and Virgin Islands.
#' @param type character(1). One of `c("Seamless", "OceanCatchment")`.
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
#' Default is \code{FALSE}. Supports ".7z" extraction via \pkg{archive}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Downloaded files will be stored in \code{directory_to_save}.
#' @author Insang Song
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_usgs2023nhd}{amadeus}
#' @examples
#' \dontrun{
#' download_huc(
#'   region = "Lower48",
#'   type = "Seamless",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE,
#'   unzip = FALSE
#' )
#' }
#' @export
# @importFrom archive archive_extract
download_huc <-
  function(
    region = c("Lower48", "Islands"),
    type = c("Seamless", "OceanCatchment"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = TRUE,
    remove_command = FALSE,
    unzip = FALSE,
    hash = FALSE,
    show_progress = TRUE,
    max_tries = 20,
    rate_limit = 2
  ) {
    #### 1. check for data download acknowledgement
    amadeus::download_permit(acknowledgement = acknowledgement)
    #### 2. directory setup
    amadeus::download_setup_dir(directory_to_save)
    directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

    region <- match.arg(region)
    type <- match.arg(type)

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
    url_base <-
      "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NationalData/"

    if (region == "Lower48") {
      if (type == "Seamless") {
        url_template_nat <-
          "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
      }
      if (type == "OceanCatchment") {
        url_template_nat <-
          "NHDPlusV21_NationalData_OceanCatchment_Lower48_fgdb_01.7z"
      }
    }
    if (region == "Islands") {
      if (type == "Seamless") {
        url_template_nat <-
          "NHDPlusV21_NationalData_Seamless_Geodatabase_HI_PR_VI_PI_03.7z"
      }
      if (type == "OceanCatchment") {
        url_template_nat <-
          "NHDPlusV21_NationalData_OceanCatchment_Islands_fgdb_01.7z"
      }
    }
    download_urls <- paste0(url_base, url_template_nat)
    download_names <- paste0(directory_to_save, url_template_nat)

    #### Exit early if download=FALSE
    if (!isTRUE(download)) {
      message("Skipping download.\n")
      return(invisible(list(
        urls = download_urls,
        destfiles = download_names,
        n_files = 1
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
    sapply(
      download_names,
      amadeus::download_unzip,
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )

    message("Requests were processed.\n")
    return(amadeus::download_hash(hash, directory_to_save))
  }
# nolint end

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
