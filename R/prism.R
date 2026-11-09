################################################################################
# {amadeus} functions for PRISM Climate Group data.
################################################################################

################################################################################
# {download} tier
################################################################################
# 3. PRISM Climate Group Web Service
# nolint start
#' Download PRISM data
#' @description
#' Download one time period and climate element from the PRISM Climate Group
#' Web Service. ZIP archives and extracted data are stored in separate
#' subdirectories using the shared download workflow.
#' @param time character(1). Single time-period code, retaining leading zeros.
#'   For `data_type = "ts"`, use `YYYYMMDD` for a daily grid (from 1981),
#'   `YYYYMM` for a monthly grid (from 1895), or `YYYY` for an annual grid
#'   (from 1895). Each time-series request selects one grid.
#'   The legacy normals options use `MM` for a monthly normal, `"14"` for
#'   an annual normal, or `MMDD` for a daily normal; see Details for service
#'   availability. Local validation requires a character scalar with 2, 4,
#'   6, or 8 characters; it does not validate calendar dates or availability.
#' @param element character(1). Climate element: `"ppt"` (default), `"tmin"`,
#'   `"tmax"`, `"tmean"`, `"tdmean"`, `"vpdmin"`, or `"vpdmax"`.
#'   The legacy normals options also accept `"solslope"`, `"soltotal"`,
#'   `"solclear"`, and `"soltrans"`; these are rejected for `"ts"`.
#' @param data_type character(1). Selected data type: `"ts"` (default) for
#'   4 km time series, `"normals_800"` for the legacy 800 m normals URL
#'   template, or `"normals"` for the legacy 4 km normals URL template.
#'   See Details for service availability.
#' @param format character(1). Requested time-series raster format: `"nc"`
#'   (default), `"asc"`, or `"grib2"`. The argument is validated but omitted
#'   from normals requests, with a message. See Details for provider support.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are downloaded into `zip_files/` and, when `unzip = TRUE`,
#'   extracted into `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. Downloaded
#'   archives and extracted rasters can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after directory creation, without contacting the service, downloading,
#'   or validating archives. Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   No command file is generated. Default is `FALSE`.
#' @param unzip logical(1). Extract the validated ZIP archive into
#'   `data_files/`. Default is `TRUE`. `FALSE` retains the archive without
#'   extracting it; ZIP validation still runs when `download = TRUE`.
#' @param remove_zip logical(1). Remove the downloaded archive and its
#'   `zip_files/` directory, including remaining contents, after extraction.
#'   Default is `FALSE`. May be `TRUE` only when `unzip = TRUE`.
#'   No removal occurs when `download = FALSE`.
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
#' The local archive name is constructed from the requested element, data
#' type, time, and, for time series, format. Names and paths inside the
#' archive are preserved during extraction.
#'
#' After downloading, the destination archive is checked by listing its
#' contents with [utils::unzip()]. This check also applies to existing
#' non-empty archives skipped by [download_run_method()]. An unreadable ZIP
#' raises an error, with a preview of the response text when available.
#' Listing an archive does not fully verify its members or raster contents.
#'
#' Hashing covers all files under `data_files/`, including existing files,
#' and excludes archives in `zip_files/`. With `unzip = FALSE`, hashing still
#' uses `data_files/`, which may be empty or contain previously extracted data.
#'
#' The PRISM Web Service Guide dated 26 March 2025 documents time-series ZIP
#' downloads, with daily data from 1981 and monthly and annual data from 1895.
#' It states that normals are unavailable through this service and lists
#' `nc`, `asc`, and `bil` format conversions, rather than `grib2`. The normals
#' and `grib2` options remain accepted by this function but are not documented
#' as supported by the current service. See the guide in References.
#' @author Insang Song
#' @return
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`. Otherwise, after downloading, validation, and
#' optional extraction, `hash = FALSE` returns the download summary from
#' [download_run_method()] invisibly. With `hash = TRUE`, returns a visible
#' character scalar containing the combined MD5 hash of `data_files/`.
#' @seealso [download_setup_dir()], [download_unzip()], [process_prism()]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_daly2000prism}{amadeus}
#'
#' * [PRISM Climate Group](https://prism.oregonstate.edu/)
#' * [PRISM Web Service Guide](https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf)
#' @examples
#' \dontrun{
#' download_prism(
#'   time = "202104",
#'   element = "ppt",
#'   data_type = "ts",
#'   format = "nc",
#'   directory_to_save = file.path(tempdir(), "prism"),
#'   acknowledgement = TRUE,
#'   download = FALSE # Preview the URL and zip_files/ destination.
#' )
#' }
#' @export
# nolint end
download_prism <- function(
  time,
  element = c(
    "ppt",
    "tmin",
    "tmax",
    "tmean",
    "tdmean",
    "vpdmin",
    "vpdmax",
    "solslope",
    "soltotal",
    "solclear",
    "soltrans"
  ),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
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
    dataset_name = "prism",
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
  chr_datatype <- match.arg(data_type)
  checkmate::assert_choice(chr_datatype, c("ts", "normals_800", "normals"))
  chr_element <- match.arg(element)
  checkmate::assert_choice(
    chr_element,
    c(
      "ppt",
      "tmin",
      "tmax",
      "tmean",
      "tdmean",
      "vpdmin",
      "vpdmax",
      "solslope",
      "soltotal",
      "solclear",
      "soltrans"
    )
  )
  chr_format <- match.arg(format)
  checkmate::assert_choice(chr_format, c("nc", "asc", "grib2"))
  if (startsWith(chr_datatype, "ts")) {
    if (startsWith(chr_element, "sol")) {
      stop("sol* elements are not available for 'ts' data type.")
    }
  } else {
    message("format is ignored for normals data type.")
  }
  checkmate::assert_character(time, min.chars = 1L)
  checkmate::assert_choice(nchar(time), c(2, 4, 6, 8))
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(
    amadeus::download_sanitize_path(directory_to_save),
    zip = TRUE
  )
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define file and URL templates based on type.
  chr_filetemp <- switch(
    chr_datatype,
    "ts" = "4km/%s/%s?format=%s",
    "normals_800" = "normals/800m/%s/%s",
    "normals" = "normals/4km/%s/%s"
  )
  chr_urltemp <- paste0(
    "https://services.nacse.org/prism/data/get/us/",
    chr_filetemp
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- ifelse(
    chr_datatype == "ts",
    sprintf(chr_urltemp, chr_element, time, chr_format),
    sprintf(chr_urltemp, chr_element, time)
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- paste0(
    chr_dir_to_download,
    "PRISM_",
    chr_element,
    "_",
    chr_datatype,
    "_",
    time,
    ifelse(chr_datatype == "ts", paste0("_", chr_format), ""),
    ".zip"
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run_method(
    urls = chr_urls,
    destfiles = chr_filenames,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Validate archives.
  amadeus:::validate_zip(chr_filenames)
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files.
  amadeus::download_unzip(
    file_name = chr_filenames,
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
#' Process PRISM data
#' @description
#' This function imports and cleans raw PRISM data,
#' returning a single `SpatRaster` object.
#' @param path character giving PRISM data path
#' Both file and directory path are acceptable.
#' @param element character(1). PRISM element name
#' @param time character(1). PRISM time name.
#' Should be character in length of 2, 4, 6, or 8.
#' "annual" is acceptable.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads time series or 30-year normal PRISM data.
#' @return a `SpatRaster` object with metadata of time and element.
#' @seealso [`terra::rast`], [`terra::metags`]
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' prism <- process_prism(
#'   path = "./data/PRISM_ppt_stable_4kmM3_202104_nc.nc",
#'   element = "ppt",
#'   time = "202104"
#' )
#' }
#' @export
# nolint start
process_prism <-
  function(
    path = NULL,
    element = NULL,
    time = NULL,
    extent = NULL,
    ...
  ) {
    prism_layer_name_from_file <- function(prism_file, fallback_element) {
      stem <- tools::file_path_sans_ext(basename(prism_file[1]))
      stem <- gsub("^(?i)prism_", "", stem, perl = TRUE)
      tokens <- strsplit(stem, "_", fixed = TRUE)[[1]]
      tokens <- tokens[nzchar(tokens)]

      if (length(tokens) == 0L) {
        return(tolower(fallback_element))
      }

      gsub("[^a-z0-9_]", "", tolower(tokens[1]))
    }

    # check inputs
    if (
      !element %in%
        c(
          "ppt",
          "tmin",
          "tmax",
          "tmean",
          "tdmean",
          "vpdmin",
          "vpdmax",
          "solslope",
          "soltotal",
          "solclear",
          "soltrans"
        )
    ) {
      stop("element is not a valid PRISM element.")
    }
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!nchar(time) %in% seq(2, 8, 2)) {
      stop("time does not have valid length.")
    }

    if (dir.exists(path)) {
      pattern <- "PRISM_%s*.*_%s_*.*(bil|nc|grib2|asc)$"
      pattern <- sprintf(pattern, element, time)
      prism_file <-
        list.files(
          path,
          pattern = pattern,
          full.names = TRUE
        )
    } else {
      prism_file <- path
    }
    prism <- terra::rast(prism_file, win = extent)
    if (terra::nlyr(prism) == 1L) {
      names(prism) <- prism_layer_name_from_file(prism_file, element)
    }
    terra::metags(prism) <- c(
      paste0("time=", time),
      paste0("element=", element)
    )
    return(prism)
  }
# nolint end

################################################################################
# {calculate} tier
################################################################################
# prism, cropscape, huc
#' Calculate PRISM covariates
#' @description
#' Extract PRISM values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and PRISM variable. PRISM
#' variable column name reflects the PRISM variable and
#' circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_prism()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_prism()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_prism(
#'   from = prism, # derived from process_prism() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_prism <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  # check input class
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
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
  sites_id <- sites_list[[2]]
  #### perform extraction

  message(
    sprintf(
      "Calculating PRISM covariates with %d meters radius...",
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
  fun_extract <- amadeus::calc_weighted_fun(
    fun = "mean",
    weighted = !is.null(weights_prepared)
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    # use terra::extract for point locations
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
  } else {
    # use exactextractr::exact_extract for polygon locations and buffered points
    sites_e_buf <- sf::st_as_sf(sites_e)
    extract_args <- c(
      list(
        x = from,
        y = sites_e_buf,
        fun = fun_extract,
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)
  }

  # clean up names if they are from exact_extract (prefix "mean.")
  if (radius > 0 || is_polygon_locs) {
    exact_names <- colnames(sites_extracted)
    if (length(exact_names) == 1 && identical(exact_names, "mean")) {
      exact_names <- names(from)[1]
    } else {
      exact_names <- gsub("^mean\\.", "", exact_names)
    }
    colnames(sites_extracted) <- exact_names
  }

  # append radius
  new_names <- sprintf("%s_%d", colnames(sites_extracted), radius)
  colnames(sites_extracted) <- new_names

  # Combine with IDs
  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  # reorder to put ID first
  sites_extracted <- sites_extracted[, c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )]

  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    if (!"time" %in% names(sites_extracted)) {
      value_cols_now <- setdiff(names(sites_extracted), c(locs_id, "geometry"))
      if (length(value_cols_now) != 1L) {
        stop(
          "PRISM `.by_time` summarization requires a single covariate column ",
          "or an existing `time` column.\n"
        )
      }
      prism_time <- NA
      time_vals <- try(terra::time(from), silent = TRUE)
      if (
        !inherits(time_vals, "try-error") &&
          length(time_vals) >= 1L &&
          !is.na(time_vals[1])
      ) {
        prism_time <- time_vals[1]
      }
      if (is.na(prism_time)) {
        meta <- try(terra::metags(from), silent = TRUE)
        if (
          !inherits(meta, "try-error") &&
            is.data.frame(meta) &&
            nrow(meta) > 0
        ) {
          idx_time <- which(meta[, 1] == "time")
          if (length(idx_time) == 1L) {
            time_raw <- meta[idx_time, 2]
            if (grepl("^[0-9]{8}$", time_raw)) {
              prism_time <- as.Date(time_raw, format = "%Y%m%d")
            } else if (grepl("^[0-9]{6}$", time_raw)) {
              prism_time <- as.Date(paste0(time_raw, "01"), format = "%Y%m%d")
            } else if (grepl("^[0-9]{4}$", time_raw)) {
              prism_time <- as.Date(paste0(time_raw, "-01-01"))
            }
          }
        }
      }
      if (is.na(prism_time)) {
        stop(
          "Could not derive PRISM time for `.by_time` summarization. ",
          "Provide data with explicit time in layer metadata.\n"
        )
      }
      sites_extracted$time <- prism_time
    }
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}
################################################################################
