################################################################################
# {amadeus} functions for MODIS and Black Marble products.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download MODIS product files
#' @description Downloads MODIS data using httr2 with robust retry logic and
#' rate limiting. This function queries NASA's CMR API for available granules
#' and downloads relevant tiles based on the specified extent.
#' @note Due to NASA data access policies, downloads require a valid NASA
#' Earthdata token for authentication. For security, it's recommended to store
#' your token in an environment variable or file rather than in your code.
#' Use \code{setup_nasa_token()} for easy, secure token setup.
#' @note Both dates in \code{date} should be in the same year.
#'  Directory structure:
#'  input/modis/raw/\{version\}/\{product\}/\{year\}/\{day_of_year\}.
#' @param product character(1). MODIS product code
#' @param version character(1). Default is `"061"`, meaning v061.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token.
#'   For security, recommended options (in priority order):
#'   \itemize{
#'     \item NULL (default): Reads from NASA_EARTHDATA_TOKEN environment
#'     variable
#'     \item File path: e.g., "~/.nasa_earthdata_token"
#'     \item Token string: Direct token (not recommended for scripts)
#'   }
#'   Use \code{setup_nasa_token()} for interactive setup.
#' @param date character(1 or 2). Date range "YYYY-MM-DD" format
#' @param extent numeric(4). Bounding box `c(min_lon, max_lon, min_lat,
#' max_lat)`.
#' Default covers continental US: `c(-125, 22, -64, 50)`.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed with
#' download
#' @param download logical(1). DEPRECATED. Downloads now happen automatically.
#'   Set to FALSE to skip downloading (generates file list only).
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum download retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_mcd19a22021}{amadeus}
#' \insertRef{data_mod06l2_2017}{amadeus}
#' \insertRef{data_mod09ga2021}{amadeus}
#' \insertRef{data_mod11a12021}{amadeus}
#' \insertRef{data_mod13a22021}{amadeus}
#' \insertRef{article_roman2018vnp46}{amadeus}
#' @examples
#' \dontrun{
#' # RECOMMENDED: Set up token once (persists across sessions)
#' setup_nasa_token()
#'
#' # Then download without specifying token
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = "2024-01-01",
#'   extent = c(-80, 35, -75, 40),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # ALTERNATIVE: Token from file
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = "2024-01-01",
#'   extent = c(-80, 35, -75, 40),
#'   nasa_earth_data_token = "~/.nasa_earthdata_token",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # ALTERNATIVE: Set token for current session
#' Sys.setenv(NASA_EARTHDATA_TOKEN = "your_token_here")
#' download_modis(
#'   product = "MOD09GA",
#'   date = "2024-01-01",
#'   acknowledgement = TRUE
#' )
#'
#' # Date range
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = c("2024-01-01", "2024-01-07"),
#'   extent = c(-80, 35, -75, 40),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_modis <- function(
  product = c(
    "MOD09GA",
    "MYD09GA",
    "MOD09GQ",
    "MYD09GQ",
    "MOD09A1",
    "MYD09A1",
    "MOD09Q1",
    "MYD09Q1",
    "MOD11A1",
    "MYD11A1",
    "MOD11A2",
    "MYD11A2",
    "MOD11B1",
    "MYD11B1",
    "MOD13A1",
    "MYD13A1",
    "MOD13A2",
    "MYD13A2",
    "MOD13Q1",
    "MYD13Q1",
    "MOD13A3",
    "MYD13A3",
    "MCD12Q1",
    "MOD14A1",
    "MYD14A1",
    "MOD14A2",
    "MYD14A2",
    "MOD14CM1",
    "MYD14CM1",
    "MOD16A2",
    "MYD16A2",
    "MCD64A1",
    "MCD64CMQ",
    "MOD06_L2",
    "MCD14ML",
    "MCD19A2",
    "VNP46A2",
    "VNP64A1"
  ),
  version = "061",
  nasa_earth_data_token = NULL,
  date = c("2023-09-01", "2023-09-01"),
  extent = c(-125, 22, -64, 50),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 4. Check and retrieve NASA token (BEFORE null check)
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### 5. Check for null parameters (AFTER token retrieval)
  params_to_check <- mget(ls())
  params_to_check <- params_to_check
  amadeus::check_for_null_parameters(params_to_check)
  product <- match.arg(product)

  if (substr(date[1], 1, 4) != substr(date[2], 1, 4)) {
    if (
      !product %in%
        c(
          "MOD06_L2",
          "MOD14CM1",
          "MYD14CM1",
          "MCD14ML",
          "MCD64A1",
          "MCD64CMQ",
          "VNP64A1"
        )
    ) {
      stop("dates should be in the same year.\n")
    }
  }

  #### 7. Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after querying available files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 8. Warning for excessive query
  dt_date <- as.Date(date)
  if (diff(dt_date) > 31) {
    warning(
      "Date range is greater than 31 days. ",
      "The results may not include all dates in the range.",
      call. = FALSE
    )
  }

  #### 9. Version fix
  if (product == "MOD06_L2") {
    str_version <- "6.1"
  } else if (product %in% c("MOD14CM1", "MYD14CM1")) {
    str_version <- "005"
  } else if (product == "MCD64CMQ") {
    str_version <- "006"
  } else if (product == "MCD14ML") {
    str_version <- "6.1NRT"
  } else if (product %in% c("VNP46A2", "VNP64A1")) {
    str_version <- NULL
  } else {
    str_version <- version
  }

  #### 10. Query CMR
  message("Querying NASA CMR for available granules...\n")
  chr_extent <- paste(extent, collapse = ",")
  query_granules <- function(short_name, version) {
    resp <- tryCatch(
      httr2::request(
        "https://cmr.earthdata.nasa.gov/search/granules.json"
      ) |>
        httr2::req_url_query(
          short_name = short_name,
          version = version,
          temporal = paste(date[1], date[2], sep = ","),
          bounding_box = chr_extent,
          page_size = 2000
        ) |>
        httr2::req_options(connecttimeout = 30L) |>
        httr2::req_retry(retry_on_failure = TRUE, max_tries = 5L) |>
        httr2::req_timeout(120) |>
        httr2::req_perform(),
      error = function(e) {
        stop(
          "Failed to query NASA CMR (cmr.earthdata.nasa.gov). ",
          "Check network connectivity and NASA EarthData status at ",
          "https://status.earthdata.nasa.gov. Original error: ",
          conditionMessage(e)
        )
      }
    )
    resp |> httr2::resp_body_json()
  }
  granules <- query_granules(product, str_version)

  if (
    product %in% c("MOD14CM1", "MYD14CM1") && length(granules$feed$entry) == 0
  ) {
    product_fallback <- switch(
      product,
      MOD14CM1 = "MOD14A2",
      MYD14CM1 = "MYD14A2"
    )
    message(
      sprintf(
        "No granules found for %s; retrying with %s (v006).\n",
        product,
        product_fallback
      )
    )
    granules <- query_granules(product_fallback, "006")
  }

  # Extract product data URLs.
  granule_entries <- granules$feed$entry
  urls <- vapply(
    granule_entries,
    function(g) {
      links <- g$links
      data_links <- Filter(
        function(l) {
          grepl("data#", l$rel) &&
            if (product == "MCD14ML") {
              grepl("\\.txt$", l$href, ignore.case = TRUE)
            } else {
              grepl("\\.(hdf|h5)$", l$href, ignore.case = TRUE)
            }
        },
        links
      )
      if (length(data_links) > 0) data_links[[1]]$href else NA_character_
    },
    character(1)
  )

  keep <- !is.na(urls)
  urls <- urls[keep]

  if (length(urls) == 0) {
    stop("No granules found for the specified query parameters.\n")
  }

  #### 12. Filter by date range
  urls_filtered <- modis_filter_paths_by_date(urls, date = date)
  if (length(urls_filtered) == 0) {
    stop("No granules matched the requested date range.\n")
  }

  keep_date <- urls %in% urls_filtered
  urls <- urls[keep_date]
  download_names <- basename(urls)

  scale_detected <- modis_extract_temporal_scale(download_names[1])
  if (scale_detected == "monthly") {
    month_start <- as.Date(format(as.Date(date[1]), "%Y-%m-01"))
    month_end <- as.Date(format(as.Date(date[2]), "%Y-%m-01"))
    month_sequence <- seq(month_start, month_end, by = "month")
    message(sprintf(
      "Found %d / %d monthly files in the queried date range.\n",
      length(urls),
      length(month_sequence)
    ))
  } else {
    date_sequence <- vapply(
      download_names,
      modis_extract_temporal_key,
      character(1)
    )
    date_sequence <- unique(date_sequence[!is.na(date_sequence)])
    date_start_i <- as.integer(strftime(date[1], "%Y%j"))
    date_end_i <- as.integer(strftime(date[2], "%Y%j"))
    date_range_julian <- seq(date_start_i, date_end_i)
    date_sequence_i <- as.integer(date_sequence)
    date_sequence_in <- date_sequence_i %in% date_range_julian

    message(sprintf(
      "Found %d / %d days of data in the queried date range.\n",
      sum(date_sequence_in),
      length(date_range_julian)
    ))
  }

  #### 13. Prepare download paths
  destfiles <- paste0(directory_to_save, download_names)

  #### 14. Exit early if download=FALSE (deprecated behavior)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(urls)
    ))
    return(invisible(list(
      urls = urls,
      destfiles = destfiles,
      n_files = length(urls)
    )))
  }

  #### 15. Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = urls,
    destfiles = destfiles,
    token = nasa_earth_data_token,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  message("Download process complete.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

################################################################################
modis_extract_temporal_key <- function(path) {
  filename <- basename(path)

  key_daily <- regmatches(
    filename,
    regexpr("A20\\d{5}", filename)
  )
  if (length(key_daily) > 0 && nzchar(key_daily)) {
    return(sub("^A", "", key_daily))
  }

  key_txt <- regmatches(
    filename,
    regexpr("_[12][0-9]{6}", filename)
  )
  if (length(key_txt) > 0 && nzchar(key_txt)) {
    return(sub("^_", "", key_txt))
  }

  key_monthly <- regmatches(
    filename,
    regexpr("\\.20[0-9]{4}\\.", filename)
  )
  if (length(key_monthly) > 0 && nzchar(key_monthly)) {
    return(gsub("\\.", "", key_monthly))
  }

  return(NA_character_)
}

################################################################################
modis_extract_temporal_scale <- function(path) {
  filename <- basename(path)

  if (grepl("A20\\d{5}", filename)) {
    return("daily")
  }
  if (grepl("_[12][0-9]{6}", filename)) {
    return("daily")
  }
  if (grepl("\\.20[0-9]{4}\\.", filename)) {
    return("monthly")
  }

  return(NA_character_)
}

################################################################################
modis_key_to_date <- function(
  key,
  scale
) {
  if (length(scale) == 1L) {
    scale <- rep(scale, length(key))
  }
  stopifnot(length(key) == length(scale))

  as_date <- mapply(
    function(key_i, scale_i) {
      if (is.na(key_i) || is.na(scale_i)) {
        return(as.Date(NA))
      }
      if (scale_i == "daily") {
        return(as.Date(key_i, format = "%Y%j"))
      }
      if (scale_i == "monthly") {
        return(as.Date(paste0(key_i, "01"), format = "%Y%m%d"))
      }
      stop("Unsupported MODIS temporal scale.\n")
    },
    key,
    scale,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  return(as.Date(as_date, origin = "1970-01-01"))
}

################################################################################
modis_filter_paths_by_date <- function(
  paths,
  date
) {
  if (length(paths) == 0) {
    return(paths)
  }
  if (length(date) == 1L) {
    date <- rep(date, 2L)
  }

  keys <- vapply(paths, modis_extract_temporal_key, character(1))
  scales <- vapply(paths, modis_extract_temporal_scale, character(1))

  if (all(is.na(keys)) || all(is.na(scales))) {
    return(character(0))
  }

  scales_unique <- unique(stats::na.omit(scales))
  if (length(scales_unique) != 1L) {
    stop("MODIS paths contain mixed or unsupported temporal patterns.\n")
  }

  scale_target <- scales_unique[[1]]
  if (scale_target == "monthly") {
    month_start <- as.Date(format(as.Date(date[1]), "%Y-%m-01"))
    month_end <- as.Date(format(as.Date(date[2]), "%Y-%m-01"))
    month_sequence <- seq(month_start, month_end, by = "month")
    target_keys <- format(month_sequence, "%Y%m")
    keep <- keys %in% target_keys
  } else {
    parsed_dates <- modis_key_to_date(keys, scales)
    keep <- parsed_dates >= as.Date(date[1]) & parsed_dates <= as.Date(date[2])
  }

  return(paths[keep])
}

################################################################################
#' Convert spatial extent to MODIS sinusoidal tile codes
#' @description
#' Returns the set of MODIS sinusoidal grid tile codes (e.g. \code{"h08v04"})
#' whose geographic footprint overlaps the supplied bounding box.
#' @details
#' The MODIS sinusoidal grid divides the globe into 18 x 36 tiles, each
#' nominally covering 10 degrees of latitude. Because the sinusoidal projection
#' compresses longitude at high latitudes, the geographic lon/lat bounding
#' boxes of tiles are \emph{not} simple 10-degree squares — they can be
#' significantly wider in geographic longitude near the poles.
#'
#' This function uses the official NASA MODLAND sinusoidal tile bounding
#' coordinates table (\code{sn_bound_10deg.txt},
#' \url{https://modis-land.gsfc.nasa.gov/pdf/sn_bound_10deg.txt}) bundled in
#' \code{inst/extdata/}. It returns every non-fill tile whose geographic
#' bounding box overlaps the requested extent.
#'
#' Horizontal tile numbers (h) range from 0 to 35 (west to east); vertical
#' tile numbers (v) range from 0 to 17 (north to south).
#' @param extent numeric(4). Bounding box \code{c(xmin, ymin, xmax, ymax)}
#'   in decimal degrees (EPSG:4326).
#' @return character vector of tile codes in \code{"hXXvYY"} format, ordered
#'   by increasing v then h.
#' @author Kyle Messier
#' @seealso \code{\link{download_modis}}
#' @examples
#' extent_to_modis_tiles(c(-125, 22, -64, 50))
#' @keywords internal auxiliary
#' @export
extent_to_modis_tiles <- function(extent) {
  stopifnot(
    "extent must be numeric(4)" = is.numeric(extent) && length(extent) == 4,
    "extent[1] (xmin) must be >= -180" = extent[1] >= -180,
    "extent[3] (xmax) must be <= 180" = extent[3] <= 180,
    "extent[2] (ymin) must be >= -90" = extent[2] >= -90,
    "extent[4] (ymax) must be <= 90" = extent[4] <= 90,
    "xmin must be < xmax" = extent[1] < extent[3],
    "ymin must be < ymax" = extent[2] < extent[4]
  )

  bounds_file <- system.file(
    "extdata",
    "sn_bound_10deg.txt",
    package = "amadeus"
  )
  stopifnot(
    "sn_bound_10deg.txt not found in inst/extdata/" = nzchar(bounds_file)
  )

  lines <- readLines(bounds_file, warn = FALSE)
  data_lines <- grep("^ *[0-9]", lines, value = TRUE)
  tiles_df <- utils::read.table(
    text = paste(data_lines, collapse = "\n"),
    col.names = c("iv", "ih", "lon_min", "lon_max", "lat_min", "lat_max")
  )

  # Drop fill tiles (lon_min == -999)
  tiles_df <- tiles_df[tiles_df$lon_min > -900, ]

  xmin <- extent[1]
  ymin <- extent[2]
  xmax <- extent[3]
  ymax <- extent[4]

  # Bounding-box overlap: tile overlaps query if
  #   tile_lon_max >= xmin  AND  tile_lon_min <= xmax
  #   tile_lat_max >= ymin  AND  tile_lat_min <= ymax
  hits <- tiles_df[
    tiles_df$lon_max >= xmin &
      tiles_df$lon_min <= xmax &
      tiles_df$lat_max >= ymin &
      tiles_df$lat_min <= ymax,
  ]

  sprintf("h%02dv%02d", hits$ih, hits$iv)
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process MODIS sub-datasets
#' @description
#' Selected MODIS sinusoidal grid product subdataset name selector.
#' Four presets are supported. `custom_sel` supersedes
#' presets of `product` values.
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @param ... Placeholders.
#' @note
#' Preset product codes and associated variables include
#' * "MOD11A1" - Land surface temperature (LST)
#' * "MOD13A2" - Normalized Difference Vegetation Index (NDVI)
#' * "MOD09GA" - Surface reflectance, and
#' * "MCD19A2" - Aerosol optical depth (AOD) and plume injection height.
#'
#' For a full list of available
#' MODIS product codes, see the "Short Name" column at
#' [NASA LP DAAC Search Data Catalog](https://www.earthdata.nasa.gov/centers/lp-daac).
#' When utilizing a product code from this "Short Name" column, \strong{do
#' not include} the version number following the period. For example, if "Short
#' Name" = MCD12C1.006, then `product = "MCD12C1"`.
# nolint end
#' @author Insang Song
#' @return A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calculate_modis]
#' @keywords internal
#' @noRd
#' @examples
#' amadeus:::process_modis_sds(product = "MOD09GA")
# previously modis_prefilter_sds
process_modis_sds <-
  function(
    product = c(
      "MOD11A1",
      "MOD13A2",
      "MOD13Q1",
      "MYD13Q1",
      "MOD09GA",
      "MCD19A2",
      "MOD14A1",
      "MYD14A1",
      "MOD14A2",
      "MYD14A2",
      "MOD16A2",
      "MYD16A2",
      "MCD64A1",
      "MCD64CMQ",
      "MCD12Q1",
      "VNP64A1"
    ),
    custom_sel = NULL,
    ...
  ) {
    if (!is.null(custom_sel)) {
      modis_sds <- custom_sel
    } else {
      product <- match.arg(product)
      modis_sds <-
        switch(
          product,
          MOD11A1 = "(LST_)",
          MOD13A2 = "(NDVI)",
          MOD13Q1 = "250m 16 days (NDVI|EVI)",
          MYD13Q1 = "250m 16 days (NDVI|EVI)",
          MOD09GA = "(sur_refl_b0)",
          MCD19A2 = "(Optical_Depth|Injection_Height)",
          MOD14A1 = "(FireMask)",
          MYD14A1 = "(FireMask)",
          MOD14A2 = "(FireMask)",
          MYD14A2 = "(FireMask)",
          MOD16A2 = "(ET_500m|PET_500m)",
          MYD16A2 = "(ET_500m|PET_500m)",
          MCD64A1 = "(Burn Date|BurnDate)",
          MCD64CMQ = "(Burn Date|BurnDate)",
          MCD12Q1 = "(LC_Type)",
          VNP64A1 = "(BurnDate)"
        )
      if (product == "MCD19A2") {
        message(
          sprintf(
            "For MCD19A2, use %s for 5km resolution sub-datasets.\n",
            "(cos|RelAZ|Angle)"
          )
        )
      } else if (product == "MCD12Q1") {
        message(
          sprintf(
            "For MCD12Q1, use %s to select a specific land cover layer.\n",
            "(LC_Type1|LC_Type2|LC_Type3|LC_Type4|LC_Type5)"
          )
        )
      }
    }
    return(modis_sds)
  }

################################################################################
#' Process MODIS layers
#' @description
#' Aggregate layers in a sub-dataset in sinusoidal MODIS products.
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param subdataset character(1). Exact or regular expression filter of
#' sub-dataset.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @keywords internal
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @description Some MODIS products consist of multi-layer subdatasets.
#' This function aggregates multiple layers into single layer `SpatRaster.`
#' `fun_agg` is applied at overlapping cells.
#' @note HDF values are read as original without scaling.
#' Users should consult MODIS product documentation to apply proper
#' scaling factor for post-hoc adjustment. If users have no preliminary
#' information about MODIS sub-datasets, consider running
#' `terra::describe(__filename__, sds = TRUE)` to navigate the full
#' list of sub-datasets in the input file then consult the documentation
#' of MODIS product.
#' @importFrom terra describe
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @importFrom terra is.rotated
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_flatten <- process_flatten_sds(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE)[1],
#'   subdataset = "(sur_refl_b0)",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# previously modis_aggregate_sds
process_flatten_sds <-
  function(
    path = NULL,
    subdataset = NULL,
    fun_agg = "mean",
    ...
  ) {
    # if curvilinear, halt
    status_curv <-
      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop(
        "The raster is curvilinear. Please rectify or warp
the input then flatten it manually."
      )
    }

    # describe provides subdataset information
    if (!any(grepl("^HDF", path))) {
      if (
        !is.character(subdataset) ||
          length(subdataset) != 1L ||
          is.na(subdataset) ||
          !nzchar(subdataset)
      ) {
        stop("Argument subdataset should be a non-empty character value.\n")
      }
      # we use var to get detailed information in subdatasets
      sds_desc <- terra::describe(path, sds = TRUE)
      index_sds <- sort(unique(c(
        grep(subdataset, sds_desc$var),
        grep(subdataset, sds_desc$name)
      )))
      if (length(index_sds) == 0L) {
        available_sds <- paste(stats::na.omit(sds_desc$var), collapse = ", ")
        stop(
          sprintf(
            paste0(
              "No MODIS subdatasets matched `subdataset = \"%s\"` ",
              "in `%s`.\nAvailable subdatasets: %s\n"
            ),
            subdataset,
            path,
            available_sds
          )
        )
      }
      sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]
      # raw is TRUE to ignore scaling factor.
      sds_read <- terra::rast(sds_desc$name, raw = TRUE)
      sds_nsds <- nrow(sds_desc)
      sds_nlyr <- sds_desc$nlyr
      sds_varn <- sds_desc$var
    } else {
      sds_read <- terra::rast(path, raw = TRUE)
      sds_nsds <- 1L
      sds_nlyr <- terra::nlyr(sds_read)
      sds_varn <- names(sds_read)
    }
    if (all(sds_nlyr == 1L)) {
      sds_agg <- sds_read
    } else if (sds_nsds == 1L) {
      sds_agg <- terra::app(sds_read, fun = fun_agg, na.rm = TRUE)
    } else {
      sds_aggindex <- rep(seq_len(sds_nsds), times = sds_nlyr)
      # if there are multiple layers in a subdataset,
      # aggregate overlapping pixel values
      sds_agg <-
        terra::tapp(sds_read, index = sds_aggindex, fun = fun_agg, na.rm = TRUE)
    }
    # restore names
    names(sds_agg) <- sds_varn
    gc()
    return(sds_agg)
  }

################################################################################
# nolint start
#' Process MODIS .hdf files
#' @description
#' Get mosaic or merged raster from multiple MODIS hdf files.
#' @param path character. Full list of hdf file paths.
#'  preferably a recursive search result from [`base::list.files`].
#' @param date character(1). date to query. Should be in
#' `"YYYY-MM-DD"` format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' Default is `NULL`, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (e.g., Aqua files) to fuse with `path` for improved temporal coverage.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, `"secondary_first"`.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/package=MODISTools),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [`download_data`]
#' @author Insang Song
#' @return a `SpatRaster` object
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_merge <- process_modis_merge(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = "2024-01-01",
#'   subdataset = "sur_refl_b01_1",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# nolint end
# previously modis_get_vrt
process_modis_merge <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  ...
) {
  fusion_method <- match.arg(fusion_method)
  if (!is.character(path)) {
    stop("Argument path should be a list of hdf files (character).\n")
  }
  if (!is.null(path_secondary) && !is.character(path_secondary)) {
    stop("Argument path_secondary should be a list of hdf files (character).\n")
  }
  if (!(is.character(fun_agg) || is.function(fun_agg))) {
    stop(
      "Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n"
    )
  }
  # date format check
  amadeus::is_date_proper(instr = date)

  # interpret date
  ftarget <- modis_filter_paths_by_date(path, date = date)
  if (length(ftarget) == 0) {
    stop("No MODIS files matched the requested date.\n")
  }

  # get layer information
  layer_target <-
    lapply(ftarget, function(x) {
      process_flatten_sds(
        x,
        subdataset = subdataset,
        fun_agg = fun_agg
      )
    })
  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(ftarget) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
    gc()
  } else {
    result_merged <- layer_target[[1]]
  }

  if (!is.null(path_secondary)) {
    ftarget_secondary <- modis_filter_paths_by_date(path_secondary, date = date)
    if (length(ftarget_secondary) > 0) {
      layer_target_secondary <-
        lapply(ftarget_secondary, function(x) {
          process_flatten_sds(
            x,
            subdataset = subdataset,
            fun_agg = fun_agg
          )
        })

      if (length(ftarget_secondary) > 1) {
        result_secondary <- do.call(terra::merge, layer_target_secondary)
      } else {
        result_secondary <- layer_target_secondary[[1]]
      }

      if (
        !isTRUE(terra::compareGeom(
          result_merged,
          result_secondary,
          stopOnError = FALSE
        ))
      ) {
        stop(
          "Primary and secondary MODIS rasters have incompatible geometry.\n"
        )
      }
      if (terra::nlyr(result_merged) != terra::nlyr(result_secondary)) {
        stop(
          "Primary and secondary MODIS rasters have different layer counts.\n"
        )
      }

      if (fusion_method == "primary_first") {
        result_merged <- terra::cover(result_merged, result_secondary)
      } else if (fusion_method == "secondary_first") {
        result_merged <- terra::cover(result_secondary, result_merged)
      } else {
        idx_layers <- seq_len(terra::nlyr(result_merged))
        fused <- lapply(idx_layers, function(k) {
          terra::app(
            c(result_merged[[k]], result_secondary[[k]]),
            mean,
            na.rm = TRUE
          )
        })
        result_merged <- do.call(c, fused)
      }
    }
  }
  return(result_merged)
}

################################################################################
# nolint start
#' Process MODIS files as daily outputs
#' @description
#' Process MODIS HDF/H5 files into day-specific rasters over a requested
#' date range. This helper preserves daily slices instead of flattening a
#' multi-day range into one merged result.
#' @param path character. Full list of HDF/H5 file paths.
#' @param date character(1:2). Date or date range in `"YYYY-MM-DD"` format.
#' @param subdataset character(1). Subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (for example, Aqua files) to fuse with `path` by date.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, or `"secondary_first"`.
#' @param return_type character(1). Return `"stack"` for a multi-layer
#' `SpatRaster` (default) or `"list"` for a named list of daily `SpatRaster`
#' objects.
#' @param ... Additional arguments passed to [`process_modis_merge`].
#' @return A day-preserving MODIS result as a `SpatRaster`
#' (`return_type = "stack"`) or named list (`return_type = "list"`).
#' @seealso [`process_modis_merge`], [`download_data`]
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_daily <- process_modis_daily(
#'   path = list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = c("2024-01-01", "2024-01-07"),
#'   subdataset = "sur_refl_b01_1",
#'   return_type = "list"
#' )
#' }
#' @export
# nolint end
process_modis_daily <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  return_type = c("stack", "list"),
  ...
) {
  return_type <- match.arg(return_type)
  amadeus::is_date_proper(instr = date)

  date <- if (length(date) == 1) rep(date, 2) else date
  date_seq <- format(
    seq(as.Date(date[1]), as.Date(date[2]), by = "day"),
    "%Y-%m-%d"
  )
  daily_rasters <- vector("list", length(date_seq))
  names(daily_rasters) <- date_seq

  for (i in seq_along(date_seq)) {
    day_i <- date_seq[[i]]
    daily_rasters[i] <- list(tryCatch(
      process_modis_merge(
        path = path,
        date = day_i,
        subdataset = subdataset,
        fun_agg = fun_agg,
        path_secondary = path_secondary,
        fusion_method = fusion_method,
        ...
      ),
      error = function(e) {
        if (
          grepl(
            "No MODIS files matched the requested date",
            e$message,
            fixed = TRUE
          )
        ) {
          return(NULL)
        }
        stop(e)
      }
    ))
  }

  daily_rasters <- daily_rasters[!vapply(daily_rasters, is.null, logical(1))]
  if (length(daily_rasters) == 0L) {
    stop("No MODIS files matched any day in the requested date range.\n")
  }

  if (return_type == "list") {
    return(daily_rasters)
  }

  rasters_named <- mapply(
    FUN = function(r_i, date_i) {
      names(r_i) <- paste0(names(r_i), "_", gsub("-", "", date_i))
      r_i
    },
    daily_rasters,
    names(daily_rasters),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  do.call(c, rasters_named)
}

################################################################################
process_mcd14ml <- function(
  path = NULL,
  date = NULL,
  extent = NULL,
  ...
) {
  if (is.null(path)) {
    stop("path is required.\n")
  }

  if (length(path) == 1L && dir.exists(path)) {
    path <- list.files(
      path = path,
      pattern = "\\.txt$",
      recursive = TRUE,
      full.names = TRUE
    )
  }

  path <- path[grepl("\\.txt$", path, ignore.case = TRUE)]
  if (length(path) == 0) {
    stop("No MCD14ML text files were found.\n")
  }

  txt_list <- lapply(path, data.table::fread)
  txt_data <- data.table::rbindlist(txt_list, fill = TRUE)
  names(txt_data) <- tolower(names(txt_data))

  required_cols <- c("latitude", "longitude", "acq_date")
  if (!all(required_cols %in% names(txt_data))) {
    stop("MCD14ML input is missing one or more required columns.\n")
  }

  if (!is.null(date)) {
    date <- if (length(date) == 1) rep(date, 2) else date

    amadeus::is_date_proper(instr = date)
    txt_data$acq_date <- as.Date(txt_data$acq_date)
    txt_data <- txt_data[
      txt_data$acq_date >= as.Date(date[1]) &
        txt_data$acq_date <= as.Date(date[2])
    ]
  } else {
    txt_data$acq_date <- as.Date(txt_data$acq_date)
  }

  if (!"frp" %in% names(txt_data)) {
    txt_data$frp <- NA_real_
  }
  txt_data$frp <- as.numeric(txt_data$frp)
  txt_data$fire_count <- 1L
  txt_data$time <- as.integer(format(txt_data$acq_date, "%Y%m%d"))

  txt_vect <- terra::vect(
    as.data.frame(txt_data),
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326",
    keepgeom = TRUE
  )

  if (!is.null(extent)) {
    txt_vect <- apply_extent(txt_vect, extent)
  }

  return(txt_vect)
}

################################################################################
# nolint start
#' Process Black Marble corners
#' @description
#' Tile corner generator for Black Marble products.
#' @param hrange integer(2). Both should be in 0-35.
#' @param vrange integer(2). Both should be in 0-17.
#' @description Black Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a `data.frame` of corner coordinates for assignment.
#' @return `data.frame` with xmin, xmax, ymin, and ymax fields
#' @author Insang Song
#' @keywords internal
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @examples
#' process_blackmarble_corners(hrange = c(1, 2), vrange = c(1, 2))
#' @export
# nolint end
process_blackmarble_corners <-
  function(
    hrange = c(5, 11),
    vrange = c(3, 6)
  ) {
    # should be in range
    if (!all(hrange %in% seq(0, 35)) || !all(vrange %in% seq(0, 17))) {
      stop("hrange or vrange are out of range.")
    }
    # in case range is put in reverse order
    hrange <- sort(hrange)
    vrange <- sort(vrange)

    hseq <- seq(hrange[1], hrange[2])
    vseq <- seq(vrange[1], vrange[2])

    tile_df <-
      expand.grid(
        vaddr = sprintf("v%02d", vseq),
        haddr = sprintf("h%02d", hseq)
      )
    hrangec <- -180 + (hseq * 10L)
    vrangec <- 90 - (vseq * 10L)

    hlen <- abs(diff(hrange)) + 1
    vlen <- abs(diff(vrange)) + 1

    tile_df$tile <- paste0(tile_df$haddr, tile_df$vaddr)
    tile_df <- data.frame(tile = tile_df$tile)
    #return(tile_df)
    tile_df$xmin <- rep(hrangec, each = vlen)
    tile_df$xmax <- tile_df$xmin + 10L
    tile_df$ymin <- rep(vrangec, hlen) - 10L
    tile_df$ymax <- tile_df$ymin + 10L

    return(tile_df)
  }

################################################################################
# nolint start
#' Assign VIIRS Black Marble products corner coordinates to retrieve a merged raster
#' @description This function will return a `SpatRaster` object with
#' georeferenced h5 files of Black Marble product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param path character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [`process_blackmarble_corners`] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs character(1). terra::crs compatible CRS.
#' Default is `"EPSG:4326"`
#' @param ... For internal use.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @seealso
#' * [`terra::describe`]
#' * [`terra::merge`]
#' * [`process_blackmarble_corners`]
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' vnp46a2 <- process_blackmarble(
#'   path =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   date = "2024-01-01",
#'   tile_df =
#'     process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
#'   subdataset = 3L,
#'   crs = "EPSG:4326"
#' )
#' }
#' @export
# previously modis_preprocess_vnp46
# nolint end
process_blackmarble <- function(
  path = NULL,
  date = NULL,
  tile_df = process_blackmarble_corners(),
  subdataset = 3L,
  crs = "EPSG:4326",
  ...
) {
  amadeus::is_date_proper(instr = date)
  # interpret date from paths
  datejul <- strftime(as.Date(date), format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(
      paste0(
        "(",
        paste(stdtile, collapse = "|"),
        ")"
      ),
      filepaths_today,
      value = TRUE
    )

  filepaths_today_tiles <-
    regmatches(
      filepaths_today,
      regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today)
    )

  vnp_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))
  # for filenames,
  # assign corner coordinates then merge
  # Subdataset 3 is BRDF-corrected nighttime light
  vnp_assigned <-
    mapply(
      function(vnp, tile_in) {
        read_vnp <- function(subds) {
          withCallingHandlers(
            terra::rast(vnp, subds = subds),
            warning = function(w) {
              if (grepl("unknown extent", conditionMessage(w), fixed = TRUE)) {
                invokeRestart("muffleWarning")
              }
            }
          )
        }
        vnp_ <- if (length(subdataset) > 1L) {
          do.call(c, lapply(subdataset, read_vnp))
        } else {
          read_vnp(subdataset)
        }
        tile_ext <- tile_df[tile_df$tile == tile_in, -1]

        terra::crs(vnp_) <- terra::crs(crs)
        terra::ext(vnp_) <- unlist(tile_ext)
        vnp_
      },
      vnp_today,
      filepaths_today_tiles_list,
      SIMPLIFY = FALSE
    )
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned[[1]]
  }
  vnp_all[vnp_all == 65535L] <- NaN
  vnp_all[is.nan(vnp_all)] <- NA
  return(vnp_all)
}

################################################################################
#' Warp MODIS swath data into rectilinear grid raster
#' @description Swath data is a type of MODIS data,
#' where curvilinear points are stored with varying resolution depending on
#' the relative position of the sensor axis. As this type of data
#' typically does not work well with planar spatial data, users
#' should warp or rectify this data into a rectilinear raster.
#' Main procedure is done with [`stars::st_warp`], in which users are able to
#' customize the threshold to fill potential gaps that appear where
#' the target resolution is finer than the local resolution of curvilinear
#' grid points.
#' @param path File path of MODIS swath with exact sub-dataset specification.
#' @param cellsize numeric(1). Cell size (spatial resolution) of
#' output rectilinear grid raster.
#' @param threshold numeric(1). Maximum distance to fill gaps if occur.
#' @param crs integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [`terra::crs`] and [`sf::st_crs`] / [EPSG](https://epsg.io/)
#' @param ... For internal use.
#' @note This function handles one file at a time.
#' @return a `stars` object
#' @author Insang Song
#' @seealso [`terra::rectify`]
#' @keywords internal
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_warp <- process_modis_warp(
#'   path = paste0(
#'     "HDF4_EOS:EOS_SWATH:",
#'     list.files(
#'       "./data/mod06l2",
#'       full.names = TRUE,
#'       pattern = ".hdf"
#'     )[1],
#'     ":mod06:Cloud_Fraction"
#'   ),
#'   cellsize = 0.1,
#'   threshold = 0.4,
#'   crs = 4326
#' )
#' }
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path = NULL,
    cellsize = 0.1,
    threshold = cellsize * 4,
    crs = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs,
        segments = 500,
        cellsize = cellsize,
        threshold = threshold
      )
    return(rtd)
  }

################################################################################
# nolint start
#' Mosaic MODIS swaths
#' @description This function will return a `SpatRaster` object with
#' values of selected subdatasets. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large `SpatRaster` object.
#' Users need to select a subdataset to process. The full path looks like
#' `"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
#' the full path to the hdf file.
#' @param path character. Full paths of hdf files.
#' @param date character(1). Date to query.
#' @param subdataset character. Subdatasets to process.
#' __Unlike other preprocessing functions, this argument should specify
#' the exact subdataset name.__ For example, when using MOD06_L2 product,
#' one may specify `c("Cloud_Fraction", "Cloud_Optical_Thickness")`,
#' etc. The subdataset names can be found in `terra::describe()` output.
#' @param suffix character(1). Should be formatted `:{product}:`,
#' e.g., `:mod06:`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree (decimal degree in WGS84).
#' @param ... For internal use.
#' @seealso
#' * [`process_modis_warp()`], [`stars::read_stars()`], [`stars::st_warp()`]
#' * [GDAL HDF4 driver documentation](https://gdal.org/en/latest/drivers/raster/hdf4.html)
#' * [`terra::describe()`]: to list the full subdataset list with `sds = TRUE`
#' * [`terra::sprc()`], [`terra::rast()`]
#' @return
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a single file with
#' full specification of subdataset.
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a list of files. In this case, the returned object will have the maximal extent of multiple warped layers
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mosaic
#' @importFrom stars st_mosaic
#' @importFrom terra values
#' @importFrom terra sprc
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_swath <- process_modis_swath(
#'   path = list.files(
#'     "./data/mod06l2",
#'     full.names = TRUE,
#'     pattern = ".hdf"
#'   ),
#'   date = "2024-01-01",
#'   subdataset = "Cloud_Fraction",
#'   suffix = ":mod06:",
#'   resolution = 0.05
#' )
#' }
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    path = NULL,
    date = NULL,
    subdataset = NULL,
    suffix = ":mod06:",
    resolution = 0.05,
    ...
  ) {
    # check date format
    amadeus::is_date_proper(instr = date)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", length = length(subdataset))
    datejul <- strftime(date, format = "%Y%j")
    ## FIXME: this part may result in underperformance.
    ##        Find a way to optimize this part.
    paths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaic
    for (element in seq_along(subdataset)) {
      target_text <-
        sprintf("%s%s%s%s", header, paths_today, suffix, subdataset[element])
      # rectified stars objects to SpatRaster
      mod06_element <- split(target_text, target_text) |>
        lapply(process_modis_warp, cellsize = resolution)
      # Remove all NA layers to avoid erroneous values
      mod06_element_nas <-
        sapply(
          mod06_element,
          function(x) {
            xvals <- x[[subdataset[element]]]
            all(is.na(xvals)) || all(is.nan(xvals))
          }
        )
      mod06_element <-
        mod06_element[!mod06_element_nas & !is.null(mod06_element_nas)]

      # prepare a fail-safe alternative return
      # It will be used again later.
      alt <- terra::rast(
        xmin = -128,
        xmax = -64,
        ymin = 20,
        ymax = 52,
        resolution = resolution
      )
      # initialize values with NA
      alt[] <- NA
      alt_dim <- dim(alt)
      alt[1, 1] <- 0
      alt[1, alt_dim[2]] <- 0
      alt[alt_dim[1], 1] <- 0
      alt[alt_dim[1], alt_dim[2]] <- 0
      terra::crs(alt) <- "EPSG:4326"

      if (is.null(mod06_element) || length(mod06_element) == 0) {
        message("All layers are NA or NaN.")
        mod06_element_mosaic <- terra::deepcopy(alt)
      } else {
        # mosaick the warped SpatRasters into one
        mod06_element_mosaic <-
          do.call(stars::st_mosaic, mod06_element) |>
          terra::rast()
        # assigning variable name
        mod06_element_mosaic <-
          terra::crop(mod06_element_mosaic, terra::ext(alt))
      }
      names(mod06_element_mosaic) <- subdataset[element]
      ras_mod06[[element]] <- mod06_element_mosaic
    }
    # SpatRasterCollection can accommodate multiple SpatRasters
    # with different extents (most flexible kind)
    mod06_sprc <- terra::sprc(ras_mod06)
    # post-hoc: stack multiple layers with different extent
    # into one SpatRaster
    # 1. mosaic all layers into one
    mod06_mosaic <- terra::mosaic(mod06_sprc, fun = "median")
    # 2. Assign NAs to prepare "etching"; NA will result in NaNs
    terra::values(mod06_mosaic) <- NA
    # 3. Looping main "etching"; each element is put first
    mod06_etched <-
      sapply(mod06_sprc, terra::mosaic, y = mod06_mosaic, fun = "first")
    # 4. stack
    mod06_return <- do.call(c, mod06_etched)
    return(mod06_return)
  }

################################################################################
# Internal helper: derive MODIS subdataset labels without loading raster values
info_modis_subdatasets <- function(path = NULL) {
  sds_desc <- try(terra::describe(path, sds = TRUE), silent = TRUE)
  if (!inherits(sds_desc, "try-error") && nrow(sds_desc) > 0) {
    candidate_col <- if ("var" %in% names(sds_desc)) "var" else "name"
    if (!is.null(candidate_col) && candidate_col %in% names(sds_desc)) {
      sds <- trimws(as.character(sds_desc[[candidate_col]]))
      sds <- sds[!is.na(sds) & nzchar(sds)]
      if (length(sds) > 0L) {
        return(sort(unique(sds)))
      }
    }
  }
  sds_read <- try(terra::rast(path, raw = TRUE), silent = TRUE)
  if (inherits(sds_read, "try-error")) {
    return(character(0))
  }
  sds <- trimws(as.character(names(sds_read)))
  sds <- sds[!is.na(sds) & nzchar(sds)]
  sort(unique(sds))
}

################################################################################
#' Get MODIS product subdataset lookup information
#' @description
#' Returns a lookup table of available MODIS product and subdataset selectors
#' from locally downloaded MODIS/VIIRS-style HDF/H5 files. This helper uses
#' metadata inspection (`terra::describe(..., sds = TRUE)` and layer names) and
#' does not read raster values into memory.
#' @param path character(1+) Path(s) to MODIS file(s) and/or directory(ies)
#'   containing `.hdf`/`.h5` files.
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each product-subdataset row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with MODIS product and subdataset selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_modis_info(path = "./data/modis")
#' get_modis_info(path = "./data/modis", include_file = TRUE)
#' }
#' @importFrom terra describe
#' @importFrom terra rast
#' @export
get_modis_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "\\.(hdf|h5)$",
    source_name = "MODIS HDF/H5"
  )
  out_rows <- lapply(files, function(f) {
    sds <- info_modis_subdatasets(path = f)
    product <- sub("\\..*$", "", basename(f))
    if (length(sds) == 0L || !nzchar(product)) {
      return(NULL)
    }
    data.frame(
      product = rep(product, length(sds)),
      subdataset = sds,
      file = rep(f, length(sds)),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop("No MODIS product-subdataset metadata could be derived from `path`.\n")
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("product", "subdataset"), drop = FALSE])
    out <- out[order(out$product, out$subdataset), , drop = FALSE]
  } else {
    out <- unique(out[, c("product", "subdataset", "file"), drop = FALSE])
    out <- out[order(out$product, out$subdataset, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
################################################################################
