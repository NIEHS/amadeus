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


