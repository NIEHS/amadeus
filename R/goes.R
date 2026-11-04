################################################################################
# {amadeus} functions for NOAA GOES Aerosol Detection Product (ADP).
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download NOAA GOES ADP data
#' @description
#' The \code{download_goes()} function accesses and downloads NOAA GOES-16 or
#' GOES-18 Aerosol Detection Product (ADP) files from the
#' NOAA Open Data Dissemination (NODD) AWS S3 bucket. Files are in NetCDF
#' format and contain aerosol detection variables (e.g. \code{"Smoke"},
#' \code{"Dust"}) on the GOES fixed geostationary grid.
#' @note
#' \itemize{
#'   \item GOES data does not require authentication.
#'   \item GOES-16 (East) covers the Americas; GOES-18 (West) covers the
#'     western hemisphere and Pacific.
#'   \item ADP-C (CONUS) scans are produced approximately every 5 minutes.
#'     A single day may contain several hundred files.
#'   \item GOES ADP files use the GOES fixed geostationary projection. Use
#'     \code{process_goes()} to load and reproject to EPSG:4326.
#' }
#' @param date character(1 or 2). Date (YYYY-MM-DD) or start and end dates.
#' @param satellite character(1). GOES satellite number: \code{"16"} (East,
#'   default) or \code{"18"} (West).
#' @param product character(1). ADP scan sector: \code{"ADP-C"} (CONUS,
#'   default), \code{"ADP-F"} (Full Disk), or \code{"ADP-M"} (Mesoscale).
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default \code{TRUE}).
#' @param hash logical(1). Return hash of downloaded files (default \code{FALSE}).
#' @param max_tries integer(1). Maximum retry attempts (default 20).
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2).
#' @author Mitchell Manware
#' @return invisible list with download results; or hash character if
#'   \code{hash = TRUE}
#' @importFrom httr2 request
#' @importFrom httr2 req_retry
#' @importFrom httr2 req_timeout
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_string
#' @importFrom httr2 resp_status
#' @examples
#' \dontrun{
#' download_goes(
#'   date = "2024-01-01",
#'   satellite = "16",
#'   product = "ADP-C",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_goes <- function(
  date = c("2024-01-01", "2024-01-01"),
  satellite = "16",
  product = "ADP-C",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

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

  #### Validate satellite
  satellite <- as.character(satellite)
  if (!satellite %in% c("16", "18")) {
    stop("satellite must be '16' or '18'.\n")
  }

  #### Validate product
  product <- toupper(product)
  valid_products <- c("ADP-C", "ADP-F", "ADP-M")
  if (!product %in% valid_products) {
    stop(paste0(
      "product must be one of: ",
      paste(valid_products, collapse = ", "),
      ".\n"
    ))
  }
  product_prefix <- switch(
    product,
    "ADP-C" = "ABI-L2-ADPC",
    "ADP-F" = "ABI-L2-ADPF",
    "ADP-M" = "ABI-L2-ADPM"
  )

  #### Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  # nolint start
  #### S3 bucket base URL (NOAA NODD public bucket, no auth required)
  bucket_url <- paste0("https://noaa-goes", satellite, ".s3.amazonaws.com/")
  # nolint end

  #### Iterate over each day and list available files from S3
  dates_seq <- seq(as.Date(date[1]), as.Date(date[2]), by = "day")
  all_urls <- character()
  all_destfiles <- character()

  for (d in seq_along(dates_seq)) {
    d_date <- dates_seq[d]
    year <- format(d_date, "%Y")
    doy <- sprintf("%03d", as.integer(format(d_date, "%j")))
    prefix <- paste0(product_prefix, "/", year, "/", doy, "/")

    # nolint start
    list_url <- paste0(
      bucket_url,
      "?list-type=2&prefix=",
      prefix,
      "&max-keys=1000"
    )
    # nolint end

    tryCatch(
      {
        resp <- httr2::request(list_url) |>
          httr2::req_retry(
            max_tries = 3,
            is_transient = function(r) {
              httr2::resp_status(r) %in% c(429, 503, 504)
            }
          ) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()

        xml_body <- httr2::resp_body_string(resp)
        key_matches <- gregexpr("<Key>[^<]+\\.nc</Key>", xml_body)
        keys <- regmatches(xml_body, key_matches)[[1]]
        keys <- gsub("<Key>|</Key>", "", keys)

        if (length(keys) > 0) {
          for (k in seq_along(keys)) {
            key <- keys[k]
            file_url <- paste0(bucket_url, key)
            destfile <- paste0(directory_to_save, key)
            all_urls <- c(all_urls, file_url)
            all_destfiles <- c(all_destfiles, destfile)
          }
        } else {
          message(sprintf(
            "No files found for %s on %s (DOY %s).\n",
            product,
            format(d_date, "%Y-%m-%d"),
            doy
          ))
        }
      },
      error = function(e) {
        warning(
          sprintf(
            "Failed to list GOES files for %s/%s/%s: %s\n",
            product,
            year,
            doy,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }

  if (length(all_urls) == 0) {
    message("No GOES ADP files found for the specified parameters.\n")
    return(invisible(list(success = 0, failed = 0, skipped = 0)))
  }

  #### Exit early if download=FALSE (deprecated path)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available.\n",
      length(all_urls)
    ))
    return(invisible(list(
      urls = all_urls,
      destfiles = all_destfiles,
      n_files = length(all_urls)
    )))
  }

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process NOAA GOES ADP data
#' @description
#' The \code{process_goes()} function imports and cleans NOAA GOES-16/18
#' Aerosol Detection Product (ADP) NetCDF files downloaded by
#' \code{download_goes()}, returning a single \code{SpatRaster} object with
#' CRS \code{EPSG:4326}.
#' @param date character(1 or 2). Date (YYYY-MM-DD) or start and end dates.
#' @param variable character(1). Variable name to extract: \code{"Smoke"}
#'   or \code{"Dust"}.
#' @param path character(1+). Directory with downloaded GOES ADP NetCDF files
#'   or a vector of full NetCDF file paths.
#' @param extent numeric(4) or SpatExtent. Crop extent
#'   (\code{xmin, xmax, ymin, ymax} in EPSG:4326). Default \code{NULL} loads
#'   the full raster.
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves original sub-daily layers.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"` or `"sum"`). Ignored when `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' \itemize{
#'   \item Layer names follow the convention
#'     \code{{variable}_{YYYYMMDD}_{HHMMSS}} when `daily_agg = FALSE`, e.g.
#'     \code{"Smoke_20240101_000000"}. With `daily_agg = TRUE`, layer names
#'     contain \code{{variable}_{YYYYMMDD}} and `terra::time()` is set to
#'     midnight UTC.
#'   \item \code{terra::time()} is set to POSIXct UTC for each layer.
#'   \item Files with GOES geostationary projection are reprojected to
#'     EPSG:4326.
#' }
#' @author Mitchell Manware
#' @return a \code{SpatRaster} object
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra crop
#' @importFrom terra subset
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
#' ##       data files.
#' \dontrun{
#' goes <- process_goes(
#'   date = c("2024-01-01", "2024-01-01"),
#'   variable = "Smoke",
#'   path = "./data/goes/"
#' )
#' goes_daily <- process_goes(
#'   date = c("2024-01-01", "2024-01-01"),
#'   variable = "Smoke",
#'   path = "./data/goes/",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
# nolint end
process_goes <- function(
  date = c("2024-01-01", "2024-01-01"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  daily_agg = FALSE,
  fun = "mean",
  ...
) {
  #### resolve file paths from directory or explicit vector
  if (length(path) == 1 && dir.exists(path)) {
    path <- amadeus::download_sanitize_path(path)
    paths <- list.files(
      path,
      pattern = "^OR_(ADP|ABI-L2-ADP)",
      full.names = TRUE,
      recursive = TRUE
    )
  } else {
    paths <- path
  }
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### identify file paths matching GOES ADP naming convention
  paths <- paths[grepl("^OR_(ADP|ABI-L2-ADP).*\\.nc$", basename(paths))]
  if (length(paths) == 0) {
    stop(
      paste0(
        "No GOES ADP NetCDF files found in `path`.\n",
        "When `path` is a directory, files must match ",
        "'^OR_(ADP|ABI-L2-ADP).*\\.nc$'.\n",
        "When `path` is a vector, provide full paths to matching GOES ADP ",
        "NetCDF files.\n"
      )
    )
  }
  if (!all(file.exists(paths))) {
    missing_paths <- paths[!file.exists(paths)]
    stop(
      paste0(
        "Some GOES paths do not exist:\n",
        paste0("  - ", missing_paths, collapse = "\n"),
        "\nFiles must match pattern '^OR_(ADP|ABI-L2-ADP).*\\.nc$'.\n"
      )
    )
  }
  #### parse start datetime from each filename and filter to date range
  date_from <- as.Date(date[1])
  date_to <- as.Date(date[2])
  file_dates <- vapply(
    paths,
    function(p) {
      tryCatch(
        {
          as.Date(goes_parse_start_datetime(p))
        },
        error = function(e) {
          as.Date(NA)
        }
      )
    },
    FUN.VALUE = as.Date(NA)
  )
  mask <- !is.na(file_dates) &
    file_dates >= date_from &
    file_dates <= date_to
  data_paths <- paths[mask]
  data_paths <- data_paths[order(file_dates[mask], data_paths)]
  if (length(data_paths) == 0) {
    stop(paste0(
      "No GOES ADP files matching the requested date range were found.\n",
      "Date range: ",
      date[1],
      " to ",
      date[2],
      "\n"
    ))
  }
  #### vectorized fast path for homogeneous GOES granules
  data_first <- terra::rast(data_paths[1])
  var_idx <- grep(
    paste0("^", variable, "$"),
    names(data_first)
  )
  if (length(var_idx) == 0) {
    var_idx <- grep(variable, names(data_first))
  }
  if (length(var_idx) == 0) {
    stop(paste0(
      "Requested variable '",
      variable,
      "' was not found in ",
      basename(data_paths[1]),
      ".\n"
    ))
  }
  dt_chr <- vapply(
    data_paths,
    function(p) format(goes_parse_start_datetime(p), "%Y-%m-%d %H:%M:%S"),
    character(1)
  )
  dt_vec <- as.POSIXct(dt_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  message(paste0(
    "Cleaning ",
    variable,
    " data for ",
    length(data_paths),
    " GOES files...\n"
  ))
  data_raw <- terra::rast(data_paths)
  n_files <- length(data_paths)
  n_total_layers <- terra::nlyr(data_raw)
  if (n_total_layers %% n_files != 0) {
    stop(
      paste0(
        "GOES files are not structurally homogeneous; ",
        "unable to map layers to files.\n"
      )
    )
  }
  layers_per_file <- n_total_layers / n_files
  if (max(var_idx) > layers_per_file) {
    stop(
      paste0(
        "GOES variable layer index exceeds per-file layer count; ",
        "files appear inconsistent.\n"
      )
    )
  }
  selected_idx <- unlist(lapply(
    seq_len(n_files),
    function(i) var_idx + (i - 1) * layers_per_file
  ))
  data_return <- terra::subset(data_raw, subset = selected_idx)
  #### reproject to EPSG:4326 if file uses geostationary projection
  crs_proj <- terra::crs(data_return, proj = TRUE)
  if (!is.na(crs_proj) && grepl("\\+proj=geos", crs_proj)) {
    # nocov start
    data_return <- terra::project(data_return, "EPSG:4326")
  } else if (is.na(terra::crs(data_return)) || terra::crs(data_return) == "") {
    terra::crs(data_return) <- "EPSG:4326"
  } # nocov end
  #### crop to extent (applied after reprojection)
  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }
  #### set layer name: {variable}_{YYYYMMDD}_{HHMMSS}
  dt_layer <- rep(dt_vec, each = length(var_idx))
  names(data_return) <- paste0(
    variable,
    "_",
    format(dt_layer, "%Y%m%d"),
    "_",
    format(dt_layer, "%H%M%S")
  )
  #### set time
  terra::time(data_return) <- dt_layer
  #### ensure EPSG:4326
  terra::crs(data_return) <- "EPSG:4326"
  #### optional daily aggregation
  if (isTRUE(daily_agg)) {
    t <- terra::time(data_return)
    if (!anyNA(t) && length(t) == terra::nlyr(data_return)) {
      date_str <- format(as.Date(t), "%Y%m%d")
    } else {
      # nocov start
      date_str <- regmatches(
        names(data_return),
        regexpr(
          "(?<![0-9])[0-9]{8}(?![0-9])",
          names(data_return),
          perl = TRUE
        )
      )
      if (length(date_str) != terra::nlyr(data_return)) {
        stop("daily_agg: cannot determine dates from layer times or names.\n")
      }
    } # nocov end
    var_prefix <- sub("_[0-9]{8}_[0-9]{6}$", "", names(data_return))
    tapp_index <- paste(var_prefix, date_str, sep = "_")
    saved_crs <- terra::crs(data_return)
    if (length(unique(tapp_index)) == 1L) {
      out_name <- unique(tapp_index)
      data_return <- terra::app(data_return, fun = fun, na.rm = TRUE)
      names(data_return) <- out_name
    } else {
      data_return <- terra::tapp(
        data_return,
        tapp_index,
        fun = fun,
        na.rm = TRUE
      )
    }
    terra::crs(data_return) <- saved_crs
    out_dates <- regmatches(
      names(data_return),
      regexpr("[0-9]{8}$", names(data_return))
    )
    terra::time(data_return) <- as.POSIXct(
      paste0(out_dates, " 00:00:00"),
      format = "%Y%m%d %H:%M:%S",
      tz = "UTC"
    )
  }
  message(paste0(
    "Returning ",
    ifelse(isTRUE(daily_agg), "daily ", "sub-daily "),
    variable,
    " data from ",
    date[1],
    " to ",
    date[2],
    ".\n"
  ))
  return(data_return)
}

################################################################################
#' Parse GOES start datetime from filename
#' @description
#' Extracts the scan start datetime from a GOES-R series ADP filename.
#' The start timestamp field uses the format \code{sYYYYDDDHHMMSSf}
#' where \code{DDD} is the day of year (1--366).
#' @param path character(1). Full or base file path.
#' @return POSIXct scalar (UTC).
#' @keywords internal auxiliary
#' @export
goes_parse_start_datetime <- function(path) {
  fname <- basename(path)
  m <- regmatches(fname, regexpr("_s([0-9]{14})_", fname))
  if (length(m) == 0 || nchar(m) < 16) {
    stop(paste0(
      "Cannot parse GOES start datetime from filename: ",
      fname,
      "\n"
    ))
  }
  ts <- substr(m, 3, 16)
  year <- as.integer(substr(ts, 1, 4))
  doy <- as.integer(substr(ts, 5, 7))
  hour <- as.integer(substr(ts, 8, 9))
  min <- as.integer(substr(ts, 10, 11))
  sec <- as.integer(substr(ts, 12, 13))
  base_date <- as.Date(
    paste0(year, sprintf("%03d", doy)),
    format = "%Y%j"
  )
  ISOdatetime(
    year = year,
    month = as.integer(format(base_date, "%m")),
    day = as.integer(format(base_date, "%d")),
    hour = hour,
    min = min,
    sec = sec,
    tz = "UTC"
  )
}

################################################################################
# {calculate} tier
################################################################################
# nolint start
#' Calculate NOAA GOES ADP covariates
#' @description
#' Extract NOAA GOES Aerosol Detection Product (ADP) values at point
#' locations from a \code{SpatRaster} returned by \code{process_goes()}.
#' Returns a \code{data.frame} (or \code{SpatVector} / \code{sf}) containing
#' \code{locs_id}, \code{time}, and the extracted variable column
#' (\code{{variable}_{radius}}).
#' @param from SpatRaster(1). Output from \code{process_goes()}.
#' @param locs data.frame, character file path, \code{SpatVector}, or
#'   \code{sf} object with point locations.
#' @param locs_id character(1). Column name for unique location identifier.
#' @param radius integer(1). Circular buffer radius in metres around each
#'   site (default 0 = point extraction).
#' @param fun character(1). Summary function for buffered extractions
#'   (default \code{"mean"}).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom \code{FALSE}/\code{"sf"}/\code{"terra"}. Return geometry with
#'   results. Default \code{FALSE}. The CRS is inherited from \code{from}.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso \code{\link{process_goes}}
#' @author Mitchell Manware
#' @return a \code{data.frame} or \code{SpatVector} object.
#' @importFrom terra crs
#' @importFrom terra nlyr
#' @importFrom terra time
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra extract
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
#' ##       and processed data.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -95.0, lat = 34.5)
#' calculate_goes(
#'   from = goes,  # derived from process_goes() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean"
#' )
#' }
#' @export
# nolint end
calculate_goes <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
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
  sites_extracted <- amadeus::calc_worker(
    dataset = "goes",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = c(2, 3),
    time_type = "hour",
    level = NULL,
    weights = weights,
    ...
  )
  #### optional `.by_time` summarization
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }
  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}
################################################################################
