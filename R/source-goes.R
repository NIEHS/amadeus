################################################################################
# {amadeus} functions for NOAA GOES Aerosol Detection Product (ADP).
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download NOAA GOES ADP data
#' @description
#' Discover and download NOAA GOES-16 or GOES-18 Aerosol Detection Product
#' (ADP) files from the NOAA Open Data Dissemination (NODD) AWS S3 archive.
#' Files are in NetCDF format and contain aerosol detection variables
#' (e.g. `"Smoke"`, `"Dust"`) on the GOES fixed geostationary grid.
#' @note
#' \itemize{
#'   \item GOES data does not require authentication.
#'   \item ADP-C (CONUS) scans are produced approximately every 5 minutes.
#'     A single day may contain several hundred files.
#'   \item GOES ADP files use the GOES fixed geostationary projection. Use
#'     \code{process_goes()} to load and reproject to EPSG:4326.
#' }
#' @param date character(1 or 2). Single date or inclusive range endpoints in
#'   `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.
#' @param satellite character(1). GOES satellite number: `"16"` (default) or
#'   `"18"`. Selects one satellite per call.
#' @param product character(1). ADP scan sector: `"ADP-C"` (CONUS, default),
#'   `"ADP-F"` (Full Disk), or `"ADP-M"` (Mesoscale). Selects one product per
#'   call.
#' @param directory_to_save character(1). Non-empty output directory, created
#'   if needed. Files retain their S3 directory structure below this root;
#'   see Details.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after directory creation and discovery, without downloading data files.
#'   Discovery still queries S3 listings in preview mode. Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   Default is `FALSE`.
#' @param show_progress logical(1). Show per-file download progress. Default
#'   is `TRUE`; discovery and summary messages may still be printed when
#'   `FALSE`.
#' @param hash logical(1). Return the combined directory MD5 hash when `TRUE`.
#'   Default is `FALSE`. Ignored when `download = FALSE`.
#' @param max_tries integer(1). Maximum attempts per listing or file request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Non-negative request-throttling interval in
#'   seconds, passed to discovery and downloads. Default is 2. Discovery uses
#'   a capacity-one bucket; 0 disables discovery throttling. File downloads
#'   follow the throttling policy in [download_run_method()].
#' @details
#' `satellite` and `product` are matched with [base::match.arg()], allowing
#' case-sensitive, unambiguous partial matches. Omitted arguments select the
#' first listed choice. Products `"ADP-C"`, `"ADP-F"`, and `"ADP-M"` map to
#' S3 prefixes `"ABI-L2-ADPC"`, `"ABI-L2-ADPF"`, and `"ABI-L2-ADPM"`.
#'
#' Daily listings supply the actual file names. Files are saved under
#' `directory_to_save/<product-prefix>/<year>/<day-of-year>/<hour>/<filename>`.
#' The day of year uses three digits, such as `"001"`. Existing non-empty
#' destination files are skipped by [download_run_method()]; preview results
#' include them because discovery does not filter local files.
#'
#' Discovery requests one S3 listing page of up to 1000 keys per day and keeps
#' keys ending in `.nc`. Continuation tokens are not followed, so results can
#' be incomplete when a daily listing spans multiple pages.
#'
#' A successfully retrieved listing with no matching files emits a message
#' and contributes no rows. Errors while requesting, parsing, or processing
#' any listing stop discovery before downloading. Files are not required for
#' every requested date. If all listings are empty, `download = TRUE` errors
#' with `"No URLs provided for download."`, whereas `download = FALSE` returns
#' an empty preview.
#'
#' Discovery uses a 60-second timeout per request attempt. File downloads use
#' [download_run_method()]'s default timeout of 3600 seconds.
#' @return
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`. Otherwise, with `hash = FALSE`, the invisible
#' summary from [download_run_method()], including `success`, `failed`, and
#' `skipped` counts. With `hash = TRUE`, a visible combined MD5 hash for all
#' files under `directory_to_save`, including previously downloaded files.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @author Mitchell Manware
#' @seealso [download_data()], [process_goes()], [download_run_method()],
#'   [download_preview()]
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
  satellite = c("16", "18"),
  product = c("ADP-C", "ADP-F", "ADP-M"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  download_check(
    dataset_name = "goes",
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = FALSE,
    remove_zip = FALSE,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  chr_satellite <- match.arg(satellite)
  chr_product <- match.arg(product)
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- sequence_dates(date, hyphen = FALSE)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_dir_to_save <- download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Define prefix based on product code.
  chr_prefix <- switch(
    chr_product,
    "ADP-C" = "ABI-L2-ADPC",
    "ADP-F" = "ABI-L2-ADPF",
    "ADP-M" = "ABI-L2-ADPM"
  )
  # ----------------------------------------------------------------------------
  # Define S3 bucket base URL.
  chr_bucket <- paste0("https://noaa-goes", chr_satellite, ".s3.amazonaws.com/")
  # ----------------------------------------------------------------------------
  # Generate one listing row per date.
  df_bucket <- expand.grid(
    base = chr_bucket,
    product = chr_product,
    date = chr_dates,
    dir = chr_dir_to_save,
    stringsAsFactors = FALSE
  )
  df_bucket$year <- substr(df_bucket$date, 1, 4)
  df_bucket$doy <- format(as.Date(df_bucket$date, format = "%Y%m%d"), "%j")
  df_bucket$prefix <- paste0(
    chr_prefix,
    "/",
    df_bucket$year,
    "/",
    df_bucket$doy,
    "/"
  )
  # ----------------------------------------------------------------------------
  # Discover actual files and construct the download table.
  df_download <- discover_goes(
    df = df_bucket,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
#' Discover GOES files from daily S3 listings
#' @description
#' Read the S3 listings described by a daily request table and return one row
#' per discovered NetCDF file, with its download URL and local destination.
#' This helper retrieves listing metadata; it creates no directories and
#' downloads no data files.
#' @param df data.frame. Non-empty listing table prepared by [download_goes()],
#'   with one row per listing request and the following character columns:
#'
#'   * `base`: S3 bucket URL ending in a forward slash.
#'   * `product`: Product code retained as metadata and used in messages.
#'   * `date`: Date label retained as metadata and used in messages.
#'   * `prefix`: S3 key prefix to query, such as `ABI-L2-ADPC/2024/001/`.
#'   * `dir`: Sanitized local destination root ending in a forward slash.
#'
#'   Additional columns, such as `year` and `doy`, are retained. The caller
#'   supplies validated inputs; this helper uses `prefix` directly and does
#'   not parse `date` or derive a prefix from it.
#' @param max_tries integer(1). Maximum attempts per listing request, including
#'   the initial attempt. Default is 20. Passed to [httr2::req_retry()];
#'   HTTP status codes 429, 503, and 504 are treated as transient.
#' @param rate_limit numeric(1). Non-negative interval in seconds for listing
#'   request throttling. Default is 2; zero disables throttling. Positive
#'   values use a bucket capacity of one and the shared `amadeus:goes` realm
#'   across listing requests and helper calls. Validated by the caller.
#' @param timeout numeric(1). Per-attempt timeout in seconds. Default is 60.
#'   Applies separately to each listing request and retry.
#' @details
#' One S3 ListObjectsV2 page is requested per input row, with `max-keys = 1000`.
#' Continuation tokens are not followed and truncated listings are not
#' detected, so results may be incomplete when a listing spans multiple pages.
#'
#' Listings are parsed as XML with namespaces removed. Keys under
#' `Contents/Key` are retained only when they end in lowercase `.nc`.
#' Each retained key receives a copy of its input row's metadata. The complete
#' key is appended to `base` and `dir`, preserving the S3 directory structure
#' in local destination paths. Results follow input-row and listing order;
#' duplicate keys are not removed and existing local files are not checked.
#'
#' A listing with no matching keys emits a message and contributes no rows.
#' An error while requesting, parsing, or processing any listing stops
#' discovery with the product, date, and original error in the message.
#' Successfully queried empty days are allowed, so a returned table can
#' contain results for only some requested dates.
#' @return
#' A data.frame retaining the input columns, with one row per discovered `.nc`
#' file and these additional columns:
#'
#' * `file`: File name without the S3 directory path.
#' * `url`: Download URL formed from `base` and the complete S3 key.
#' * `vdir`: Local destination directory ending in a forward slash.
#' * `destfile`: Local file path formed from `dir` and the complete S3 key.
#'
#' For a non-empty input table, returns a zero-row data.frame with the same
#' output columns if every listing is successfully queried but has no matching
#' files. Listing errors stop execution instead of returning a table. Row
#' names are reset.
#' @seealso [download_goes()], [download_run_method()]
#' @keywords internal
#' @noRd
discover_goes <- function(
  df,
  max_tries = 20,
  rate_limit = 2,
  timeout = 60
) {
  # ----------------------------------------------------------------------------
  # Preserve the output columns when a listing is empty or fails.
  df_empty <- cbind(
    df[FALSE, , drop = FALSE],
    data.frame(
      file = character(),
      url = character(),
      vdir = character(),
      destfile = character()
    )
  )
  # ----------------------------------------------------------------------------
  # Read each daily listing.
  list_files <- lapply(seq_len(nrow(df)), function(i) {
    df_day <- df[i, , drop = FALSE]
    tryCatch(
      {
        # ----------------------------------------------------------------------
        # Build the listing request using the parent's request controls.
        req <- httr2::request(df_day$base) |>
          httr2::req_url_query(
            `list-type` = 2,
            prefix = df_day$prefix,
            `max-keys` = 1000
          ) |>
          httr2::req_retry(
            max_tries = max_tries,
            is_transient = function(resp) {
              httr2::resp_status(resp) %in% c(429, 503, 504)
            }
          ) |>
          httr2::req_timeout(timeout)
        # ----------------------------------------------------------------------
        # Apply rate limit.
        if (rate_limit > 0) {
          req <- httr2::req_throttle(
            req,
            capacity = 1L,
            fill_time_s = rate_limit,
            realm = "amadeus:goes"
          )
        }
        # ----------------------------------------------------------------------
        # Parse XML and remove namespaces to simplify element selection.
        xml_listing <- httr2::req_perform(req) |>
          httr2::resp_body_xml() |>
          xml2::xml_ns_strip()
        chr_keys <- xml2::xml_find_all(
          xml_listing,
          ".//Contents/Key"
        ) |>
          xml2::xml_text()
        chr_keys <- chr_keys[endsWith(chr_keys, ".nc")]
        # ----------------------------------------------------------------------
        # Message for empty request.
        if (length(chr_keys) == 0L) {
          message(
            "No GOES files found for ",
            df_day$product,
            " on ",
            df_day$date,
            "."
          )
          return(df_empty)
        }
        # ----------------------------------------------------------------------
        # Repeat this listing's metadata once per discovered file.
        df_files <- df_day[rep(1L, length(chr_keys)), , drop = FALSE]
        # ----------------------------------------------------------------------
        # Construct all paths together, preserving the S3 directory structure.
        df_files$file <- basename(chr_keys)
        df_files$url <- paste0(df_files$base, chr_keys)
        df_files$destfile <- paste0(df_files$dir, chr_keys)
        df_files$vdir <- paste0(dirname(df_files$destfile), "/")
        df_files
      },
      error = function(e) {
        stop(
          "Failed to list GOES files for ",
          df_day$product,
          " on ",
          df_day$date,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
        df_empty
      }
    )
  })
  # ----------------------------------------------------------------------------
  # Combine daily results into one download table.
  df_download <- do.call(rbind, list_files)
  rownames(df_download) <- NULL
  df_download
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
    path <- download_sanitize_path(path)
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
  check_for_null_parameters(mget(ls()))
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
