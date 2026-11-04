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

