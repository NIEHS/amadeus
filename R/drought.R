# nolint start
#' Download drought index data
#' @description
#' The \code{download_drought()} function downloads drought index data from
#' publicly available sources. Three source datasets are supported:
#' \itemize{
#'   \item \strong{SPEI} (Standardized Precipitation-Evapotranspiration Index):
#'     Multi-year netCDF files by timescale from
#'     \url{https://spei.csic.es}.
#'   \item \strong{EDDI} (Evaporative Demand Drought Index): Weekly raster
#'     files by timescale from
#'     \url{https://www.drought.gov/data-maps-tools/evaporative-demand-drought-index-eddi}.
#'   \item \strong{USDM} (U.S. Drought Monitor): Weekly drought class
#'     shapefiles from
#'     \url{https://droughtmonitor.unl.edu}.
#' }
# nolint end
#' @param source character(1). Drought data source. One of \code{"spei"},
#'   \code{"eddi"}, or \code{"usdm"}.
#' @param date character(1 or 2). Single date or start/end dates.
#'   Format \code{"YYYY-MM-DD"}. For SPEI/EDDI the year component selects
#'   the annual file(s); for USDM the full date is used to select weekly
#'   release(s).
#' @param timescale integer(1). Accumulation timescale in months (SPEI/EDDI
#'   only; ignored for USDM). Typical values are 1, 3, 6, 12, 24, 48.
#'   Default is \code{1L}.
#' @param directory_to_save character(1). Directory to save downloaded data.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param hash logical(1). Return \code{rlang::hash_file()} hash of
#'   downloaded files. Default \code{FALSE}.
#' @param show_progress logical(1). Show download progress bar.
#'   Default \code{TRUE}.
#' @param max_tries integer(1). Maximum retry attempts. Default \code{3L}.
#' @param rate_limit numeric(1). Minimum seconds between HTTP requests.
#'   Default \code{2}.
#' @param unzip logical(1). Unzip downloaded zip archives (USDM only).
#'   Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip archives after unzipping
#'   (USDM only). Default \code{FALSE}.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item SPEI and EDDI are raster products; USDM is a polygon
#'     product (shapefile). Their \code{process_drought()} and
#'     \code{calculate_drought()} handling differ accordingly.
#'   \item No authentication is required for any of these sources.
#' }
#' @author Insang Song
#' @return \code{invisible(NULL)} when \code{hash = FALSE}; a character hash
#'   string when \code{hash = TRUE}.
#' @seealso \code{\link{process_drought}}, \code{\link{calculate_drought}}
#' @examples
#' \dontrun{
#' download_drought(
#'   source = "spei",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L,
#'   directory_to_save = "./data/drought",
#'   acknowledgement = TRUE
#' )
#' download_drought(
#'   source = "usdm",
#'   date = c("2020-01-07", "2020-03-31"),
#'   directory_to_save = "./data/drought",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_drought <- function(
  source = c("spei", "eddi", "usdm"),
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  ...
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Validate source
  source <- match.arg(source)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Validate date
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2L)
  date <- date[order(as.Date(date))]

  #### Validate unzip/remove_zip
  if (!unzip && remove_zip) {
    stop(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    )
  }

  #### Validate timescale (SPEI/EDDI only)
  timescale <- as.integer(timescale)
  if (source %in% c("spei", "eddi")) {
    if (is.na(timescale) || timescale < 1L) {
      stop("`timescale` must be a positive integer (months).\n")
    }
  }

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Source-specific download logic

  if (source == "spei") {
    # SPEI: one global multi-year netCDF per timescale
    # nolint start
    ts_str <- sprintf("%02d", timescale)
    spei_file <- paste0("spei", ts_str, ".nc")
    spei_url_candidates <- c(
      paste0("https://spei.csic.es/spei_database_2_11/nc/", spei_file),
      paste0("https://spei.csic.es/spei_database_2_10/nc/", spei_file),
      paste0("https://spei.csic.es/files/", spei_file)
    )
    # nolint end
    destfile <- paste0(directory_to_save, "spei", ts_str, ".nc")

    download_spei <- function(url) {
      amadeus::download_run_method(
        urls = url,
        destfiles = destfile,
        token = NULL,
        show_progress = show_progress,
        max_tries = max_tries,
        rate_limit = rate_limit
      )
    }

    if (!amadeus::check_destfile(destfile)) {
      message("SPEI file already exists. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_save))
      }
      return(invisible(list(success = 0, failed = 0, skipped = 1)))
    }

    url <- NULL
    for (candidate_url in spei_url_candidates) {
      if (amadeus::check_url_status(candidate_url)) {
        url <- candidate_url
        break
      }
    }
    candidate_order <- if (is.null(url)) {
      spei_url_candidates
    } else {
      c(url, setdiff(spei_url_candidates, url))
    }
    last_error <- NULL
    download_result <- NULL

    for (candidate_url in candidate_order) {
      attempt <- tryCatch(
        download_spei(candidate_url),
        error = function(e) e
      )
      if (inherits(attempt, "error")) {
        last_error <- conditionMessage(attempt)
        next
      }

      success_n <- if (!is.null(attempt$success)) attempt$success else 0
      failed_n <- if (!is.null(attempt$failed)) attempt$failed else 0
      if (failed_n == 0 && success_n > 0) {
        download_result <- attempt
        break
      }

      last_error <- sprintf(
        "%s file(s) failed to download from %s",
        failed_n,
        candidate_url
      )
    }

    if (is.null(download_result)) {
      stop(sprintf(
        paste0(
          "SPEI timescale %s could not be downloaded. Check upstream ",
          "availability and `timescale` parameter.\nLast error: %s\n"
        ),
        ts_str,
        if (is.null(last_error)) "none" else last_error
      ))
    }

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    }
    return(invisible(download_result))
  }

  if (source == "eddi") {
    # EDDI: weekly CONUS rasters by timescale, organised in year folders
    # nolint start
    ts_str <- sprintf("%02d", timescale)
    eddi_base <- "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data"
    eddi_ext <- "asc"
    # nolint end

    week_dates <- amadeus::drought_weekly_dates(date[1], date[2])
    if (length(week_dates) == 0) {
      stop(
        "No Tuesday dates found in the specified date range. ",
        "EDDI is a weekly (Tuesday) product.\n"
      )
    }

    all_urls <- character(0)
    all_destfiles <- character(0)

    for (wd in week_dates) {
      year_str <- substr(wd, 1, 4)
      url <- sprintf(
        "%s/%s/EDDI_ETrs_%smn_%s.%s",
        eddi_base,
        year_str,
        ts_str,
        wd,
        eddi_ext
      )
      destfile <- sprintf(
        "%sEDDI_ETrs_%smn_%s.%s",
        directory_to_save,
        ts_str,
        wd,
        eddi_ext
      )
      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
    }

    if (length(all_urls) == 0L) {
      message("All EDDI files already exist. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_save))
      }
      return(invisible(list(
        success = 0,
        failed = 0,
        skipped = length(week_dates)
      )))
    }

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
    }
    return(invisible(download_result))
  }

  if (source == "usdm") {
    # USDM: weekly CONUS drought-class shapefiles (zip archives, Tuesdays)
    directories <- amadeus::download_setup_dir(directory_to_save, zip = TRUE)
    directory_to_download <- directories[1]
    directory_to_unzip <- directories[2]

    # nolint start
    base <- "https://droughtmonitor.unl.edu/data/shapefiles_m"
    # nolint end

    week_dates <- amadeus::drought_weekly_dates(date[1], date[2])
    if (length(week_dates) == 0) {
      stop(
        "No Tuesday dates found in the specified date range. ",
        "USDM is a weekly (Tuesday) product.\n"
      )
    }

    all_urls <- character(0)
    all_destfiles <- character(0)

    for (wd in week_dates) {
      url <- sprintf("%s/USDM_%s_M.zip", base, wd)
      destfile <- sprintf("%sUSDM_%s_M.zip", directory_to_download, wd)
      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
    }

    if (length(all_urls) == 0L) {
      message("All USDM files already exist. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_unzip))
      }
      return(invisible(list(
        success = 0,
        failed = 0,
        skipped = length(week_dates)
      )))
    }

    if (length(all_urls) > 0 && !amadeus::check_url_status(all_urls[1])) {
      stop(
        "First USDM URL returned HTTP 404. ",
        "Check `date` parameter.\n"
      )
    }

    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )

    for (d in seq_along(all_destfiles)) {
      amadeus::download_unzip(
        file_name = all_destfiles[d],
        directory_to_unzip = directory_to_unzip,
        unzip = unzip
      )
    }

    amadeus::download_remove_zips(
      remove = remove_zip,
      download_name = all_destfiles
    )

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_unzip))
    }
    return(invisible(download_result))
  }
}

#' Generate weekly Tuesday dates for drought products
#' @description
#' Return a character vector of YYYYMMDD strings for each Tuesday falling
#' within \code{[date_start, date_end]}.  EDDI and USDM are both released on
#' Tuesdays; this helper centralises the logic.
#' @param date_start character(1). Start date, \code{"YYYY-MM-DD"}.
#' @param date_end character(1). End date, \code{"YYYY-MM-DD"}.
#' @return character vector of \code{"YYYYMMDD"} strings (may be length 0).
#' @keywords internal
#' @export
drought_weekly_dates <- function(date_start, date_end) {
  all_dates <- seq(
    as.Date(date_start, format = "%Y-%m-%d"),
    as.Date(date_end, format = "%Y-%m-%d"),
    by = "day"
  )
  tuesdays <- all_dates[weekdays(all_dates) == "Tuesday"]
  format(tuesdays, "%Y%m%d")
}
