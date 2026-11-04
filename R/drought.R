################################################################################
# {amadeus} functions for SPEI, EDDI, and USDM drought data.
################################################################################

################################################################################
# {download} tier
################################################################################
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

################################################################################
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

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process drought index data
#' @description
#' The \code{process_drought()} function imports and cleans raw drought index
#' files returned by \code{download_drought()}, producing a harmonized output
#' object ready for \code{calculate_drought()}:
#' \itemize{
#'   \item \strong{SPEI / EDDI} — returns a \code{SpatRaster} with one layer
#'     per time step, layer names in \code{"<source>_<timescale>_YYYY-MM-DD"}
#'     format, CRS set to \code{EPSG:4326}.
#'   \item \strong{USDM} — returns a \code{SpatVector} (polygon) with columns
#'     \code{DM} (drought-monitor class, integer 0–4), \code{date}
#'     (\code{Date}), and \code{source} (\code{"usdm"}), CRS
#'     \code{EPSG:4326}.
#' }
# nolint end
#' @param source character(1). Drought data source. One of \code{"spei"},
#'   \code{"eddi"}, or \code{"usdm"}. When called through
#'   \code{process_covariates(covariate = "spei")} the alias is forwarded
#'   automatically.
#' @param path character(1). Directory containing downloaded drought files
#'   (output of \code{download_drought()}).
#' @param date character(1 or 2). Single date or start/end dates.
#'   Format \code{"YYYY-MM-DD"}.
#' @param timescale integer(1). Accumulation timescale in months (SPEI/EDDI
#'   only; ignored for USDM). Must match the timescale used in
#'   \code{download_drought()}. Default \code{1L}.
#' @param extent numeric(4) or \code{SpatExtent}. Optional spatial crop
#'   applied before returning. \code{NULL} (default) returns full extent.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item SPEI/EDDI files are expected to follow the naming convention
#'     produced by \code{download_drought()}: \code{spei<timescale>.nc} and
#'     either legacy \code{eddi<timescale>mn<year>.nc} or current
#'     \code{EDDI_ETrs_<timescale>mn_<YYYYMMDD>.asc}.
#'   \item USDM files are expected to be weekly shapefiles named
#'     \code{USDM_<YYYYMMDD>.shp}.
#'   \item Layer/column naming is standardised so that
#'     \code{calculate_drought()} can operate identically regardless of source.
#' }
#' @author Insang Song
#' @return
#' \itemize{
#'   \item \code{SpatRaster} for SPEI or EDDI sources.
#'   \item \code{SpatVector} (polygons) for USDM source.
#' }
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom terra time
#' @importFrom terra crs
#' @importFrom terra crop
#' @importFrom terra subset
#' @importFrom terra project
#' @importFrom terra nlyr
#' @importFrom terra varnames
#' @seealso \code{\link{download_drought}}, \code{\link{calculate_drought}}
#' @examples
# nolint start
#' \dontrun{
#' ## SPEI
#' spei <- process_drought(
#'   source = "spei",
#'   path = "./data/drought",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L
#' )
#' ## USDM
#' usdm <- process_drought(
#'   source = "usdm",
#'   path = "./data/drought",
#'   date = c("2020-01-07", "2020-03-31")
#' )
#' }
#' @export
# nolint end
process_drought <- function(
  source = c("spei", "eddi", "usdm"),
  path = NULL,
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  extent = NULL,
  ...
) {
  #### Validate source
  source <- match.arg(source)

  #### Sanitize path
  path <- amadeus::download_sanitize_path(path)

  #### Validate dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2L)
  date <- date[order(as.Date(date))]

  #### Check required parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Dispatch to source-specific pathway
  if (source %in% c("spei", "eddi")) {
    drought_process_nc(
      source = source,
      path = path,
      date = date,
      timescale = timescale,
      extent = extent
    )
  } else {
    drought_process_usdm(
      path = path,
      date = date,
      extent = extent
    )
  }
}

################################################################################
#### Internal helper: ensure drought raster CRS
drought_ensure_crs <- function(data_return) {
  data_crs <- terra::crs(data_return)
  if (is.na(data_crs) || data_crs == "") {
    terra::crs(data_return) <- "EPSG:4326"
  }
  data_return
}

################################################################################
#### Internal helper: SPEI / EDDI netCDF pathway
drought_process_nc <- function(source, path, date, timescale, extent) {
  ts_fmt <- sprintf("%02d", as.integer(timescale))
  date_range <- as.Date(date)

  if (source == "spei") {
    #### Single multi-year file: spei<TS>.nc
    nc_pattern <- paste0("^spei", ts_fmt, "\\.nc$")
    nc_files <- list.files(path, pattern = nc_pattern, full.names = TRUE)
    if (length(nc_files) == 0L) {
      stop(sprintf(
        "No SPEI file matching '%s' found in: %s",
        nc_pattern,
        path
      ))
    }
    data_full <- terra::rast(nc_files[1], win = extent)
    data_full <- drought_set_time_nc(data_full, source, ts_fmt, nc_files[1])
  } else {
    #### EDDI: legacy yearly netCDF (eddi01mn2020.nc) or daily/tuesday ASCII
    nc_pattern <- paste0("eddi", ts_fmt, "mn[0-9]{4}\\.nc$")
    asc_pattern <- paste0("^EDDI_ETrs_", ts_fmt, "mn_[0-9]{8}\\.asc$")
    nc_files <- list.files(path, pattern = nc_pattern, full.names = TRUE)
    asc_files <- list.files(path, pattern = asc_pattern, full.names = TRUE)

    if (length(nc_files) == 0L && length(asc_files) == 0L) {
      stop(sprintf(
        "No EDDI files matching '%s' or '%s' found in: %s",
        nc_pattern,
        asc_pattern,
        path
      ))
    }

    if (length(nc_files) > 0L) {
      data_full <- terra::rast()
      for (f in nc_files) {
        yr_rast <- terra::rast(f, win = extent)
        yr_rast <- drought_set_time_nc(yr_rast, source, ts_fmt, f)
        data_full <- c(data_full, yr_rast, warn = FALSE)
      }
    } else {
      asc_dates <- as.Date(
        sub(".*_([0-9]{8})\\.asc$", "\\1", basename(asc_files)),
        format = "%Y%m%d"
      )
      asc_files <- asc_files[order(asc_dates)]
      asc_dates <- asc_dates[order(asc_dates)]

      data_full <- terra::rast()
      for (i in seq_along(asc_files)) {
        asc_rast <- terra::rast(asc_files[i], win = extent)
        terra::time(asc_rast) <- asc_dates[i]
        names(asc_rast) <- paste0(
          source,
          "_",
          ts_fmt,
          "_",
          format(asc_dates[i], "%Y-%m-%d")
        )
        terra::varnames(asc_rast) <- source
        data_full <- c(data_full, asc_rast, warn = FALSE)
      }
    }
  }

  #### Filter layers to requested date range
  all_times <- as.Date(terra::time(data_full))
  keep_idx <- which(all_times >= date_range[1] & all_times <= date_range[2])
  if (length(keep_idx) == 0L) {
    stop(sprintf(
      "No %s data found in date range %s to %s.",
      toupper(source),
      date[1],
      date[2]
    ))
  }
  data_return <- terra::subset(data_full, keep_idx)

  #### Ensure EPSG:4326
  data_return <- drought_ensure_crs(data_return)

  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }

  message(sprintf(
    "Returning %s (timescale = %d month%s) data from %s to %s.\n",
    toupper(source),
    as.integer(timescale),
    if (as.integer(timescale) == 1L) "" else "s",
    date[1],
    date[2]
  ))
  data_return
}

################################################################################
#### Internal helper: assign time metadata and layer names for netCDF rasters
drought_set_time_nc <- function(r, source, ts_fmt, filepath) {
  times <- terra::time(r)
  #### If terra could not read CF time, derive from filename (EDDI only)
  if (is.null(times) || all(is.na(times))) {
    yr_str <- regmatches(
      basename(filepath),
      regexpr("[0-9]{4}", basename(filepath))
    )
    if (length(yr_str) == 0L || is.na(yr_str)) {
      stop(sprintf(
        "Cannot determine time coordinates from file: %s",
        basename(filepath)
      ))
    }
    times <- seq.Date(
      as.Date(paste0(yr_str, "-01-01")),
      by = "month",
      length.out = terra::nlyr(r)
    )
    terra::time(r) <- times
  }
  names(r) <- paste0(
    source,
    "_",
    ts_fmt,
    "_",
    format(as.Date(times), "%Y-%m-%d")
  )
  terra::varnames(r) <- source
  r
}

################################################################################
#### Internal helper: USDM weekly polygon pathway
drought_process_usdm <- function(path, date, extent) {
  search_paths <- unique(c(path, file.path(path, "data_files")))
  shp_files <- unlist(
    lapply(
      search_paths,
      function(dir_path) {
        if (!dir.exists(dir_path)) {
          return(character(0))
        }
        list.files(
          dir_path,
          pattern = "^USDM_[0-9]{8}\\.shp$",
          full.names = TRUE
        )
      }
    ),
    use.names = FALSE
  )

  if (length(shp_files) == 0L) {
    stop(sprintf(
      "No USDM shapefiles matching 'USDM_YYYYMMDD.shp' found in: %s",
      path
    ))
  }

  #### Extract dates from filenames
  file_dates <- as.Date(
    regmatches(
      basename(shp_files),
      regexpr("[0-9]{8}", basename(shp_files))
    ),
    format = "%Y%m%d"
  )

  #### Subset to requested date range
  date_range <- as.Date(date)
  keep_idx <- which(file_dates >= date_range[1] & file_dates <= date_range[2])
  if (length(keep_idx) == 0L) {
    stop(sprintf(
      "No USDM files found in date range %s to %s.",
      date[1],
      date[2]
    ))
  }
  shp_files <- shp_files[keep_idx]
  file_dates <- file_dates[keep_idx]

  #### Read, standardise, and bind
  vlist <- vector("list", length(shp_files))
  for (i in seq_along(shp_files)) {
    v <- terra::vect(shp_files[i])
    #### Reproject to EPSG:4326
    if (!is.na(terra::crs(v)) && terra::crs(v) != "") {
      v <- terra::project(v, "EPSG:4326")
    } else {
      terra::crs(v) <- "EPSG:4326"
    }
    #### Retain only DM column plus metadata
    v <- v[, "DM"]
    v$date <- as.character(file_dates[i])
    v$source <- "usdm"
    vlist[[i]] <- v
  }
  data_return <- do.call(rbind, vlist)

  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }

  message(sprintf(
    "Returning USDM polygon data from %s to %s (%d file%s).\n",
    date[1],
    date[2],
    length(shp_files),
    if (length(shp_files) == 1L) "" else "s"
  ))
  data_return
}
################################################################################
