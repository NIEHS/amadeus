################################################################################
# {amadeus} functions for SPEI, EDDI, and USDM drought data.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download drought index data
#' @description
#' Dispatch to [download_spei()], [download_eddi()], or [download_usdm()].
#' The selected function validates its arguments, constructs download URLs,
#' and handles previews, downloads, optional extraction, and return values.
#' @param source character(1). `"spei"` (default), `"eddi"`, or `"usdm"`.
#'   Uses [base::match.arg()], allowing unambiguous partial matches.
#' @param date character(1 or 2). One date or inclusive range endpoints in
#'   `"YYYY-MM-DD"` format, in either order. EDDI and USDM select Tuesdays
#'   within the interval and stop if none occur. Dates must be between
#'   1950-01-01 and today. Default is `c("2020-01-01", "2020-12-31")`.
#'   Ignored for SPEI, which downloads a complete multi-year file.
#' @param timescale integer(1). Positive accumulation timescale in months,
#'   used by SPEI and EDDI. Default is `1L`. Ignored for USDM. Input validation
#'   does not establish whether the provider has a file for that timescale.
#' @param directory_to_save character(1). Non-empty output directory. SPEI
#'   and EDDI save files directly here. USDM uses `zip_files/` for archives
#'   and `data_files/` for extracted files beneath this directory.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed, including
#'   for previews. Downloads and extracted files may require substantial
#'   storage. Default is `FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of the output directory
#'   instead of the download summary. Uses `directory_to_save` for SPEI/EDDI
#'   and `data_files/` for USDM, including existing files. Default is `FALSE`;
#'   ignored when `download = FALSE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status and summary messages may still be printed when
#'   `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file request, including
#'   the initial attempt. Must be at least 1. Default is `3L`. Preflight
#'   checks use their own request settings.
#' @param rate_limit numeric(1). Finite, non-negative request-throttling
#'   interval in seconds, passed to [download_run_method()]. Default is 2.
#' @param unzip logical(1). Extract USDM archives into `data_files/`.
#'   Default is `TRUE`. Ignored for SPEI and EDDI.
#' @param remove_zip logical(1). Remove USDM archives and their `zip_files/`
#'   directory recursively after extraction, including remaining contents.
#'   Requires `unzip = TRUE` for USDM. Default is `FALSE`. Ignored for SPEI
#'   and EDDI.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a
#'   preview after directory creation, before preflight checks or downloading.
#'   Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   No command file is generated. Default is `FALSE`.
#' @param version character(1). SPEI selection: `"2.11"` (default),
#'   `"2.10"`, or `"legacy"`. The last choice uses the unversioned `/files/`
#'   endpoint; its release is not identified by the URL. Uses
#'   [base::match.arg()]. No automatic fallback between versions occurs.
#'   Ignored for EDDI and USDM.
#' @param ... Additional arguments forwarded to the selected source function.
#'   Its `...` argument is currently reserved and ignored.
#' @details
#' Only arguments relevant to the selected source are forwarded. Common
#' validation occurs once in the source function; this wrapper does not
#' repeat validation, directory creation, or download execution.
#'
#' Previews create the output directories but make no network requests.
#' They include existing destination files. Actual downloads check the first
#' URL before calling [download_run_method()], which skips existing non-empty
#' files and reports individual failures. A failed preflight warns and allows
#' downloading to proceed. See the source functions for endpoint limitations
#' and extraction behavior.
#'
#' SPEI retains the flat `speiNN.nc` filename across versions for compatibility
#' with [process_drought()]. Existing-file checks do not distinguish versions.
#' The `date` argument does not subset SPEI downloads; temporal selection is
#' performed during processing.
#' @note No authentication is required. SPEI and EDDI provide raster inputs;
#' USDM provides polygon inputs for the downstream drought workflow.
#' @author Insang Song
#' @return The selected source function's result, with visibility preserved:
#' * With `download = FALSE`, an invisible list containing `urls`,
#'   `destfiles`, and `n_files`.
#' * With `download = TRUE` and `hash = FALSE`, an invisible download summary
#'   containing `success`, `failed`, `skipped`, `failed_urls`, and
#'   `failed_files`. The two failure vectors are `character(0)` when no
#'   downloads failed, including when every file was skipped.
#' * With `download = TRUE` and `hash = TRUE`, a visible character scalar
#'   containing the combined MD5 hash of the source's output directory.
#'
#' Counts describe file downloads, not extraction or scientific validity.
#' Validation or extraction failures can prevent a normal return.
#' @seealso [download_spei()], [download_eddi()], [download_usdm()],
#'   [process_drought()], [calculate_drought()]
#' @examples
#' \dontrun{
#' download_drought(
#'   source = "spei",
#'   timescale = 1L,
#'   version = "2.11",
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
  download = TRUE,
  remove_command = FALSE,
  version = c("2.11", "2.10", "legacy"),
  ...
) {
  # ----------------------------------------------------------------------------
  # Select the source-specific downloader; it handles argument validation.
  chr_source <- match.arg(source)
  fun_download <- switch(
    chr_source,
    spei = amadeus:::download_spei,
    eddi = amadeus:::download_eddi,
    usdm = amadeus:::download_usdm
  )
  # ----------------------------------------------------------------------------
  # Forward the controls shared by all three sources.
  list_common_args <- list(
    directory_to_save = directory_to_save,
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    hash = hash,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Forward only the selections and archive options used by this source.
  list_source_args <- switch(
    chr_source,
    spei = list(timescale = timescale, version = version),
    eddi = list(date = date, timescale = timescale),
    usdm = list(date = date, unzip = unzip, remove_zip = remove_zip)
  )
  # ----------------------------------------------------------------------------
  # Return the delegated result without changing its visibility.
  do.call(fun_download, c(list_common_args, list_source_args, list(...)))
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
  # ----------------------------------------------------------------------------
  # Generate daily dates within the requested range.
  all_dates <- seq(
    as.Date(date_start, format = "%Y-%m-%d"),
    as.Date(date_end, format = "%Y-%m-%d"),
    by = "day"
  )
  # ----------------------------------------------------------------------------
  # Retain Tuesdays and format them as compact date strings.
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
  path <- amadeus:::download_sanitize_path(path)

  #### Validate dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2L)
  date <- date[order(as.Date(date))]

  #### Check required parameters
  amadeus:::check_for_null_parameters(mget(ls()))

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
# {calculate} tier
################################################################################
# nolint start
#' Calculate drought index covariates
#' @description
#' The \code{calculate_drought()} function extracts drought index values at
#' point locations from an object returned by \code{process_drought()}.
#' Three source datasets are supported:
#' \itemize{
#'   \item \strong{SPEI / EDDI} (\code{SpatRaster}): cell values are
#'     extracted at each location using the standard raster-extraction
#'     pipeline (\code{calc_prepare_locs()} -> \code{calc_worker()} ->
#'     \code{calc_return_locs()}).  Time column format is
#'     \code{"YYYY-MM-DD"}.
#'   \item \strong{USDM} (\code{SpatVector} polygons): the drought monitor
#'     class (\code{DM}, integer 0-4) at each location is determined via
#'     spatial overlay. A \code{time} column of class \code{Date} is
#'     populated from the \code{date} attribute of \code{from}.
#' }
#' When \code{.by_time} is supplied the extracted result is
#' passed through \code{calc_summarize_by()} using the same semantics as
#' all other \code{calculate_*()} functions in this package.
# nolint end
#' @param from SpatRaster or SpatVector. Output of \code{process_drought()}.
#'   \itemize{
#'     \item \code{SpatRaster} for SPEI or EDDI sources.
#'     \item \code{SpatVector} (polygons) for USDM source.
#'   }
#' @param locs data.frame, character (path to CSV), \code{SpatVector}, or
#'   \code{sf} object. Point locations at which to extract values.
#' @param locs_id character(1). Name of the unique location identifier column
#'   in \code{locs}. Default \code{"site_id"}.
#' @param radius integer(1). Circular buffer radius in metres around each
#'   site location used for extraction. For SPEI/EDDI this controls raster
#'   buffering; for USDM, \code{radius > 0} additionally returns class
#'   proportions within the buffer. Default \code{0L}.
#' @param fun character(1). Summary function applied to raster cells within
#'   the buffer (SPEI/EDDI only). Default \code{"mean"}.
#' @param geom \code{FALSE}, \code{"sf"}, or \code{"terra"}. Whether to
#'   attach geometry to the returned object. Default \code{FALSE}.
#' @param .by_time NULL or character(1). Name of the time column to use
#'   temporal summarization unit token. \code{NULL} disables
#'   \code{"time"}.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item The column name for extracted drought values follows the pattern
#'     \code{"<source>_<timescale>_<radius>"} (e.g. \code{"spei_01_0"}) for
#'     SPEI/EDDI, and \code{"usdm_dm_0"} for USDM.
#'   \item For USDM with \code{radius > 0}, proportion columns are added as
#'     \code{"usdm_dm_<class>_<radius>"} for classes 0–4.
#' }
#' @author Insang Song
#' @return A \code{data.frame} (default) or \code{SpatVector}/\code{sf}
#'   object (when \code{geom} is set) with columns:
#'   \describe{
#'     \item{\code{<locs_id>}}{Location identifier.}
#'     \item{\code{time}}{Date of the observation (\code{Date} or
#'       \code{"YYYY-MM-DD"} character).}
#'     \item{\code{<value_column>}}{Extracted drought index or class value.}
#'   }
#'   When \code{.by_time} is non-\code{NULL}, rows are
#'   aggregated to the specified resolution via \code{calc_summarize_by()}.
#' @importFrom terra extract
#' @importFrom terra time
#' @importFrom terra crs
#' @seealso
#' \code{\link{process_drought}}, \code{\link{download_drought}},
#' \code{\link{calc_summarize_by}}
#' @examples
#' \dontrun{
#' locs <- data.frame(site_id = "001", lon = -97.5, lat = 35.5)
#' ## SPEI example
#' spei <- process_drought(
#'   source = "spei",
#'   path = "./data/drought",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L
#' )
#' calculate_drought(
#'   from = spei,
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = 0L,
#'   fun = "mean"
#' )
#' ## USDM example
#' usdm <- process_drought(
#'   source = "usdm",
#'   path = "./data/drought",
#'   date = c("2020-01-07", "2020-03-31")
#' )
#' calculate_drought(
#'   from = usdm,
#'   locs = locs,
#'   locs_id = "site_id"
#' )
#' }
#' @export
calculate_drought <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0L,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  .by_time = NULL,
  ...
) {
  #### Validate .by_time
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)

  #### Dispatch on input type
  if (inherits(from, "SpatRaster")) {
    #### SPEI / EDDI raster extraction pipeline
    sites_list <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    sites_e <- sites_list[[1]]
    sites_id <- sites_list[[2]]

    #### Derive source and timescale from first layer name
    #### (e.g. "spei_01_2020-01-01")
    lyr_parts <- strsplit(names(from)[1], "_")[[1]]
    src_name <- lyr_parts[1]
    ts_fmt <- lyr_parts[2]
    col_name <- paste0(src_name, "_", ts_fmt, "_", radius)
    weighted_drought <- amadeus::calc_prepare_weights(
      from = from[[1]],
      weights = weights
    )
    drought_fun_extract <- amadeus::calc_weighted_fun(
      fun = fun,
      weighted = !is.null(weighted_drought)
    )

    sites_extracted <- NULL
    for (l in seq_len(terra::nlyr(from))) {
      data_layer <- from[[l]]
      data_time <- as.POSIXct(as.Date(terra::time(data_layer)), tz = "UTC")

      if (terra::geomtype(sites_e) == "polygons") {
        extract_args <- list(
          x = data_layer,
          y = sf::st_as_sf(sites_e),
          fun = drought_fun_extract,
          progress = FALSE,
          force_df = TRUE,
          max_cells_in_memory = 1e8
        )
        if (!is.null(weighted_drought)) {
          extract_args$weights <- weighted_drought
        }
        layer_vals <- do.call(exactextractr::exact_extract, extract_args)
      } else {
        if (is.null(weighted_drought)) {
          layer_vals <- terra::extract(
            data_layer,
            sites_e,
            method = "simple",
            ID = FALSE,
            bind = FALSE,
            na.rm = TRUE
          )
        } else {
          weighted_geoms <- amadeus::calc_prepare_exact_geoms(
            locs_vector = sites_e,
            radius = radius
          )
          layer_vals <- exactextractr::exact_extract(
            x = data_layer,
            y = weighted_geoms,
            weights = weighted_drought,
            fun = drought_fun_extract,
            progress = FALSE,
            force_df = TRUE,
            max_cells_in_memory = 1e8
          )
        }
      }

      row_df <- data.frame(
        sites_id,
        time = rep(data_time, nrow(sites_id)),
        val = layer_vals[[1]],
        stringsAsFactors = FALSE
      )
      colnames(row_df) <- c(colnames(sites_id), "time", col_name)
      sites_extracted <- rbind(sites_extracted, row_df)
    }
    crs_from <- terra::crs(from)
  } else if (inherits(from, "SpatVector")) {
    #### USDM polygon overlay and optional buffered class proportions
    use_usdm_buffer <- as.numeric(radius) > 0
    prep_radius <- if (use_usdm_buffer) radius else 0L
    sites_list <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = prep_radius,
      geom = geom
    )
    sites_e <- sites_list[[1]]
    sites_id <- sites_list[[2]]

    col_name <- "usdm_dm_0"
    prop_col_names <- if (use_usdm_buffer) {
      paste0("usdm_dm_", 0:4, "_", radius)
    } else {
      character(0)
    }
    dates_unique <- sort(unique(terra::values(from)$date))

    result_list <- vector("list", length(dates_unique))
    for (i in seq_along(dates_unique)) {
      d <- dates_unique[i]
      from_date <- from[terra::values(from)$date == d, ]
      d_posix <- as.POSIXct(as.Date(d), tz = "UTC")
      dm_values <- rep(NA_real_, nrow(sites_id))

      if (!use_usdm_buffer) {
        extracted <- terra::extract(from_date, sites_e)
        if (!is.null(extracted) && nrow(extracted) > 0L) {
          #### keep first match per site (polygons should not overlap)
          first_per_site <- !duplicated(extracted$id.y)
          dm_values[extracted$id.y[first_per_site]] <-
            extracted$DM[first_per_site]
        }

        row_df <- data.frame(
          sites_id,
          time = rep(d_posix, nrow(sites_id)),
          dm = dm_values,
          stringsAsFactors = FALSE
        )
        colnames(row_df) <- c(colnames(sites_id), "time", col_name)
      } else {
        sites_buffer <- sites_e
        if (terra::geomtype(sites_buffer) != "polygons") {
          sites_buffer <- terra::buffer(sites_buffer, width = radius)
        }
        site_index_col <- ".__site_row__"
        sites_buffer[[site_index_col]] <- seq_len(nrow(sites_buffer))
        sites_buffer <- sites_buffer[, site_index_col, drop = FALSE]

        prop_values <- matrix(
          NA_real_,
          nrow = nrow(sites_id),
          ncol = length(prop_col_names),
          dimnames = list(NULL, prop_col_names)
        )

        intersections <- terra::intersect(sites_buffer, from_date)
        if (!is.null(intersections) && nrow(intersections) > 0L) {
          inter_df <- data.frame(
            site_row = terra::values(intersections)[[site_index_col]],
            dm = terra::values(intersections)[["DM"]],
            area = terra::expanse(intersections, unit = "m"),
            stringsAsFactors = FALSE
          )
          inter_df <- inter_df[
            !(is.na(inter_df$site_row) | is.na(inter_df$dm)),
            ,
            drop = FALSE
          ]

          if (nrow(inter_df) > 0L) {
            site_dm_area <- stats::aggregate(
              area ~ site_row + dm,
              data = inter_df,
              FUN = sum
            )
            site_area <- terra::expanse(sites_buffer, unit = "m")
            by_site <- split(site_dm_area, site_dm_area$site_row)

            for (site_row_chr in names(by_site)) {
              site_row <- as.integer(site_row_chr)
              dm_area <- by_site[[site_row_chr]]
              dm_values[site_row] <- as.numeric(
                dm_area$dm[which.max(dm_area$area)]
              )

              prop_values[site_row, ] <- 0
              denom <- site_area[site_row]
              if (is.finite(denom) && denom > 0) {
                for (j in seq_len(nrow(dm_area))) {
                  dm_class <- as.integer(dm_area$dm[j])
                  if (!is.na(dm_class) && dm_class %in% 0:4) {
                    prop_values[site_row, dm_class + 1L] <-
                      dm_area$area[j] / denom
                  }
                }
              }
            }
          }
        }

        row_df <- data.frame(
          sites_id,
          time = rep(d_posix, nrow(sites_id)),
          dm = dm_values,
          prop_values,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        colnames(row_df)[seq_len(ncol(sites_id))] <- colnames(sites_id)
        colnames(row_df)[ncol(sites_id) + 1L] <- "time"
        colnames(row_df)[ncol(sites_id) + 2L] <- col_name
      }

      result_list[[i]] <- row_df
    }
    sites_extracted <- do.call(rbind, result_list)
    crs_from <- terra::crs(from)
  } else {
    stop("`from` must be a SpatRaster (SPEI/EDDI) or SpatVector (USDM).\n")
  }

  #### Optional .by_time summarization
  did_summarize <- FALSE
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = fun,
      locs_id = locs_id
    )
    did_summarize <- TRUE
  }

  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }

  amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = crs_from
  )
}
################################################################################
