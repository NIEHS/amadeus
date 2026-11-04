################################################################################
# {amadeus} functions for US EPA Air Data Pre-Generated Data Files (AQS).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download air quality data
#' @description
#' The \code{download_aqs()} function accesses and downloads Air Quality
#' System (AQS) data from the U.S. Environmental Protection Agency's (EPA)
#' Pre-Generated Data Files.
#' @note AQS data does not require authentication.
#' AQS measurements are generally intended for use as dependent variables, so
#' the package supports download and processing for AQS but does not expose
#' AQS through `calculate_covariates()`.
#' @details Common AQS parameter codes include:
#' * `88101` — PM2.5 - Local Conditions
#' * `88502` — Acceptable PM2.5 AQI & Speciation Mass
#' * `81102` — PM10 Total 0-10um STP
#' * `44201` — Ozone
#' * `42602` — Nitrogen dioxide (NO2)
#' * `42401` — Sulfur dioxide (SO2)
#' * `42101` — Carbon monoxide
#'
#' This list is not exhaustive; for the full official table, see the linked EPA
#' AQS parameter code table.
#' @param parameter_code integer(1). EPA pollutant parameter code. See
#'   Details for a short list of common codes.
#' @param resolution_temporal character(1). "daily" or "hourly".
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip zip files (default TRUE).
#' @param remove_zip logical(1). Remove zip files after unzipping (default
#' FALSE).
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @seealso
#'   [EPA AQS Parameter Codes](
#'     https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv
#'   )
#' @references
#' \insertRef{data_usepa2023airdata}{amadeus}
#' @examples
#' \dontrun{
#' download_aqs(
#'   parameter_code = 88101,
#'   resolution_temporal = "daily",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_aqs <- function(
  parameter_code = 88101,
  resolution_temporal = "daily",
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeus:::download_check(
    dataset_name = "aqs",
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
  checkmate::assert_int(parameter_code, lower = 1L)
  checkmate::assert_choice(resolution_temporal, c("daily", "hourly"))
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus::download_setup_dir(
    amadeus::download_sanitize_path(directory_to_save),
    zip = TRUE
  )
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(
    paste(
      "https://aqs.epa.gov/aqsweb/airdata/",
      resolution_temporal,
      "_",
      parameter_code,
      "_%d.zip",
      sep = ""
    ),
    int_years
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- sprintf(
    paste(
      chr_dir_to_download,
      "aqs_",
      resolution_temporal,
      "_",
      parameter_code,
      "_%d.zip",
      sep = ""
    ),
    int_years
  )
  # ----------------------------------------------------------------------------
  # Check first URL for valid generation; error if not.
  if (
    isTRUE(download) &&
      length(chr_urls) > 0 &&
      !amadeus::check_url_status(chr_urls[1])
  ) {
    stop(paste0(
      "Invalid year returns HTTP code 404. ",
      "Check `year` parameter.\n"
    ))
  }
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(chr_urls)
    ))
    return(invisible(list(
      urls = chr_urls,
      destfiles = chr_filenames,
      n_files = length(chr_urls)
    )))
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
  # Unzip downloaded .zip files.
  sapply(
    chr_filenames,
    amadeus::download_unzip,
    directory_to_unzip = chr_dir_to_save,
    unzip = unzip
  )
  # ----------------------------------------------------------------------------
  # Flatten nested AQS CSV output.
  if (isTRUE(unzip)) {
    sapply(
      int_years,
      function(year_i) {
        amadeus:::download_normalize_aqs_unzip(
          directory_to_unzip = chr_dir_to_save,
          resolution_temporal = resolution_temporal,
          parameter_code = parameter_code,
          year = year_i
        )
      }
    )
  }
  # ----------------------------------------------------------------------------
  # Remove zip files.
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = chr_filenames
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
#' Normalize AQS unzip layout
#' @description
#' Move files from an AQS archive's nested year directory into the main
#' `data_files` directory when the archive unpacks into a single folder.
#' @param directory_to_unzip character(1). Directory where files were unzipped.
#' @param resolution_temporal character(1). Temporal resolution used in the
#'   AQS archive name.
#' @param parameter_code integer(1). AQS parameter code used in the archive
#'   name.
#' @param year integer(1). Year represented by the archive.
#' @return NULL; normalizes the AQS data file layout in place
#' @keywords internal
#' @noRd
download_normalize_aqs_unzip <- function(
  directory_to_unzip,
  resolution_temporal,
  parameter_code,
  year
) {
  # ----------------------------------------------------------------------------
  # Locate the nested directory for this AQS year.
  nested_dir <- file.path(
    directory_to_unzip,
    sprintf("%s_%s_%s", resolution_temporal, parameter_code, year)
  )
  # ----------------------------------------------------------------------------
  # Skip years without a nested directory.
  if (!dir.exists(nested_dir)) {
    return(invisible(NULL))
  }
  # ----------------------------------------------------------------------------
  # List the immediate contents of the nested directory.
  nested_files <- list.files(
    nested_dir,
    full.names = TRUE,
    recursive = FALSE
  )
  # ----------------------------------------------------------------------------
  # Leave empty directories or further nested layouts unchanged.
  if (length(nested_files) == 0 || any(dir.exists(nested_files))) {
    return(invisible(NULL))
  }
  # ----------------------------------------------------------------------------
  # Move files into the main data directory.
  for (nested_file in nested_files) {
    target_file <- file.path(directory_to_unzip, basename(nested_file))
    if (!file.exists(target_file)) {
      moved <- file.rename(nested_file, target_file)
      if (!isTRUE(moved)) {
        # nocov start
        stop(sprintf(
          "Failed to normalize AQS unzip layout for %s.",
          basename(nested_file)
        ))
        # nocov end
      }
    } else {
      # Keep the existing destination and remove this nested file.
      unlink(nested_file)
    }
  }
  # ----------------------------------------------------------------------------
  # Remove the flattened directory.
  unlink(nested_dir, recursive = TRUE)
  invisible(NULL)
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process U.S. EPA AQS daily CSV data
#' @description
#' The \code{process_aqs()} function cleans and imports raw air quality
#' monitoring sites from pre-generated daily CSV files, returning a single
#' `SpatVector` or `sf` object.
#' `date` is used to filter the raw data read from csv files.
#' Filtered rows are then processed according to `mode` argument.
#' Some sites report multiple measurements per day with and without
#' [exceptional events](https://www.epa.gov/sites/default/files/2016-10/documents/exceptional_events.pdf)
#' the internal procedure of this function keeps "Included" if there
#' are multiple event types per site-time.
#' @param path character(1). Directory path to daily measurement data.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#'  Should be in `"YYYY-MM-DD"` format and sorted.
#' @param mode character(1). One of
#'   * "date-location" (all dates * all locations)
#'   * "available-data" (date-location pairs with available data)
#'   * "location" (unique locations).
#' @param data_field character(1). Data field to extract.
#' @param return_format character(1). `"terra"` or `"sf"` or `"data.table"`.
#' @param resolution_temporal character(1). Daily or hourly data, must match
#' `resolution_temporal` argument passed to `amadeus::download_aqs`
#' @param tz character(1). "local" or "gmt" for time zone associated with hourly data.
#' @param extent numeric(4). Spatial extent of the resulting object.
#'   The order should be `c(xmin, xmax, ymin, ymax)`.
#'   The coordinate system should be WGS84 (EPSG:4326).
#' @param ... Placeholders.
#' @seealso
#' * [`download_aqs()`]
#' * [EPA, n.d., _AQS Parameter Codes_](
#'   https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv)
#' @return a SpatVector, sf, or data.table object depending on the `return_format`
#' @importFrom data.table as.data.table
#' @importFrom utils read.csv
#' @importFrom terra vect project
#' @importFrom sf st_as_sf
#' @importFrom dplyr group_by ungroup filter mutate select distinct
#' @note Choose `date` and `mode` values with caution.
#' The function may return a massive data.table depending on the time range,
#' resulting in a long processing time or even a crash if data is too large
#' for your computing environment to process.
#' AQS data are generally intended for use as dependent variables, so
#' `process_aqs()` does not have a companion route in `calculate_covariates()`.
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' aqs <- process_aqs(
#'   path = "./data/aqs_daily_example.csv",
#'   date = c("2022-12-01", "2023-01-31"),
#'   mode = "date-location",
#'   return_format = "terra"
#' )
#' }
#' @export
process_aqs <-
  function(
    path = NULL,
    date = c("2018-01-01", "2022-12-31"),
    mode = c("date-location", "available-data", "location"),
    data_field = "Arithmetic.Mean",
    return_format = c("terra", "sf", "data.table"),
    extent = NULL,
    resolution_temporal = c("daily", "hourly"),
    tz = c("local", "gmt"),
    ...
  ) {
    mode <- match.arg(mode)
    return_format <- match.arg(return_format)
    resolution_temporal <- match.arg(resolution_temporal)
    if (missing(data_field)) {
      data_field <- if (resolution_temporal == "hourly") {
        "Sample.Measurement"
      } else {
        "Arithmetic.Mean"
      }
    }
    tz <- match.arg(tz)
    date_tz <- ifelse(tz == "local", "Date.Local", "Date.GMT")
    if (resolution_temporal == "daily" & tz == "gmt") {
      message(
        "GMT time zone not applicable for daily data; using `Date.Local`."
      )
      date_tz <- "Date.Local"
    }
    time_tz <- ifelse(tz == "local", "Time.Local", "Time.GMT")

    if (!is.null(date)) {
      date <- try(as.Date(date), silent = TRUE)
      if (inherits(date, "try-error")) {
        stop("date has invalid format(s). Please check the values.")
      }
      if (length(date) != 2) {
        if (length(date) == 1) {
          date <- c(date, date)
        } else {
          stop("date should be a character vector of length 1 or 2.")
        }
      }
    } else {
      stop("date should be defined.")
    }
    if (length(path) == 1 && dir.exists(path)) {
      path <- list.files(
        path = path,
        pattern = "*.csv$",
        full.names = TRUE
      )
    }

    if (length(path) == 0) {
      stop("path does not contain csv files.")
    }
    pathfiles <- lapply(path, read.csv)

    sites <- data.table::rbindlist(pathfiles, fill = TRUE)

    ## get unique sites
    sites$site_id <-
      sprintf(
        "%02d%03d%04d%05d",
        as.integer(sites$State.Code),
        as.integer(sites$County.Code),
        as.integer(sites$Site.Num),
        as.integer(sites$Parameter.Code)
      )

    site_id <- NULL
    Datum <- NULL
    POC <- NULL
    Date.Local <- NULL
    Sample.Duration <- NULL
    Observation.Count <- NULL

    date_start <- as.Date(date[1])
    date_end <- as.Date(date[2])
    date_sequence <- seq(date_start, date_end, "day")
    time_sequence <- sprintf("%02d:00", 0:23)

    parsed_dates <- as.Date(rep(NA_character_, nrow(sites)))
    raw_dates <- as.character(sites[[date_tz]])
    slash_idx <- grepl("/", raw_dates, fixed = TRUE)
    dash_idx <- grepl("-", raw_dates, fixed = TRUE)
    parsed_dates[slash_idx] <- as.Date(
      raw_dates[slash_idx],
      format = "%m/%d/%Y"
    )
    parsed_dates[dash_idx] <- as.Date(raw_dates[dash_idx], format = "%Y-%m-%d")
    sites[[date_tz]] <- parsed_dates
    duration_keep <- startsWith(as.character(sites$Sample.Duration), "24")
    if (resolution_temporal == "hourly") {
      duration_keep <- rep(TRUE, nrow(sites))
    } else if ("Observation.Count" %in% names(sites)) {
      duration_keep <-
        duration_keep |
        (!is.na(sites$Observation.Count) & sites$Observation.Count == 24)
    }
    sites$duration_keep <- duration_keep

    # Time column
    chr_time <- if (resolution_temporal == "daily") {
      as.character(sites[[date_tz]])
    } else {
      as.character(paste0(sites[[date_tz]], " ", sites[[time_tz]]))
    }
    # select relevant fields only
    sites <- sites |>
      dplyr::as_tibble() |>
      dplyr::mutate(time = chr_time) |>
      dplyr::filter(.data[[date_tz]] %in% date_sequence) |>
      dplyr::filter(duration_keep) |>
      dplyr::group_by(site_id) |>
      dplyr::filter(POC == min(POC)) |>
      dplyr::ungroup()

    col_sel <- c("site_id", "Longitude", "Latitude", "Datum")
    if (mode != "available-data") {
      sites_v <- unique(sites[, col_sel])
    } else {
      col_sel <- unique(c(
        col_sel,
        if (resolution_temporal == "daily") "Event.Type",
        "time",
        data_field
      ))
      sites_v <- sites |>
        dplyr::select(dplyr::all_of(col_sel)) |>
        dplyr::distinct()

      if ("Event.Type" %in% col_sel) {
        # excluding site-time with multiple event types
        # sites_vdup will be "subtracted" from the original sites_v
        sites_vdup <- sites_v |>
          dplyr::group_by(site_id, time) |>
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::filter(!!dplyr::sym("Event.Type") == "Excluded") |>
          dplyr::ungroup()
        sites_v <-
          dplyr::anti_join(
            sites_v,
            sites_vdup,
            by = c("site_id", "time", "Event.Type")
          )
      }
    }
    names(sites_v)[2:3] <- c("lon", "lat")
    sites_v <- data.table::as.data.table(sites_v)

    # subset mainland
    sites_v <- sites_v[!grepl("^(02|15|72|78|6|80)", site_id), ]

    # NAD83 to WGS84
    sites_v_nad <-
      sites_v[sites_v$Datum == "NAD83", ]
    if (nrow(sites_v_nad) > 0) {
      sites_v_nad <- sf::st_as_sf(
        sites_v_nad,
        remove = FALSE,
        coords = c("lon", "lat"),
        crs = "EPSG:4269"
      )
      sites_v_nad <- sf::st_transform(sites_v_nad, "EPSG:4326")
      sites_v_nad$lon <- sf::st_coordinates(sites_v_nad)[, 1]
      sites_v_nad$lat <- sf::st_coordinates(sites_v_nad)[, 2]
      sites_v_nad <- sf::st_drop_geometry(sites_v_nad)
      sites_v_nad <- data.table::as.data.table(sites_v_nad)
    }
    # postprocessing: combine WGS84 and new WGS84 records
    sites_v_wgs <- sites_v[sites_v$Datum == "WGS84"]
    final_sites <- data.table::rbindlist(
      list(sites_v_wgs, sites_v_nad),
      fill = TRUE
    )
    final_sites <-
      final_sites[,
        grep("Datum", names(final_sites), invert = TRUE),
        with = FALSE
      ]

    if (mode == "date-location") {
      dl_sequence <- if (resolution_temporal == "daily") {
        date_sequence
      } else {
        df_sequence <- expand.grid(date_sequence, time_sequence)
        paste0(df_sequence[, 1], " ", df_sequence[, 2])
      }
      final_sites <-
        split(as.character(dl_sequence), as.character(dl_sequence)) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$time <- x
          return(fs_time)
        })
      final_sites <- data.table::rbindlist(final_sites, fill = TRUE)
    }
    if (mode == "available-data") {
      final_sites <- unique(final_sites)
    }

    final_sites <-
      switch(
        return_format,
        terra = terra::vect(
          final_sites,
          keepgeom = TRUE,
          crs = "EPSG:4326"
        ),
        sf = sf::st_as_sf(
          final_sites,
          remove = FALSE,
          dim = "XY",
          coords = c("lon", "lat"),
          crs = "EPSG:4326"
        ),
        data.table = final_sites
      )
    if (!is.null(extent)) {
      if (return_format == "data.table") {
        warning(
          "Extent is not applicable for data.table. Returning data.table...\n"
        )
        return(final_sites)
      }
      final_sites <- apply_extent(final_sites, extent)
    }

    return(final_sites)
  }
# nolint end
################################################################################
