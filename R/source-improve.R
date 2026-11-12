################################################################################
# {amadeus} functions for Interagency Monitoring of Protected Visual
# Environments (IMPROVE).
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download IMPROVE aerosol monitoring data
#' @description
#' Download annual IMPROVE (Interagency Monitoring of Protected Visual
#' Environments) measurement files from the VIEWS/VIBE data export service
#' hosted at CIRA/CSU. Files are downloaded as `.txt.zip` archives and can be
#' extracted to pipe-delimited `.txt` files using the shared download workflow.
#' @note IMPROVE data does not require authentication. Annual downloads contain
#'   measurement files; site metadata is resolved separately by
#'   [process_improve()].
#' @param year integer(1 or more). One year, two endpoints, or a non-empty
#'   vector of years. Every year from the minimum through the maximum is
#'   requested, including gaps: `c(2018, 2023)` requests `2018:2023`.
#'   Default is `c(2018, 2022)`. Missing, fractional, pre-1900, and future
#'   years are rejected. File availability depends on the selected product
#'   and provider; local validation does not check product coverage.
#' @param product character(1). Product selector: `"raw"` (default) for
#'   aerosol measurements (`IMPAER`), `"rhr2"` for Regional Haze Rule II
#'   data (`IMPRHR2`), or `"rhr3"` for Regional Haze Rule III data (`IMPRHR3`).
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are downloaded into `zip_files/` and, when `unzip = TRUE`,
#'   extracted into `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. Downloaded
#'   archives and extracted measurement files can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` emits a warning and returns
#'   a preview of the planned files without downloading archives or extracting
#'   their contents. Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   No command file is generated. Default is `FALSE`.
#' @param unzip logical(1). Extract downloaded ZIP archives into
#'   `data_files/`. Default is `TRUE`; `FALSE` retains the archives without
#'   extracting their contents.
#' @param remove_zip logical(1). Remove downloaded archives and their
#'   `zip_files/` directory, including remaining contents, after the extraction
#'   loop finishes. Default is `FALSE`. May be `TRUE` only when
#'   `unzip = TRUE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status messages may still be printed when `FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of the files under
#'   `data_files/` instead of the download summary when `download = TRUE`.
#'   Ignored when `download = FALSE`. Default is `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file download request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Non-negative throttling interval in seconds,
#'   passed to [download_run_method()]. Default is 2.
#' @details
#' The selected product determines the filename prefix. For example,
#' `product = "raw"` and `year = 2022` request `IMPAER_2022.txt.zip`.
#' `zip_files/` and `data_files/` are sibling directories created beneath
#' `directory_to_save`, including when `unzip = FALSE` or `download = FALSE`.
#'
#' Preview mode creates the output directories and makes no network requests.
#' Individual download failures are recorded by [download_run_method()] during
#' file transfers.
#'
#' All archives share `data_files/` as their extraction destination. Names
#' and paths stored inside each archive are preserved. Pass the directory
#' containing the extracted `PREFIX_YYYY.txt` files, normally `data_files/`,
#' as `path` to [process_improve()].
#'
#' [download_run_method()] skips existing non-empty archives in `zip_files/`.
#' Those archives are still passed to extraction when `unzip = TRUE`.
#' Existing extracted `.txt` files alone do not cause a download to be skipped.
#'
#' Hashing covers all files recursively under `data_files/`, including existing
#' files, and excludes archives in `zip_files/`. With `unzip = FALSE`, hashing
#' still uses `data_files/`, which may be empty or contain previously extracted
#' data.
#' @author Insang Song, Mitchell Manware
#' @return
#' When `download = FALSE`, an invisible list containing `urls`, `destfiles`,
#' and `n_files` is returned regardless of `hash`. These describe all planned
#' downloads, including files that already exist locally; `n_files` is the
#' number of planned URLs.
#'
#' When `download = TRUE`, after downloading and optional extraction,
#' `hash = FALSE` returns the download summary from [download_run_method()]
#' invisibly, including counts of successful, failed, and skipped files.
#' With `hash = TRUE`, returns a visible character scalar containing the
#' combined MD5 hash of `data_files/`.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @importFrom httr2 request req_retry req_timeout req_perform resp_status
#' @seealso [download_setup_dir()], [download_unzip()], [process_improve()]
#' @examples
#' \dontrun{
#' download_improve(
#'   year = 2022,
#'   product = "raw",
#'   directory_to_save = file.path(tempdir(), "improve"),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_improve <- function(
  year = c(2018, 2022),
  product = c("raw", "rhr2", "rhr3"),
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
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "improve",
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
  chr_product <- match.arg(product)
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Define file prefix based on product.
  chr_prefix <- switch(
    chr_product,
    raw = "IMPAER",
    rhr2 = "IMPRHR2",
    rhr3 = "IMPRHR3"
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(
    "%s%s/%s_%d.txt.zip",
    "https://vibe.cira.colostate.edu/data/export/",
    chr_prefix,
    chr_prefix,
    int_years
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- sprintf(
    "%s%s_%d.txt.zip",
    chr_dir_to_download,
    chr_prefix,
    int_years
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = chr_urls,
    destfiles = chr_filenames,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files.
  amadeus:::download_unzip(
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
# nolint start
#' Process IMPROVE aerosol monitoring data
#' @description
#' The \code{process_improve()} function reads pipe-delimited IMPROVE
#' (Interagency Monitoring of Protected Visual Environments) measurement
#' files downloaded by \code{download_improve()} and joins them with a site
#' metadata table to attach geographic coordinates and auxiliary site
#' attributes. Returns a
#' \code{SpatVector}, \code{sf}, or \code{data.table} object.
#' @details
#' Three product types are supported via \code{product}:
#' \describe{
#'   \item{\code{"raw"}}{IMPAER speciated aerosol mass concentrations.
#'     Key columns: \code{SiteCode}, \code{FactDate}, \code{ParamCode},
#'     \code{FactValue}, \code{Units}.}
#'   \item{\code{"rhr2"}}{IMPRHR2 Regional Haze Rule II light extinction
#'     (\code{bext}, \eqn{Mm^{-1}}).}
#'   \item{\code{"rhr3"}}{IMPRHR3 Regional Haze Rule III deciview index
#'     (\code{dv}).}
#' }
#' Measurement values are \strong{not} filtered by \code{Status}; callers
#' may apply their own validity flags (e.g., keep only \code{Status == "V0"}).
#' @param path character(1). Directory containing downloaded IMPROVE
#'   \code{.txt} files.
#' @param product character(1). Product type: \code{"raw"} (default),
#'   \code{"rhr2"}, or \code{"rhr3"}.
#' @param date character(1 or 2). Date (\code{"YYYY-MM-DD"}) or start/end
#'   date pair to filter measurements. Defaults to no filtering when
#'   \code{NULL}.
#' @param sites_file character(1) or \code{NULL}. Path to a site metadata
#'   file. When \code{NULL} (default), the function first looks for a file
#'   named \code{improve_sites.txt} inside \code{path}, then falls back to an
#'   embedded IMPROVE aerosol site table included in \code{amadeus}.
#' @param return_format character(1). Return object type: \code{"terra"},
#'   \code{"sf"}, or \code{"data.table"}.
#' @param extent numeric(4) or \code{NULL}. Optional crop extent
#'   \code{c(xmin, xmax, ymin, ymax)} in WGS84 / EPSG:4326. Applied only
#'   when \code{return_format} is \code{"terra"} or \code{"sf"}.
#' @param ... Placeholders.
#' @seealso \code{\link{download_improve}}
#' @return a \code{SpatVector}, \code{sf}, or \code{data.table} object
#'   depending on \code{return_format}.
#' @note IMPROVE data are measured on an every-third-day sampling schedule.
#'   Gaps between measurement dates are expected.
#' @importFrom data.table fread rbindlist as.data.table setnames
#' @importFrom terra vect crop ext project
#' @importFrom sf st_as_sf
#' @examples
#' improve <- process_improve(
#'   path = system.file("testdata/improve", package = "amadeus"),
#'   product = "raw",
#'   date = c("2022-01-01", "2022-01-31"),
#'   return_format = "data.table"
#' )
#' @export
# nolint end
process_improve <- function(
  path = NULL,
  product = c("raw", "rhr2", "rhr3"),
  date = NULL,
  sites_file = NULL,
  return_format = c("terra", "sf", "data.table"),
  extent = NULL,
  ...
) {
  product <- match.arg(product)
  return_format <- match.arg(return_format)

  #### Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("`path` must be a valid directory path.\n")
  }
  path <- amadeus:::download_sanitize_path(path)

  #### Resolve file prefix from product
  prefix_map <- c(raw = "IMPAER", rhr2 = "IMPRHR2", rhr3 = "IMPRHR3")
  prefix <- prefix_map[[product]]

  #### Find measurement files
  meas_files <- list.files(
    path,
    pattern = paste0("^", prefix, "_[0-9]{4}\\.txt$"),
    full.names = TRUE
  )
  if (length(meas_files) == 0) {
    stop(sprintf(
      "No %s_YYYY.txt files found in '%s'.\n",
      prefix,
      path
    ))
  }

  #### Read and bind measurement files
  meas_list <- lapply(meas_files, function(f) {
    dt <- data.table::fread(
      f,
      sep = "|",
      header = TRUE,
      showProgress = FALSE,
      data.table = TRUE
    )
    dt
  })
  meas <- data.table::rbindlist(meas_list, fill = TRUE)

  #### Standardise date column
  data.table::set(
    meas,
    j = "FactDate",
    value = as.Date(meas[["FactDate"]])
  )

  #### Filter by date if provided
  if (!is.null(date)) {
    date <- if (length(date) == 1) rep(date, 2) else date
    stopifnot(length(date) == 2)
    d_start <- as.Date(date[1])
    d_end <- as.Date(date[2])
    meas <- meas[
      get("FactDate") >= d_start & get("FactDate") <= d_end,
    ]
    if (nrow(meas) == 0) {
      warning(
        "No IMPROVE measurements found for the specified date range.\n",
        call. = FALSE
      )
      return(meas)
    }
  }

  #### Resolve and read site metadata
  if (is.null(sites_file)) {
    candidate <- file.path(path, "improve_sites.txt")
    if (file.exists(candidate)) {
      sites_file <- candidate
    }
  }

  if (!is.null(sites_file) && file.exists(sites_file)) {
    sites <- data.table::fread(
      sites_file,
      sep = "|",
      header = TRUE,
      showProgress = FALSE,
      data.table = TRUE
    )
  } else {
    sites <- process_improve_sites_builtin() # nolint: object_usage_linter.
  }

  #### Deduplicate site table and merge all available metadata columns
  if ("SiteCode" %in% names(sites)) {
    sites <- data.table::as.data.table(sites)
    if (all(c("DataEndDate", "DataStartDate") %in% names(sites))) {
      data.table::set(
        sites,
        j = "DataEndDate_sort",
        value = as.Date(sites[["DataEndDate"]], format = "%m/%d/%y")
      )
      data.table::set(
        sites,
        j = "DataStartDate_sort",
        value = as.Date(sites[["DataStartDate"]], format = "%m/%d/%y")
      )
      ord <- order(
        sites[["SiteCode"]],
        sites[["DataEndDate_sort"]],
        sites[["DataStartDate_sort"]],
        decreasing = c(FALSE, TRUE, TRUE),
        method = "radix",
        na.last = TRUE
      )
      sites <- sites[ord]
      sites <- sites[!duplicated(sites[["SiteCode"]])]
      data.table::set(sites, j = "DataEndDate_sort", value = NULL)
      data.table::set(sites, j = "DataStartDate_sort", value = NULL)
    } else {
      sites <- sites[!duplicated(sites[["SiteCode"]])]
    }

    coord_cols <- c("SiteCode", "Latitude", "Longitude")
    coord_cols_present <- coord_cols[coord_cols %in% names(sites)]
    if (length(coord_cols_present) < 3) {
      warning(
        "Sites file is missing Latitude and/or Longitude columns.\n",
        call. = FALSE
      )
    } else {
      meas <- merge(meas, sites, by = "SiteCode", all.x = TRUE)
    }
  }

  #### Enforce numeric coordinate columns when present
  if ("Latitude" %in% names(meas)) {
    data.table::set(
      meas,
      j = "Latitude",
      value = suppressWarnings(as.numeric(meas[["Latitude"]]))
    )
  }
  if ("Longitude" %in% names(meas)) {
    data.table::set(
      meas,
      j = "Longitude",
      value = suppressWarnings(as.numeric(meas[["Longitude"]]))
    )
  }

  #### Return early as data.table if requested or no coordinates
  has_coords <- all(c("Latitude", "Longitude") %in% names(meas))
  if (return_format == "data.table" || !has_coords) {
    if (!has_coords && return_format != "data.table") {
      warning(
        "No site coordinates available; returning data.table.\n",
        call. = FALSE
      )
    }
    return(meas)
  }

  #### Build spatial object
  # nolint start: object_usage_linter.
  meas_complete <- meas[
    !is.na(get("Latitude")) & !is.na(get("Longitude")),
  ]
  # nolint end
  sv <- terra::vect(
    meas_complete,
    geom = c("Longitude", "Latitude"),
    crs = "EPSG:4326"
  )

  #### Apply optional extent crop
  if (!is.null(extent)) {
    sv <- terra::crop(sv, terra::ext(extent))
  }

  if (return_format == "terra") {
    return(sv)
  } else {
    return(sf::st_as_sf(sv))
  }
}
################################################################################
