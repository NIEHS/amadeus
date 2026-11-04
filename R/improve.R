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
#' The \code{download_improve()} function accesses and downloads IMPROVE
#' (Interagency Monitoring of Protected Visual Environments) data files
#' from the VIEWS/VIBE data export service hosted at CIRA/CSU. Annual files
#' are downloaded as \code{.txt.zip} archives and extracted to
#' pipe-delimited \code{.txt} files containing aerosol measurements at
#' federal-land monitoring stations.
#' @note
#' \itemize{
#'   \item IMPROVE data does not require authentication.
#'   \item Three product types are available:
#'     \code{"raw"} (IMPAER — speciated aerosol mass concentrations),
#'     \code{"rhr2"} (IMPRHR2 — Regional Haze Rule II light extinction),
#'     \code{"rhr3"} (IMPRHR3 — Regional Haze Rule III deciview index).
#'   \item Site metadata is handled by \code{\link{process_improve}} using an
#'     embedded table; annual downloads include measurement files only.
#'   \item IMPROVE monitors ~\eqn{1 \mu g / m^3} precision instruments
#'     deployed at Class I and other federal land areas.
#' }
#' @param year integer(1 or 2). Year or start/end years.
#' @param product character(1). Product selector:
#'   \code{"raw"} (aerosol, default), \code{"rhr2"} (Regional Haze Rule II),
#'   or \code{"rhr3"} (Regional Haze Rule III).
#' @param url_improve character(1). Base URL to the IMPROVE data export
#'   service.
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default
#'   \code{TRUE}).
#' @param hash logical(1). Return hash of downloaded files (default
#'   \code{FALSE}).
#' @param max_tries integer(1). Maximum retry attempts (default 20).
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2).
#' @author Insang Song, Mitchell Manware
#' @return invisible list with download results; or hash character if
#'   \code{hash = TRUE}.
#' @importFrom httr2 request req_retry req_timeout req_perform resp_status
#' @seealso
#'   \code{\link{process_improve}}
#' @examples
#' \dontrun{
#' download_improve(
#'   year = 2022,
#'   product = "raw",
#'   directory_to_save = "./data/improve/",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_improve <- function(
  year = c(2018, 2022),
  product = c("raw", "rhr2", "rhr3"),
  url_improve = "https://vibe.cira.colostate.edu/data/export/",
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
      call. = FALSE
    )
  }
  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Validate product
  product <- match.arg(product)

  #### Map product to file prefix
  prefix_map <- c(raw = "IMPAER", rhr2 = "IMPRHR2", rhr3 = "IMPRHR3")
  file_prefix <- prefix_map[[product]]

  #### Check years
  year <- if (length(year) == 1) rep(year, 2) else year
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  year_sequence <- seq(year[1], year[2], 1)

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Build download URLs and destination paths
  # nolint start
  download_urls <- sprintf(
    "%s%s/%s_%d.txt.zip",
    url_improve,
    file_prefix,
    file_prefix,
    year_sequence
  )
  # nolint end
  download_names <- sprintf(
    "%s%s_%d.txt",
    directory_to_save,
    file_prefix,
    year_sequence
  )

  #### Filter to files that need downloading
  needs_dl <- vapply(download_names, amadeus::check_destfile, logical(1))
  download_urls <- download_urls[needs_dl]
  download_names <- download_names[needs_dl]

  if (length(download_urls) == 0) {
    message("All IMPROVE files already present.\n")
    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    }
    return(invisible(list(
      success = 0,
      failed = 0,
      skipped = length(download_names)
    )))
  }

  #### Download files
  zip_destfiles <- sprintf("%s.zip", download_names)
  zip_destfiles <- sub("\\.txt\\.zip$", ".txt.zip", zip_destfiles)

  download_result <- amadeus::download_run_method(
    urls = download_urls,
    destfiles = zip_destfiles,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  #### Extract annual IMPROVE zip files to text files
  zip_targets <- zip_destfiles[grepl("\\.txt\\.zip$", zip_destfiles)]
  if (length(zip_targets) > 0) {
    for (zip_file in zip_targets) {
      if (!file.exists(zip_file)) {
        next
      }
      utils::unzip(
        zipfile = zip_file,
        exdir = directory_to_save,
        overwrite = TRUE
      )
      file.remove(zip_file)
    }
  }

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  }
  return(invisible(download_result))
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
  path <- amadeus::download_sanitize_path(path)

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
