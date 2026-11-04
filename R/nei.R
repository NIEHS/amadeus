################################################################################
# {amadeus} functions for US EPA National Emissions Inventory (NEI).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download road emissions data
#' @description
#' The \code{download_nei()} function accesses and downloads road emissions
#' data from the U.S Environmental Protection Agency's (EPA) National
#' Emissions Inventory (NEI).
#' @note NEI data does not require authentication.
#' @param epa_certificate_path TO BE DEPRECATED. Certificate path.
#' @param certificate_url TO BE DEPRECATED. Certificate URL.
#' @param year integer(1). Available years of NEI data.
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
#' @author Kyle Messier, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024nei}{amadeus}
#' @examples
#' \dontrun{
#' download_nei(
#'   year = c(2017L, 2020L),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_nei <- function(
  epa_certificate_path = NULL,
  certificate_url = paste0(
    # nolint: line_length_linter.
    "http://cacerts.digicert.com/",
    "DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ),
  year = c(2017L, 2020L),
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
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

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

  if (
    !is.null(epa_certificate_path) ||
      certificate_url !=
        "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ) {
    warning(
      "Parameters 'epa_certificate_path' and 'certificate_url'",
      " are deprecated.\n",
      "SSL certificates are now handled automatically by httr2.\n",
      call. = FALSE
    )
  }

  #### Define measurement data paths
  # Each NEI year has a distinct URL path suffix; use a named lookup so the
  # function works correctly whether `year` is a scalar (as dispatched from
  # the targets pipeline via pattern = map()) or a full c(2017, 2020) vector.
  url_download_base <- "https://gaftp.epa.gov/air/nei/%s/data_summaries/"
  nei_url_map <- c(
    "2017" = "2017v1/2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  nei_file_map <- c(
    "2017" = "2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  year_chr <- as.character(year)
  unknown <- setdiff(year_chr, names(nei_url_map))
  if (length(unknown) > 0) {
    stop(paste0(
      "NEI data is not available for year(s): ",
      paste(unknown, collapse = ", "),
      ". Available years: ",
      paste(names(nei_url_map), collapse = ", "),
      ".\n"
    ))
  }
  download_urls <- vapply(
    year_chr,
    function(y) paste0(sprintf(url_download_base, y), nei_url_map[y]),
    character(1)
  )
  download_names_file <- unname(nei_file_map[year_chr])
  download_names <- paste0(directory_to_download, download_names_file)

  #### Filter to files that need downloading
  needs_download <- sapply(download_names, amadeus::check_destfile)
  download_urls_filtered <- download_urls[needs_download]
  download_names_filtered <- download_names[needs_download]

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(download_urls_filtered)
    ))
    return(invisible(list(
      urls = download_urls_filtered,
      destfiles = download_names_filtered,
      n_files = length(download_urls_filtered)
    )))
  }

  #### Download files using httr2
  if (length(download_urls_filtered) > 0) {
    download_result <- amadeus::download_run_method(
      urls = download_urls_filtered,
      destfiles = download_names_filtered,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("All files already exist. Nothing to download.\n")
    download_result <- list(
      success = 0,
      failed = 0,
      skipped = length(download_names)
    )
  }

  #### Unzip data (custom unzipping to separate directories per file)
  if (unzip) {
    dir_unzip <- paste0(
      directory_to_save,
      sub(".zip", "", download_names_file)
    )
    for (fn in seq_along(dir_unzip)) {
      if (file.exists(download_names[fn])) {
        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }
    }
  }

  #### Remove zip files
  if (remove_zip) {
    for (fn in download_names) {
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }
  }

  message("Requests were processed.\n")

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
#' Process road emissions data
#' @description
#' The \code{process_nei()} function imports and cleans raw road emissions data,
#' returning a single `SpatVector` object.
#' @param path character(1). Directory with NEI csv files.
#' @param county `SpatVector`/`sf`. County boundaries.
#' @param year integer(1) Year to use. Currently only 2017 or 2020
#' is accepted.
#' @param ... Placeholders.
#' @return a `SpatVector` object
#' @author Insang Song
#' @note Base files for `county` argument can be downloaded directly from
#' [U.S. Census Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' or by using `tigris` package. This function does not reproject census boundaries.
#' Users should be aware of the coordinate system of census boundary data for
#' other analyses.
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level. With raw data files,
#' this function will join a combined table of NEI data and county
#' boundary, then perform a spatial join to target locations.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' nei <- process_nei(
#'   path = "./data",
#'   county = system.file("gpkg/nc.gpkg", package = "sf"),
#'   year = 2017
#' )
#' }
#' @export
# nolint end
process_nei <- function(
  path = NULL,
  county = NULL,
  year = c(2017, 2020),
  ...
) {
  if (is.null(county)) {
    stop("county argument is required.")
  }
  if (!methods::is(county, "SpatVector")) {
    county <- try(terra::vect(county), silent = TRUE)
    if (inherits(county, "try-error")) {
      stop("county is unable to be converted to SpatVector.\n")
    }
  }
  if (!"GEOID" %in% names(county)) {
    stop("county should include a field named \"GEOID\"")
  }
  if (!as.character(year) %in% c("2017", "2020")) {
    stop("year should be one of 2017 or 2020.\n")
  }
  # Concatenate NEI csv files
  csvs_nei <-
    list.files(
      path = path,
      pattern = "*.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
  csvs_nei <- grep(year, csvs_nei, value = TRUE)
  if (is.null(csvs_nei) || length(csvs_nei) == 0) {
    stop("No files found for the year. The file names should include the year")
  }
  csvs_nei <- lapply(csvs_nei, data.table::fread)
  csvs_nei <- data.table::rbindlist(csvs_nei)
  # column name readjustment
  target_nm <- c("fips code", "total emissions", "emissions uom")
  # not grep-ping at once for flexibility
  target_cns <- sapply(target_nm, function(x) grep(x, colnames(csvs_nei)))
  colnames(csvs_nei)[target_cns] <-
    c("geoid", "emissions_total", "unit_measurement")

  # unify unit of measurement
  # TON here is short tonne, which is 2000 lbs.
  csvs_nei$emissions_total_ton <-
    ifelse(
      csvs_nei$unit_measurement == "TON",
      csvs_nei$emissions_total,
      csvs_nei$emissions_total / 2000
    )
  emissions_total_ton <- NULL
  geoid <- NULL
  # yearabbr <- substr(year, 3, 4)
  csvs_nei$geoid <- sprintf("%05d", as.integer(csvs_nei$geoid))
  csvs_nei <-
    csvs_nei[,
      list(
        TRF_NEINP_0_00000 = sum(emissions_total_ton, na.rm = TRUE)
      ),
      by = geoid
    ]
  csvs_nei$time <- as.integer(year)

  # read county vector
  county$GEOID <- sprintf("%05d", as.integer(county$GEOID))
  csvs_nei_df <- as.data.frame(csvs_nei)
  county_rows <- match(county$GEOID, csvs_nei_df$geoid)
  county$time <- csvs_nei_df$time[county_rows]
  county$TRF_NEINP_0_00000 <- csvs_nei_df$TRF_NEINP_0_00000[county_rows]
  county$geoid <- county$GEOID
  cnty_vect <- county[!is.na(county$TRF_NEINP_0_00000), ]
  cnty_vect <- cnty_vect[, c("geoid", "time", "TRF_NEINP_0_00000")]
  return(cnty_vect)
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate road emissions covariates
#' @param from SpatVector(1). Output of \code{process_nei()}.
#' @param locs sf/SpatVector. Locations at NEI values are joined.
#' @param locs_id character(1). Unique site identifier column name.
#' Unused but kept for compatibility.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song, Ranadeep Daw
#' @seealso [`process_nei`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom methods is
#' @importFrom terra project
#' @importFrom terra intersect
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_nei(
#'   from = nei, # derived from process_nei example
#'   locs = loc,
#'   locs_id = "id"
#' )
#' }
#' @export
calculate_nei <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs), silent = TRUE)
    if (inherits(locs, "try-error")) {
      stop("locs is unable to be converted to SpatVector.\n")
    }
  }
  # spatial join
  locs_re <- terra::project(locs, terra::crs(from))
  locs_re <- terra::intersect(locs_re, from)

  # If returning geometry, keep as SpatVector
  if (geom %in% c("terra", "sf")) {
    if (geom == "terra") {
      return(locs_re)
    } else if (geom == "sf") {
      return(sf::st_as_sf(locs_re))
    }
  } else {
    # Only convert to data.frame if geom = FALSE
    locs_re <- as.data.frame(locs_re)
    return(locs_re)
  }
}
################################################################################
