################################################################################
# {amadeus} functions for US EPA National Emissions Inventory (NEI).
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download NEI onroad emissions data
#' @description
#' Download onroad emissions CSV archives from the U.S. Environmental
#' Protection Agency's National Emissions Inventory (NEI). Archives and
#' extracted data are stored in separate subdirectories using the shared
#' download workflow.
#' @note NEI data does not require authentication.
#' @param year integer(1 or more). Selected inventory years. Supported values
#'   are 2008, 2011, 2014, 2017, 2020, and 2023. Default is
#'   `c(2017L, 2020L)`. Each value selects an individual inventory;
#'   `c(2017, 2023)` requests only those two years, without filling gaps.
#'   Unsupported years are omitted with a warning that lists the retained
#'   years. If no supported years remain, the function stops before creating
#'   directories or contacting the provider.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are downloaded into `zip_files/` and, when `unzip = TRUE`,
#'   extracted into separate per-archive directories under `data_files/`.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed. Downloaded
#'   archives and extracted CSV files can require substantial storage.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after directory creation, before the preflight check of the first URL. No
#'   archives are downloaded or extracted in preview mode. Default is `TRUE`.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#'   No command file is generated. Default is `FALSE`.
#' @param unzip logical(1). Extract downloaded ZIP archives into per-archive
#'   directories under `data_files/`. Default is `TRUE`; `FALSE` skips
#'   extraction and creation of the per-archive directories.
#' @param remove_zip logical(1). Remove downloaded archives and their
#'   `zip_files/` directory, including remaining contents, after the extraction
#'   loop finishes. Default is `FALSE`. May be `TRUE` only when
#'   `unzip = TRUE`. No removal occurs when `download = FALSE`.
#' @param show_progress logical(1). Show file download progress. Default is
#'   `TRUE`. Other status messages may still be printed when `FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of the files under
#'   `data_files/` instead of the download summary. Default is `FALSE`.
#'   Ignored when `download = FALSE`.
#' @param max_tries integer(1). Maximum attempts per file download request,
#'   including the initial attempt. Default is 20.
#' @param rate_limit numeric(1). Non-negative throttling interval in seconds,
#'   passed to [download_run_method()]. Default is 2.
#' @details
#' A year-specific lookup selects the provider's archive path and filename.
#' NEI releases use different version directories and naming conventions.
#'
#' `zip_files/` and `data_files/` are sibling directories created beneath
#' `directory_to_save`, including for previews and when `unzip = FALSE`.
#' Each archive is extracted into a directory named after the archive with
#' its `.zip` suffix removed. For example, the 2020 archive is saved as
#' `zip_files/2020nei_onroad_byregion.zip` and extracted into
#' `data_files/2020nei_onroad_byregion/`. This isolates duplicate filenames
#' across inventories. Paths stored inside each archive are preserved.
#'
#' Preview destinations refer to the archives in `zip_files/`, including
#' files that already exist locally. For actual downloads,
#' [download_run_method()] skips existing non-empty archives; they are still
#' passed to extraction when `unzip = TRUE`.
#' For actual downloads, the preflight check contacts the first URL; a failed
#' check emits a warning. Preview mode skips this check. Its request settings
#' are separate from those used for file downloads.
#'
#' Hashing covers all files recursively under `data_files/`, including the
#' per-archive subdirectories and existing files, and excludes archives in
#' `zip_files/`. With `unzip = FALSE`, hashing still uses `data_files/`, which
#' may be empty or contain previously extracted data.
#'
#' Download support covers more inventory years than [process_nei()], which
#' currently accepts only 2017 and 2020.
#' @author Kyle Messier, Insang Song
#' @return
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`. Otherwise, after downloading and optional
#' extraction, `hash = FALSE` returns the download summary from
#' [download_run_method()] invisibly. With `hash = TRUE`, returns a visible
#' character scalar containing the combined MD5 hash of `data_files/`.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @seealso [download_setup_dir()], [download_unzip()], [process_nei()]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024nei}{amadeus}
#' @examples
#' \dontrun{
#' download_nei(
#'   year = c(2017L, 2020L),
#'   directory_to_save = file.path(tempdir(), "nei"),
#'   acknowledgement = TRUE,
#'   download = FALSE # Preview selected inventory URLs and archive paths.
#' )
#' }
#' @export
download_nei <- function(
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
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "nei",
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
  # Manage `year` argument.
  idx_year <- year %in% c(2008, 2011, 2014, 2017, 2020, 2023)
  int_year <- year[idx_year]
  if (any(!idx_year)) {
    # --------------------------------------------------------------------------
    # If request includes any invalid years.
    chr_year_message <- paste0(
      "NEI data is not available for year(s): ",
      paste(year[!idx_year], collapse = ", "),
      ". Available years: 2008, 2011, 2014, 2017, 2020, 2023."
    )
    # --------------------------------------------------------------------------
    # Error if all years requested are invalid.
    if (length(int_year) == 0L) {
      stop(chr_year_message, " No available years to download.", call. = FALSE)
    }
    # --------------------------------------------------------------------------
    # Continue with valid year downlod but warn some requests were invalid.
    warning(
      chr_year_message,
      "\nProceeding with download for year(s): ",
      paste(int_year, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  checkmate::assert_integerish(int_year, lower = 2008, min.len = 1L)
  checkmate::assert_subset(int_year, c(2008, 2011, 2014, 2017, 2020, 2023))
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Map year selection to file name; subsetting to year selections.
  chr_urlmap <- c(
    "2008" = "2008neiv3_onroad_byregions.zip",
    "2011" = "2011v2/2011neiv2_onroad_byregions.zip",
    "2014" = "2014v2/2014neiv2_onroad_byregions.zip",
    "2017" = "2017v1/2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip",
    "2023" = "2023nei_onroad_byregion_21jul2026.zip"
  )
  chr_urlmap <- unname(
    grep(
      paste0(int_year, collapse = "|"),
      chr_urlmap,
      value = TRUE
    )
  )
  chr_filemap <- c(
    "2008" = "2008neiv3_onroad_byregions.zip",
    "2011" = "2011neiv2_onroad_byregions.zip",
    "2014" = "2014neiv2_onroad_byregions.zip",
    "2017" = "2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip",
    "2023" = "2023nei_onroad_byregion_21jul2026.zip"
  )
  chr_filemap <- unname(
    grep(
      paste0(int_year, collapse = "|"),
      chr_filemap,
      value = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs.
  chr_urls <- sprintf(
    "https://gaftp.epa.gov/air/nei/%s/data_summaries/%s",
    int_year,
    chr_urlmap
  )
  # ----------------------------------------------------------------------------
  # Generate download file names.
  chr_filenames <- paste0(chr_dir_to_download, chr_filemap)
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(chr_urls, chr_filenames)))
  }
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(chr_urls[1])
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
  # Per-file directories to avoid filename collisions.
  chr_dirs_to_unzip <- paste0(
    chr_dir_to_save,
    sub("\\.(zip|tar\\.gz)$", "", basename(chr_filenames))
  )
  if (isTRUE(unzip)) {
    invisible(lapply(chr_dirs_to_unzip, amadeus:::download_setup_dir))
  }
  # ----------------------------------------------------------------------------
  # Unzip downloaded .zip files.
  amadeus:::download_unzip(
    file_name = chr_filenames,
    directory_to_unzip = chr_dirs_to_unzip,
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
