################################################################################
# {amadeus} functions for Modern-Era Retrospective Analysis for Research and
# Applications (MERRA-2).
################################################################################

################################################################################
# {download} tier
################################################################################
# nolint start
#' Download meteorological and atmospheric data
#' @description
#' The \code{download_merra2()} function accesses and downloads various
#' meteorological and atmospheric collections from [NASA's Modern-Era
#' Retrospective analysis for Research and Applications, Version 2
#' (MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), and the
#' daily corrected Global Fire Weather Index (FWI) product derived from MERRA-2
#' weather inputs.
#' @note Due to NASA data access policies, standard MERRA-2 GES DISC downloads
#' require a valid NASA Earthdata token for authentication. Use
#' [setup_nasa_token()] for setup. The `"fwi"` collection is hosted on the
#' public GlobalFWI portal and does not require Earthdata authentication.
#' @param collection character vector. One or more supported MERRA-2 collection
#'   identifiers, or `"fwi"` for the daily corrected Global Fire Weather Index
#'   product (`MERRA2.CORRECTED`). Repeated identifiers are removed. The default
#'   selects all collections listed in the function signature, including FWI.
#'   Supported identifiers and product metadata are stored in the installed
#'   `extdata/df_merra2.csv` lookup table.
#' @param nasa_earth_data_token character(1) or NULL. NASA Earthdata token,
#'   path to a file containing a token, or `NULL` to use the
#'   `NASA_EARTHDATA_TOKEN` environment variable. [get_token()] checks the
#'   environment variable first, then a supplied file path or token string.
#'   Token lookup is required when any standard MERRA-2 collection is requested.
#' @param date character(1 or 2). A single day or two range endpoints in
#'   `"YYYY-MM-DD"` format. Endpoints may be supplied in either order; the
#'   resulting daily sequence includes both endpoints.
#' @param directory_to_save character(1). Non-empty directory path for downloads.
#'   A path must be supplied despite the `NULL` default. Data files are saved
#'   under a subdirectory named for each collection; available XML companions
#'   are saved in that collection's `metadata` subdirectory.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed, acknowledging
#'   that the downloads may require substantial storage and memory.
#' @param download logical(1). Deprecated. `FALSE` emits a warning and returns
#'   a download preview. Authentication checks, file discovery, directory
#'   creation, and the preflight URL check still run before returning.
#' @param remove_command logical(1). Deprecated and ignored. `TRUE` emits a
#'   warning.
#' @param hash logical(1). If `TRUE`, return the combined MD5 hash calculated
#'   by [download_hash()] for all files under `directory_to_save`, including
#'   metadata and files from earlier downloads. Default is `FALSE`. Ignored
#'   when `download = FALSE`.
#' @param show_progress logical(1). Show per-file progress messages and download
#'   progress bars. Default is `TRUE`; summary and status messages may still be
#'   printed when `FALSE`.
#' @param max_tries integer(1). Maximum attempts per listing or file request,
#'   including the initial attempt. Must be at least 1. Default is 20.
#' @param rate_limit numeric(1). Non-negative request-throttling interval in
#'   seconds. Default is 2. Passed to both discovery and file downloads; their
#'   throttling behavior is described below.
#' @details
#' Standard MERRA-2 file URLs are discovered from monthly HTML directory
#' listings. Published stream and revision prefixes are retained, and all
#' matching files are included when more than one revision is listed for a
#' date. Every requested date must have at least one matching `.nc4` file in
#' its directory. Companion `.nc4.xml` metadata is optional and is included
#' only when the corresponding data file is also listed. FWI `.nc` URLs are
#' constructed directly from the requested dates.
#'
#' Discovery stops before file downloads if a monthly listing cannot be read,
#' a requested data date is missing, or different URLs in a listing would
#' use the same local filename. Once discovery succeeds, [download_run_method()]
#' skips existing non-empty files and records individual download failures,
#' warning after the remaining files have been attempted.
#'
#' Listing requests to the same host are spaced by at least `rate_limit`
#' seconds; 0 disables listing throttling. File downloads use the host-based
#' token bucket described in [download_run_method()]. The preflight URL check
#' uses its own request settings. Discovery and file downloads use their
#' default per-attempt timeout of 3600 seconds; this function does not expose
#' a `timeout` argument.
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return
#' With `download = TRUE` and `hash = FALSE`, an invisible list containing
#' `success`, `failed`, and `skipped` file counts, plus `failed_urls` and
#' `failed_files` character vectors. All five fields are always present; the
#' two failure vectors are `character(0)` when no downloads failed, including
#' when every file was skipped. Counts include XML metadata files.
#'
#' With `download = TRUE` and `hash = TRUE`, a character scalar containing the
#' combined directory MD5 hash, returned visibly.
#'
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`, regardless of `hash`.
#' @seealso [setup_nasa_token()], [get_token()], [download_run_method()],
#'   [download_hash()], [process_merra2()], [get_merra2_info()]
#' @importFrom Rdpack reprompt
#' @examples
#' \dontrun{
#' # Configure NASA_EARTHDATA_TOKEN with setup_nasa_token() before downloading.
#' download_merra2(
#'   collection = "inst1_2d_int_Nx",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # Download two collections for an inclusive date range.
#' download_merra2(
#'   collection = c("inst1_2d_int_Nx", "tavg1_2d_slv_Nx"),
#'   date = c("2024-01-01", "2024-01-02"),
#'   directory_to_save = file.path(tempdir(), "merra2"),
#'   acknowledgement = TRUE,
#'   max_tries = 3
#' )
#' }
#' @export
# nolint end
download_merra2 <- function(
  collection = c(
    "inst1_2d_asm_Nx",
    "inst1_2d_int_Nx",
    "inst1_2d_lfo_Nx",
    "inst3_3d_asm_Np",
    "inst3_3d_aer_Nv",
    "inst3_3d_asm_Nv",
    "inst3_3d_chm_Nv",
    "inst3_3d_gas_Nv",
    "inst3_2d_gas_Nx",
    "inst6_3d_ana_Np",
    "inst6_3d_ana_Nv",
    "statD_2d_slv_Nx",
    "tavg1_2d_adg_Nx",
    "tavg1_2d_aer_Nx",
    "tavg1_2d_chm_Nx",
    "tavg1_2d_csp_Nx",
    "tavg1_2d_flx_Nx",
    "tavg1_2d_int_Nx",
    "tavg1_2d_lfo_Nx",
    "tavg1_2d_lnd_Nx",
    "tavg1_2d_ocn_Nx",
    "tavg1_2d_rad_Nx",
    "tavg1_2d_slv_Nx",
    "tavg3_3d_mst_Ne",
    "tavg3_3d_trb_Ne",
    "tavg3_3d_nav_Ne",
    "tavg3_3d_cld_Np",
    "tavg3_3d_mst_Np",
    "tavg3_3d_rad_Np",
    "tavg3_3d_tdt_Np",
    "tavg3_3d_trb_Np",
    "tavg3_3d_udt_Np",
    "tavg3_3d_odt_Np",
    "tavg3_3d_qdt_Np",
    "tavg3_3d_asm_Nv",
    "tavg3_3d_cld_Nv",
    "tavg3_3d_mst_Nv",
    "tavg3_3d_rad_Nv",
    "tavg3_2d_glc_Nx",
    "fwi"
  ),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "merra2",
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
  checkmate::assert_character(collection, min.char = 1L)
  checkmate::assert_subset(collection, .amadeus$df_merra2$collection_id)
  # ----------------------------------------------------------------------------
  # Check NASA EarthData token.
  chr_nasatoken <- NULL
  if (any(!grepl("fwi", collection))) {
    chr_nasatoken <- amadeus:::get_token(
      token = nasa_earth_data_token,
      env_var = "NASA_EARTHDATA_TOKEN"
    )
    checkmate::assert_string(chr_nasatoken, min.chars = 1L)
  }
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- amadeus:::sequence_dates(date, hyphen = FALSE)
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- amadeus:::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Store collections as a list.
  list_collections <- as.list(unique(collection))
  # ----------------------------------------------------------------------------
  # Generate download URLs, file names, and collection-specific directories.
  df_download <- do.call(
    rbind,
    lapply(
      list_collections,
      function(x) {
        # ----------------------------------------------------------------------
        # Extract collection-specific metadata.
        df_p <- .amadeus$df_merra2[
          match(x, .amadeus$df_merra2$collection_id),
          ,
          drop = FALSE
        ]
        # ----------------------------------------------------------------------
        # Expand collection and date combinations.
        df_x <- expand.grid(
          base = df_p$base_url,
          collection = x,
          date = chr_dates,
          dir = chr_dir_to_save,
          stringsAsFactors = FALSE
        )
        df_x$year <- substr(df_x$date, 1, 4)
        df_x$month <- substr(df_x$date, 5, 6)
        df_x$vdir <- with(df_x, paste0(dir, collection, "/"))
        # ----------------------------------------------------------------------
        # Construct FWI file names and URLs directly.
        if (x == "fwi") {
          df_x$file <- with(
            df_x,
            paste0(
              "FWI.MERRA2.CORRECTED.Daily.Default.",
              date,
              df_p$file_extension
            )
          )
          df_x$url <- with(df_x, paste0(base, year, "/", file))
          df_x$destfile <- with(df_x, paste0(vdir, file))
          return(df_x)
        }
        # ----------------------------------------------------------------------
        # Construct standard MERRA-2 monthly directory URLs.
        df_x$base <- with(
          df_x,
          paste0(
            base,
            df_p$esdt_name,
            ".",
            df_p$version,
            "/",
            year,
            "/",
            month,
            "/"
          )
        )
        # ----------------------------------------------------------------------
        # Discover and filter daily URLs from monthly base directory.
        amadeus:::discover_merra2(
          df = df_x,
          collection = x,
          max_tries = max_tries,
          rate_limit = rate_limit
        )
      }
    )
  )
  # ----------------------------------------------------------------------------
  # Create missing directories.
  invisible(suppressWarnings(sapply(unique(df_download$vdir), dir.create)))
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = chr_nasatoken,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
#' Discover MERRA-2 download files
#' @description
#' Read monthly HTML directory listings for one standard MERRA-2 collection
#' and build a download table for the requested dates. Uses the filenames
#' published by the server, including their stream and revision prefixes.
#' @param df data.frame. Non-empty table with the following character columns:
#'   \describe{
#'     \item{base}{Monthly HTTP(S) directory URL ending in a forward slash.}
#'     \item{date}{Requested date in \code{"YYYYMMDD"} format.}
#'     \item{vdir}{Local directory in which to save the collection's data
#'      files.}
#'   }
#'   Each \code{base}/\code{date} pair must identify one input row after
#'   identical rows are removed. If a \code{collection} column is present,
#'   it is checked for a single unique character value. The caller is
#'   responsible for ensuring this value matches the \code{collection}
#'   argument, which controls filename matching. Other columns are carried
#'   into the returned table.
#' @param collection character(1). Standard MERRA-2 collection identifier,
#'   such as \code{"inst1_2d_asm_Nx"}. FWI URLs are constructed separately by
#'   [download_merra2()].
#' @param max_tries integer(1). Maximum number of attempts for each listing
#'   request, including the initial attempt. Must be at least 1. Default is 20.
#'   Validated by the caller's \code{download_check()} call.
#' @param rate_limit numeric(1). Minimum seconds between listing requests to
#'   the same host. Must be non-negative. Default is 2; 0 disables throttling.
#'   Validated by the caller's \code{download_check()} call.
#' @param timeout numeric(1). Timeout in seconds for each request attempt.
#'   Must be at least 0.001. Default is 3600. This is not a time limit for
#'   discovery across all directories and retries.
#' @details
#' Each distinct monthly directory is read once, subject to retries for
#' transient HTTP errors and transport failures. HTML listings are parsed
#' with \pkg{xml2} to extract anchor \code{href} attributes and decode HTML
#' entities. This reads links present in the HTML response; JavaScript is not
#' executed. Relative links are resolved against the final response URL after
#' redirects. Download URLs retain their directory paths and query parameters.
#' All matching stream and revision prefixes are retained, including multiple
#' revisions for one date; the helper does not select a preferred revision.
#'
#' Every requested date within each directory must have at least one matching
#' \code{.nc4} data file. XML metadata is optional and is included only when its
#' corresponding data file is also listed. Duplicate URLs are removed.
#' Discovery stops if data dates are missing, a listing request fails after
#' retries, or different URLs in one listing share the same filename.
#'
#' Directory creation and data-file downloads are handled by
#' [download_merra2()] and [download_run_method()].
#' @return
#' A \code{data.frame} retaining the input columns, with one row per discovered
#' data file or associated XML metadata file. The following columns are added
#' or updated:
#' \describe{
#'   \item{file}{Decoded filename, without URL directories or query parameters.}
#'   \item{url}{Absolute download URL resolved from the directory listing.}
#'   \item{vdir}{Destination directory; XML rows use its \code{metadata}
#'     subdirectory.}
#'   \item{destfile}{Local file path formed from \code{vdir} and \code{file}.}
#' }
#' @seealso [download_merra2()], [download_run_method()]
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   base = paste0(
#'     "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/",
#'     "M2I1NXASM.5.12.4/2024/01/"
#'   ),
#'   date = "20240101",
#'   vdir = file.path(tempdir(), "inst1_2d_asm_Nx")
#' )
#' amadeus:::discover_merra2(
#'   df = df,
#'   collection = "inst1_2d_asm_Nx",
#'   max_tries = 3,
#'   timeout = 60
#' )
#' }
#' @keywords internal
discover_merra2 <- function(
  df,
  collection,
  max_tries = 20,
  rate_limit = 2,
  timeout = 3600
) {
  # ----------------------------------------------------------------------------
  # Check arguments.
  checkmate::assert_data_frame(df, min.rows = 1L)
  checkmate::assert_subset(c("base", "date", "vdir"), names(df))
  checkmate::assert_string(collection, pattern = "^[A-Za-z0-9_]+$")
  checkmate::assert_number(timeout, lower = 0.001, finite = TRUE)
  # ----------------------------------------------------------------------------
  # Ensure non-missing columns.
  for (column in c("base", "date", "vdir")) {
    checkmate::assert_character(
      df[[column]],
      any.missing = FALSE,
      min.chars = 1L
    )
  }
  # ----------------------------------------------------------------------------
  # Ensure each base is HTTP(s) and open (e.g. ends in "/" not ".nc4").
  if (any(!grepl("^https?://", df$base)) || any(!endsWith(df$base, "/"))) {
    stop("Each base must be an HTTP(S) directory URL ending in '/'.")
  }
  # ----------------------------------------------------------------------------
  # Parse dates.
  date_parsed <- as.Date(df$date, format = "%Y%m%d")
  if (
    any(!grepl("^[0-9]{8}$", df$date)) ||
      anyNA(date_parsed) ||
      any(format(date_parsed, "%Y%m%d") != df$date)
  ) {
    stop("Dates must be valid YYYYMMDD strings.")
  }
  # ----------------------------------------------------------------------------
  # Ensure single collection per discovery request.
  if ("collection" %in% names(df)) {
    checkmate::assert_character(unique(df$collection), len = 1L)
  }
  # ----------------------------------------------------------------------------
  # Deduplicate identical rows; reject conflicting rows for one base/date.
  df <- unique(as.data.frame(df))
  if (anyDuplicated(df[c("base", "date")])) {
    stop("Each base/date pair must identify exactly one input row.")
  }
  # ----------------------------------------------------------------------------
  # Read child URLs from each monthly base.
  df_download <- do.call(
    rbind,
    lapply(
      unique(df$base),
      function(y) {
        # ----------------------------------------------------------------------
        # Extract base `y`
        df_month <- df[df$base == y, , drop = FALSE]
        # ----------------------------------------------------------------------
        # Retry transport failures and transient HTTP errors. max_tries includes
        # the first attempt; timeout applies to each attempt.
        req <- httr2::request(y) |>
          httr2::req_retry(
            max_tries = max_tries,
            retry_on_failure = TRUE,
            is_transient = function(resp) {
              httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
            },
            backoff = function(i) stats::runif(1) * pmin(i^2, 30)
          ) |>
          httr2::req_timeout(timeout) |>
          httr2::req_options(connecttimeout = min(30, timeout))
        # ----------------------------------------------------------------------
        # Apply throttling limit.
        if (rate_limit > 0) {
          req <- httr2::req_throttle(
            req,
            capacity = 1L,
            fill_time_s = rate_limit,
            realm = paste0("amadeus:merra2:", httr2::url_parse(y)$hostname)
          )
        }
        # ----------------------------------------------------------------------
        # Perform request.
        resp <- tryCatch(
          httr2::req_perform(req),
          error = function(e) {
            stop(
              "Failed to read MERRA-2 listing at ",
              y,
              ": ",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )
        # ----------------------------------------------------------------------
        # Parse the listing as HTML.
        # Handles quoted/unquoted attributes and excludes commented-out links
        # from the selected elements.
        chr_hrefs <- httr2::resp_body_html(resp) |>
          xml2::xml_find_all(".//a[@href]") |>
          xml2::xml_attr("href")
        chr_hrefs <- trimws(chr_hrefs)
        # ----------------------------------------------------------------------
        # Keep relative and HTTP(S) links; navigation fragments are not files.
        chr_hrefs <- unique(chr_hrefs[
          nzchar(chr_hrefs) &
            !startsWith(chr_hrefs, "#") &
            (!grepl("^[A-Za-z][A-Za-z0-9+.-]*:", chr_hrefs) |
              grepl("^https?://", chr_hrefs, ignore.case = TRUE))
        ])
        # ----------------------------------------------------------------------
        # Resolve hrefs against the final response URL; 1 href -> 1 URL.
        chr_listing_url <- httr2::resp_url(resp)
        chr_urls <- unique(vapply(
          chr_hrefs,
          function(href) {
            httr2::req_url_relative(
              httr2::request(chr_listing_url),
              href
            )$url
          },
          character(1),
          USE.NAMES = FALSE
        ))
        # ----------------------------------------------------------------------
        # Match 1 URL -> 1 filename; clean '?query'/'#fragment'.
        chr_files <- vapply(
          chr_urls,
          function(url) {
            utils::URLdecode(basename(httr2::url_parse(url)$path))
          },
          character(1),
          USE.NAMES = FALSE
        )
        # ----------------------------------------------------------------------
        # Enforce file naming convention:
        # MERRA2_<3 digits>.<collection>.<8-digit date>.nc4[.xml].
        chr_pattern <- paste0(
          "^MERRA2_[0-9]{3}\\.",
          collection,
          "\\.([0-9]{8})\\.nc4(\\.xml)?$"
        )
        # ----------------------------------------------------------------------
        # Filter to to-download URL and filename pairs.
        lgl_keep <- grepl(chr_pattern, chr_files)
        chr_urls <- chr_urls[lgl_keep]
        chr_files <- chr_files[lgl_keep]
        # ----------------------------------------------------------------------
        # Replace complete match with '\\1' for caputred date.
        chr_file_dates <- sub(chr_pattern, "\\1", chr_files)
        lgl_keep <- chr_file_dates %in% df_month$date
        chr_urls <- chr_urls[lgl_keep]
        chr_files <- chr_files[lgl_keep]
        chr_file_dates <- chr_file_dates[lgl_keep]
        # ----------------------------------------------------------------------
        # Require .nc4 data for every requested date; stop on partial result.
        lgl_data <- endsWith(chr_files, ".nc4")
        chr_missing <- setdiff(df_month$date, chr_file_dates[lgl_data])
        if (length(chr_missing) > 0L) {
          stop(
            "Missing MERRA-2 data for ",
            paste(chr_missing, collapse = ", "),
            " in: ",
            y,
            call. = FALSE
          )
        }
        lgl_keep <- lgl_data |
          sub("\\.xml$", "", chr_files) %in% chr_files[lgl_data]
        chr_urls <- chr_urls[lgl_keep]
        chr_files <- chr_files[lgl_keep]
        chr_file_dates <- chr_file_dates[lgl_keep]
        # ----------------------------------------------------------------------
        # Check for duplicates after duplicate filtering above.
        if (anyDuplicated(chr_files)) {
          stop("Multiple URLs map to the same file name in: ", y, call. = FALSE)
        }
        # ----------------------------------------------------------------------
        # Return files, URLs, collection directory, and destination file.
        df_y <- df_month[match(chr_file_dates, df_month$date), , drop = FALSE]
        df_y$file <- chr_files
        df_y$url <- chr_urls
        lgl_meta <- endsWith(df_y$file, ".xml")
        df_y$vdir[lgl_meta] <- file.path(df_y$vdir[lgl_meta], "metadata")
        df_y$destfile <- file.path(df_y$vdir, df_y$file)
        df_y
      }
    )
  )
  # ----------------------------------------------------------------------------
  # Drop row names.
  rownames(df_download) <- NULL
  # ----------------------------------------------------------------------------
  # Return prepared data.frame.
  df_download
}

################################################################################
# {process} tier
################################################################################
#' Process meteorological and atmospheric data
#' @description
#' The \code{process_merra2()} function imports and cleans raw atmospheric,
#' meteorological, and MERRA2-based Fire Weather Index data, returning a single
#' `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). MERRA2 variable name(s). For daily corrected
#'   Fire Weather Index files (`collection = "fwi"` during download), use one
#'   of `"DC"`, `"DMC"`, `"FFMC"`, `"ISI"`, `"BUI"`, or `"FWI"` (or the full raw
#'   layer name).
#' @param path character(1). Directory with downloaded netCDF (`.nc4` or `.nc`)
#'   files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves the original sub-daily
#'   output. Aggregation groups layers by variable/level and date. Silently
#'   ignored for FWI collections, which are already daily.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
#'   `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour for standard MERRA-2 collections when
#' `daily_agg = FALSE` (default). When `daily_agg = TRUE`, layer names contain
#' the variable, pressure level, and date only, and `terra::time()` is set to
#' midnight UTC of each date. For daily Fire Weather Index files, layer names
#' contain the variable and date only regardless of `daily_agg`.
#' Pressure level values utilized for layer names are taken directly from raw
#' data and are not edited to retain pressure level information.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra tapp
#' @importFrom terra time
#' @importFrom terra names
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' merra2 <- process_merra2(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "CPT",
#'   path = "./data/inst1_2d_int_Nx"
#' )
#' ## daily mean CPT
#' merra2_daily <- process_merra2(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "CPT",
#'   path = "./data/inst1_2d_int_Nx",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
process_merra2 <-
  function(
    date = c("2018-01-01", "2018-01-10"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    daily_agg = FALSE,
    fun = "mean",
    ...
  ) {
    #### directory setup
    path <- amadeus:::download_sanitize_path(path)
    #### check for variable
    amadeus:::check_for_null_parameters(mget(ls()))
    #### check dates
    date <- if (length(date) == 1) rep(date, 2) else date
    stopifnot(length(date) == 2)
    date <- date[order(as.Date(date))]
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "MERRA2_400|^FWI\\.",
      full.names = TRUE
    )
    paths <- paths[grep(
      "\\.nc4?$",
      paths
    )]
    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
    )
    #### subset file paths to only dates of interest
    data_paths <- unique(
      grep(
        paste(
          dates_of_interest,
          collapse = "|"
        ),
        paths,
        value = TRUE
      )
    )
    if (length(data_paths) == 0) {
      stop("No MERRA2 files matching the requested date were found.\n")
    }
    #### identify collection
    collection <- amadeus::process_collection(
      data_paths[1],
      source = "merra2",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_list <- vector("list", length(data_paths))
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p], win = extent)
      data_date <- amadeus::process_collection(
        data_paths[p],
        source = "merra2",
        date = TRUE
      )
      message(
        paste0(
          "Cleaning ",
          variable,
          " data for ",
          as.Date(
            data_date,
            format = "%Y%m%d"
          ),
          "...\n"
        )
      )
      #### subset to user-selected variable
      data_variable_index <- grep(
        variable,
        names(data_raw),
        fixed = TRUE
      )
      if (length(data_variable_index) == 0) {
        data_variable_index <- grep(
          variable,
          names(data_raw)
        )
      }
      if (length(data_variable_index) == 0) {
        stop(
          paste0(
            "Requested variable ",
            variable,
            " was not found in ",
            basename(data_paths[p]),
            ".\n"
          )
        )
      }
      data_variable <- terra::subset(
        data_raw,
        subset = data_variable_index
      )
      if (collection == "fwi") {
        #### set layer names for daily GlobalFWI files
        names(data_variable) <- paste0(
          gsub("_", ".", names(data_variable)),
          "_",
          data_date
        )
        #### set layer times
        terra::time(data_variable) <- ISOdatetime(
          year = substr(data_date, 1, 4),
          month = substr(data_date, 5, 6),
          day = substr(data_date, 7, 8),
          hour = 0,
          min = 0,
          sec = 0,
          tz = "UTC"
        )
      } else {
        #### identify time step
        times <- amadeus::process_merra2_time(
          collection = collection,
          from = data_variable
        )
        #### identify unique pressure levels
        levels <-
          unique(
            grep(
              "lev=",
              unlist(
                strsplit(names(data_variable), "_")
              ),
              value = TRUE
            )
          )
        #### empty `levels` if 2 dimensional data
        if (length(levels) == 0) {
          levels <- ""
        }
        #### merge levels and times
        leveltimes <- merge(levels, times)
        #### set layer names
        names(data_variable) <- gsub(
          "__",
          "_",
          paste0(
            variable,
            "_",
            leveltimes[, 1],
            "_",
            data_date,
            "_",
            leveltimes[, 2]
          )
        )
        #### set layer times
        terra::time(data_variable) <- ISOdatetime(
          year = substr(data_date, 1, 4),
          month = substr(data_date, 5, 6),
          day = substr(data_date, 7, 8),
          hour = substr(leveltimes[, 2], 1, 2),
          min = substr(leveltimes[, 2], 3, 4),
          sec = substr(leveltimes[, 2], 5, 6),
          tz = "UTC"
        )
      }
      #### store for one-shot stack creation
      data_list[[p]] <- data_variable
    }
    #### combine all layers in one call
    data_return <- terra::rast(data_list)
    if (collection == "fwi") {
      terra::crs(data_return) <- "EPSG:4326"
    } else {
      terra::crs(data_return) <- "EPSG:4267"
    }
    #### optional daily aggregation (not applicable to already-daily FWI)
    if (isTRUE(daily_agg) && collection != "fwi") {
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
      var_prefix <- sub("_[0-9]{8}.*$", "", names(data_return))
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
      ifelse(
        collection == "fwi" || isTRUE(daily_agg),
        "daily ",
        "hourly "
      ),
      variable,
      " data from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ".\n"
    ))
    #### return SpatRaster
    return(data_return)
  }

################################################################################
#' Process MERRA2 time steps
#' @description
#' Identify the time step of data observations based on MERRA2 collection and
#' filter to time values in `from`.
#' @param collection character(1). MERRA2 collection name.
#' @param from SpatRaster(1). Object to extract time values from.
#' @importFrom stringi stri_pad
#' @keywords internal auxiliary
#' @return character
#' @export
process_merra2_time <-
  function(collection, from) {
    split <- unlist(strsplit(collection, "_"))
    code <- split[1]
    if (code == "inst1") {
      step <- seq(from = 0, to = 2300, by = 100)
    } else if (code == "inst3") {
      step <- seq(from = 0, to = 2100, by = 300)
    } else if (code == "inst6") {
      step <- seq(from = 0, to = 1800, by = 600)
    } else if (code == "statD") {
      step <- 1200
    } else if (code == "tavg1") {
      step <- seq(from = 0030, to = 2330, by = 100)
    } else if (code == "tavg3") {
      step <- seq(from = 0130, to = 2330, by = 300)
    } else if (collection == "fwi") {
      step <- 0000
    }
    pad_l <- stringi::stri_pad(step, side = "left", width = 4, pad = 0)
    pad_r <- stringi::stri_pad(pad_l, side = "right", width = 6, pad = 0)
    time_f <- gsub(
      " ",
      "",
      gsub(
        "-",
        "",
        gsub(
          ":",
          "",
          terra::time(from)
        )
      )
    )
    for (f in seq_along(time_f)) {
      if (nchar(time_f[f]) == 8) {
        time_f[f] <- paste0(time_f[f], "000000")
      }
    }
    time_return <- pad_r[
      pad_r %in% unique(substr(time_f, 9, 14))
    ]
    return(time_return)
  }

################################################################################
#' Get MERRA2 variable lookup information
#' @description
#' Returns a lookup table of available MERRA2 collection and variable selectors
#' from locally downloaded MERRA2 netCDF files. This helper inspects layer
#' metadata only and does not read raster values into memory.
#' @param path character(1+) Path(s) to MERRA2 file(s) and/or directory(ies)
#'   containing MERRA2 `.nc4` files (and optional FWI `.nc` files).
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each collection-variable row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with MERRA2 collection and variable selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_merra2_info(path = "./data/merra2")
#' get_merra2_info(path = "./data/merra2", include_file = TRUE)
#' }
#' @importFrom terra rast
#' @export
get_merra2_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "(MERRA2_400\\..*\\.nc4$|FWI\\..*\\.nc$)",
    source_name = "MERRA2 netCDF"
  )

  out_rows <- lapply(files, function(f) {
    data_raw <- terra::rast(f)
    collection <- unique(amadeus::process_collection(
      f,
      source = "merra2",
      collection = TRUE
    ))
    vars <- info_normalize_layer_variables(names(data_raw))
    if (length(collection) == 1L && collection == "fwi") {
      vars <- sub("^MERRA2\\.CORRECTED_", "", vars)
    }
    vars <- sort(unique(vars[nzchar(vars)]))
    if (length(vars) == 0L || length(collection) == 0L) {
      return(NULL)
    }
    data.frame(
      collection = rep(collection[1], length(vars)),
      variable = vars,
      file = rep(f, length(vars)),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop(
      "No MERRA2 collection-variable metadata could be derived from `path`.\n"
    )
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("collection", "variable"), drop = FALSE])
    out <- out[order(out$collection, out$variable), , drop = FALSE]
  } else {
    out <- unique(out[, c("collection", "variable", "file"), drop = FALSE])
    out <- out[order(out$collection, out$variable, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate meteorological and atmospheric covariates
#' @description
#' Extract meteorological and atmospheric values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and meteorological or atmospheric variable. Variable column
#' name reflects variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_merra2()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [calculate_geos()], [process_merra2()]
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_merra2(
#'   from = merra2, # derived from process_merra2() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_merra2 <- function(
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
  #### identify pressure level or monolevel data
  merra2_name <- strsplit(names(from)[1], "_")[[1]]
  if (grepl("lev", names(from)[1])) {
    merra2_time <- c(3, 4)
    merra2_level <- 2
    merra2_time_type <- "hour"
  } else if (length(merra2_name) == 2) {
    merra2_time <- 2
    merra2_level <- NULL
    merra2_time_type <- "date"
  } else {
    merra2_time <- c(2, 3)
    merra2_level <- NULL
    merra2_time_type <- "hour"
  }
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "merra2",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = merra2_time,
    time_type = merra2_time_type,
    level = merra2_level,
    weights = weights,
    ...
  )
  #### optional `.by_time` summarization
  merra2_group_extra <- if (!is.null(merra2_level)) "level" else NULL
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = merra2_group_extra
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
  #### return data.frame
  return(sites_return)
}
################################################################################
