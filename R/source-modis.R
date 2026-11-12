################################################################################
# {amadeus} functions for MODIS and Black Marble products.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download MODIS product files using the shared download workflow
#' @description
#' Query NASA CMR for file URLs, prepare a download table, and download MODIS
#' or VIIRS files using the shared validation, preview, download, and return
#' helpers.
#' @param product character(1). Supported MODIS or VIIRS product code. The
#'   default selects the first listed product.
#' @param version character(1). Requested CMR version, default `"061"`.
#'   Existing product-specific overrides are retained; see Details.
#' @param nasa_earth_data_token character(1) or NULL. NASA Earthdata token,
#'   token-file path, or `NULL` to use `NASA_EARTHDATA_TOKEN`. Resolved by
#'   [get_token()], which checks the environment variable first.
#' @param date character(1 or 2). Single date or range endpoints in
#'   `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.
#' @param extent numeric(4). Bounding box in decimal degrees, ordered
#'   `c(west, south, east, north)`. Default covers the continental US.
#' @param directory_to_save character(1). Non-empty output directory. Files
#'   are saved directly in this directory.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed.
#' @param download logical(1). Deprecated. `FALSE` warns and returns a preview
#'   after token lookup, directory creation, and discovery. Discovery still
#'   queries CMR in preview mode; data files are not downloaded.
#' @param remove_command logical(1). Deprecated and ignored; `TRUE` warns.
#' @param show_progress logical(1). Show per-file download progress. Discovery
#'   and summary messages may still be printed when `FALSE`.
#' @param hash logical(1). Return the combined directory MD5 hash when `TRUE`.
#'   Ignored when `download = FALSE`.
#' @param max_tries integer(1). Maximum attempts per CMR or file request,
#'   including the first attempt. Default is 20.
#' @param rate_limit numeric(1). Non-negative request-throttling interval in
#'   seconds, passed to discovery and downloads. Default is 2. Discovery uses
#'   a capacity-one bucket; 0 disables discovery throttling. File downloads
#'   follow the throttling policy in [download_run_method()].
#' @details
#' Product-specific version overrides and a CM1-to-A2 fallback are applied
#' before filename date filtering. Files use a flat destination directory.
#' MOD06_L2 uses `"6.1"`; MOD14CM1/MYD14CM1 use `"005"`; MCD64CMQ uses
#' `"006"`; MCD14ML uses `"6.1NRT"`; VNP46A2/VNP64A1 omit the version filter.
#' An empty CM1 response triggers the existing A2 version `"006"` fallback.
#'
#' Cross-year ranges remain restricted to MOD06_L2, MOD14CM1, MYD14CM1,
#' MCD14ML, MCD64A1, MCD64CMQ, and VNP64A1. Discovery retrieves successive CMR
#' pages of up to 2000 granules using the `CMR-Search-After` response header.
#' There is no 31-day warning or restriction. The query parameters, retry
#' settings, timeout, and throttle are retained across pages.
#'
#' When supplied, `CMR-Hits` is checked against the number of retrieved granule
#' records before link or date filtering. Discovery stops if a page request
#' fails, the reported count changes, a required continuation value is missing
#' or repeated, or the final record count does not match `CMR-Hits`.
#'
#' CMR supplies actual file URLs, so a product/date Cartesian grid is not
#' needed. The inclusive date sequence is used to establish ordered query
#' endpoints. A file count is reported without claiming daily or monthly
#' completeness for tiled, swath, or composite products.
#'
#' Discovery keeps the existing 120-second per-attempt timeout; file downloads
#' use [download_run_method()]'s default timeout.
#' @return
#' With `download = FALSE`, an invisible preview list containing `urls`,
#' `destfiles`, and `n_files`. Otherwise, with `hash = FALSE`, the invisible
#' download summary from [download_run_method()]. With `hash = TRUE`, a visible
#' combined MD5 hash for all files under `directory_to_save`.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @author Mitchell Manware, Insang Song
#' @seealso [download_data()], [download_merra2()], [download_run_method()]
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_mcd19a22021}{amadeus}
#' \insertRef{data_mod06l2_2017}{amadeus}
#' \insertRef{data_mod09ga2021}{amadeus}
#' \insertRef{data_mod11a12021}{amadeus}
#' \insertRef{data_mod13a22021}{amadeus}
#' \insertRef{article_roman2018vnp46}{amadeus}
#' @examples
#' \dontrun{
#' download_modis(
#'   product = "MOD09GA",
#'   date = c("2024-01-01", "2024-01-03"),
#'   extent = c(-80, 35, -75, 40),
#'   directory_to_save = file.path(tempdir(), "modis"),
#'   acknowledgement = TRUE,
#'   download = FALSE
#' )
#' }
#' @export
download_modis <- function(
  product = c(
    "MOD09GA",
    "MYD09GA",
    "MOD09GQ",
    "MYD09GQ",
    "MOD09A1",
    "MYD09A1",
    "MOD09Q1",
    "MYD09Q1",
    "MOD11A1",
    "MYD11A1",
    "MOD11A2",
    "MYD11A2",
    "MOD11B1",
    "MYD11B1",
    "MOD13A1",
    "MYD13A1",
    "MOD13A2",
    "MYD13A2",
    "MOD13Q1",
    "MYD13Q1",
    "MOD13A3",
    "MYD13A3",
    "MCD12Q1",
    "MOD14A1",
    "MYD14A1",
    "MOD14A2",
    "MYD14A2",
    "MOD14CM1",
    "MYD14CM1",
    "MOD16A2",
    "MYD16A2",
    "MCD64A1",
    "MCD64CMQ",
    "MOD06_L2",
    "MCD14ML",
    "MCD19A2",
    "VNP46A2",
    "VNP64A1"
  ),
  version = "061",
  nasa_earth_data_token = NULL,
  date = c("2023-09-01", "2023-09-01"),
  extent = c(-125, 22, -64, 50),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  download_check(
    dataset_name = "modis",
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
  product <- match.arg(product)
  checkmate::assert_string(version, min.chars = 1L)
  checkmate::assert_numeric(
    extent,
    len = 4L,
    any.missing = FALSE,
    finite = TRUE
  )
  checkmate::assert_numeric(extent[c(1L, 3L)], lower = -180, upper = 180)
  checkmate::assert_numeric(extent[c(2L, 4L)], lower = -90, upper = 90)
  checkmate::assert_true(extent[2L] < extent[4L])
  # ----------------------------------------------------------------------------
  # Check NASA Earthdata token, including when a preview is requested.
  chr_nasatoken <- get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )
  checkmate::assert_string(chr_nasatoken, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Check `date` and generate the same inclusive sequence as other downloaders.
  # CMR takes range endpoints, not one query per requested day. Repeat the sole
  # date for a one-day request; otherwise select the first and last dates.
  chr_dates <- sequence_dates(date, hyphen = TRUE)
  chr_date_range <- chr_dates[c(1L, length(chr_dates))]
  # ----------------------------------------------------------------------------
  # Retain the existing cross-year policy during the consistency migration.
  if (
    substr(chr_date_range[1L], 1L, 4L) != substr(chr_date_range[2L], 1L, 4L) &&
      !product %in%
        c(
          "MOD06_L2",
          "MOD14CM1",
          "MYD14CM1",
          "MCD14ML",
          "MCD64A1",
          "MCD64CMQ",
          "VNP64A1"
        )
  ) {
    stop("dates should be in the same year.\n")
  }
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Resolve the existing CMR version overrides in one place. Explicit NULL
  # omits the version query parameter; it does not mean "latest version".
  chr_version <- switch(
    product,
    MOD06_L2 = "6.1",
    MOD14CM1 = "005",
    MYD14CM1 = "005",
    MCD64CMQ = "006",
    MCD14ML = "6.1NRT",
    VNP46A2 = NULL,
    VNP64A1 = NULL,
    version
  )
  # ----------------------------------------------------------------------------
  # Discover actual URLs and construct the common download table.
  df_download <- discover_modis(
    product = product,
    version = chr_version,
    date = chr_date_range,
    extent = extent,
    directory_to_save = chr_dir_to_save,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Run download using the same retry and rate-limit inputs as discovery.
  download_result <- download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = chr_nasatoken,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
#' Discover MODIS files and build a download table
#' @description
#' Query CMR for one product and return a download table. Keeps the current
#' MODIS fallback, first-data-link selection, and filename date filtering.
#' @param product character(1). Validated product code.
#' @param version character(1) or NULL. Resolved CMR version. `NULL` omits the
#'   version filter. Product-specific overrides are handled by the caller.
#' @param date character(2). Validated, ordered range endpoints in
#'   `"YYYY-MM-DD"` format. For one day, supply that date twice.
#' @param extent numeric(4). Validated bounding box, ordered
#'   `c(west, south, east, north)`.
#' @param directory_to_save character(1). Sanitized destination directory,
#'   ending in a forward slash, as returned by [download_setup_dir()].
#' @param max_tries integer(1). Maximum attempts per CMR request, including
#'   the initial attempt. Validated by the caller's `download_check()` call.
#' @param rate_limit numeric(1). Minimum seconds between CMR requests. Zero
#'   disables throttling. Validated by the caller's `download_check()` call.
#' @param timeout numeric(1). Per-attempt timeout in seconds, at least 0.001.
#'   Default is 120, retaining the original MODIS discovery timeout.
#' @details
#' `query_modis_granules()` retrieves and checks the complete CMR result set
#' before this function selects file URLs. Query parameters and request
#' controls are passed explicitly for the original and fallback products.
#'
#' CM1 queries returning no entries use the existing A2 version `"006"`
#' fallback, which starts a fresh paginated query for another product. The
#' download table records the product/version used for the successful query;
#' an omitted version is recorded as `NA_character_`.
#'
#' Each granule can contain several links. The first link whose relation
#' contains `"data#"` and whose URL ends in an accepted extension is retained.
#' MCD14ML accepts `.txt`; other products accept `.hdf` or `.h5`. Granules with
#' no matching link are omitted. Published URLs and their order are retained.
#'
#' Revised version/fallback rules, malformed links, URL query strings, and
#' duplicate destination handling remain for the later bug pass.
#' Filename date filtering reuses the existing MODIS helper shared with the
#' processing tier. No daily or monthly completeness requirement is imposed.
#' @return A data.frame with `product`, `version`, `file`, `url`, `vdir`, and
#'   `destfile` columns, one row per selected granule. Stops if the CMR request
#'   fails, pagination cannot be completed, or no URLs remain before or after
#'   date filtering.
#' @seealso [download_modis()], `query_modis_granules()`, `discover_merra2()`
#' @keywords internal
#' @noRd
discover_modis <- function(
  product,
  version,
  date,
  extent,
  directory_to_save,
  max_tries = 20,
  rate_limit = 2,
  timeout = 120
) {
  # ----------------------------------------------------------------------------
  # Query the requested product/version first.
  message("Querying NASA CMR for available granules...\n")
  list_granules <- query_modis_granules(
    product = product,
    version = version,
    date = date,
    extent = extent,
    max_tries = max_tries,
    rate_limit = rate_limit,
    timeout = timeout
  )
  # ----------------------------------------------------------------------------
  # Preserve the legacy fallback. This substitutes another science product;
  # it is not a second mirror of the originally requested CM1 files.
  if (product %in% c("MOD14CM1", "MYD14CM1") && length(list_granules) == 0L) {
    chr_fallback <- switch(
      product,
      MOD14CM1 = "MOD14A2",
      MYD14CM1 = "MYD14A2"
    )
    message(sprintf(
      "No granules found for %s; retrying with %s (v006).\n",
      product,
      chr_fallback
    ))
    list_granules <- query_modis_granules(
      product = chr_fallback,
      version = "006",
      date = date,
      extent = extent,
      max_tries = max_tries,
      rate_limit = rate_limit,
      timeout = timeout
    )
    # Record the actual fallback query in the returned table.
    product <- chr_fallback
    version <- "006"
  }
  # ----------------------------------------------------------------------------
  # Decide the filename extension once, rather than once for every CMR link.
  # `\\.` matches a literal period; `$` requires the extension at the URL end.
  chr_file_pattern <- if (product == "MCD14ML") {
    "\\.txt$"
  } else {
    "\\.(hdf|h5)$"
  }
  # ----------------------------------------------------------------------------
  # A granule can include data, metadata, and browse-image links. Filter()
  # keeps the links for which this predicate returns TRUE, in their input order.
  is_data_link <- function(link) {
    grepl("data#", link$rel) &&
      grepl(chr_file_pattern, link$href, ignore.case = TRUE)
  }
  # ----------------------------------------------------------------------------
  # Preserve the existing first-match policy: one URL per granule, or NA when
  # no data link matches. The scalar return makes vapply()'s type explicit.
  first_data_url <- function(granule) {
    links <- Filter(is_data_link, granule$links)
    if (length(links) == 0L) {
      return(NA_character_)
    }
    links[[1L]]$href
  }
  chr_urls <- vapply(list_granules, first_data_url, character(1))
  chr_urls <- chr_urls[!is.na(chr_urls)]
  if (length(chr_urls) == 0L) {
    stop("No granules found for the specified query parameters.\n")
  }
  # ----------------------------------------------------------------------------
  # The existing helper returns the selected paths in their original order.
  # Assign that result directly; a second membership filter is unnecessary.
  chr_urls <- modis_filter_paths_by_date(chr_urls, date = date)
  # An unrecognized filename date can yield NA in the helper's subset. Drop
  # those placeholders, as the original membership filter did implicitly.
  chr_urls <- chr_urls[!is.na(chr_urls)]
  if (length(chr_urls) == 0L) {
    stop("No granules matched the requested date range.\n")
  }
  # ----------------------------------------------------------------------------
  # Build one row per discovered file, preserving the current flat layout.
  # NA records an unspecified version; it must not be labeled as "latest".
  df_download <- data.frame(
    product = product,
    version = if (is.null(version)) NA_character_ else version,
    file = basename(chr_urls),
    url = chr_urls,
    vdir = directory_to_save,
    stringsAsFactors = FALSE
  )
  df_download$destfile <- with(df_download, paste0(vdir, file))
  # ----------------------------------------------------------------------------
  # Count files only: several tiles may share a date, and a filename date can
  # label a multi-day composite. File counts do not demonstrate completeness.
  message(sprintf(
    "Found %d MODIS files in the queried date range.\n",
    nrow(df_download)
  ))
  df_download
}

################################################################################
#' Query CMR for MODIS granule records
#' @description
#' Retrieve the complete CMR result set for one product and version. Handles
#' request construction, retries, throttling, pagination, and record counts.
#' `discover_modis()` handles fallback products, file-link selection, date
#' filtering, and destination paths after the query returns.
#' @param product character(1). Validated MODIS or VIIRS product short name.
#' @param version character(1) or NULL. Resolved CMR version. `NULL` omits the
#'   version filter. Product-specific overrides are handled by the caller.
#' @param date character(2). Validated, ordered range endpoints in
#'   `"YYYY-MM-DD"` format. For one day, supply that date twice.
#' @param extent numeric(4). Validated bounding box, ordered
#'   `c(west, south, east, north)`.
#' @param max_tries integer(1). Maximum attempts per CMR page request,
#'   including the initial attempt. Validated by `download_check()`.
#' @param rate_limit numeric(1). Minimum seconds between CMR requests to the
#'   same host; 0 disables throttling. Validated by `download_check()`.
#' @param timeout numeric(1). Per-attempt timeout in seconds, at least 0.001.
#'   Default is 120. Applies separately to each page request and retry.
#' @details
#' Each query reads successive pages of up to 2000 granules. The opaque
#' `CMR-Search-After` response header is sent unchanged with the next request;
#' the product, version, temporal bounds, and spatial extent remain fixed.
#' A short or empty page, or reaching `CMR-Hits`, ends the query. A full page
#' with an unknown or incomplete total requires a new continuation value.
#' Missing or repeated continuation values raise an error in that case.
#'
#' If `CMR-Hits` is supplied, it must remain constant across pages and match
#' the final number of retrieved records. This check precedes link and date
#' filtering, which may legitimately retain fewer URLs. CMR is a live catalog;
#' the count check does not guarantee a fixed snapshot if records change while
#' paging. A failed page request or count mismatch stops the query.
#'
#' Each call has its own page and continuation state. The shared throttle realm
#' spaces requests to the CMR host across calls, including fallback queries.
#' @return A list of CMR granule records in page order, preserving each record's
#'   nested metadata and links. Returns `NULL` when the query finds no records.
#'   This helper creates no directories and downloads no data files.
#' @seealso `discover_modis()`, [download_modis()]
#' @keywords internal
#' @noRd
query_modis_granules <- function(
  product,
  version,
  date,
  extent,
  max_tries = 20,
  rate_limit = 2,
  timeout = 120
) {
  # ----------------------------------------------------------------------------
  # Shared inputs have already been validated by download_modis(). Only the
  # query-specific timeout needs an additional check here.
  checkmate::assert_number(timeout, lower = 0.001, finite = TRUE)
  # ----------------------------------------------------------------------------
  # Encode this query's explicit bounds once and reuse them for every page.
  chr_extent <- paste(extent, collapse = ",")
  chr_temporal <- paste(date, collapse = ",")
  chr_base <- "https://cmr.earthdata.nasa.gov/search/granules.json"
  int_page_size <- 2000L
  # --------------------------------------------------------------------------
  # CMR returns published URLs in JSON. Do not infer tile names, production
  # timestamps, or download-host directory layouts from dates.
  req <- httr2::request(chr_base) |>
    httr2::req_url_query(
      short_name = product,
      version = version,
      temporal = chr_temporal,
      bounding_box = chr_extent,
      page_size = int_page_size
    ) |>
    httr2::req_retry(max_tries = max_tries, retry_on_failure = TRUE) |>
    httr2::req_timeout(timeout) |>
    httr2::req_options(connecttimeout = min(30, timeout))
  # --------------------------------------------------------------------------
  # Match MERRA-2 discovery's capacity-one throttle. A shared realm also
  # spaces the fallback and subsequent discovery calls to this CMR host.
  if (rate_limit > 0) {
    req <- httr2::req_throttle(
      req,
      capacity = 1L,
      fill_time_s = rate_limit,
      realm = paste0("amadeus:modis:", httr2::url_parse(chr_base)$hostname)
    )
  }
  # --------------------------------------------------------------------------
  # Collect whole pages, then combine them once. Count raw granule records:
  # some records will later be excluded by the link or filename date filters.
  list_pages <- list()
  chr_cursors <- character()
  num_received <- 0
  num_hits <- NA_real_
  repeat {
    # ------------------------------------------------------------------------
    # Every page uses the same request settings. A failure on any page stops
    # discovery so a partial set cannot reach the download or preview step.
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        stop(
          "Failed to query NASA CMR (cmr.earthdata.nasa.gov). ",
          "Check network connectivity and NASA EarthData status at ",
          "https://status.earthdata.nasa.gov. Original error: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    # ------------------------------------------------------------------------
    # Read the total before filtering. If the catalog's reported total changes
    # between pages, ask for a fresh query instead of claiming completeness.
    chr_hits <- httr2::resp_header(resp, "CMR-Hits")
    if (!is.null(chr_hits)) {
      num_page_hits <- suppressWarnings(as.numeric(chr_hits))
      checkmate::assert_number(num_page_hits, lower = 0, finite = TRUE)
      if (!is.na(num_hits) && num_page_hits != num_hits) {
        stop(
          "CMR result count changed during pagination; retry the query.",
          call. = FALSE
        )
      }
      num_hits <- num_page_hits
    }
    # ------------------------------------------------------------------------
    # Each page is a list of granule records, each with its own nested links.
    list_page <- httr2::resp_body_json(resp)$feed$entry
    int_count <- length(list_page)
    if (int_count > 0L) {
      list_pages[[length(list_pages) + 1L]] <- list_page
    }
    num_received <- num_received + int_count
    # ------------------------------------------------------------------------
    # A short page ends the search. A known total also avoids an unnecessary
    # empty-page request when the final page contains exactly 2000 records.
    # The count check below rejects a short page that arrived prematurely.
    if (
      int_count < int_page_size ||
        (!is.na(num_hits) && num_received >= num_hits)
    ) {
      break
    }
    # ------------------------------------------------------------------------
    # "Search after" identifies where the next batch starts. Keep this opaque
    # value exactly as supplied by CMR; do not parse or reconstruct its JSON.
    chr_cursor <- httr2::resp_header(resp, "CMR-Search-After")
    if (is.null(chr_cursor) || !nzchar(chr_cursor)) {
      stop(
        "CMR pagination is incomplete: missing CMR-Search-After header.",
        call. = FALSE
      )
    }
    # ------------------------------------------------------------------------
    # Detect a repeated continuation value rather than requesting the same
    # batch indefinitely. Only this header changes for the next request.
    if (chr_cursor %in% chr_cursors) {
      stop(
        "CMR pagination is incomplete: repeated CMR-Search-After value.",
        call. = FALSE
      )
    }
    chr_cursors <- c(chr_cursors, chr_cursor)
    req <- httr2::req_headers(req, `CMR-Search-After` = chr_cursor)
  }
  # --------------------------------------------------------------------------
  # The retrieved raw record count must match the advertised total, when
  # available. Do not compare CMR-Hits with the filtered number of file URLs.
  if (!is.na(num_hits) && num_received != num_hits) {
    stop(
      sprintf(
        "CMR pagination is incomplete: retrieved %d of %d granule records.",
        num_received,
        num_hits
      ),
      call. = FALSE
    )
  }
  message(sprintf(
    "Retrieved %d CMR granule records for %s.\n",
    num_received,
    product
  ))
  # --------------------------------------------------------------------------
  # Concatenate only the page lists; preserve the nested metadata and links
  # within each granule. An empty query returns no entries for fallback logic.
  do.call(c, list_pages)
}

################################################################################
modis_extract_temporal_key <- function(path) {
  # ----------------------------------------------------------------------------
  # Extract the filename from the supplied path.
  filename <- basename(path)
  # ----------------------------------------------------------------------------
  # Read the year and day-of-year key from an AYYYYDDD filename pattern.
  key_daily <- regmatches(
    filename,
    regexpr("A20\\d{5}", filename)
  )
  if (length(key_daily) > 0 && nzchar(key_daily)) {
    return(sub("^A", "", key_daily))
  }
  # ----------------------------------------------------------------------------
  # Read the year and day-of-year key from a _YYYYDDD filename pattern.
  key_txt <- regmatches(
    filename,
    regexpr("_[12][0-9]{6}", filename)
  )
  if (length(key_txt) > 0 && nzchar(key_txt)) {
    return(sub("^_", "", key_txt))
  }
  # ----------------------------------------------------------------------------
  # Read the year and month key from a .YYYYMM. filename pattern.
  key_monthly <- regmatches(
    filename,
    regexpr("\\.20[0-9]{4}\\.", filename)
  )
  if (length(key_monthly) > 0 && nzchar(key_monthly)) {
    return(gsub("\\.", "", key_monthly))
  }
  # ----------------------------------------------------------------------------
  # Return a missing key when no supported pattern is found.
  return(NA_character_)
}

################################################################################
modis_extract_temporal_scale <- function(path) {
  # ----------------------------------------------------------------------------
  # Extract the filename from the supplied path.
  filename <- basename(path)
  # ----------------------------------------------------------------------------
  # Classify supported filename patterns as daily or monthly.
  if (grepl("A20\\d{5}", filename)) {
    return("daily")
  }
  if (grepl("_[12][0-9]{6}", filename)) {
    return("daily")
  }
  if (grepl("\\.20[0-9]{4}\\.", filename)) {
    return("monthly")
  }
  # ----------------------------------------------------------------------------
  # Return a missing scale when no supported pattern is found.
  return(NA_character_)
}

################################################################################
modis_key_to_date <- function(
  key,
  scale
) {
  # ----------------------------------------------------------------------------
  # Repeat a single temporal scale and check that each key has a scale.
  if (length(scale) == 1L) {
    scale <- rep(scale, length(key))
  }
  stopifnot(length(key) == length(scale))
  # ----------------------------------------------------------------------------
  # Convert each temporal key according to its scale.
  as_date <- mapply(
    function(key_i, scale_i) {
      # ------------------------------------------------------------------------
      # Preserve missing keys or scales as missing dates.
      if (is.na(key_i) || is.na(scale_i)) {
        return(as.Date(NA))
      }
      # ------------------------------------------------------------------------
      # Parse daily keys as a year and day of year.
      if (scale_i == "daily") {
        return(as.Date(key_i, format = "%Y%j"))
      }
      # ------------------------------------------------------------------------
      # Parse monthly keys as the first day of the month.
      if (scale_i == "monthly") {
        return(as.Date(paste0(key_i, "01"), format = "%Y%m%d"))
      }
      # ------------------------------------------------------------------------
      # Reject unsupported temporal scales.
      stop("Unsupported MODIS temporal scale.\n")
    },
    key,
    scale,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  # ----------------------------------------------------------------------------
  # Restore the Date class after simplifying the converted values.
  return(as.Date(as_date, origin = "1970-01-01"))
}

################################################################################
modis_filter_paths_by_date <- function(
  paths,
  date
) {
  # ----------------------------------------------------------------------------
  # Return early when there are no paths to filter.
  if (length(paths) == 0) {
    return(paths)
  }
  # ----------------------------------------------------------------------------
  # Use a single date as both range endpoints.
  if (length(date) == 1L) {
    date <- rep(date, 2L)
  }
  # ----------------------------------------------------------------------------
  # Extract temporal keys and scales from the filenames.
  keys <- vapply(paths, modis_extract_temporal_key, character(1))
  scales <- vapply(paths, modis_extract_temporal_scale, character(1))
  # ----------------------------------------------------------------------------
  # Return no paths when temporal keys or scales cannot be identified.
  if (all(is.na(keys)) || all(is.na(scales))) {
    return(character(0))
  }
  # ----------------------------------------------------------------------------
  # Require one recognized temporal scale across the paths.
  scales_unique <- unique(stats::na.omit(scales))
  if (length(scales_unique) != 1L) {
    stop("MODIS paths contain mixed or unsupported temporal patterns.\n")
  }
  # ----------------------------------------------------------------------------
  # Select files within the requested monthly or daily range.
  scale_target <- scales_unique[[1]]
  if (scale_target == "monthly") {
    # --------------------------------------------------------------------------
    # Match monthly keys against every month touched by the date range.
    month_start <- as.Date(format(as.Date(date[1]), "%Y-%m-01"))
    month_end <- as.Date(format(as.Date(date[2]), "%Y-%m-01"))
    month_sequence <- seq(month_start, month_end, by = "month")
    target_keys <- format(month_sequence, "%Y%m")
    keep <- keys %in% target_keys
  } else {
    # --------------------------------------------------------------------------
    # Match parsed daily dates against the inclusive date range.
    parsed_dates <- modis_key_to_date(keys, scales)
    keep <- parsed_dates >= as.Date(date[1]) & parsed_dates <= as.Date(date[2])
  }
  # ----------------------------------------------------------------------------
  # Return matching paths in their original order.
  return(paths[keep])
}

################################################################################
#' Convert spatial extent to MODIS sinusoidal tile codes
#' @description
#' Returns the set of MODIS sinusoidal grid tile codes (e.g. \code{"h08v04"})
#' whose geographic footprint overlaps the supplied bounding box.
#' @details
#' The MODIS sinusoidal grid divides the globe into 18 x 36 tiles, each
#' nominally covering 10 degrees of latitude. Because the sinusoidal projection
#' compresses longitude at high latitudes, the geographic lon/lat bounding
#' boxes of tiles are \emph{not} simple 10-degree squares — they can be
#' significantly wider in geographic longitude near the poles.
#'
#' This function uses the official NASA MODLAND sinusoidal tile bounding
#' coordinates table (\code{sn_bound_10deg.txt},
#' \url{https://modis-land.gsfc.nasa.gov/pdf/sn_bound_10deg.txt}) bundled in
#' \code{inst/extdata/}. It returns every non-fill tile whose geographic
#' bounding box overlaps the requested extent.
#'
#' Horizontal tile numbers (h) range from 0 to 35 (west to east); vertical
#' tile numbers (v) range from 0 to 17 (north to south).
#' @param extent numeric(4). Bounding box \code{c(xmin, ymin, xmax, ymax)}
#'   in decimal degrees (EPSG:4326).
#' @return character vector of tile codes in \code{"hXXvYY"} format, ordered
#'   by increasing v then h.
#' @author Kyle Messier
#' @seealso \code{\link{download_modis}}
#' @examples
#' extent_to_modis_tiles(c(-125, 22, -64, 50))
#' @keywords internal auxiliary
#' @export
extent_to_modis_tiles <- function(extent) {
  # ----------------------------------------------------------------------------
  # Check the geographic bounding box and coordinate order.
  stopifnot(
    "extent must be numeric(4)" = is.numeric(extent) && length(extent) == 4,
    "extent[1] (xmin) must be >= -180" = extent[1] >= -180,
    "extent[3] (xmax) must be <= 180" = extent[3] <= 180,
    "extent[2] (ymin) must be >= -90" = extent[2] >= -90,
    "extent[4] (ymax) must be <= 90" = extent[4] <= 90,
    "xmin must be < xmax" = extent[1] < extent[3],
    "ymin must be < ymax" = extent[2] < extent[4]
  )
  # ----------------------------------------------------------------------------
  # Locate the bundled MODIS tile bounds table.
  bounds_file <- system.file(
    "extdata",
    "sn_bound_10deg.txt",
    package = "amadeus"
  )
  stopifnot(
    "sn_bound_10deg.txt not found in inst/extdata/" = nzchar(bounds_file)
  )
  # ----------------------------------------------------------------------------
  # Read the tile coordinates and geographic bounds.
  lines <- readLines(bounds_file, warn = FALSE)
  data_lines <- grep("^ *[0-9]", lines, value = TRUE)
  tiles_df <- utils::read.table(
    text = paste(data_lines, collapse = "\n"),
    col.names = c("iv", "ih", "lon_min", "lon_max", "lat_min", "lat_max")
  )
  # ----------------------------------------------------------------------------
  # Drop fill tiles with invalid longitude bounds.
  tiles_df <- tiles_df[tiles_df$lon_min > -900, ]
  # ----------------------------------------------------------------------------
  # Extract the requested longitude and latitude limits.
  xmin <- extent[1]
  ymin <- extent[2]
  xmax <- extent[3]
  ymax <- extent[4]
  # ----------------------------------------------------------------------------
  # Retain tiles whose geographic bounds overlap the requested extent.
  hits <- tiles_df[
    tiles_df$lon_max >= xmin &
      tiles_df$lon_min <= xmax &
      tiles_df$lat_max >= ymin &
      tiles_df$lat_min <= ymax,
  ]
  # ----------------------------------------------------------------------------
  # Format the matching horizontal and vertical tile indices.
  sprintf("h%02dv%02d", hits$ih, hits$iv)
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process MODIS sub-datasets
#' @description
#' Selected MODIS sinusoidal grid product subdataset name selector.
#' Four presets are supported. `custom_sel` supersedes
#' presets of `product` values.
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @param ... Placeholders.
#' @note
#' Preset product codes and associated variables include
#' * "MOD11A1" - Land surface temperature (LST)
#' * "MOD13A2" - Normalized Difference Vegetation Index (NDVI)
#' * "MOD09GA" - Surface reflectance, and
#' * "MCD19A2" - Aerosol optical depth (AOD) and plume injection height.
#'
#' For a full list of available
#' MODIS product codes, see the "Short Name" column at
#' [NASA LP DAAC Search Data Catalog](https://www.earthdata.nasa.gov/centers/lp-daac).
#' When utilizing a product code from this "Short Name" column, \strong{do
#' not include} the version number following the period. For example, if "Short
#' Name" = MCD12C1.006, then `product = "MCD12C1"`.
# nolint end
#' @author Insang Song
#' @return A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calculate_modis]
#' @keywords internal
#' @noRd
#' @examples
#' process_modis_sds(product = "MOD09GA")
# previously modis_prefilter_sds
process_modis_sds <-
  function(
    product = c(
      "MOD11A1",
      "MOD13A2",
      "MOD13Q1",
      "MYD13Q1",
      "MOD09GA",
      "MCD19A2",
      "MOD14A1",
      "MYD14A1",
      "MOD14A2",
      "MYD14A2",
      "MOD16A2",
      "MYD16A2",
      "MCD64A1",
      "MCD64CMQ",
      "MCD12Q1",
      "VNP64A1"
    ),
    custom_sel = NULL,
    ...
  ) {
    if (!is.null(custom_sel)) {
      modis_sds <- custom_sel
    } else {
      product <- match.arg(product)
      modis_sds <-
        switch(
          product,
          MOD11A1 = "(LST_)",
          MOD13A2 = "(NDVI)",
          MOD13Q1 = "250m 16 days (NDVI|EVI)",
          MYD13Q1 = "250m 16 days (NDVI|EVI)",
          MOD09GA = "(sur_refl_b0)",
          MCD19A2 = "(Optical_Depth|Injection_Height)",
          MOD14A1 = "(FireMask)",
          MYD14A1 = "(FireMask)",
          MOD14A2 = "(FireMask)",
          MYD14A2 = "(FireMask)",
          MOD16A2 = "(ET_500m|PET_500m)",
          MYD16A2 = "(ET_500m|PET_500m)",
          MCD64A1 = "(Burn Date|BurnDate)",
          MCD64CMQ = "(Burn Date|BurnDate)",
          MCD12Q1 = "(LC_Type)",
          VNP64A1 = "(BurnDate)"
        )
      if (product == "MCD19A2") {
        message(
          sprintf(
            "For MCD19A2, use %s for 5km resolution sub-datasets.\n",
            "(cos|RelAZ|Angle)"
          )
        )
      } else if (product == "MCD12Q1") {
        message(
          sprintf(
            "For MCD12Q1, use %s to select a specific land cover layer.\n",
            "(LC_Type1|LC_Type2|LC_Type3|LC_Type4|LC_Type5)"
          )
        )
      }
    }
    return(modis_sds)
  }

################################################################################
#' Process MODIS layers
#' @description
#' Aggregate layers in a sub-dataset in sinusoidal MODIS products.
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param subdataset character(1). Exact or regular expression filter of
#' sub-dataset.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @keywords internal
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @description Some MODIS products consist of multi-layer subdatasets.
#' This function aggregates multiple layers into single layer `SpatRaster.`
#' `fun_agg` is applied at overlapping cells.
#' @note HDF values are read as original without scaling.
#' Users should consult MODIS product documentation to apply proper
#' scaling factor for post-hoc adjustment. If users have no preliminary
#' information about MODIS sub-datasets, consider running
#' `terra::describe(__filename__, sds = TRUE)` to navigate the full
#' list of sub-datasets in the input file then consult the documentation
#' of MODIS product.
#' @importFrom terra describe
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @importFrom terra is.rotated
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_flatten <- process_flatten_sds(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE)[1],
#'   subdataset = "(sur_refl_b0)",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# previously modis_aggregate_sds
process_flatten_sds <-
  function(
    path = NULL,
    subdataset = NULL,
    fun_agg = "mean",
    ...
  ) {
    # if curvilinear, halt
    status_curv <-
      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop(
        "The raster is curvilinear. Please rectify or warp
the input then flatten it manually."
      )
    }

    # describe provides subdataset information
    if (!any(grepl("^HDF", path))) {
      if (
        !is.character(subdataset) ||
          length(subdataset) != 1L ||
          is.na(subdataset) ||
          !nzchar(subdataset)
      ) {
        stop("Argument subdataset should be a non-empty character value.\n")
      }
      # we use var to get detailed information in subdatasets
      sds_desc <- terra::describe(path, sds = TRUE)
      index_sds <- sort(unique(c(
        grep(subdataset, sds_desc$var),
        grep(subdataset, sds_desc$name)
      )))
      if (length(index_sds) == 0L) {
        available_sds <- paste(stats::na.omit(sds_desc$var), collapse = ", ")
        stop(
          sprintf(
            paste0(
              "No MODIS subdatasets matched `subdataset = \"%s\"` ",
              "in `%s`.\nAvailable subdatasets: %s\n"
            ),
            subdataset,
            path,
            available_sds
          )
        )
      }
      sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]
      # raw is TRUE to ignore scaling factor.
      sds_read <- terra::rast(sds_desc$name, raw = TRUE)
      sds_nsds <- nrow(sds_desc)
      sds_nlyr <- sds_desc$nlyr
      sds_varn <- sds_desc$var
    } else {
      sds_read <- terra::rast(path, raw = TRUE)
      sds_nsds <- 1L
      sds_nlyr <- terra::nlyr(sds_read)
      sds_varn <- names(sds_read)
    }
    if (all(sds_nlyr == 1L)) {
      sds_agg <- sds_read
    } else if (sds_nsds == 1L) {
      sds_agg <- terra::app(sds_read, fun = fun_agg, na.rm = TRUE)
    } else {
      sds_aggindex <- rep(seq_len(sds_nsds), times = sds_nlyr)
      # if there are multiple layers in a subdataset,
      # aggregate overlapping pixel values
      sds_agg <-
        terra::tapp(sds_read, index = sds_aggindex, fun = fun_agg, na.rm = TRUE)
    }
    # restore names
    names(sds_agg) <- sds_varn
    gc()
    return(sds_agg)
  }

################################################################################
# nolint start
#' Process MODIS .hdf files
#' @description
#' Get mosaic or merged raster from multiple MODIS hdf files.
#' @param path character. Full list of hdf file paths.
#'  preferably a recursive search result from [`base::list.files`].
#' @param date character(1). date to query. Should be in
#' `"YYYY-MM-DD"` format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' Default is `NULL`, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (e.g., Aqua files) to fuse with `path` for improved temporal coverage.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, `"secondary_first"`.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/package=MODISTools),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [`download_data`]
#' @author Insang Song
#' @return a `SpatRaster` object
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_merge <- process_modis_merge(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = "2024-01-01",
#'   subdataset = "sur_refl_b01_1",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# nolint end
# previously modis_get_vrt
process_modis_merge <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  ...
) {
  fusion_method <- match.arg(fusion_method)
  if (!is.character(path)) {
    stop("Argument path should be a list of hdf files (character).\n")
  }
  if (!is.null(path_secondary) && !is.character(path_secondary)) {
    stop("Argument path_secondary should be a list of hdf files (character).\n")
  }
  if (!(is.character(fun_agg) || is.function(fun_agg))) {
    stop(
      "Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n"
    )
  }
  # date format check
  amadeus::is_date_proper(instr = date)

  # interpret date
  ftarget <- modis_filter_paths_by_date(path, date = date)
  if (length(ftarget) == 0) {
    stop("No MODIS files matched the requested date.\n")
  }

  # get layer information
  layer_target <-
    lapply(ftarget, function(x) {
      process_flatten_sds(
        x,
        subdataset = subdataset,
        fun_agg = fun_agg
      )
    })
  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(ftarget) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
    gc()
  } else {
    result_merged <- layer_target[[1]]
  }

  if (!is.null(path_secondary)) {
    ftarget_secondary <- modis_filter_paths_by_date(path_secondary, date = date)
    if (length(ftarget_secondary) > 0) {
      layer_target_secondary <-
        lapply(ftarget_secondary, function(x) {
          process_flatten_sds(
            x,
            subdataset = subdataset,
            fun_agg = fun_agg
          )
        })

      if (length(ftarget_secondary) > 1) {
        result_secondary <- do.call(terra::merge, layer_target_secondary)
      } else {
        result_secondary <- layer_target_secondary[[1]]
      }

      if (
        !isTRUE(terra::compareGeom(
          result_merged,
          result_secondary,
          stopOnError = FALSE
        ))
      ) {
        stop(
          "Primary and secondary MODIS rasters have incompatible geometry.\n"
        )
      }
      if (terra::nlyr(result_merged) != terra::nlyr(result_secondary)) {
        stop(
          "Primary and secondary MODIS rasters have different layer counts.\n"
        )
      }

      if (fusion_method == "primary_first") {
        result_merged <- terra::cover(result_merged, result_secondary)
      } else if (fusion_method == "secondary_first") {
        result_merged <- terra::cover(result_secondary, result_merged)
      } else {
        idx_layers <- seq_len(terra::nlyr(result_merged))
        fused <- lapply(idx_layers, function(k) {
          terra::app(
            c(result_merged[[k]], result_secondary[[k]]),
            mean,
            na.rm = TRUE
          )
        })
        result_merged <- do.call(c, fused)
      }
    }
  }
  return(result_merged)
}

################################################################################
# nolint start
#' Process MODIS files as daily outputs
#' @description
#' Process MODIS HDF/H5 files into day-specific rasters over a requested
#' date range. This helper preserves daily slices instead of flattening a
#' multi-day range into one merged result.
#' @param path character. Full list of HDF/H5 file paths.
#' @param date character(1:2). Date or date range in `"YYYY-MM-DD"` format.
#' @param subdataset character(1). Subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (for example, Aqua files) to fuse with `path` by date.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, or `"secondary_first"`.
#' @param return_type character(1). Return `"stack"` for a multi-layer
#' `SpatRaster` (default) or `"list"` for a named list of daily `SpatRaster`
#' objects.
#' @param ... Additional arguments passed to [`process_modis_merge`].
#' @return A day-preserving MODIS result as a `SpatRaster`
#' (`return_type = "stack"`) or named list (`return_type = "list"`).
#' @seealso [`process_modis_merge`], [`download_data`]
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_daily <- process_modis_daily(
#'   path = list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = c("2024-01-01", "2024-01-07"),
#'   subdataset = "sur_refl_b01_1",
#'   return_type = "list"
#' )
#' }
#' @export
# nolint end
process_modis_daily <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  return_type = c("stack", "list"),
  ...
) {
  return_type <- match.arg(return_type)
  amadeus::is_date_proper(instr = date)

  date <- if (length(date) == 1) rep(date, 2) else date
  date_seq <- format(
    seq(as.Date(date[1]), as.Date(date[2]), by = "day"),
    "%Y-%m-%d"
  )
  daily_rasters <- vector("list", length(date_seq))
  names(daily_rasters) <- date_seq

  for (i in seq_along(date_seq)) {
    day_i <- date_seq[[i]]
    daily_rasters[i] <- list(tryCatch(
      process_modis_merge(
        path = path,
        date = day_i,
        subdataset = subdataset,
        fun_agg = fun_agg,
        path_secondary = path_secondary,
        fusion_method = fusion_method,
        ...
      ),
      error = function(e) {
        if (
          grepl(
            "No MODIS files matched the requested date",
            e$message,
            fixed = TRUE
          )
        ) {
          return(NULL)
        }
        stop(e)
      }
    ))
  }

  daily_rasters <- daily_rasters[!vapply(daily_rasters, is.null, logical(1))]
  if (length(daily_rasters) == 0L) {
    stop("No MODIS files matched any day in the requested date range.\n")
  }

  if (return_type == "list") {
    return(daily_rasters)
  }

  rasters_named <- mapply(
    FUN = function(r_i, date_i) {
      names(r_i) <- paste0(names(r_i), "_", gsub("-", "", date_i))
      r_i
    },
    daily_rasters,
    names(daily_rasters),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  do.call(c, rasters_named)
}

################################################################################
process_mcd14ml <- function(
  path = NULL,
  date = NULL,
  extent = NULL,
  ...
) {
  if (is.null(path)) {
    stop("path is required.\n")
  }

  if (length(path) == 1L && dir.exists(path)) {
    path <- list.files(
      path = path,
      pattern = "\\.txt$",
      recursive = TRUE,
      full.names = TRUE
    )
  }

  path <- path[grepl("\\.txt$", path, ignore.case = TRUE)]
  if (length(path) == 0) {
    stop("No MCD14ML text files were found.\n")
  }

  txt_list <- lapply(path, data.table::fread)
  txt_data <- data.table::rbindlist(txt_list, fill = TRUE)
  names(txt_data) <- tolower(names(txt_data))

  required_cols <- c("latitude", "longitude", "acq_date")
  if (!all(required_cols %in% names(txt_data))) {
    stop("MCD14ML input is missing one or more required columns.\n")
  }

  if (!is.null(date)) {
    date <- if (length(date) == 1) rep(date, 2) else date

    amadeus::is_date_proper(instr = date)
    txt_data$acq_date <- as.Date(txt_data$acq_date)
    txt_data <- txt_data[
      txt_data$acq_date >= as.Date(date[1]) &
        txt_data$acq_date <= as.Date(date[2])
    ]
  } else {
    txt_data$acq_date <- as.Date(txt_data$acq_date)
  }

  if (!"frp" %in% names(txt_data)) {
    txt_data$frp <- NA_real_
  }
  txt_data$frp <- as.numeric(txt_data$frp)
  txt_data$fire_count <- 1L
  txt_data$time <- as.integer(format(txt_data$acq_date, "%Y%m%d"))

  txt_vect <- terra::vect(
    as.data.frame(txt_data),
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326",
    keepgeom = TRUE
  )

  if (!is.null(extent)) {
    txt_vect <- apply_extent(txt_vect, extent)
  }

  return(txt_vect)
}

################################################################################
# nolint start
#' Process Black Marble corners
#' @description
#' Tile corner generator for Black Marble products.
#' @param hrange integer(2). Both should be in 0-35.
#' @param vrange integer(2). Both should be in 0-17.
#' @description Black Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a `data.frame` of corner coordinates for assignment.
#' @return `data.frame` with xmin, xmax, ymin, and ymax fields
#' @author Insang Song
#' @keywords internal
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @examples
#' process_blackmarble_corners(hrange = c(1, 2), vrange = c(1, 2))
#' @export
# nolint end
process_blackmarble_corners <-
  function(
    hrange = c(5, 11),
    vrange = c(3, 6)
  ) {
    # should be in range
    if (!all(hrange %in% seq(0, 35)) || !all(vrange %in% seq(0, 17))) {
      stop("hrange or vrange are out of range.")
    }
    # in case range is put in reverse order
    hrange <- sort(hrange)
    vrange <- sort(vrange)

    hseq <- seq(hrange[1], hrange[2])
    vseq <- seq(vrange[1], vrange[2])

    tile_df <-
      expand.grid(
        vaddr = sprintf("v%02d", vseq),
        haddr = sprintf("h%02d", hseq)
      )
    hrangec <- -180 + (hseq * 10L)
    vrangec <- 90 - (vseq * 10L)

    hlen <- abs(diff(hrange)) + 1
    vlen <- abs(diff(vrange)) + 1

    tile_df$tile <- paste0(tile_df$haddr, tile_df$vaddr)
    tile_df <- data.frame(tile = tile_df$tile)
    #return(tile_df)
    tile_df$xmin <- rep(hrangec, each = vlen)
    tile_df$xmax <- tile_df$xmin + 10L
    tile_df$ymin <- rep(vrangec, hlen) - 10L
    tile_df$ymax <- tile_df$ymin + 10L

    return(tile_df)
  }

################################################################################
# nolint start
#' Assign VIIRS Black Marble products corner coordinates to retrieve a merged raster
#' @description This function will return a `SpatRaster` object with
#' georeferenced h5 files of Black Marble product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param path character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [`process_blackmarble_corners`] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs character(1). terra::crs compatible CRS.
#' Default is `"EPSG:4326"`
#' @param ... For internal use.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @seealso
#' * [`terra::describe`]
#' * [`terra::merge`]
#' * [`process_blackmarble_corners`]
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' vnp46a2 <- process_blackmarble(
#'   path =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   date = "2024-01-01",
#'   tile_df =
#'     process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
#'   subdataset = 3L,
#'   crs = "EPSG:4326"
#' )
#' }
#' @export
# previously modis_preprocess_vnp46
# nolint end
process_blackmarble <- function(
  path = NULL,
  date = NULL,
  tile_df = process_blackmarble_corners(),
  subdataset = 3L,
  crs = "EPSG:4326",
  ...
) {
  amadeus::is_date_proper(instr = date)
  # interpret date from paths
  datejul <- strftime(as.Date(date), format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(
      paste0(
        "(",
        paste(stdtile, collapse = "|"),
        ")"
      ),
      filepaths_today,
      value = TRUE
    )

  filepaths_today_tiles <-
    regmatches(
      filepaths_today,
      regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today)
    )

  vnp_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))
  # for filenames,
  # assign corner coordinates then merge
  # Subdataset 3 is BRDF-corrected nighttime light
  vnp_assigned <-
    mapply(
      function(vnp, tile_in) {
        read_vnp <- function(subds) {
          withCallingHandlers(
            terra::rast(vnp, subds = subds),
            warning = function(w) {
              if (grepl("unknown extent", conditionMessage(w), fixed = TRUE)) {
                invokeRestart("muffleWarning")
              }
            }
          )
        }
        vnp_ <- if (length(subdataset) > 1L) {
          do.call(c, lapply(subdataset, read_vnp))
        } else {
          read_vnp(subdataset)
        }
        tile_ext <- tile_df[tile_df$tile == tile_in, -1]

        terra::crs(vnp_) <- terra::crs(crs)
        terra::ext(vnp_) <- unlist(tile_ext)
        vnp_
      },
      vnp_today,
      filepaths_today_tiles_list,
      SIMPLIFY = FALSE
    )
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned[[1]]
  }
  vnp_all[vnp_all == 65535L] <- NaN
  vnp_all[is.nan(vnp_all)] <- NA
  return(vnp_all)
}

################################################################################
#' Warp MODIS swath data into rectilinear grid raster
#' @description Swath data is a type of MODIS data,
#' where curvilinear points are stored with varying resolution depending on
#' the relative position of the sensor axis. As this type of data
#' typically does not work well with planar spatial data, users
#' should warp or rectify this data into a rectilinear raster.
#' Main procedure is done with [`stars::st_warp`], in which users are able to
#' customize the threshold to fill potential gaps that appear where
#' the target resolution is finer than the local resolution of curvilinear
#' grid points.
#' @param path File path of MODIS swath with exact sub-dataset specification.
#' @param cellsize numeric(1). Cell size (spatial resolution) of
#' output rectilinear grid raster.
#' @param threshold numeric(1). Maximum distance to fill gaps if occur.
#' @param crs integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [`terra::crs`] and [`sf::st_crs`] / [EPSG](https://epsg.io/)
#' @param ... For internal use.
#' @note This function handles one file at a time.
#' @return a `stars` object
#' @author Insang Song
#' @seealso [`terra::rectify`]
#' @keywords internal
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_warp <- process_modis_warp(
#'   path = paste0(
#'     "HDF4_EOS:EOS_SWATH:",
#'     list.files(
#'       "./data/mod06l2",
#'       full.names = TRUE,
#'       pattern = ".hdf"
#'     )[1],
#'     ":mod06:Cloud_Fraction"
#'   ),
#'   cellsize = 0.1,
#'   threshold = 0.4,
#'   crs = 4326
#' )
#' }
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path = NULL,
    cellsize = 0.1,
    threshold = cellsize * 4,
    crs = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs,
        segments = 500,
        cellsize = cellsize,
        threshold = threshold
      )
    return(rtd)
  }

################################################################################
# nolint start
#' Mosaic MODIS swaths
#' @description This function will return a `SpatRaster` object with
#' values of selected subdatasets. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large `SpatRaster` object.
#' Users need to select a subdataset to process. The full path looks like
#' `"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
#' the full path to the hdf file.
#' @param path character. Full paths of hdf files.
#' @param date character(1). Date to query.
#' @param subdataset character. Subdatasets to process.
#' __Unlike other preprocessing functions, this argument should specify
#' the exact subdataset name.__ For example, when using MOD06_L2 product,
#' one may specify `c("Cloud_Fraction", "Cloud_Optical_Thickness")`,
#' etc. The subdataset names can be found in `terra::describe()` output.
#' @param suffix character(1). Should be formatted `:{product}:`,
#' e.g., `:mod06:`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree (decimal degree in WGS84).
#' @param ... For internal use.
#' @seealso
#' * [`process_modis_warp()`], [`stars::read_stars()`], [`stars::st_warp()`]
#' * [GDAL HDF4 driver documentation](https://gdal.org/en/latest/drivers/raster/hdf4.html)
#' * [`terra::describe()`]: to list the full subdataset list with `sds = TRUE`
#' * [`terra::sprc()`], [`terra::rast()`]
#' @return
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a single file with
#' full specification of subdataset.
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a list of files. In this case, the returned object will have the maximal extent of multiple warped layers
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mosaic
#' @importFrom stars st_mosaic
#' @importFrom terra values
#' @importFrom terra sprc
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_swath <- process_modis_swath(
#'   path = list.files(
#'     "./data/mod06l2",
#'     full.names = TRUE,
#'     pattern = ".hdf"
#'   ),
#'   date = "2024-01-01",
#'   subdataset = "Cloud_Fraction",
#'   suffix = ":mod06:",
#'   resolution = 0.05
#' )
#' }
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    path = NULL,
    date = NULL,
    subdataset = NULL,
    suffix = ":mod06:",
    resolution = 0.05,
    ...
  ) {
    # check date format
    amadeus::is_date_proper(instr = date)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", length = length(subdataset))
    datejul <- strftime(date, format = "%Y%j")
    ## FIXME: this part may result in underperformance.
    ##        Find a way to optimize this part.
    paths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaic
    for (element in seq_along(subdataset)) {
      target_text <-
        sprintf("%s%s%s%s", header, paths_today, suffix, subdataset[element])
      # rectified stars objects to SpatRaster
      mod06_element <- split(target_text, target_text) |>
        lapply(process_modis_warp, cellsize = resolution)
      # Remove all NA layers to avoid erroneous values
      mod06_element_nas <-
        sapply(
          mod06_element,
          function(x) {
            xvals <- x[[subdataset[element]]]
            all(is.na(xvals)) || all(is.nan(xvals))
          }
        )
      mod06_element <-
        mod06_element[!mod06_element_nas & !is.null(mod06_element_nas)]

      # prepare a fail-safe alternative return
      # It will be used again later.
      alt <- terra::rast(
        xmin = -128,
        xmax = -64,
        ymin = 20,
        ymax = 52,
        resolution = resolution
      )
      # initialize values with NA
      alt[] <- NA
      alt_dim <- dim(alt)
      alt[1, 1] <- 0
      alt[1, alt_dim[2]] <- 0
      alt[alt_dim[1], 1] <- 0
      alt[alt_dim[1], alt_dim[2]] <- 0
      terra::crs(alt) <- "EPSG:4326"

      if (is.null(mod06_element) || length(mod06_element) == 0) {
        message("All layers are NA or NaN.")
        mod06_element_mosaic <- terra::deepcopy(alt)
      } else {
        # mosaick the warped SpatRasters into one
        mod06_element_mosaic <-
          do.call(stars::st_mosaic, mod06_element) |>
          terra::rast()
        # assigning variable name
        mod06_element_mosaic <-
          terra::crop(mod06_element_mosaic, terra::ext(alt))
      }
      names(mod06_element_mosaic) <- subdataset[element]
      ras_mod06[[element]] <- mod06_element_mosaic
    }
    # SpatRasterCollection can accommodate multiple SpatRasters
    # with different extents (most flexible kind)
    mod06_sprc <- terra::sprc(ras_mod06)
    # post-hoc: stack multiple layers with different extent
    # into one SpatRaster
    # 1. mosaic all layers into one
    mod06_mosaic <- terra::mosaic(mod06_sprc, fun = "median")
    # 2. Assign NAs to prepare "etching"; NA will result in NaNs
    terra::values(mod06_mosaic) <- NA
    # 3. Looping main "etching"; each element is put first
    mod06_etched <-
      sapply(mod06_sprc, terra::mosaic, y = mod06_mosaic, fun = "first")
    # 4. stack
    mod06_return <- do.call(c, mod06_etched)
    return(mod06_return)
  }

################################################################################
# Internal helper: derive MODIS subdataset labels without loading raster values
info_modis_subdatasets <- function(path = NULL) {
  sds_desc <- try(terra::describe(path, sds = TRUE), silent = TRUE)
  if (!inherits(sds_desc, "try-error") && nrow(sds_desc) > 0) {
    candidate_col <- if ("var" %in% names(sds_desc)) "var" else "name"
    if (!is.null(candidate_col) && candidate_col %in% names(sds_desc)) {
      sds <- trimws(as.character(sds_desc[[candidate_col]]))
      sds <- sds[!is.na(sds) & nzchar(sds)]
      if (length(sds) > 0L) {
        return(sort(unique(sds)))
      }
    }
  }
  sds_read <- try(terra::rast(path, raw = TRUE), silent = TRUE)
  if (inherits(sds_read, "try-error")) {
    return(character(0))
  }
  sds <- trimws(as.character(names(sds_read)))
  sds <- sds[!is.na(sds) & nzchar(sds)]
  sort(unique(sds))
}

################################################################################
#' Get MODIS product subdataset lookup information
#' @description
#' Returns a lookup table of available MODIS product and subdataset selectors
#' from locally downloaded MODIS/VIIRS-style HDF/H5 files. This helper uses
#' metadata inspection (`terra::describe(..., sds = TRUE)` and layer names) and
#' does not read raster values into memory.
#' @param path character(1+) Path(s) to MODIS file(s) and/or directory(ies)
#'   containing `.hdf`/`.h5` files.
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each product-subdataset row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with MODIS product and subdataset selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_modis_info(path = "./data/modis")
#' get_modis_info(path = "./data/modis", include_file = TRUE)
#' }
#' @importFrom terra describe
#' @importFrom terra rast
#' @export
get_modis_info <- function(
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
    pattern = "\\.(hdf|h5)$",
    source_name = "MODIS HDF/H5"
  )
  out_rows <- lapply(files, function(f) {
    sds <- info_modis_subdatasets(path = f)
    product <- sub("\\..*$", "", basename(f))
    if (length(sds) == 0L || !nzchar(product)) {
      return(NULL)
    }
    data.frame(
      product = rep(product, length(sds)),
      subdataset = sds,
      file = rep(f, length(sds)),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop("No MODIS product-subdataset metadata could be derived from `path`.\n")
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("product", "subdataset"), drop = FALSE])
    out <- out[order(out$product, out$subdataset), , drop = FALSE]
  } else {
    out <- unique(out[, c("product", "subdataset", "file"), drop = FALSE])
    out <- out[order(out$product, out$subdataset, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate MODIS product covariates
#' @param from character, SpatRaster, or SpatVector. Either a list of
#' MODIS/VIIRS file paths (raw path mode), a preprocessed raster (direct raster
#' mode), or processed MODIS fire detections as a SpatVector with `time`,
#' `fire_count`, and `frp` fields.
#' @param from_secondary character or SpatRaster. Optional secondary input for
#' fused temporal coverage in raster/path workflows. Type must match `from`
#' (`character` with `character`, or `SpatRaster` with `SpatRaster`).
#' @param locs sf/SpatVector object. Unique locs where covariates
#' will be calculated.
#' @param locs_id character(1). Site identifier. Default is `"site_id"`
#' @param radius numeric. Radii to calculate covariates.
#' Default is `c(0, 1000, 10000, 50000)`.
#' @param preprocess function. Function to handle HDF files in raw path mode.
#' Ignored when `from` is a `SpatRaster` or `SpatVector`.
#' @param name_covariates character. Name header of covariates.
#' e.g., `"MOD_NDVIF_0_"`.
#' The calculated covariate names will have a form of
#' "\code{\{name_covariates\}\{zero-padded buffer radius in meters\}}",
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param subdataset Indices, names, or search patterns for subdatasets.
#' Find detail usage of the argument in notes.
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param package_list_add character. Reserved for backward compatibility;
#'   currently not used by `calculate_modis()`.
#' @param export_list_add character. Reserved for backward compatibility;
#'   currently not used by `calculate_modis()`.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param scale character(1). Scale expression to be applied to the raw values.
#' It is crucial that users review the technical documentation of the MODIS
#' product
#' they are using to ensure proper scale.
#' An example for the MOD11A1 product's LST_Day_1km variable (land surface
#' temperature)
#' would be `scale = "* 0.02"`.
#' Default is `NULL`, which applies no scale.
#' @param fusion_method character(1). Fusion method used only when
#' `from_secondary` is provided. Options are `"mean"` (pixel-wise mean with
#' `na.rm = TRUE`), `"primary_first"` (use `from` first), and
#' `"secondary_first"` (use `from_secondary` first).
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Arguments passed to `preprocess`.
# nolint start
#' @description `calculate_modis` orchestrates daily extraction using
#' [`calculate_modis_daily()`]. In raw-path mode, files are grouped by inferred
#' date, preprocessed for each day, and then extracted over requested radii.
#' With product-specific preprocessing, files are handled according to each
#' product's structure and naming conventions.
# nolint end
#' @note `locs` is expected to be convertible to `sf` object.
#' `sf`, `SpatVector`, and other class objects that could be converted to
#' `sf` can be used.
#' In raw path mode, `preprocess` is called once per inferred day using a
#' single-date value. Temporal aggregation across extracted rows should be done
#' with `.by_time`.
#' Common arguments in `preprocess` functions such as `date` and `path` are
#' automatically detected and passed to the function. Please note that
#' `locs` here and `path` in `preprocess` functions are assumed to have a
#' standard naming convention of raw files from NASA.
#' The argument `subdataset` should be in a proper format
#' depending on `preprocess` function:
#' * `process_modis_merge()`: Regular expression pattern.
#'   e.g., `"^LST_"`
#' * `process_modis_swath()`: Subdataset names.
#'   e.g., `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`
#' * `process_blackmarble()`: Subdataset number.
#'   e.g., for VNP46A2 product, 3L.
#'
#' For MOD13/MYD13 families, Terra and Aqua composites are 16-day phased
#' products offset by 8 days; combining both can improve effective temporal
#' coverage. This behavior aligns with NASA MOD13 product guidance:
#' <https://modis.gsfc.nasa.gov/data/dataprod/mod13.php>
#'
#' For MCD19A2 MAIAC, common sub-datasets include both optical depth and
#' plume injection height layers. Typical selectors are
#' `"(Optical_Depth|Injection_Height)"` for both families or
#' `"(Injection_Height)"` when targeting plume height only.
#'
#' For MOD14A1/MYD14A1 fire grids, the `FireMask` raw values are commonly
#' interpreted as:
#' \tabular{rll}{
#' Raw value \tab Meaning \tab Binary fire mask?\cr
#' 0 \tab not processed, missing input \tab NA / no observation\cr
#' 1 \tab obsolete, not used since Collection 1 \tab NA\cr
#' 2 \tab not processed, other reason \tab NA\cr
#' 3 \tab non-fire water pixel \tab 0\cr
#' 4 \tab cloud, land or water \tab NA or 0, depending on analysis\cr
#' 5 \tab non-fire land pixel \tab 0\cr
#' 6 \tab unknown, land or water \tab NA\cr
#' 7 \tab fire, low confidence \tab 1, or exclude for stricter mask\cr
#' 8 \tab fire, nominal confidence \tab 1\cr
#' 9 \tab fire, high confidence \tab 1\cr
#' }
#'
#' Dates with less than 80 percent of the expected number of tiles,
#' which are determined by the mode of the number of tiles, are removed.
#' Users will be informed of the dates with insufficient tiles.
#' The result data.frame will have an attribute with the dates with
#' insufficient tiles.
#' @return A data.frame or SpatVector with an attribute:
#' * `attr(., "dates_dropped")`: Dates with insufficient tiles.
#'   Note that the dates mean the dates with insufficient tiles,
#'   not the dates without available tiles. When \code{.by_time} is provided,
#'   rows are summarized with \code{calc_summarize_by()} semantics.
#' @seealso
#' This function leverages the calculation of single-day MODIS
#' covariates:
#' * [`calculate_modis_daily()`]
#'
#' Also, for preprocessing, please refer to:
#' * [`process_modis_merge()`]
#' * [`process_modis_swath()`]
#' * [`process_blackmarble()`]
#' @importFrom methods is
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows left_join
#' @importFrom rlang inject
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
#' calculate_modis(
#'   from =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = c(0L, 1000L),
#'   preprocess = process_modis_merge,
#'   name_covariates = "cloud_fraction_0",
#'   subdataset = "Cloud_Fraction",
#'   fun_summary = "mean"
#' )
#' }
#' @export
calculate_modis <-
  function(
    from = NULL,
    from_secondary = NULL,
    locs = NULL,
    locs_id = "site_id",
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    preprocess = amadeus::process_modis_merge,
    name_covariates = NULL,
    subdataset = NULL,
    fun_summary = "mean",
    .by_time = NULL,
    weights = NULL,
    package_list_add = NULL,
    export_list_add = NULL,
    max_cells = 3e7,
    geom = FALSE,
    scale = NULL,
    fusion_method = c("mean", "primary_first", "secondary_first"),
    ...
  ) {
    amadeus::check_geom(geom)
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_by_time(.by_time)
    fusion_method <- match.arg(fusion_method)
    from_is_character <- is.character(from)
    from_is_raster <- inherits(from, "SpatRaster")
    from_is_vector <- inherits(from, "SpatVector")
    if (!from_is_character && !from_is_raster && !from_is_vector) {
      stop(
        paste0(
          "from should be a character vector of paths, SpatRaster, ",
          "or SpatVector.\n"
        )
      )
    }
    if (from_is_character) {
      if (!is.null(from_secondary) && !is.character(from_secondary)) {
        stop("from_secondary should be a character vector of file paths.\n")
      }
    } else if (from_is_raster) {
      if (!is.null(from_secondary) && !inherits(from_secondary, "SpatRaster")) {
        stop("from_secondary should be SpatRaster when from is SpatRaster.\n")
      }
    } else if (!is.null(from_secondary)) {
      stop(
        paste0(
          "from_secondary is only supported for character ",
          "or SpatRaster inputs.\n"
        )
      )
    }
    if (from_is_character && !is.function(preprocess)) {
      stop(
        "preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble."
      )
    }
    # read all arguments
    # nolint start
    hdf_args <- c(as.list(environment()), list(...))
    # nolint end
    if (from_is_character) {
      dates_available_m <-
        vapply(from, modis_extract_temporal_key, character(1))
      date_scales <- vapply(from, modis_extract_temporal_scale, character(1))
      if (!is.null(from_secondary)) {
        dates_secondary_m <-
          vapply(from_secondary, modis_extract_temporal_key, character(1))
        date_scales_secondary <-
          vapply(from_secondary, modis_extract_temporal_scale, character(1))
        dates_available_m <- c(dates_available_m, dates_secondary_m)
        date_scales <- c(date_scales, date_scales_secondary)
      }
      date_scale_unique <- unique(stats::na.omit(date_scales))
      if (length(date_scale_unique) != 1L) {
        stop(
          "MODIS input files contain mixed or unsupported temporal patterns.\n"
        )
      }
      dates_available <- sort(unique(dates_available_m))

      if (is.null(from_secondary)) {
        # When multiple dates are concerned,
        # the number of tiles are expected to be the same.
        # Exceptions could exist, so here the number of tiles are checked.
        summary_available <- table(dates_available_m)
        summary_available_mode <-
          sort(table(summary_available), decreasing = TRUE)[1]
        summary_available_mode <- as.numeric(names(summary_available_mode))
        summary_available_insuf <-
          which(summary_available < floor(summary_available_mode * 0.8))

        if (length(summary_available_insuf) > 0) {
          dates_insuf <-
            modis_key_to_date(
              dates_available[summary_available_insuf],
              date_scale_unique
            )
          message(
            paste0(
              "The number of tiles on the following dates are insufficient: ",
              paste(dates_insuf, collapse = ", "),
              ".\n"
            )
          )
          # finally it removes the dates with insufficient tiles
          dates_available <- dates_available[-summary_available_insuf]
        } else {
          dates_insuf <- NA
        }
      } else {
        dates_insuf <- NA
      }
    } else {
      date_scale_unique <- NA_character_
      dates_available <- NA_character_
      dates_insuf <- NA
    }

    locs_input <- try(sf::st_as_sf(locs), silent = TRUE)
    if (inherits(locs_input, "try-error")) {
      stop(
        "locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n"
      )
    }

    if (from_is_vector) {
      calc_results_return <-
        calculate_modis_fire_vector(
          from = from,
          locs_input = locs_input,
          locs_id = locs_id,
          radius = radius,
          fun_summary = fun_summary,
          .by_time = .by_time,
          geom = geom
        )
      attr(calc_results_return, "dates_dropped") <- NA
      return(calc_results_return)
    }

    if (!is.null(scale)) {
      stopifnot(is.character(scale))
    }
    if (is.null(scale)) {
      warning(
        paste0(
          "`scale` parameter not defined. Review technical documentation ",
          "to apply proper scale. Calculation proceeding with unscaled values."
        )
      )
      scale <- "* 1"
    }

    export_list <- c()
    package_list <-
      c(
        "sf",
        "terra",
        "exactextractr",
        "data.table",
        "stars",
        "dplyr",
        "parallelly",
        "rlang",
        "amadeus"
      )
    if (!is.null(export_list_add)) {
      export_list <- append(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- append(package_list, package_list_add)
    }

    # make clusters
    if (from_is_character) {
      idx_date_available <- seq_along(dates_available)
      list_date_available <-
        split(idx_date_available, idx_date_available)
    } else {
      list_date_available <- list(1L)
    }
    calc_results <-
      lapply(
        list_date_available,
        FUN = function(datei) {
          options(sf_use_s2 = FALSE)
          radiusindex <- seq_along(radius)
          radiusindexlist <- split(radiusindex, radiusindex)
          if (from_is_character) {
            # nolint start
            day_to_pick <- dates_available[datei]
            # nolint end
            day_to_pick <- modis_key_to_date(day_to_pick, date_scale_unique)
            calc_time <- as.character(day_to_pick)
            hdf_args <- c(hdf_args, list(date = day_to_pick))
            if (is.null(from_secondary)) {
              hdf_args <- c(hdf_args, list(path = hdf_args$from))
              # unified interface with rlang::inject
              vrt_today <- rlang::inject(preprocess(!!!hdf_args))
            } else {
              day_key <- dates_available[datei]
              has_primary <- day_key %in%
                vapply(
                  hdf_args$from,
                  modis_extract_temporal_key,
                  character(1)
                )
              has_secondary <- day_key %in%
                vapply(
                  hdf_args$from_secondary,
                  modis_extract_temporal_key,
                  character(1)
                )
              if (!has_primary && !has_secondary) {
                stop("No MODIS files found for selected fusion date.\n")
              }

              raster_primary <- NULL
              raster_secondary <- NULL

              if (has_primary) {
                hdf_args_primary <- hdf_args
                hdf_args_primary$path <- hdf_args$from
                hdf_args_primary$from_secondary <- NULL
                raster_primary <- rlang::inject(preprocess(!!!hdf_args_primary))
              }
              if (has_secondary) {
                hdf_args_secondary <- hdf_args
                hdf_args_secondary$path <- hdf_args$from_secondary
                hdf_args_secondary$from_secondary <- NULL
                raster_secondary <-
                  rlang::inject(preprocess(!!!hdf_args_secondary))
              }

              if (is.null(raster_primary)) {
                vrt_today <- raster_secondary
              } else if (is.null(raster_secondary)) {
                vrt_today <- raster_primary
              } else {
                if (
                  !isTRUE(terra::compareGeom(
                    raster_primary,
                    raster_secondary,
                    stopOnError = FALSE
                  ))
                ) {
                  stop(
                    "Primary and secondary MODIS rasters have incompatible ",
                    "geometry.\n"
                  )
                }
                if (
                  terra::nlyr(raster_primary) != terra::nlyr(raster_secondary)
                ) {
                  stop(
                    "Primary and secondary MODIS rasters have different ",
                    "layer counts.\n"
                  )
                }
                if (fusion_method == "primary_first") {
                  vrt_today <- terra::cover(raster_primary, raster_secondary)
                } else if (fusion_method == "secondary_first") {
                  vrt_today <- terra::cover(raster_secondary, raster_primary)
                } else {
                  idx_layers <- seq_len(terra::nlyr(raster_primary))
                  fused <- lapply(idx_layers, function(k) {
                    terra::app(
                      c(raster_primary[[k]], raster_secondary[[k]]),
                      mean,
                      na.rm = TRUE
                    )
                  })
                  vrt_today <- do.call(c, fused)
                  names(vrt_today) <- names(raster_primary)
                }
              }
            }
          } else {
            calc_time <- NA_character_
            if (is.null(from_secondary)) {
              vrt_today <- from
            } else {
              raster_primary <- from
              raster_secondary <- from_secondary
              if (
                !isTRUE(terra::compareGeom(
                  raster_primary,
                  raster_secondary,
                  stopOnError = FALSE
                ))
              ) {
                stop(
                  "Primary and secondary MODIS rasters have incompatible ",
                  "geometry.\n"
                )
              }
              if (
                terra::nlyr(raster_primary) != terra::nlyr(raster_secondary)
              ) {
                stop(
                  "Primary and secondary MODIS rasters have different ",
                  "layer counts.\n"
                )
              }
              if (fusion_method == "primary_first") {
                vrt_today <- terra::cover(raster_primary, raster_secondary)
              } else if (fusion_method == "secondary_first") {
                vrt_today <- terra::cover(raster_secondary, raster_primary)
              } else {
                idx_layers <- seq_len(terra::nlyr(raster_primary))
                fused <- lapply(idx_layers, function(k) {
                  terra::app(
                    c(raster_primary[[k]], raster_secondary[[k]]),
                    mean,
                    na.rm = TRUE
                  )
                })
                vrt_today <- do.call(c, fused)
                names(vrt_today) <- names(raster_primary)
              }
            }
          }

          if (sum(terra::nlyr(vrt_today)) != length(name_covariates)) {
            message(
              "The number of layers in the input raster do not match
                    the length of name_covariates.\n"
            )
          }

          res0 <-
            lapply(radiusindexlist, function(k) {
              name_radius <-
                sprintf("%s%05d", name_covariates, radius[k])
              extracted <-
                try(
                  amadeus::calculate_modis_daily(
                    locs = locs_input,
                    from = vrt_today,
                    locs_id = locs_id,
                    date = calc_time,
                    fun_summary = fun_summary,
                    name_extracted = name_radius,
                    radius = radius[k],
                    weights = weights,
                    max_cells = max_cells,
                    geom = FALSE,
                    scale = scale
                  )
                )
              if (inherits(extracted, "try-error")) {
                # coerce to avoid errors
                error_df <- data.frame(
                  matrix(
                    -99999,
                    ncol = length(name_radius) + 1,
                    nrow = nrow(locs_input)
                  )
                )
                error_df <- stats::setNames(error_df, c(locs_id, name_radius))
                error_df[[locs_id]] <- unlist(locs_input[[locs_id]])
                if (is.na(calc_time)) {
                  error_df$time <- as.POSIXlt(as.Date(NA))
                } else {
                  error_df$time <- as.POSIXlt(calc_time, tz = "UTC")
                }
                extracted <- error_df
              }
              return(extracted)
            })
          res <-
            Reduce(
              function(x, y) {
                dplyr::left_join(x, y, by = c(locs_id, "time"))
              },
              res0
            )
          return(res)
        }
      )
    calc_results <- do.call(dplyr::bind_rows, calc_results)
    if (!is.null(.by_time)) {
      calc_results <- amadeus::calc_summarize_by(
        covar = calc_results,
        .by_time = .by_time,
        fun_summary = fun_summary,
        locs_id = locs_id
      )
    }
    if (geom %in% c("sf", "terra")) {
      # merge
      calc_results_return <- merge(
        locs_input[, locs_id],
        calc_results,
        by = locs_id
      )
      if (geom == "terra") {
        calc_results_return <- terra::vect(calc_results_return)
      }
    } else {
      calc_results_return <- calc_results
    }
    attr(calc_results_return, "dates_dropped") <- dates_insuf
    Sys.sleep(1L)
    return(calc_results_return)
  }

################################################################################
calculate_modis_fire_vector <- function(
  from,
  locs_input,
  locs_id,
  radius,
  fun_summary,
  .by_time,
  geom
) {
  if (!methods::is(from, "SpatVector")) {
    stop("from should be a SpatVector returned by process_mcd14ml.\n")
  }
  if (!all(c("time", "fire_count", "frp") %in% names(from))) {
    stop("from is missing required MCD14ML fields.\n")
  }

  locs_base <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs_input,
    locs_id = locs_id,
    radius = 0L,
    geom = geom
  )
  locs_points <- locs_base[[1]]
  locs_return <- locs_base[[2]]
  loc_index <- seq_len(nrow(locs_points))

  date_keys <- sort(unique(as.integer(from$time)))
  results_by_day <- lapply(date_keys, function(day_key) {
    from_day <- from[from$time == day_key, ]
    results_by_radius <- lapply(radius, function(radius_i) {
      col_count <- sprintf("fire_count_%05d", radius_i)
      col_frp <- sprintf("frp_%05d", radius_i)

      dist_matrix <- terra::distance(locs_points, from_day)
      dist_df <- data.frame(
        expand.grid(
          loc_index = loc_index,
          from_index = seq_len(nrow(from_day))
        ),
        distance = as.vector(dist_matrix)
      )
      if (radius_i == 0L) {
        dist_df <- dist_df[dist_df$distance == 0, ]
      } else {
        dist_df <- dist_df[dist_df$distance <= radius_i, ]
      }

      if (nrow(dist_df) == 0) {
        result_empty <- data.frame(
          loc_index = loc_index,
          fire_count = 0,
          frp = 0
        )
      } else {
        dist_df$fire_count <- from_day$fire_count[dist_df$from_index]
        dist_df$frp <- from_day$frp[dist_df$from_index]
        result_empty <-
          stats::aggregate(
            cbind(fire_count, frp) ~ loc_index,
            data = dist_df,
            FUN = sum,
            na.rm = TRUE
          )
        result_empty <-
          merge(
            data.frame(loc_index = loc_index),
            result_empty,
            by = "loc_index",
            all.x = TRUE
          )
        result_empty$fire_count[is.na(result_empty$fire_count)] <- 0
        result_empty$frp[is.na(result_empty$frp)] <- 0
      }

      names(result_empty)[names(result_empty) == "fire_count"] <- col_count
      names(result_empty)[names(result_empty) == "frp"] <- col_frp
      result_empty
    })

    result_day <- Reduce(
      function(x, y) merge(x, y, by = "loc_index", all = TRUE),
      results_by_radius
    )
    result_day[[locs_id]] <- locs_return[[locs_id]]
    result_day$time <- as.POSIXlt(
      as.character(day_key),
      format = "%Y%m%d",
      tz = "UTC"
    )
    ordered_cols <- c(
      locs_id,
      "time",
      setdiff(names(result_day), c("loc_index", locs_id, "time"))
    )
    result_day[, ordered_cols]
  })

  result_all <- do.call(rbind, results_by_day)
  if (!is.null(.by_time)) {
    result_all <- amadeus::calc_summarize_by(
      covar = result_all,
      .by_time = .by_time,
      fun_summary = fun_summary,
      locs_id = locs_id
    )
  }
  if (geom %in% c("sf", "terra")) {
    result_all <- merge(locs_return, result_all, by = locs_id)
  }

  amadeus::calc_return_locs(
    covar = result_all,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
}

################################################################################
#' A single-date MODIS worker
#' @param from SpatRaster. Preprocessed objects.
#' @param locs SpatVector/sf/sftime object. Locations where MODIS values
#' are summarized.
#' @param locs_id character(1). Field name where unique site identifiers
#' are stored. Default is `"site_id"`
#' @param radius numeric. Radius to generate circular buffers.
#' @param date Date(1). date to query.
#' @param name_extracted character. Names of calculated covariates.
#' @param fun_summary function. Summary function for
#' multilayer rasters. Passed to `foo`. See [`exactextractr::exact_extract`]
#' for details.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' See [`exactextractr::exact_extract`] for details.
#' @param scale character(1). Scale expression to be applied to the raw values.
#' It is crucial that users review the technical documentation of the MODIS
#' product
#' they are using to ensure proper scale.
#' An example for the MOD11A1 product's LST_Day_1km variable (land surface
#' temperature)
#' would be `scale = "* 0.02"`.
#' Default is `NULL`, which applies no scale.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @description The function operates at MODIS/VIIRS products
#' on a daily basis. Given that the raw hdf files are downloaded from
#' NASA, standard file names include a data retrieval date flag starting
#' with letter "A". Leveraging that piece of information, the function will
#' select files of scope on the date of interest.
#' Please note that this function does not provide a function to filter
#' swaths or tiles, so it is strongly recommended to check and pre-filter
#' the file names at users' discretion.
#' @seealso
#' * Preprocessing: [process_modis_merge()], [process_modis_swath()],
#'     [process_blackmarble()]
#' @keywords internal
#' @author Insang Song
#' @return a data.frame or SpatVector object.
#' @importFrom terra extract project vect nlyr describe
#' @importFrom methods is
#' @importFrom sf st_as_sf st_drop_geometry
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' calculate_modis_daily(
#'   from = mod06l2_warp, # dervied from process_modis() example
#'   locs = locs,
#'   locs_id = "id",
#'   radius = 0,
#'   date = "2024-01-01",
#'   name_extracted = "cloud_fraction_0",
#'   fun_summary = "mean",
#'   max_cells = 3e7
#' )
#' }
#' @export
calculate_modis_daily <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = 0L,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  weights = NULL,
  max_cells = 3e7,
  geom = FALSE,
  scale = NULL,
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs), silent = TRUE)
    if (inherits(locs, "try-error")) {
      stop("locs should be a SpatVector or convertible object.")
    }
  }
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n", locs_id))
  }

  extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    func = "mean",
    weights = NULL,
    maxcells = NULL
  ) {
    # generate buffers
    if (radius == 0) {
      radius <- 1e-6
    } # approximately 1 meter in degree
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # extract raster values
    weights_norm <- amadeus::calc_prepare_weights(
      from = surf[[1]],
      weights = weights
    )
    func_extract <- amadeus::calc_weighted_fun(
      fun = func,
      weighted = !is.null(weights_norm)
    )
    extract_args <- list(
      x = surf,
      y = sf::st_as_sf(bufs),
      fun = func_extract,
      force_df = TRUE,
      stack_apply = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = maxcells
    )
    if (!is.null(weights_norm)) {
      extract_args$weights <- weights_norm
    }
    surf_at_bufs <- do.call(exactextractr::exact_extract, extract_args)
    return(surf_at_bufs)
  }

  ## NaN to NA
  from[is.nan(from)] <- NA

  # apply scale as expression to `from` values
  chr_scale <- paste0("from ", scale)
  # Evaluate the scale expression
  from_scale <- eval(parse(text = chr_scale)[[1]])

  # from_scale[is.nan(from_scale)] <- NA

  # raster used to be vrt_today
  extracted <-
    extract_with_buffer(
      points = locs,
      surf = from_scale,
      id = locs_id,
      radius = radius,
      func = fun_summary,
      weights = weights,
      maxcells = max_cells
    )
  # cleaning names
  # assuming that extracted is a data.frame
  name_offset <- terra::nlyr(from)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset + 1, ncol(extracted), 1)
  colnames(extracted)[name_range] <- name_extracted
  extracted$time <- as.POSIXlt(date)
  check_geom(geom)
  if (geom %in% c("sf", "terra")) {
    # convert to base date, as terra::vect does not like class "POSIXlt"
    extracted$time <- as.Date.POSIXlt(extracted$time)
    # location ID with geometry
    locs_geom_id <- suppressMessages(calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )[[2]])
    # merge
    extracted_merge <- merge(
      locs_geom_id,
      extracted,
      by = locs_id
    )
    # re-convert to POSIXlt after creating the vect
    extracted_merge$time <- as.POSIXlt(extracted_merge$time)
    extracted_return <- calc_return_locs(
      covar = extracted_merge,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )
  } else {
    calc_check_time(covar = extracted, POSIXt = TRUE)
    extracted_return <- extracted
  }
  gc()
  return(extracted_return)
}
################################################################################
