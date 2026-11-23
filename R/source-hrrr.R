################################################################################
# {amadeus} functions for
################################################################################

################################################################################
# {metadata} tier
################################################################################
#' Get HRRR variable lookup information
#'
#' @description
#' Read a bundled catalogue of HRRR variable short names, descriptions,
#' vertical levels, and reference raster labels. No downloaded HRRR files or
#' internet connection are required. Supply a unique short name to identify
#' a field, or a variable code and optional levels to select related fields.
#' Omit both arguments to browse the complete catalogue.
#'
#' @param variable character vector or `NULL`. Unique field short names such
#'   as `"TMP_2MAGL"`, `"TMP_SFC"`, or `"TMP_850MB"`, or NCEP variable codes
#'   such as `"TMP"`, `"DPT"`, or `"UGRD"`. A variable code selects every
#'   catalogued level for that variable, subject to `level`. Matching is
#'   case-insensitive. The default `NULL` includes all fields. Unknown names
#'   raise an error. Selections follow the requested order; overlapping or
#'   repeated selections return each field once, at its first occurrence.
#' @param level character vector or `NULL`. Exact catalogue level labels,
#'   such as `"2 m above ground"`, `"surface"`, `"850 mb"`, or
#'   `"1 hybrid level"`. Matching is case-insensitive. The default `NULL`
#'   retains all levels for the selected variables. Unknown levels, or
#'   requested variables with none of the selected levels, raise an error.
#'
#' @details
#' The standalone table is installed at `extdata/hrrr_variables.csv`. It
#' contains one row per variable-level combination, combining the NCEP CONUS
#' pressure, native, surface, and sub-hourly forecast-hour-02 inventories
#' retrieved on 2026-11-23. It is a reference catalogue, not a full historical
#' inventory or a guarantee that a field exists for every product, date,
#' initialization cycle, or forecast hour.
#'
#' `short_name` uniquely identifies a variable-level combination within the
#' catalogue. It combines the original NCEP code, retained in `variable`,
#' with a level label separated by one underscore. Examples are
#' `"TMP_2MAGL"` (2 m above ground), `"TMP_SFC"` (surface), `"TMP_850MB"`
#' (850 mb), and `"TMP_HYBRID1"` (hybrid level 1). These are amadeus field
#' identifiers, not native NCEP variable codes. Numeric labels use `P` for
#' decimal points and `TO` for layer bounds, for example `"TMP_1013P2MB"`.
#' `MAGL` and `MBGL` distinguish metres above and below ground; `MBAGL`
#' denotes pressure differences above ground in millibars. The complete
#' level description remains in `level`.
#'
#' The identifiers use uppercase letters and digits with exactly one
#' underscore. Appending `"_YYYYMMDD_HHMM"` therefore gives separate
#' variable, level, date, and time components for layer and column names.
#' Short names distinguish levels, but do not identify initialization cycles,
#' forecast leads, or statistical intervals.
#'
#' `long_name`, `level`, and `units` describe the published field.
#' `layer_name` is its full reference terra layer name, including the level,
#' GDAL variable description, and GDAL units. Names combine descriptors from
#' 2021-05-04 files read using GDAL 3.0.4 with the inventory's level values.
#' They were checked against sampled surface, pressure, native, and
#' sub-hourly fields. Published names and GDAL labels can differ. GDAL labels
#' can also vary between versions; processors must check the actual file
#' metadata before selecting layers. Reference labels retain GDAL's
#' spelling, including any errors, for matching purposes.
#'
#' Match `layer_name` against complete terra layer names, as shown below.
#' This distinguishes 2 m temperature from surface and pressure-level
#' temperature, as well as dew point and potential temperature. Omitting
#' `level` returns every catalogued level for the selected variable. A field
#' can still occur at several forecast times or with different statistical
#' intervals: this lookup does not choose a forecast or provide stable band
#' numbers. Product membership is recorded separately for each variable and
#' level, not inferred from the other levels of that variable.
#'
#' `units` are the published GRIB units, not necessarily the units returned
#' by GDAL. For example, temperatures are published in kelvin but GDAL
#' normally exposes them in degrees Celsius. Processing must use the actual
#' raster units.
#'
#' @return
#' A data.frame with character columns `short_name` (unique variable-level
#' identifier), `variable` (original NCEP code), `long_name`
#' (published description without units), `level` (published vertical level
#' or layer), `units` (published units), `layer_name` (full reference terra
#' layer name, including level and GDAL units), and
#' `products` (semicolon-separated file codes: `prs`, `nat`, `sfc`, `subh`).
#' With `variable = NULL`, rows follow the catalogue's fixed short-name
#' order. Within each variable-code selection, fields retain catalogue order.
#'
#' @source
#' NCEP HRRR product inventories:
#' <https://www.nco.ncep.noaa.gov/pmb/products/hrrr/>.
#' GDAL GRIB metadata and unit handling:
#' <https://gdal.org/en/stable/drivers/raster/grib.html>.
#'
#' @author Mitchell Manware
#' @seealso [download_hrrr()], [terra::rast()]
#' @examples
#' metadata_hrrr()
#' metadata_hrrr(c("TMP", "DPT", "UGRD"))
#' metadata_hrrr(c("TMP_2MAGL", "TMP_SFC", "TMP_850MB"))
#' metadata_hrrr("TMP", level = "850 mb")
#'
#' # Resolve a unique short name to its full reference layer name.
#' temperature <- metadata_hrrr("tmp_2magl")
#' temperature[, c("short_name", "long_name", "level")]
#' layer_names <- c(
#'   "2[m] HTGL=Specified height level above ground; Temperature [C]",
#'   "2[m] HTGL=Specified height level above ground; Dew point temperature [C]",
#'   "85000[Pa] ISBL=Isobaric surface; Temperature [C]"
#' )
#' which(layer_names %in% temperature$layer_name)
#' paste0(temperature$short_name, "_20210504_0200")
#'
#' # Access the standalone CSV directly.
#' system.file("extdata", "hrrr_variables.csv", package = "amadeus")
#'
#' @export
metadata_hrrr <- function(variable = NULL, level = NULL) {
  # ----------------------------------------------------------------------------
  # Check optional variable and level selectors.
  checkmate::assert_character(
    variable,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE,
    null.ok = TRUE
  )
  checkmate::assert_character(
    level,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE,
    null.ok = TRUE
  )
  # ----------------------------------------------------------------------------
  # Read the standalone catalogue without inspecting data files.
  df_variables <- utils::read.csv(
    system.file(
      "extdata",
      "hrrr_variables.csv",
      package = "amadeus",
      mustWork = TRUE
    ),
    stringsAsFactors = FALSE
  )
  # ----------------------------------------------------------------------------
  # Resolve field identifiers or variable codes in the requested order.
  if (!is.null(variable)) {
    variable <- unique(toupper(variable))
    checkmate::assert_subset(
      variable,
      c(df_variables$short_name, df_variables$variable)
    )
    int_rows <- unique(unlist(
      lapply(variable, function(x) {
        which(df_variables$short_name == x | df_variables$variable == x)
      }),
      use.names = FALSE
    ))
    df_variables <- df_variables[int_rows, , drop = FALSE]
  }
  # ----------------------------------------------------------------------------
  # Select levels without dropping their variable identity.
  if (!is.null(level)) {
    level <- unique(tolower(level))
    checkmate::assert_subset(level, tolower(df_variables$level))
    df_variables <- df_variables[
      tolower(df_variables$level) %in% level,
      ,
      drop = FALSE
    ]
    missing_variables <- setdiff(
      variable,
      c(df_variables$short_name, df_variables$variable)
    )
    if (length(missing_variables) > 0L) {
      stop(
        "No requested levels found for variable(s): ",
        paste(missing_variables, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  # ----------------------------------------------------------------------------
  # Return a consistently shaped lookup table.
  rownames(df_variables) <- NULL
  df_variables
}

################################################################################
# {download} tier
################################################################################
#' Download NOAA High-Resolution Rapid Refresh (HRRR) data
#'
#' @description
#' Downloads NOAA High-Resolution Rapid Refresh (HRRR) model data from the
#' NOAA HRRR public Amazon S3 archive.
#'
#' The function supports the HRRR three-dimensional pressure-level,
#' native-level, two-dimensional surface, and sub-hourly products. Select one
#' product per call, with multiple dates, model cycle run times, and forecast
#' hours if needed.
#'
#' HRRR data are downloaded as GRIB2 files and organized under a sector-specific
#' directory within `directory_to_save`.
#'
#' @param product character(1). Required HRRR product to download. Accepted
#'   values are
#'   `"3d pressure"`, `"native"`, `"2d surface"`, `"sub-hourly"`, or
#'   `"subhourly"`. Matching is case-insensitive. The default `NULL` must be
#'   replaced with a product name.
#' @param sector character(1). HRRR geographic sector to download. Currently
#'   only `"conus"` is supported and is the default. Matching is
#'   case-insensitive.
#' @param date character(1 or 2). Dates in `"YYYY-MM-DD"` format defining
#'   the download period. A single date downloads data for that date.
#'   When two dates are supplied, all dates from the earlier date through the
#'   later date are included. All dates must be between `"2014-07-30"` and
#'   today, inclusive. Dates outside this range raise an error before directory
#'   creation or network requests.
#' @param cycle_runtime numeric vector. Non-missing model initialization
#'   hour(s), specified as whole numbers from `0` through `23`. Defaults to
#'   `0:23`. Values are formatted as two-digit UTC
#'   cycle times when constructing the HRRR file names. At least one hour is
#'   required; repeated hours are requested once.
#' @param forecast_hour numeric vector. Non-missing forecast lead hour(s),
#'   specified as whole numbers from `0` through `48`. Defaults to `0:18`,
#'   including the zero-hour lead (`f00`) at the model initialization time.
#'   Values are formatted as two-digit forecast hours
#'   when constructing the HRRR file names. At least one hour is required;
#'   repeated hours are requested once.
#' @param directory_to_save character(1). Required non-empty directory path
#'   for downloaded HRRR files. Missing parent directories and the
#'   sector-specific subdirectory are created automatically.
#' @param acknowledgement logical(1). Must be `TRUE` to acknowledge the
#'   download before data are retrieved.
#' @param download logical(1). Deprecated. `FALSE` emits a warning and returns
#'   a download preview. Argument validation, directory creation, and URL
#'   construction still run before returning.
#' @param remove_command logical(1). Deprecated and ignored. `TRUE` emits a
#'   warning.
#' @param show_progress logical(1). Show per-file messages and progress bars
#'   during sequential downloads. Defaults to `TRUE`. Ignored during
#'   concurrent downloads, where per-file progress is disabled. Status and
#'   summary messages may still be printed when `FALSE`.
#' @param hash logical(1). Return a combined MD5 hash of all files recursively
#'   under `directory_to_save`, including existing files, instead of the
#'   download summary. Defaults to `FALSE`.
#' @param max_tries integer(1). Maximum attempts per file request, including
#'   the initial attempt. Must be at least 1. Defaults to 20.
#' @param rate_limit numeric(1). Finite, non-negative request-throttling
#'   interval in seconds. Defaults to 2. Passed unchanged to
#'   [amadeus::download_run_method()] for sequential downloads. For concurrent
#'   downloads, multiplied by the configured daemon count for the `"default"`
#'   compute profile before being passed to each worker.
#'
#' @details
#' Dates select model initialization days. Each selected cycle run time and
#' forecast hour is requested for every date. URLs are constructed directly;
#' validation does not check whether each requested product, cycle, and
#' forecast hour is available in the archive.
#'
#' Product names map to the file codes `prs` (3d pressure), `nat` (native),
#' `sfc` (2d surface), and `subh` (either sub-hourly spelling). Local filenames
#' include the initialization date to distinguish files from different days.
#' For example, surface data initialized at 12 UTC on January 1, 2020, with a
#' one-hour forecast lead are saved as
#' `conus/hrrr.20200101.t12z.wrfsfcf01.grib2` under `directory_to_save`.
#'
#' Downloads use [amadeus::download_run()], which skips existing non-empty
#' destination files. Handled download failures are recorded in the returned
#' summary and reported with a warning. See that helper for concurrent
#' download setup and worker requirements.
#'
#' @section Concurrent downloads:
#' File downloads use caller-configured [mirai::daemons()] in the `"default"`
#' compute profile, or run sequentially when that profile has no daemons.
#' Configure a fixed pool with `mirai::daemons(n, .compute = "default")`
#' before calling this function. The caller manages pool creation and
#' shutdown; keep the pool size unchanged during downloads.
#'
#' Workers need compatible `amadeus` code and its dependencies, plus access
#' to the same destination storage as the calling process. Loading development
#' code in the calling process does not load it on workers; use
#' [mirai::everywhere()] to load the same code on workers when needed.
#' Validation, URL construction, directory setup, and hashing run in the
#' calling process. Only file downloads are dispatched to workers.
#'
#' @note No authentication token is required for this public archive.
#'
#' @return
#' If `hash = FALSE`, an invisible summary from [amadeus::download_run()]
#' containing `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`.
#' The two failure vectors are `character(0)` when no downloads failed,
#' including when every file was skipped. If `hash = TRUE`, a visible character
#' scalar containing the combined MD5 hash of all files recursively under
#' `directory_to_save`, including existing files.
#'
#' @seealso [amadeus::download_run()], [amadeus::sequence_dates()]
#'
#' @examples
#' \dontrun{
#' download_hrrr(
#'   product = "2d surface",
#'   sector = "conus",
#'   date = "2020-01-01",
#'   cycle_runtime = 12,
#'   forecast_hour = 1:18,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' download_hrrr(
#'   product = "sub-hourly",
#'   date = c("2020-01-01", "2020-01-03"),
#'   cycle_runtime = 0:23,
#'   forecast_hour = 1:6,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#'
#' @export
download_hrrr <- function(
  product = NULL,
  sector = "conus",
  date = c("2018-01-01", "2018-01-01"),
  cycle_runtime = 0:23,
  forecast_hour = 0:18,
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
    dataset_name = "hrrr",
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
  checkmate::assert_string(product, min.chars = 1L)
  chr_product <- tolower(product)
  checkmate::assert_choice(
    chr_product,
    c("3d pressure", "native", "2d surface", "sub-hourly", "subhourly")
  )
  checkmate::assert_string(sector, min.chars = 1L)
  chr_sector <- tolower(sector)
  checkmate::assert_choice(chr_sector, "conus")
  checkmate::assert_integerish(
    cycle_runtime,
    lower = 0L,
    upper = 23L,
    min.len = 1L,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    forecast_hour,
    lower = 0L,
    upper = 48L,
    min.len = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- sequence_dates(date, min = "2014-07-30", hyphen = FALSE)
  # ----------------------------------------------------------------------------
  # Generate saving and sector-specific directories.
  chr_dir_to_save <- download_setup_dir(directory_to_save)
  chr_dir_sector <- download_setup_dir(file.path(chr_dir_to_save, chr_sector))
  # ----------------------------------------------------------------------------
  # Switch product name to product code, including the sub-hourly alias.
  chr_product_code <- switch(
    gsub("-", "", chr_product, fixed = TRUE),
    "3d pressure" = "prs",
    "native" = "nat",
    "2d surface" = "sfc",
    "subhourly" = "subh"
  )
  # ----------------------------------------------------------------------------
  # Format distinct cycle run times and forecast hours.
  chr_cc <- sprintf("%02d", unique(as.integer(cycle_runtime)))
  chr_fh <- sprintf("%02d", unique(as.integer(forecast_hour)))
  # ----------------------------------------------------------------------------
  # Generate download URLs and destination files.
  df_download <- expand.grid(
    base = "https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.",
    date = chr_dates,
    sector = chr_sector,
    cc = chr_cc,
    product_code = chr_product_code,
    fh = chr_fh,
    dir = chr_dir_sector,
    stringsAsFactors = FALSE
  )
  df_download$file <- with(
    df_download,
    paste0("hrrr.t", cc, "z.wrf", product_code, "f", fh, ".grib2")
  )
  df_download$url <- with(
    df_download,
    paste0(base, date, "/", sector, "/", file)
  )
  df_download$destfile <- with(
    df_download,
    file.path(
      dir,
      paste0(
        "hrrr.",
        date,
        ".t",
        cc,
        "z.wrf",
        product_code,
        "f",
        fh,
        ".grib2"
      )
    )
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
  # Run download.
  download_result <- download_run(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  download_return(download_result, hash, chr_dir_to_save)
}

################################################################################
# {process} tier
################################################################################
process_hrrr <- function(
  date = c("2021-05-04", "2021-05-04"),
  variable = NULL,
  forecast_hour = 0,
  path = NULL,
  extent = NULL,
  daily_agg = FALSE,
  fun = "mean",
  ...
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  checkmate::assert_choice(variable, .amadeus$df_hrrr$layer_name)
  checkmate::assert_integerish(
    forecast_hour,
    lower = 0L,
    upper = 48L,
    len = 1L,
    any.missing = FALSE
  )
  # ----------------------------------------------------------------------------
  # Coerce `date` to vector of 2 if not already.
  date <- fl_dates(sequence_dates(date, hyphen = TRUE))
  # ----------------------------------------------------------------------------
  # Identify time-relevant HRRR file paths in `path`.
  chr_datapaths <- resolve_paths(
    date = date,
    path = path,
    pattern = paste0(
      "^hrrr\\.[0-9]{8}\\.t[0-9]{2}z\\.wrf",
      "(prs|nat|sfc|subh)f",
      sprintf("%02d", forecast_hour),
      "\\.grib2$"
    ),
    hyphen = FALSE
  )
  # ----------------------------------------------------------------------------
  # Import, clean, and stack hourly files; return raster.
  stack_hourly(
    paths = chr_datapaths,
    extent = extent,
    variable = variable,
    short = .amadeus$df_hrrr$short_name[.amadeus$df_hrrr$layer_name == variable]
  )
}

################################################################################
# {calculate} tier
################################################################################
calculate_hrrr <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  .by_time = NULL,
  ...
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  # ----------------------------------------------------------------------------
  # Perform extraction/zonal statistics.
  sf_extracted <- calc_worker_mirai(
    dataset = "hrrr",
    from = from,
    locs = locs,
    locs_id = locs_id,
    variable = 1,
    level = 2,
    time = c(3, 4),
    time_type = "hour",
    radius = radius,
    fun = fun,
    weights = weights,
    geom = geom,
    ...
  )
  # ----------------------------------------------------------------------------
  # Summarize by time if requested.
  lgl_summarized <- FALSE
  if (!is.null(.by_time)) {
    sf_extracted <- amadeus::calc_summarize_by(
      covar = sf_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = "level"
    )
    lgl_summarized <- TRUE
  }
  if (lgl_summarized && "time" %in% names(sf_extracted)) {
    sf_extracted$time <- as.POSIXct(sf_extracted$time, tz = "UTC")
  }
  # ----------------------------------------------------------------------------
  # Return cleaned output.
  amadeus::calc_return_locs(
    covar = sf_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
}

################################################################################
