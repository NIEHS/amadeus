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
  amadeus:::download_check(
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
