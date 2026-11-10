################################################################################
# {amadeus} functions for EDDI drought data.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download EDDI drought data
#' @description
#' Construct EDDI ASCII-grid URLs for Tuesdays within a date interval and
#' download the selected accumulation timescale using the shared workflow.
#' @param date character(1 or 2). Required date or inclusive range endpoints
#'   in `"YYYY-MM-DD"` format, in either order. Dates must be between
#'   1950-01-01 and today. A single Tuesday selects one file; an interval
#'   containing no Tuesdays raises an error before directory creation.
#' @param timescale integer(1). Required positive accumulation timescale in
#'   months. Validation does not establish provider availability. The value
#'   is padded to at least two digits in filenames, such as `01mn` for `1L`.
#' @param directory_to_save character(1). Non-empty directory for downloaded
#'   `.asc` files. Files are saved directly here without archive or
#'   variable-specific subdirectories.
#' @inheritParams download_drought acknowledgement download remove_command
#' @inheritParams download_drought show_progress max_tries rate_limit
#' @param hash logical(1). Return a combined MD5 hash of all files under
#'   `directory_to_save`, including existing files, instead of the download
#'   summary. Default is `FALSE`; ignored when `download = FALSE`.
#' @param ... Reserved for future use; currently ignored.
#' @details
#' URLs use the FTP `Projects/EDDI/CONUS_archive/data` endpoint with one year
#' directory per file. Local names follow `EDDI_ETrs_NNmn_YYYYMMDD.asc`.
#' All requested Tuesdays are included in the plan; remote availability is
#' not discovered from directory listings.
#'
#' A preview creates the output directory and returns all planned URLs and
#' destinations, including existing files, without contacting the provider.
#' Actual downloads check the first URL before calling [download_run_method()].
#' The current preflight helper uses HTTP status checks although these URLs
#' use FTP; a failed check warns and allows the download attempt to proceed.
#' The runner skips existing non-empty files and records individual failures.
#' @return With `download = FALSE`, an invisible list containing `urls`,
#'   `destfiles`, and `n_files`. Otherwise, an invisible download summary from
#'   [download_run_method()] when `hash = FALSE`, or a visible character scalar
#'   containing the output-directory MD5 hash when `hash = TRUE`.
#' @author Insang Song
#' @seealso [download_drought()], [process_drought()], [calculate_drought()]
#' @examples
#' \dontrun{
#' download_eddi(
#'   date = c("2020-01-07", "2020-01-21"),
#'   timescale = 1L,
#'   directory_to_save = file.path(tempdir(), "eddi"),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_eddi <- function(
  date,
  timescale,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  ...
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "eddi",
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
  checkmate::assert_int(timescale, lower = 1L)
  chr_timescale <- sprintf("%02d", timescale)
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- amadeus:::sequence_dates(date, hyphen = TRUE)
  chr_wdates <- amadeus:::drought_weekly_dates(
    chr_dates[1L],
    chr_dates[length(chr_dates)]
  )
  if (length(chr_wdates) == 0) {
    stop(
      "No Tuesday dates found in the specified date range. ",
      "EDDI is a weekly (Tuesday) product.\n"
    )
  }
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- amadeus::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Generate one download row per requested weekly date.
  df_download <- expand.grid(
    base = "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data/",
    timescale = chr_timescale,
    date = chr_wdates,
    dir = chr_dir_to_save,
    stringsAsFactors = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate matching file names, URLs, and local destinations.
  df_download$file <- with(
    df_download,
    sprintf("EDDI_ETrs_%smn_%s.asc", timescale, date)
  )
  df_download$url <- with(
    df_download,
    paste0(base, substr(date, 1, 4), "/", file)
  )
  df_download$destfile <- with(df_download, paste0(dir, file))
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}
