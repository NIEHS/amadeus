################################################################################
# {amadeus} functions for USDM drought data.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download USDM drought data
#' @description
#' Download U.S. Drought Monitor ZIP archives for Tuesdays within a date
#' interval, with optional extraction of drought-class shapefiles.
#' @param date character(1 or 2). Required date or inclusive range endpoints
#'   in `"YYYY-MM-DD"` format, in either order. Dates must be between
#'   1950-01-01 and today. A single Tuesday selects one archive; an interval
#'   containing no Tuesdays raises an error before directory creation.
#' @param directory_to_save character(1). Non-empty parent directory.
#'   Archives are saved in `zip_files/` and extracted into `data_files/`.
#' @inheritParams download_drought acknowledgement download remove_command
#' @inheritParams download_drought show_progress max_tries rate_limit
#' @param hash logical(1). Return a combined MD5 hash of all files under
#'   `data_files/`, including existing files, instead of the download summary.
#'   Default is `FALSE`; ignored when `download = FALSE`. With `unzip = FALSE`,
#'   hashing still uses `data_files/`, not the downloaded archives.
#' @param unzip logical(1). Extract downloaded or existing archives into
#'   `data_files/`. Default is `TRUE`; `FALSE` skips extraction.
#' @param remove_zip logical(1). Remove archives and their parent `zip_files/`
#'   directory recursively after extraction, including any remaining contents.
#'   Requires `unzip = TRUE`. Default is `FALSE`. No cleanup occurs during
#'   previews.
#' @param ... Reserved for future use; currently ignored.
#' @details
#' Each selected Tuesday maps to a `USDM_YYYYMMDD_M.zip` archive at the
#' provider's `data/shapefiles_m` endpoint. The URL plan does not query
#' directory listings or establish availability for every selected date.
#'
#' Both `zip_files/` and `data_files/` are created before a preview returns,
#' including when extraction is disabled. Preview results include existing
#' archive paths and require no network requests. Actual downloads check the
#' first URL; a failed preflight warns and downloading is still attempted.
#'
#' [download_run_method()] skips existing non-empty archives and records
#' individual download failures. All planned archives are subsequently passed
#' to [download_unzip()], including existing archives, with the selected
#' `unzip` flag. Paths inside archives are preserved. Extraction errors can
#' prevent a normal return; download summary counts do not report extraction
#' success. Pass `data_files/` to [process_drought()] for USDM processing.
#' @return With `download = FALSE`, an invisible list containing `urls`,
#'   archive `destfiles`, and `n_files`. Otherwise, after optional extraction
#'   and cleanup, an invisible summary from [download_run_method()] when
#'   `hash = FALSE`, or a visible character scalar containing the combined
#'   MD5 hash of `data_files/` when `hash = TRUE`.
#' @author Insang Song
#' @seealso [download_drought()], [process_drought()], [calculate_drought()]
#' @examples
#' \dontrun{
#' download_usdm(
#'   date = c("2020-01-07", "2020-01-21"),
#'   directory_to_save = file.path(tempdir(), "usdm"),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_usdm <- function(
  date,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  ...
) {
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "usdm",
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
  # Check `date` and generate sequence.
  chr_dates <- amadeus:::sequence_dates(date, hyphen = TRUE)
  chr_wdates <- amadeus:::drought_weekly_dates(
    chr_dates[1L],
    chr_dates[length(chr_dates)]
  )
  if (length(chr_wdates) == 0) {
    stop(
      "No Tuesday dates found in the specified date range. ",
      "USDM is a weekly (Tuesday) product.\n"
    )
  }
  # ----------------------------------------------------------------------------
  # Generate zip and data file directories.
  chr_directories <- amadeus:::download_setup_dir(directory_to_save, zip = TRUE)
  chr_dir_to_download <- chr_directories[1]
  chr_dir_to_save <- chr_directories[2]
  # ----------------------------------------------------------------------------
  # Generate download URLs for the requested weekly dates.
  chr_urls <- sprintf(
    "https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_%s_M.zip",
    chr_wdates
  )
  # ----------------------------------------------------------------------------
  # Generate matching archive paths in zip_files/.
  chr_filenames <- sprintf(
    "%sUSDM_%s_M.zip",
    chr_dir_to_download,
    chr_wdates
  )
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
  # Unzip downloaded .zip files.
  sapply(
    chr_filenames,
    amadeus:::download_unzip,
    directory_to_unzip = chr_dir_to_save,
    unzip = unzip
  )
  # ----------------------------------------------------------------------------
  # Remove zip files.
  amadeus:::download_remove_zips(
    remove = remove_zip,
    download_name = chr_filenames
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
}
