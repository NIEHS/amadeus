################################################################################
# {amadeus} functions for SPEI drought data.
################################################################################

################################################################################
# {download} tier
################################################################################
#' Download SPEI drought data
#' @description
#' Download one multi-year SPEI NetCDF file for a selected accumulation
#' timescale and database version, using the shared download workflow.
#' @param timescale integer(1). Required positive accumulation timescale in
#'   months. The value is padded to at least two digits in the filename;
#'   for example, `1L` selects `spei01.nc`. Validation does not establish
#'   whether a file is available for the requested timescale.
#' @param version character(1). `"2.11"` (default), `"2.10"`, or `"legacy"`.
#'   Uses [base::match.arg()], allowing unambiguous partial matches.
#'   `"legacy"` selects the unversioned `/files/` endpoint and does not
#'   identify a particular database release. No automatic fallback occurs.
#' @param directory_to_save character(1). Non-empty directory for downloaded
#'   files. Files retain their provider names directly in this directory;
#'   no version-specific subdirectory is created.
#' @inheritParams download_drought acknowledgement download remove_command
#' @inheritParams download_drought show_progress max_tries rate_limit
#' @param hash logical(1). Return a combined MD5 hash of all files under
#'   `directory_to_save`, including existing files, instead of the download
#'   summary. Default is `FALSE`; ignored when `download = FALSE`.
#' @param ... Reserved for future use; currently ignored.
#' @details
#' Version `"2.11"` uses `spei_database_2_11/nc/`, version `"2.10"` uses
#' `spei_database_2_10/nc/`, and `"legacy"` uses `files/` on the SPEI server.
#' Each call constructs one URL. Dates do not select or subset this download;
#' use [process_drought()] for temporal selection after downloading.
#'
#' All versions use the same local `speiNN.nc` filename. This preserves the
#' filename expected by drought processing, but existing-file checks do not
#' identify which version produced a file. Use separate output directories
#' when retaining multiple versions.
#'
#' A preview creates the output directory and returns the URL and destination
#' without contacting the server. Existing files remain in the preview.
#' Actual downloads use [download_run_method()], which skips existing
#' non-empty files and reports failures without selecting another database
#' version.
#' @return With `download = FALSE`, an invisible list containing `urls`,
#'   `destfiles`, and `n_files` (one). Otherwise, an invisible download summary
#'   from [download_run_method()] when `hash = FALSE`, or a visible character
#'   scalar containing the output-directory MD5 hash when `hash = TRUE`.
#'
#' The download summary always contains `success`, `failed`, `skipped`,
#' `failed_urls`, and `failed_files`. The two failure vectors are
#' `character(0)` when no files failed, including when every file was skipped.
#' @author Insang Song
#' @seealso [download_drought()], [process_drought()], [calculate_drought()]
#' @examples
#' \dontrun{
#' download_spei(
#'   timescale = 1L,
#'   version = "2.11",
#'   directory_to_save = file.path(tempdir(), "spei"),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_spei <- function(
  timescale,
  version = c("2.11", "2.10", "legacy"),
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
  download_check(
    dataset_name = "spei",
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
  chr_version <- match.arg(version)
  checkmate::assert_int(timescale, lower = 1L)
  chr_timescale <- sprintf("%02d", timescale)
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Select one version; legacy refers to the unversioned /files/ endpoint.
  chr_base <- if (chr_version == "legacy") {
    "https://spei.csic.es/files/"
  } else {
    sprintf(
      "https://spei.csic.es/spei_database_%s/nc/",
      gsub(".", "_", chr_version, fixed = TRUE)
    )
  }
  # ----------------------------------------------------------------------------
  # Generate the URL for the selected version's multi-year timescale file.
  chr_file <- paste0("spei", chr_timescale, ".nc")
  chr_url <- paste0(chr_base, chr_file)
  # ----------------------------------------------------------------------------
  # Retain the flat directory and filename expected by drought processing.
  chr_filename <- paste0(chr_dir_to_save, chr_file)
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(download_preview(chr_url, chr_filename)))
  }
  # ----------------------------------------------------------------------------
  # Run download, including existing-file checks and failure reporting.
  download_result <- download_run_method(
    urls = chr_url,
    destfiles = chr_filename,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  download_return(download_result, hash, chr_dir_to_save)
}
