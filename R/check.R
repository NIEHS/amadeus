################################################################################
# {amadeus} functions for checking function arguments for all tiers
################################################################################

################################################################################
# {download} tier
################################################################################
#' Validate shared download arguments
#' @description
#' Check the common arguments supplied to source-specific download functions
#' before discovery or downloading. Invalid arguments raise errors; deprecated
#' argument values emit warnings without being changed.
#' @param dataset_name character(1). Non-empty dataset label. This helper
#'   checks its type and length; supported dataset names are checked by callers.
#' @param acknowledgement logical(1). Must be `TRUE` to proceed.
#' @param download logical(1). Deprecated download flag. `FALSE` emits a
#'   warning; the caller handles returning a preview.
#' @param remove_command logical(1). Deprecated flag. `TRUE` emits a warning.
#' @param unzip logical(1). Whether downloaded archives should be extracted.
#' @param remove_zip logical(1). Whether downloaded archives should be removed.
#'   May be `TRUE` only when `unzip = TRUE`.
#' @param directory_to_save character(1). Non-empty output directory path.
#'   Existence and write permissions are not checked here.
#' @param show_progress logical(1). Whether download progress is requested.
#' @param max_tries integer(1). Maximum attempts per request, including the
#'   initial attempt. Must be at least 1.
#' @param rate_limit numeric(1). Finite, non-negative request-throttling
#'   interval in seconds. The caller determines how this value is applied.
#' @param hash logical(1). Whether a directory hash is requested.
#' @return Called for validation and warnings; callers should ignore the
#'   return value. An invalid argument stops execution with an error.
#' @seealso [download_setup_dir()], [download_preview()],
#' [download_run_method()]
#' @keywords internal
#' @noRd
download_check <- function(
  dataset_name,
  acknowledgement,
  download,
  remove_command,
  unzip,
  remove_zip,
  directory_to_save,
  show_progress,
  max_tries,
  rate_limit,
  hash
) {
  # ----------------------------------------------------------------------------
  # Ensure `dataset_name` is a non-empty character scalar.
  checkmate::assert_string(dataset_name, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Ensure `acknowledgement` is TRUE/FALSE.
  checkmate::assert_flag(acknowledgement)
  if (!acknowledgement) {
    stop(paste0(
      "Data download acknowledgement is set to FALSE. ",
      "Please acknowledge that the data downloaded using this ",
      "function may be very large and use lots of machine storage ",
      "and memory.\n"
    ))
  }
  # ----------------------------------------------------------------------------
  # Ensure `download` is TRUE/FALSE.
  checkmate::assert_flag(download)
  # ----------------------------------------------------------------------------
  # Warn deprecated use of `download`.
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # Ensure `remove_command` is TRUE/FALSE.
  checkmate::assert_flag(remove_command)
  # ----------------------------------------------------------------------------
  # Warn deprecated use of `remove_command`.
  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }
  # ----------------------------------------------------------------------------
  # Ensure `unzip` is TRUE/FALSE.
  checkmate::assert_flag(unzip)
  # ----------------------------------------------------------------------------
  # Ensure `remove_zip` is TRUE/FALSE.
  checkmate::assert_flag(remove_zip)
  # ----------------------------------------------------------------------------
  # Error if `unzip == FALSE` and `remove_zip == TRUE`.
  checkmate::assert_true(!remove_zip || unzip)
  # ----------------------------------------------------------------------------
  # Ensure `directory_to_save` is a non-empty character scalar.
  checkmate::assert_string(directory_to_save, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Ensure `show_progress` is TRUE/FALSE.
  checkmate::assert_flag(show_progress)
  # ----------------------------------------------------------------------------
  # Ensure `max_tries` is a positive integer-valued scalar.
  checkmate::assert_int(max_tries, lower = 1L)
  # ----------------------------------------------------------------------------
  # Ensure `rate_limit` is a finite, non-negative numeric scalar.
  checkmate::assert_number(rate_limit, lower = 0, finite = TRUE)
  # ----------------------------------------------------------------------------
  # Ensure `hash` is TRUE/FALSE.
  checkmate::assert_flag(hash)
  # ----------------------------------------------------------------------------
}

################################################################################
# {process} tier
################################################################################

################################################################################
# {calculate} tier
################################################################################
