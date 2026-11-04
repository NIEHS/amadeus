################################################################################
# nolint start
#' Download IMPROVE aerosol monitoring data
#' @description
#' The \code{download_improve()} function accesses and downloads IMPROVE
#' (Interagency Monitoring of Protected Visual Environments) data files
#' from the VIEWS/VIBE data export service hosted at CIRA/CSU. Annual files
#' are downloaded as \code{.txt.zip} archives and extracted to
#' pipe-delimited \code{.txt} files containing aerosol measurements at
#' federal-land monitoring stations.
#' @note
#' \itemize{
#'   \item IMPROVE data does not require authentication.
#'   \item Three product types are available:
#'     \code{"raw"} (IMPAER — speciated aerosol mass concentrations),
#'     \code{"rhr2"} (IMPRHR2 — Regional Haze Rule II light extinction),
#'     \code{"rhr3"} (IMPRHR3 — Regional Haze Rule III deciview index).
#'   \item Site metadata is handled by \code{\link{process_improve}} using an
#'     embedded table; annual downloads include measurement files only.
#'   \item IMPROVE monitors ~\eqn{1 \mu g / m^3} precision instruments
#'     deployed at Class I and other federal land areas.
#' }
#' @param year integer(1 or 2). Year or start/end years.
#' @param product character(1). Product selector:
#'   \code{"raw"} (aerosol, default), \code{"rhr2"} (Regional Haze Rule II),
#'   or \code{"rhr3"} (Regional Haze Rule III).
#' @param url_improve character(1). Base URL to the IMPROVE data export
#'   service.
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default
#'   \code{TRUE}).
#' @param hash logical(1). Return hash of downloaded files (default
#'   \code{FALSE}).
#' @param max_tries integer(1). Maximum retry attempts (default 20).
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2).
#' @author Insang Song, Mitchell Manware
#' @return invisible list with download results; or hash character if
#'   \code{hash = TRUE}.
#' @importFrom httr2 request req_retry req_timeout req_perform resp_status
#' @seealso
#'   \code{\link{process_improve}}
#' @examples
#' \dontrun{
#' download_improve(
#'   year = 2022,
#'   product = "raw",
#'   directory_to_save = "./data/improve/",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_improve <- function(
  year = c(2018, 2022),
  product = c("raw", "rhr2", "rhr3"),
  url_improve = "https://vibe.cira.colostate.edu/data/export/",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      call. = FALSE
    )
  }
  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Validate product
  product <- match.arg(product)

  #### Map product to file prefix
  prefix_map <- c(raw = "IMPAER", rhr2 = "IMPRHR2", rhr3 = "IMPRHR3")
  file_prefix <- prefix_map[[product]]

  #### Check years
  year <- if (length(year) == 1) rep(year, 2) else year
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  year_sequence <- seq(year[1], year[2], 1)

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Build download URLs and destination paths
  # nolint start
  download_urls <- sprintf(
    "%s%s/%s_%d.txt.zip",
    url_improve,
    file_prefix,
    file_prefix,
    year_sequence
  )
  # nolint end
  download_names <- sprintf(
    "%s%s_%d.txt",
    directory_to_save,
    file_prefix,
    year_sequence
  )

  #### Filter to files that need downloading
  needs_dl <- vapply(download_names, amadeus::check_destfile, logical(1))
  download_urls <- download_urls[needs_dl]
  download_names <- download_names[needs_dl]

  if (length(download_urls) == 0) {
    message("All IMPROVE files already present.\n")
    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    }
    return(invisible(list(
      success = 0,
      failed = 0,
      skipped = length(download_names)
    )))
  }

  #### Download files
  zip_destfiles <- sprintf("%s.zip", download_names)
  zip_destfiles <- sub("\\.txt\\.zip$", ".txt.zip", zip_destfiles)

  download_result <- amadeus::download_run_method(
    urls = download_urls,
    destfiles = zip_destfiles,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  #### Extract annual IMPROVE zip files to text files
  zip_targets <- zip_destfiles[grepl("\\.txt\\.zip$", zip_destfiles)]
  if (length(zip_targets) > 0) {
    for (zip_file in zip_targets) {
      if (!file.exists(zip_file)) {
        next
      }
      utils::unzip(
        zipfile = zip_file,
        exdir = directory_to_save,
        overwrite = TRUE
      )
      file.remove(zip_file)
    }
  }

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  }
  return(invisible(download_result))
}

