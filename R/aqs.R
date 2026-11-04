#' Download air quality data
#' @description
#' The \code{download_aqs()} function accesses and downloads Air Quality
#' System (AQS) data from the U.S. Environmental Protection Agency's (EPA)
#' Pre-Generated Data Files.
#' @note AQS data does not require authentication.
#' AQS measurements are generally intended for use as dependent variables, so
#' the package supports download and processing for AQS but does not expose
#' AQS through `calculate_covariates()`.
#' @details Common AQS parameter codes include:
#' * `88101` — PM2.5 - Local Conditions
#' * `88502` — Acceptable PM2.5 AQI & Speciation Mass
#' * `81102` — PM10 Total 0-10um STP
#' * `44201` — Ozone
#' * `42602` — Nitrogen dioxide (NO2)
#' * `42401` — Sulfur dioxide (SO2)
#' * `42101` — Carbon monoxide
#'
#' This list is not exhaustive; for the full official table, see the linked EPA
#' AQS parameter code table.
#' @param parameter_code integer(1). EPA pollutant parameter code. See
#'   Details for a short list of common codes.
#' @param resolution_temporal character(1). Currently only "daily" is supported.
#' @param url_aqs_download character(1). URL to the AQS pre-generated datasets.
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip zip files (default TRUE).
#' @param remove_zip logical(1). Remove zip files after unzipping (default
#' FALSE).
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @seealso
#'   [EPA AQS Parameter Codes](
#'     https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv
#'   )
#' @references
#' \insertRef{data_usepa2023airdata}{amadeus}
#' @examples
#' \dontrun{
#' download_aqs(
#'   parameter_code = 88101,
#'   resolution_temporal = "daily",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_aqs <-
  function(
    parameter_code = 88101,
    resolution_temporal = "daily",
    year = c(2018, 2022),
    url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
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
    #### Check acknowledgement
    amadeus::download_permit(acknowledgement = acknowledgement)

    #### Check for null parameters
    amadeus::check_for_null_parameters(mget(ls()))

    #### Check years
    year <- if (length(year) == 1) rep(year, 2) else year
    stopifnot(length(year) == 2)
    year <- year[order(year)]

    #### Directory setup
    directory_original <- amadeus::download_sanitize_path(directory_to_save)
    directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
    directory_to_download <- directories[1]
    directory_to_save <- directories[2]

    #### Handle deprecated parameters
    if (!isTRUE(download)) {
      warning(
        "Setting download=FALSE is deprecated.",
        " Downloads now use httr2 by default.\n",
        "To skip downloading, the function will return",
        " after discovering files.\n",
        call. = FALSE
      )
    }

    if (remove_command != FALSE) {
      warning(
        "Parameter 'remove_command' is deprecated and ignored.\n",
        call. = FALSE
      )
    }

    #### Define year sequence
    year_sequence <- seq(year[1], year[2], 1)

    #### Build URLs
    download_urls <- sprintf(
      paste(
        url_aqs_download,
        resolution_temporal,
        "_",
        parameter_code,
        "_%d.zip",
        sep = ""
      ),
      year_sequence
    )

    #### Build download file names
    download_names <- sprintf(
      paste(
        directory_to_download,
        "aqs_",
        resolution_temporal,
        "_",
        parameter_code,
        "_%d.zip",
        sep = ""
      ),
      year_sequence
    )

    #### Filter to files that need downloading
    needs_download <- sapply(download_names, amadeus::check_destfile)
    download_urls_filtered <- download_urls[needs_download]
    download_names_filtered <- download_names[needs_download]

    #### Check for valid URL only when actually downloading new files
    if (
      isTRUE(download) &&
        length(download_urls_filtered) > 0 &&
        !amadeus::check_url_status(download_urls_filtered[1])
    ) {
      stop(paste0(
        "Invalid year returns HTTP code 404. ",
        "Check `year` parameter.\n"
      ))
    }

    #### Exit early if download=FALSE
    if (!isTRUE(download)) {
      message(sprintf(
        "Skipping download. Found %d files available for download.\n",
        length(download_urls_filtered)
      ))
      return(invisible(list(
        urls = download_urls_filtered,
        destfiles = download_names_filtered,
        n_files = length(download_urls_filtered)
      )))
    }

    #### Download files using httr2
    if (length(download_urls_filtered) > 0) {
      download_result <- amadeus::download_run_method(
        urls = download_urls_filtered,
        destfiles = download_names_filtered,
        token = NULL,
        show_progress = show_progress,
        max_tries = max_tries,
        rate_limit = rate_limit
      )
    } else {
      message("All files already exist. Nothing to download.\n")
      download_result <- list(
        success = 0,
        failed = 0,
        skipped = length(download_names)
      )
    }

    #### Unzip data
    sapply(
      download_names,
      amadeus::download_unzip,
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
    if (isTRUE(unzip)) {
      sapply(
        year_sequence,
        function(year_i) {
          download_normalize_aqs_unzip(
            # nolint: object_usage_linter
            directory_to_unzip = directory_to_save,
            resolution_temporal = resolution_temporal,
            parameter_code = parameter_code,
            year = year_i
          )
        }
      )
    }

    amadeus::download_remove_zips(
      remove = remove_zip,
      download_name = download_names
    )

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    } else {
      return(invisible(download_result))
    }
  }

#' Normalize AQS unzip layout
#' @description
#' Move files from an AQS archive's nested year directory into the main
#' `data_files` directory when the archive unpacks into a single folder.
#' @param directory_to_unzip character(1). Directory where files were unzipped.
#' @param resolution_temporal character(1). Temporal resolution used in the
#'   AQS archive name.
#' @param parameter_code integer(1). AQS parameter code used in the archive
#'   name.
#' @param year integer(1). Year represented by the archive.
#' @return NULL; normalizes the AQS data file layout in place
#' @keywords internal
#' @noRd
download_normalize_aqs_unzip <- function(
  directory_to_unzip,
  resolution_temporal,
  parameter_code,
  year
) {
  nested_dir <- file.path(
    directory_to_unzip,
    sprintf("%s_%s_%s", resolution_temporal, parameter_code, year)
  )

  if (!dir.exists(nested_dir)) {
    return(invisible(NULL))
  }

  nested_files <- list.files(
    nested_dir,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(nested_files) == 0 || any(dir.exists(nested_files))) {
    return(invisible(NULL))
  }

  for (nested_file in nested_files) {
    target_file <- file.path(directory_to_unzip, basename(nested_file))
    if (!file.exists(target_file)) {
      moved <- file.rename(nested_file, target_file)
      if (!isTRUE(moved)) {
        # nocov start
        stop(sprintf(
          "Failed to normalize AQS unzip layout for %s.",
          basename(nested_file)
        ))
        # nocov end
      }
    } else {
      unlink(nested_file)
    }
  }

  unlink(nested_dir, recursive = TRUE)
  invisible(NULL)
}


