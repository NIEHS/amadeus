#' Download wildfire smoke data
#' @description
#' The \code{download_hms()} function accesses and downloads
#' wildfire smoke plume coverage data from NOAA's Hazard Mapping System Fire
#' and Smoke Product.
#' @note HMS data does not require authentication.
#' @param data_format character(1). "Shapefile" or "KML".
#' @param date character(1 or 2). Date range "YYYY-MM-DD" format
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
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom utils head tail
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_HMSabout}{amadeus}
#' @examples
#' \dontrun{
#' download_hms(
#'   data_format = "Shapefile",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_hms <- function(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
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

  #### Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  if (as.Date(date[1]) < as.Date("2005-08-05")) {
    stop("NOAA HMS wildfire smoke data begins at August 05, 2005.")
  }

  #### Directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  #### Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Check for unzip/remove_zip conflict
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    ))
  }

  #### Define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )

  #### Define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"

  #### Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (f in seq_along(date_sequence)) {
    year <- substr(date_sequence[f], 1, 4)
    month <- substr(date_sequence[f], 5, 6)

    if (tolower(data_format) == "shapefile") {
      data_format <- "Shapefile"
      suffix <- ".zip"
      directory_to_cat <- directory_to_download
    } else if (tolower(data_format) == "kml") {
      data_format <- "KML"
      suffix <- ".kml"
      directory_to_cat <- directory_to_save
    }

    url <- paste0(
      base,
      data_format,
      "/",
      year,
      "/",
      month,
      "/hms_smoke",
      date_sequence[f],
      suffix
    )

    # Validate first URL only
    if (f == 1) {
      if (!amadeus::check_url_status(url)) {
        stop(paste0(
          "Invalid date returns HTTP code 404. ",
          "Check `date` parameter.\n"
        ))
      }
    }

    destfile <- paste0(
      directory_to_cat,
      "hms_smoke_",
      data_format,
      "_",
      date_sequence[f],
      suffix
    )

    if (amadeus::check_destfile(destfile)) {
      all_urls <- c(all_urls, url)
      all_destfiles <- c(all_destfiles, destfile)
    }
  }

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(all_urls)
    ))
    return(invisible(list(
      urls = all_urls,
      destfiles = all_destfiles,
      n_files = length(all_urls)
    )))
  }

  if (length(all_urls) == 0L) {
    message("All requested HMS files already exist. Nothing to download.\n")
    return(invisible(list(
      success = 0,
      failed = 0,
      skipped = length(date_sequence)
    )))
  }

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  #### Handle KML (no unzipping needed)
  if (data_format == "KML") {
    unlink(directory_to_download, recursive = TRUE)
    message("KML files cannot be unzipped.\n")
    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    } else {
      return(invisible(download_result))
    }
  }

  #### Unzip downloaded zip files
  for (d in seq_along(all_destfiles)) {
    amadeus::download_unzip(
      file_name = all_destfiles[d],
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
  }

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = all_destfiles
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

