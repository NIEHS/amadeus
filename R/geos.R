#' Download atmospheric composition data
#' @description
#' The \code{download_geos()} function accesses and downloads various
#' atmospheric composition collections from NASA's Global Earth Observing
#' System (GEOS)
#' compositional forecast model.
#' @note Due to NASA data access policies, downloads require a valid NASA
#' Earthdata token for authentication. Use \code{setup_nasa_token()} for setup.
#' @param collection character(1). GEOS-CF data collection file name.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token.
#' @param date character(1 or 2). Date range "YYYY-MM-DD" format
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{keller_description_2021}{amadeus}
#' @examples
#' \dontrun{
#' download_geos(
#'   collection = "aqc_tavg_1hr_g1440x721_v1",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_geos <- function(
  collection = c(
    "aqc_tavg_1hr_g1440x721_v1",
    "chm_tavg_1hr_g1440x721_v1",
    "met_tavg_1hr_g1440x721_x1",
    "xgc_tavg_1hr_g1440x721_x1",
    "chm_inst_1hr_g1440x721_p23",
    "met_inst_1hr_g1440x721_p23"
  ),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 3. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 4. Check and retrieve NASA token
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### 5. Check for null parameters (AFTER token retrieval)
  params_to_check <- mget(ls())
  params_to_check <- params_to_check
  amadeus::check_for_null_parameters(params_to_check)

  #### 6. Match collection
  collection <- match.arg(collection, several.ok = TRUE)

  #### 7. Handle deprecated parameters
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

  #### 8. Define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )

  #### 9. Define URL base
  base <- "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/"

  #### 10. Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (c in seq_along(collection)) {
    collection_loop <- collection[c]
    download_folder <- paste0(directory_to_save, collection_loop, "/")

    if (!dir.exists(download_folder)) {
      dir.create(download_folder, recursive = TRUE)
    }

    for (d in seq_along(date_sequence)) {
      date_str <- date_sequence[d]
      year <- substr(date_str, 1, 4)
      month <- substr(date_str, 5, 6)
      day <- substr(date_str, 7, 8)
      time_sequence <- amadeus::generate_time_sequence(collection_loop)

      for (t in seq_along(time_sequence)) {
        download_url_base <- paste0(
          base,
          "Y",
          year,
          "/M",
          month,
          "/D",
          day,
          "/"
        )
        download_name <- paste0(
          "GEOS-CF.v01.rpl.",
          collection_loop,
          ".",
          date_str,
          "_",
          time_sequence[t],
          "z.nc4"
        )
        download_url <- paste0(download_url_base, download_name)

        # Validate first URL only
        if (c == 1 && d == 1 && t == 1) {
          if (!amadeus::check_url_status(download_url)) {
            stop(paste0(
              download_url,
              " Invalid date returns HTTP code 404. ",
              "Check `date` parameter.\n"
            ))
          }
        }

        download_folder_name <- paste0(download_folder, download_name)

        if (amadeus::check_destfile(download_folder_name)) {
          all_urls <- c(all_urls, download_url)
          all_destfiles <- c(all_destfiles, download_folder_name)
        }
      }
    }
  }

  #### 11. Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = nasa_earth_data_token,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}


#' Generate time sequence
#' @description
#' Generate a sequence of time values based on the GEOS-CF collection.
#' @param collection character(1). GEOS-CF data collection
#' @return vector
#' @note
#' GEOS-CF hourly values are observed on the hour (ie. 0000 = 12:00:00 AM, 0100
#' = 01:00:00 AM) or the half hour (ie. 0030 = 12:30:00 AM, 0130 = 01:30:00 AM).
#' Typically, 2-dimensional collections (latitude and longitude only) utilize
#' half hour, and 3-dimensional collections (latitude, longitude, and time)
#' utilize hour.
#' @keywords internal auxiliary
#' @export
generate_time_sequence <-
  function(
    collection
  ) {
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      ts <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      ts <- seq(from = 0, to = 2300, by = 100)
    }
    time_sequence <- sprintf("%04d", ts)
    return(time_sequence)
  }

