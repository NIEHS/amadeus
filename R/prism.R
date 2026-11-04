################################################################################
# 3. PRISM Climate Group Web Service
# nolint start
#' Download PRISM data
#' @description
#' Accesses and downloads Oregon State University's
#' PRISM data from the PRISM Climate Group Web Service
#' @param time character(1). Length of 2, 4, 6, or 8. Time period for
#' time series or normals. According to the PRISM Web Service Guide,
#' acceptable formats include (disclaimer: the following is a direct quote;
#' minimal formatting is applied):
#' __Time Series__:
#' * `YYYYMMDD` for daily data (between yesterday and January 1st, 1981)
#'   – returns a single grid in a .zip file
#' * `YYYYMM` for monthly data (between last month and January 1981)
#'   – returns a single grid in a .zip file
#' * `YYYY` for annual data (between last year and 1981) - returns a single
#'   grid in a .zip file
#' * `YYYY` for historical data (between 1980 and 1895) - returns a single
#'   zip file containing 12 monthly grids for `YYYY` plus the annual.
#'
#' __Normals__:
#' * Monthly normal: date is `MM` (i.e., 04 for April) or the value 14,
#'   which returns the annual normal
#' * Daily normal: date is `MMDD` (i.e., 0430 for April 30)
#' @param element character(1). Data element.
#' One of `c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")`
#' For normals, `c("solslope", "soltotal", "solclear", "soltrans")`
#' are also accepted.
#' @param data_type character(1). Data type.
#' * `"ts"`: 4km resolution time series.
#' * `"normals_800"`: 800m resolution normals.
#' * `"normals"`: 4km resolution normals.
#' @param format character(1). Data format. Only applicable for
#'   `data_type = "ts"`.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip the downloaded zip file to extract the
#'   data files (nc, grib2, etc.) into \code{directory_to_save}.
#'   Default is \code{TRUE}. The PRISM API always returns a zip
#'   regardless of the requested format.
#' @param remove_zip logical(1). Remove the zip file after unzipping.
#'   Default is \code{FALSE}. Only applies when \code{unzip = TRUE}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#'   an \code{rlang::hash_file()} hash character corresponding to the
#'   downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @author Insang Song
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * .bil (normals) or single grid files depending on the format
#' choice will be stored in \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_daly2000prism}{amadeus}
#' @examples
#' \dontrun{
#' download_prism(
#'   time = "202104",
#'   element = "ppt",
#'   data_type = "ts",
#'   format = "nc",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE
#' )
#' }
#' @references
#' * [PRISM Climate Group](https://prism.oregonstate.edu/)
#' * [PRISM Web Service Guide](https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf)
#' @export
# nolint end
download_prism <- function(
  time,
  element = c(
    "ppt",
    "tmin",
    "tmax",
    "tmean",
    "tdmean",
    "vpdmin",
    "vpdmax",
    "solslope",
    "soltotal",
    "solclear",
    "soltrans"
  ),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  data_type <- match.arg(data_type)
  element <- match.arg(element)
  format <- match.arg(format)

  if (startsWith(data_type, "ts")) {
    if (startsWith(element, "sol")) {
      stop("sol* elements are not available for 'ts' data type.")
    }
  } else {
    message("format is ignored for normals data type.")
  }

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
  if (!isFALSE(remove_command)) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 1. check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  url_middle <-
    # ts: element-date-format
    # normals: element-date
    switch(
      data_type,
      "ts" = "4km/%s/%s?format=%s",
      "normals_800" = "normals/800m/%s/%s",
      "normals" = "normals/4km/%s/%s"
    )
  #### 3. define measurement data paths
  url_download_template <-
    file.path(
      "https://services.nacse.org/prism/data/get/us",
      url_middle
    )

  download_urls <-
    ifelse(
      data_type == "ts",
      sprintf(url_download_template, element, time, format),
      sprintf(url_download_template, element, time)
    )

  #### 4. build destination file name
  download_names <- paste0(
    directory_to_download,
    "PRISM_",
    element,
    "_",
    data_type,
    "_",
    time,
    ifelse(data_type == "ts", paste0("_", format), ""),
    ".zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_urls,
      destfiles = download_names,
      n_files = length(download_urls)
    )))
  }

  #### Download using httr2
  # PRISM returns file with server-determined name via Content-Disposition.
  # We use a constructed destfile; the zip contents are identical regardless.
  amadeus::download_run_method(
    urls = download_urls,
    destfiles = download_names,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  message("Requests were processed.\n")

  #### Validate downloaded archives before unzip
  for (zip_file in download_names) {
    zip_ok <- tryCatch(
      {
        withCallingHandlers(
          {
            utils::unzip(zip_file, list = TRUE)
            TRUE
          },
          warning = function(w) {
            stop(conditionMessage(w), call. = FALSE)
          }
        )
      },
      error = function(e) FALSE
    )

    if (!isTRUE(zip_ok)) {
      preview <- tryCatch(
        {
          paste(readLines(zip_file, n = 3, warn = FALSE), collapse = " ")
        },
        error = function(e) ""
      )
      stop(
        sprintf(
          "Downloaded PRISM archive '%s' is not a valid zip file.%s",
          basename(zip_file),
          if (nzchar(preview)) {
            paste0(" Response preview: ", preview)
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
  }

  #### Unzip downloaded zip files
  amadeus::download_unzip(
    file_name = download_names,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files (stored under directory_to_download/zip_files)
  if (remove_zip && unzip) {
    file.remove(download_names)
  }

  return(
    amadeus::download_hash(hash, directory_to_save)
  )
}


