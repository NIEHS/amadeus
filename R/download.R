# download.R
#' Download raw data wrapper function
# nolint start
#' @description
#' The \code{download_data()} function accesses and downloads atmospheric,
#' meteorological, and environmental data from various open-access data
#' sources.
# nolint end
#' @param dataset_name character(1). Dataset to download.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token. Required for NASA EarthData datasets: `"geos"`,
#' `"merra2"`, `"modis"`, `"sedac_groads"` / `"groads"`, and
#' `"sedac_population"` / `"population"`. Can be a token string, a path to a
#' file containing the token, or \code{NULL} to read from the
#' \code{NASA_EARTHDATA_TOKEN} environment variable. Ignored for datasets that
#' do not use NASA EarthData authentication.
#' @param rate_limit numeric(1). Minimum seconds between HTTP requests
#' (default 2). Passed to the underlying download function.
#' @param ... Additional arguments passed to each download function.
#' @note
#' - All download function names are in \code{download_*} formats
#' @author Insang Song
#' @seealso
#' For details of each download function per dataset,
#' Please refer to:
#' * \code{\link{download_aqs}}: `"aqs"`, `"AQS"`
#' * \code{\link{download_ecoregion}}: `"ecoregions"`, `"ecoregion"`
#' * \code{\link{download_geos}}: `"geos"`
#' * \code{\link{download_goes}}: `"goes"`, `"goes_adp"`, `"GOES"`
#' * \code{\link{download_gmted}}: `"gmted"`, `"GMTED"`
#' * \code{\link{download_koppen_geiger}}: `"koppen"`, `"koppengeiger"`
#' * \code{\link{download_merra2}}: "merra2", `"merra"`, `"MERRA"`, `"MERRA2"`
#' * \code{\link{download_narr}}: `"narr"`
#' * \code{\link{download_nlcd}}: `"nlcd"`, `"NLCD"`
#' * \code{\link{download_hms}}: `"noaa"`, `"smoke"`, `"hms"`
#' * \code{\link{download_groads}}: `"sedac_groads"`, `"groads"`
#' * \code{\link{download_population}}: `"sedac_population"`,
#'   `"population"`
#' * \code{\link{download_modis}}: `"modis"`, `"MODIS"`
#' * \code{\link{download_tri}}: `"tri"`, `"TRI"`
#' * \code{\link{download_nei}}: `"nei"`, `"NEI"`
#' * \code{\link{download_gridmet}}: `"gridMET"`, `"gridmet"`
#' * \code{\link{download_terraclimate}}: `"TerraClimate"`, `"terraclimate"`
#' * \code{\link{download_huc}}: `"huc"`
#' * \code{\link{download_cropscape}}: `"cropscape"`, `"cdl"`
#' * \code{\link{download_prism}}: `"prism"`
#' * \code{\link{download_edgar}}: `"edgar"`
#' * \code{\link{download_improve}}: `"improve"`, `"IMPROVE"`
#' * \code{\link{download_drought}}: `"drought"`, `"spei"`, `"eddi"`, `"usdm"`
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Data files will be downloaded and stored in respective
#' sub-directories within \code{directory_to_save}. File format and
#' sub-directory names depend on data source and dataset of interest.
#' @examples
#' \dontrun{
#' download_data(
#'   dataset_name = "narr",
#'   variables = "weasd",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_data <-
  function(
    dataset_name = c(
      "aqs",
      "ecoregion",
      "ecoregions",
      "geos",
      "goes",
      "goes_adp",
      "GOES",
      "gmted",
      "koppen",
      "koppengeiger",
      "merra2",
      "merra",
      "modis",
      "narr",
      "nlcd",
      "noaa",
      "sedac_groads",
      "sedac_population",
      "groads",
      "population",
      "hms",
      "smoke",
      "tri",
      "nei",
      "gridmet",
      "terraclimate",
      "huc",
      "cropscape",
      "cdl",
      "prism",
      "edgar",
      "improve",
      "IMPROVE",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    hash = FALSE,
    nasa_earth_data_token = NULL,
    rate_limit = 2,
    ...
  ) {
    dataset_name <- tolower(dataset_name)
    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    what_to_run <- switch(
      dataset_name,
      aqs = download_aqs,
      ecoregion = download_ecoregion,
      ecoregions = download_ecoregion,
      geos = download_geos,
      goes = download_goes,
      goes_adp = download_goes,
      gmted = download_gmted,
      koppen = download_koppen_geiger,
      koppengeiger = download_koppen_geiger,
      merra2 = download_merra2,
      merra = download_merra2,
      narr = download_narr,
      nlcd = download_nlcd,
      noaa = download_hms,
      smoke = download_hms,
      hms = download_hms,
      sedac_groads = download_groads,
      groads = download_groads,
      sedac_population = download_population,
      population = download_population,
      modis = download_modis,
      tri = download_tri,
      nei = download_nei,
      gridmet = download_gridmet,
      terraclimate = download_terraclimate,
      huc = download_huc,
      cropscape = download_cropscape,
      cdl = download_cropscape,
      prism = download_prism,
      edgar = download_edgar,
      improve = download_improve,
      drought = download_drought,
      spei = download_drought,
      eddi = download_drought,
      usdm = download_drought
    )

    call_args <- c(
      list(
        directory_to_save = directory_to_save,
        acknowledgement = acknowledgement,
        hash = hash,
        rate_limit = rate_limit
      ),
      list(...)
    )

    # Only pass nasa_earth_data_token to functions that accept it
    if ("nasa_earth_data_token" %in% names(formals(what_to_run))) {
      call_args$nasa_earth_data_token <- nasa_earth_data_token
    }

    return <- tryCatch(
      {
        do.call(what_to_run, call_args)
      },
      error = function(e) {
        stop(
          paste0(
            e,
            "\n",
            paste0(deparse(args(what_to_run)), collapse = "\n"),
            "\n",
            "Please refer to the argument list and ",
            "the error message above to rectify the error.\n"
          )
        )
      }
    )

    return(return)
  }


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
    if (length(year) == 1) {
      year <- c(year, year)
    }
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

#' Download ecoregion data
#' @description
#' The \code{download_ecoregion()} function accesses and downloads United
#' States Ecoregions data from the U.S. Environmental Protection Agency's (EPA)
#' Ecoregions.
#' @note Ecoregion data does not require authentication.
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
#' @author Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_omernik2014ecoregions}{amadeus}
#' @examples
#' \dontrun{
#' download_ecoregion(
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_ecoregion <- function(
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

  #### Define download URL
  download_url <- paste0(
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/",
    "us_eco_l3_state_boundaries.zip"
  )

  #### Build download file name
  download_name <- file.path(
    directory_to_download,
    "us_eco_l3_state_boundaries.zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip files
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}
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
  if (length(date) == 1) {
    date <- c(date, date)
  }
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


#' Download elevation data
#' @description
#' The \code{download_gmted()} function accesses and downloads Global
#' Multi-resolution Terrain Elevation Data (GMTED2010) from U.S. Geological
#' Survey.
#' @note GMTED data does not require authentication.
#' @param statistic character(1). Available statistics.
#' @param resolution character(1). Available resolutions.
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
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{danielson_global_2011}{amadeus}
#' @examples
#' \dontrun{
#' download_gmted(
#'   statistic = "Breakline Emphasis",
#'   resolution = "7.5 arc-seconds",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_gmted <- function(
  statistic = c(
    "Breakline Emphasis",
    "Systematic Subsample",
    "Median Statistic",
    "Minimum Statistic",
    "Mean Statistic",
    "Maximum Statistic",
    "Standard Deviation Statistic"
  ),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
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

  #### Check for valid statistic
  statistic <- match.arg(statistic)

  #### Check for valid resolution
  resolution <- match.arg(resolution)

  #### Define URL base
  base <- paste0(
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo",
    "/downloads/GMTED/Grid_ZipFiles/"
  )

  #### Define URL statistic code
  statistic_code <- amadeus::process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )

  #### Define URL resolution code
  resolution_code <- amadeus::process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )

  #### Build url
  download_url <- paste0(
    base,
    statistic_code,
    resolution_code,
    "_grd.zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "gmted2010_",
    statistic_code,
    resolution_code,
    "_grd.zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}

# nolint start
#' Download meteorological and atmospheric data
#' @description
#' The \code{download_merra2()} function accesses and downloads various
#' meteorological and atmospheric collections from [NASA's Modern-Era
#' Retrospective analysis for Research and Applications, Version 2
#' (MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), and the
#' daily corrected Global Fire Weather Index (FWI) product derived from MERRA-2
#' weather inputs.
#' @note Due to NASA data access policies, standard MERRA-2 GES DISC downloads
#' require a valid NASA Earthdata token for authentication. Use
#' \code{setup_nasa_token()} for setup. The `"fwi"` collection is hosted on the
#' public GlobalFWI portal and does not require Earthdata authentication.
#' @param collection character(1). MERRA-2 data collection file name, or
#'   `"fwi"` for the daily corrected Global Fire Weather Index product
#'   (`MERRA2.CORRECTED`).
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#'   authentication token.
#' @param date character(1 or 2). length of 10. Date or start/end dates
#'   for downloading data.
#' Format "YYYY-MM-DD" (ex. January 1, 2018 = `"2018-01-01"`).
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @examples
#' \dontrun{
#' download_merra2(
#'   collection = "inst1_2d_int_Nx",
#'   date = "2024-01-01",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_merra2 <- function(
  collection = c(
    "inst1_2d_asm_Nx",
    "inst1_2d_int_Nx",
    "inst1_2d_lfo_Nx",
    "inst3_3d_asm_Np",
    "inst3_3d_aer_Nv",
    "inst3_3d_asm_Nv",
    "inst3_3d_chm_Nv",
    "inst3_3d_gas_Nv",
    "inst3_2d_gas_Nx",
    "inst6_3d_ana_Np",
    "inst6_3d_ana_Nv",
    "statD_2d_slv_Nx",
    "tavg1_2d_adg_Nx",
    "tavg1_2d_aer_Nx",
    "tavg1_2d_chm_Nx",
    "tavg1_2d_csp_Nx",
    "tavg1_2d_flx_Nx",
    "tavg1_2d_int_Nx",
    "tavg1_2d_lfo_Nx",
    "tavg1_2d_lnd_Nx",
    "tavg1_2d_ocn_Nx",
    "tavg1_2d_rad_Nx",
    "tavg1_2d_slv_Nx",
    "tavg3_3d_mst_Ne",
    "tavg3_3d_trb_Ne",
    "tavg3_3d_nav_Ne",
    "tavg3_3d_cld_Np",
    "tavg3_3d_mst_Np",
    "tavg3_3d_rad_Np",
    "tavg3_3d_tdt_Np",
    "tavg3_3d_trb_Np",
    "tavg3_3d_udt_Np",
    "tavg3_3d_odt_Np",
    "tavg3_3d_qdt_Np",
    "tavg3_3d_asm_Nv",
    "tavg3_3d_cld_Nv",
    "tavg3_3d_mst_Nv",
    "tavg3_3d_rad_Nv",
    "tavg3_2d_glc_Nx",
    "fwi"
  ),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Directory setup (can be done early)
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. Check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 3a. Handle deprecated parameters BEFORE token check
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

  #### 4. Check and retrieve NASA token for standard MERRA-2 collections only
  standard_collection <- collection != "fwi"
  if (any(standard_collection)) {
    nasa_earth_data_token <- amadeus::get_token(
      token = nasa_earth_data_token,
      env_var = "NASA_EARTHDATA_TOKEN"
    )
  }

  #### 5. Now check for null parameters
  parameters <- mget(ls())
  if (!any(standard_collection)) {
    parameters$nasa_earth_data_token <- ""
  }
  parameters <- parameters
  amadeus::check_for_null_parameters(parameters)

  #### 7. Check if collection is recognized
  identifiers <- c(
    "inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD",
    "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0",
    "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG",
    "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0",
    "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2",
    "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8",
    "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2",
    "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5",
    "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09",
    "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS",
    "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W",
    "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3",
    "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP",
    "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D",
    "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ",
    "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5",
    "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6",
    "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7",
    "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA",
    "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T",
    "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R",
    "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T",
    "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV",
    "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72",
    "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K",
    "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN",
    "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53",
    "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS",
    "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK",
    "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI",
    "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL",
    "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW",
    "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z",
    "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ",
    "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK",
    "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH",
    "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR",
    "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW",
    "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US"
  )
  identifiers <- lapply(identifiers, strsplit, split = " ")
  identifiers <- lapply(identifiers, function(x) matrix(x[[1]], nrow = 1))
  identifiers <- do.call(rbind, identifiers)
  identifiers_df <- as.data.frame(identifiers)
  colnames(identifiers_df) <- c("collection_id", "esdt_name", "DOI")

  if (!all(collection[standard_collection] %in% identifiers_df$collection_id)) {
    message(identifiers_df)
    stop(paste0(
      "Requested collection is not recognized.\n",
      "Please refer to the table above to find a proper collection.\n"
    ))
  }

  #### 8. Define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )

  #### 9. Define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))

  #### 10. Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (c in seq_along(collection)) {
    collection_loop <- collection[c]

    if (collection_loop == "fwi") {
      base <- paste0(
        "https://portal.nccs.nasa.gov/datashare/GlobalFWI/v2.0/",
        "fwiCalcs.MERRA2/Default/MERRA2.CORRECTED/"
      )
      download_folder <- paste0(directory_to_save, collection_loop)
      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }
      for (d in seq_along(date_sequence)) {
        date_loop <- date_sequence[d]
        year <- substr(date_loop, 1, 4)
        file_name <- paste0(
          "FWI.MERRA2.CORRECTED.Daily.Default.",
          date_loop,
          ".nc"
        )
        download_url <- paste0(base, year, "/", file_name)
        download_name <- paste0(download_folder, "/", file_name)
        if (amadeus::check_destfile(download_name)) {
          all_urls <- c(all_urls, download_url)
          all_destfiles <- c(all_destfiles, download_name)
        }
      }
      next
    }

    #### Define ESDT name
    identifiers_df_requested <- subset(
      identifiers_df,
      subset = identifiers_df$collection_id == collection_loop
    )
    esdt_name <- identifiers_df_requested[, 2]

    #### Define URL base (goldsmr4 vs goldsmr5)
    esdt_name_4 <- c(
      "M2I1NXASM",
      "M2I1NXINT",
      "M2I1NXLFO",
      "M2I3NXGAS",
      "M2SDNXSLV",
      "M2T1NXADG",
      "M2T1NXAER",
      "M2T1NXCHM",
      "M2T1NXCSP",
      "M2T1NXFLX",
      "M2T1NXINT",
      "M2T1NXLFO",
      "M2T1NXLND",
      "M2T1NXOCN",
      "M2T1NXRAD",
      "M2T1NXSLV",
      "M2T3NXGLC"
    )
    esdt_name_5 <- c(
      "M2I3NPASM",
      "M2I3NVAER",
      "M2I3NVASM",
      "M2I3NVCHM",
      "M2I3NVGAS",
      "M2I6NPANA",
      "M2I6NVANA",
      "M2T3NEMST",
      "M2T3NENAV",
      "M2T3NETRB",
      "M2T3NPCLD",
      "M2T3NPMST",
      "M2T3NPODT",
      "M2T3NPQDT",
      "M2T3NPRAD",
      "M2T3NPTDT",
      "M2T3NPTRB",
      "M2T3NPUDT",
      "M2T3NVASM",
      "M2T3NVCLD",
      "M2T3NVMST",
      "M2T3NVRAD"
    )

    if (esdt_name %in% esdt_name_4) {
      base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    } else if (esdt_name %in% esdt_name_5) {
      base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    }
    #### Get file listings using httr2
    for (y in seq_along(yearmonth_sequence)) {
      year <- substr(yearmonth_sequence[y], 1, 4)
      month <- substr(yearmonth_sequence[y], 5, 6)

      base_url <- paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/"
      )
      # Validate first URL only
      if (c == 1 && y == 1) {
        if (!amadeus::check_url_status(base_url)) {
          stop(paste0(
            "Invalid date returns HTTP code 404. ",
            "Check `date` parameter.\n"
          ))
        }
      }

      # Get directory listing using httr2
      tryCatch(
        {
          resp <- httr2::request(base_url) |>
            httr2::req_perform()

          html_content <- httr2::resp_body_string(resp)

          # Extract .nc4 files from HTML
          nc4_pattern <- 'href="([^"]*\\.nc4)"'
          nc4_matches <- gregexpr(nc4_pattern, html_content, perl = TRUE)
          list_urls_month <- regmatches(html_content, nc4_matches)[[1]]
          list_urls_month <- gsub('href="|"', "", list_urls_month)

          # Also get .xml files
          xml_pattern <- 'href="([^"]*\\.nc4\\.xml)"'
          xml_matches <- gregexpr(xml_pattern, html_content, perl = TRUE)
          list_xml_month <- regmatches(html_content, xml_matches)[[1]]
          list_xml_month <- gsub('href="|"', "", list_xml_month)

          # Filter by date sequence and remove duplicate links from HTML listing
          list_urls_date_sequence <- unique(list_urls_month[
            substr(basename(list_urls_month), 28, 35) %in% date_sequence
          ])
          list_xml_date_sequence <- unique(list_xml_month[
            substr(basename(list_xml_month), 28, 35) %in% date_sequence
          ])

          # Create download folder
          download_folder <- paste0(directory_to_save, collection_loop)
          if (!dir.exists(download_folder)) {
            dir.create(download_folder, recursive = TRUE)
          }

          # Create metadata folder
          download_folder_metadata <- paste0(download_folder, "/metadata/")
          if (!dir.exists(download_folder_metadata)) {
            dir.create(download_folder_metadata, recursive = TRUE)
          }

          # Build URLs and destination files for data
          for (f in list_urls_date_sequence) {
            download_url <- paste0(base_url, f)
            download_name <- paste0(download_folder, "/", f)

            if (amadeus::check_destfile(download_name)) {
              all_urls <- c(all_urls, download_url)
              all_destfiles <- c(all_destfiles, download_name)
            }
          }

          # Build URLs and destination files for metadata
          for (f in list_xml_date_sequence) {
            download_url_metadata <- paste0(base_url, f)
            download_name_metadata <- paste0(download_folder_metadata, f)

            if (amadeus::check_destfile(download_name_metadata)) {
              all_urls <- c(all_urls, download_url_metadata)
              all_destfiles <- c(all_destfiles, download_name_metadata)
            }
          }
        },
        error = function(e) {
          warning(
            sprintf(
              "Failed to get directory listing for %s: %s\n",
              base_url,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  }

  #### 11. Exit early if download=FALSE
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

  #### 12. Download files using httr2
  if (length(all_urls) > 0) {
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = nasa_earth_data_token, # Now passing the NASA token!
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("All files already exist. Nothing to download.\n")
    download_result <- list(
      success = 0,
      failed = 0,
      skipped = length(all_destfiles)
    )
  }

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

#' Download meteorological data
#' @description
#' The \code{download_narr} function accesses and downloads daily meteorological
#' data from NOAA's North American Regional Reanalysis (NARR) model via the
#' NOAA Physical Sciences Laboratory (PSL) NARR Dailies server
#' (\url{https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/}).
#' @note "Pressure levels" variables contain variable values at 29 atmospheric
#' levels, ranging from 1000 hPa to 100 hPa. All pressure levels data will be
#' downloaded for each variable.
#' @note The 88 variables supported by this function represent the complete set
#' of variables available as individual NetCDF files on the PSL NARR Dailies
#' server. The NARR archive also contains additional variables (e.g., cloud
#' water mixing ratio, ice mixing ratio, surface friction velocity, momentum
#' fluxes, and static land/soil properties) that are only present in the raw
#' merged GRIB files (\code{merged_AWIP32.YYYYMMDDHH}) available at
#' \url{https://ftp.cpc.ncep.noaa.gov/NARR/}. Those variables cannot be
#' downloaded with this function.
#' @param variables character. Variable(s) name acronym. See the
#' \emph{Available NARR Variables} section below for the complete list of
#' supported abbreviations.
#' @section Available NARR Variables:
#' The \code{variables} argument accepts one or more of the following
#' abbreviations. Variables are grouped into three categories that determine
#' the source URL path used for download.
#'
#' \strong{Monolevel variables} (single vertical level, surface / near-surface
#' fields):
#' \describe{
#'   \item{\code{acpcp}}{Convective precipitation}
#'   \item{\code{air.2m}}{Air temperature at 2 m}
#'   \item{\code{air.sfc}}{Air temperature at surface}
#'   \item{\code{albedo}}{Surface albedo}
#'   \item{\code{apcp}}{Total accumulated precipitation}
#'   \item{\code{bgrun}}{Baseflow-groundwater runoff}
#'   \item{\code{bmixl.hl1}}{Blackadar mixing length scale at hybrid level 1}
#'   \item{\code{cape}}{Convective available potential energy}
#'   \item{\code{ccond}}{Canopy conductance}
#'   \item{\code{cdcon}}{Convective cloud cover}
#'   \item{\code{cdlyr}}{Non-convective cloud cover}
#'   \item{\code{cfrzr}}{Categorical freezing rain}
#'   \item{\code{cicep}}{Categorical ice pellets}
#'   \item{\code{cin}}{Convective inhibition}
#'   \item{\code{cnwat}}{Plant canopy surface water}
#'   \item{\code{crain}}{Categorical rain}
#'   \item{\code{csnow}}{Categorical snow}
#'   \item{\code{dlwrf}}{Downward longwave radiation flux}
#'   \item{\code{dpt.2m}}{Dew point temperature at 2 m}
#'   \item{\code{dswrf}}{Downward shortwave radiation flux}
#'   \item{\code{evap}}{Evaporation}
#'   \item{\code{gflux}}{Ground heat flux}
#'   \item{\code{hcdc}}{High cloud cover}
#'   \item{\code{hgt.tropo}}{Geopotential height at tropopause}
#'   \item{\code{hlcy}}{Storm relative helicity}
#'   \item{\code{hpbl}}{Planetary boundary layer height}
#'   \item{\code{lcdc}}{Low cloud cover}
#'   \item{\code{lftx4}}{Best (4-layer) lifted index}
#'   \item{\code{lhtfl}}{Latent heat net flux}
#'   \item{\code{mcdc}}{Mid-cloud cover}
#'   \item{\code{mconv.hl1}}{Horizontal moisture divergence at hybrid level 1}
#'   \item{\code{mslet}}{Mean sea level pressure (ETA model reduction)}
#'   \item{\code{mstav}}{Moisture availability}
#'   \item{\code{pevap}}{Potential evaporation}
#'   \item{\code{pottmp.hl1}}{Potential temperature at hybrid level 1}
#'   \item{\code{pottmp.sfc}}{Potential temperature at surface}
#'   \item{\code{prate}}{Precipitation rate}
#'   \item{\code{pres.sfc}}{Surface pressure}
#'   \item{\code{pres.tropo}}{Pressure at tropopause}
#'   \item{\code{prmsl}}{Pressure reduced to mean sea level}
#'   \item{\code{pr_wtr}}{Precipitable water}
#'   \item{\code{rcq}}{Specific humidity tendency from all physics}
#'   \item{\code{rcs}}{Snowfall water equivalent tendency}
#'   \item{\code{rcsol}}{Solar radiative heating rates}
#'   \item{\code{rct}}{Temperature tendency from all physics}
#'   \item{\code{rhum.2m}}{Relative humidity at 2 m}
#'   \item{\code{shtfl}}{Sensible heat net flux}
#'   \item{\code{shum.2m}}{Specific humidity at 2 m}
#'   \item{\code{snod}}{Snow depth}
#'   \item{\code{snohf}}{Snow phase-change heat flux}
#'   \item{\code{snom}}{Snow melt}
#'   \item{\code{snowc}}{Snow cover}
#'   \item{\code{soilm}}{Soil moisture content (0–200 cm layer)}
#'   \item{\code{ssrun}}{Storm surface runoff}
#'   \item{\code{tcdc}}{Total cloud cover}
#'   \item{\code{tke.hl1}}{Turbulent kinetic energy at hybrid level 1}
#'   \item{\code{ulwrf.ntat}}{Upward longwave radiation flux at nominal
#'     top of atmosphere}
#'   \item{\code{ulwrf.sfc}}{Upward longwave radiation flux at surface}
#'   \item{\code{ustm}}{U-component of storm motion}
#'   \item{\code{uswrf.ntat}}{Upward shortwave radiation flux at nominal
#'     top of atmosphere}
#'   \item{\code{uswrf.sfc}}{Upward shortwave radiation flux at surface}
#'   \item{\code{uwnd.10m}}{U-component of wind at 10 m}
#'   \item{\code{veg}}{Vegetation fraction}
#'   \item{\code{vis}}{Visibility}
#'   \item{\code{vstm}}{V-component of storm motion}
#'   \item{\code{vvel.hl1}}{Vertical velocity at hybrid level 1}
#'   \item{\code{vwnd.10m}}{V-component of wind at 10 m}
#'   \item{\code{vwsh.tropo}}{Vertical wind shear at tropopause}
#'   \item{\code{wcconv}}{Convective wetting of vegetation canopy}
#'   \item{\code{wcinc}}{Wetting of vegetation canopy}
#'   \item{\code{wcuflx}}{U-component of convective canopy moisture flux}
#'   \item{\code{wcvflx}}{V-component of convective canopy moisture flux}
#'   \item{\code{weasd}}{Water-equivalent accumulated snow depth}
#'   \item{\code{wvconv}}{Convective column moisture convergence}
#'   \item{\code{wvinc}}{Column moisture increase}
#'   \item{\code{wvuflx}}{U-component of vertically-integrated moisture flux}
#'   \item{\code{wvvflx}}{V-component of vertically-integrated moisture flux}
#' }
#'
#' \strong{Pressure level variables} (29 atmospheric pressure levels from
#' 1000 to 100 hPa; all levels are downloaded together):
#' \describe{
#'   \item{\code{air}}{Air temperature}
#'   \item{\code{hgt}}{Geopotential height}
#'   \item{\code{omega}}{Vertical velocity (pressure / omega)}
#'   \item{\code{shum}}{Specific humidity}
#'   \item{\code{tke}}{Turbulent kinetic energy}
#'   \item{\code{uwnd}}{U-component of wind}
#'   \item{\code{vwnd}}{V-component of wind}
#' }
#'
#' \strong{Subsurface (soil) variables} (4 soil layers):
#' \describe{
#'   \item{\code{soill}}{Liquid volumetric soil moisture (non-frozen fraction)}
#'   \item{\code{soilw}}{Volumetric soil moisture content}
#'   \item{\code{tsoil}}{Soil temperature}
#' }
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save downloaded data
#' files.
#' @param acknowledgement logical(1). Must be TRUE to proceed with download.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). DEPRECATED, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum download retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{mesinger_north_2006}{amadeus}
#' @examples
#' \dontrun{
#' download_narr(
#'   variables = c("weasd", "omega"),
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # Multiple years
#' download_narr(
#'   variables = c("air.2m", "rhum.2m"),
#'   year = c(2020, 2022),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_narr <- function(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### 3. Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### 4. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 5. Handle deprecated parameters
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

  #### 6. Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  stopifnot(
    all(
      seq(year[1], year[2], 1) %in%
        seq(1979, as.numeric(substr(Sys.Date(), 1, 4)), 1)
    )
  )
  years <- seq(year[1], year[2], 1)

  #### 7. Define variables
  variables_list <- as.vector(variables)

  #### 8. Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")

    # Implement variable sorting function
    base <- amadeus::narr_variable(variable)[[1]]
    months <- amadeus::narr_variable(variable)[[2]]

    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }

    for (y in seq_along(years)) {
      year_l <- years[y]
      for (m in seq_along(months)) {
        url <- paste0(
          base,
          variable,
          ".",
          year_l,
          months[m],
          ".nc"
        )
        destfile <- paste0(
          directory_to_save,
          variable,
          "/",
          variable,
          ".",
          year_l,
          months[m],
          ".nc"
        )

        # Only add if file doesn't exist or is 0 bytes
        if (amadeus::check_destfile(destfile)) {
          all_urls <- c(all_urls, url)
          all_destfiles <- c(all_destfiles, destfile)
        }
      }
    }
  }

  #### 9. Exit early if download=FALSE (deprecated behavior)
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

  #### 10. Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL, # NARR doesn't use token authentication
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

#' Download National Land Cover Database (NLCD) data
#' @description
#' Downloads NLCD data products from the Multi-Resolution Land Characteristics
#' (MRLC) Consortium. NLCD provides nationwide land cover and land cover change
#' information for the United States at a 30m resolution.
#' @param product character(1). NLCD product type. One of:
#' \itemize{
#'   \item "Land Cover" (default)
#'   \item "Land Cover Change"
#'   \item "Land Cover Confidence"
#'   \item "Fractional Impervious Surface"
#'   \item "Impervious Descriptor"
#'   \item "Spectral Change Day of Year"
#' }
#' @param year integer(1). Year of NLCD data (1985-2024). Default is 2021.
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed with
#' download.
#' @param download logical(1). DEPRECATED. Downloads now happen automatically.
#'   Set to FALSE to skip downloading (generates file list only).
#' @param remove_command logical(1). Deprecated, ignored.
#' @param unzip logical(1). Unzip downloaded files? Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files after extraction? Default is
#' \code{FALSE}.
#' @param show_progress logical(1). Show download progress? Default is
#' \code{TRUE}.
#' @param hash logical(1). Return hash of downloaded files? Default is
#' \code{FALSE}.
#' @param max_tries integer(1). Maximum download retry attempts. Default is 20.
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song, Kyle Messier
#' @return invisible NULL; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{dewitz_national_2023}{amadeus}
#' @examples
#' \dontrun{
#' # Download 2021 Land Cover
#' download_nlcd(
#'   product = "Land Cover",
#'   year = 2021,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # Download Land Cover Change for 2019
#' download_nlcd(
#'   product = "Land Cover Change",
#'   year = 2019,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   unzip = TRUE,
#'   remove_zip = TRUE
#' )
#' }
#' @export
download_nlcd <- function(
  product = "Land Cover",
  year = 2021,
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

  #### Check for valid years
  valid_years <- 1985:2024L
  if (!(as.integer(year) %in% valid_years)) {
    stop(paste0("Requested year is not recognized.\n"))
  }

  #### Define URL base
  base <- paste0(
    "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/",
    "data-bundles/Annual_NLCD_"
  )

  #### Define collection code
  collection_code <- switch(
    tolower(product),
    "land cover" = "LndCov",
    "land cover change" = "LndChg",
    "land cover confidence" = "LndCnf",
    "fractional impervious surface" = "FctImp",
    "impervious descriptor" = "ImpDsc",
    "spectral change day of year" = "SpcChg"
  )

  #### Build URL
  download_url <- paste0(
    base,
    collection_code,
    "_",
    year,
    "_CU_C1V1.zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "Annual_NLCD_",
    collection_code,
    "_",
    year,
    "_CU_C1V1.zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  # http_version = 2L forces HTTP/1.1 to avoid nghttp2 PROTOCOL_ERRORs from
  # www.mrlc.gov, which drops HTTP/2 connections mid-transfer.
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit,
      http_version = 2L
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}

#' Download roads data
#' @description
#' The \code{download_groads()} function accesses and downloads
#' roads data from NASA's Global Roads Open Access Data Set (gROADS).
#' @note gROADS data is hosted on NASA EarthData and requires a valid
#' NASA EarthData token for authentication. Set the
#' \code{NASA_EARTHDATA_TOKEN} environment variable or pass the token
#' directly via \code{nasa_earth_data_token}.
#' Use \code{setup_nasa_token()} for setup.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#'   authentication token. Can be a token string, a path to a file
#'   containing the token, or \code{NULL} to read from the
#'   \code{NASA_EARTHDATA_TOKEN} environment variable.
#' @param data_region character(1). Data region.
#' @param data_format character(1). "Shapefile" or "Geodatabase".
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
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2013groads}{amadeus}
#' @examples
#' \dontrun{
#' download_groads(
#'   data_region = "Americas",
#'   data_format = "Shapefile",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_groads <- function(
  data_region = c(
    "Americas",
    "Global",
    "Africa",
    "Asia",
    "Europe",
    "Oceania East",
    "Oceania West"
  ),
  data_format = c("Shapefile", "Geodatabase"),
  nasa_earth_data_token = NULL,
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

  #### Retrieve NASA EarthData token (before null parameter check)
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

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

  #### Check if region is valid
  data_format <- match.arg(data_format)
  data_region <- match.arg(data_region)

  #### Define URL base
  base <- paste0(
    "https://data.earthdata.nasa.gov/nasa-earth/",
    "human-dimensions/sedac-root/downloads/data/groads/",
    "groads-global-roads-open-access-v1/",
    "groads-v1-"
  )

  #### Define data format
  if (data_format == "Shapefile" && data_region == "Global") {
    message("Geodatabase format utilized for 'Global' dataset.\n")
    format <- "gdb"
  } else if (data_format == "Shapefile" && !(data_region == "Global")) {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gdb"
  }

  #### Coerce region to lower case
  region <- tolower(data_region)

  #### Build download URL
  download_url <- paste0(
    base,
    gsub(" ", "-", region),
    "-",
    format,
    ".zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "groads_v1_",
    gsub(" ", "-", region),
    "_",
    format,
    ".zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = nasa_earth_data_token,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}

#' Download population density data
#' @description
#' The \code{download_population()} function accesses and downloads
#' population density data from NASA's UN WPP-Adjusted Population Density.
#' @note Population data may require NASA EarthData authentication depending on
#' access method.
#' @param data_resolution character(1). Available resolutions.
#' @param data_format character(1). "ASCII", "GeoTIFF", or "netCDF".
#' @param year character(1). Available years or "all".
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
#' @param nasa_earth_data_token character(1). NASA EarthData bearer token.
#' If NULL (default), reads from the \code{NASA_EARTHDATA_TOKEN} environment
#' variable via \code{get_token()}.
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2017gpwv4}{amadeus}
#' @examples
#' \dontrun{
#' # RECOMMENDED: Set up token once (persists across sessions)
#' setup_nasa_token()
#'
#' download_population(
#'   data_resolution = "30 second",
#'   data_format = "GeoTIFF",
#'   year = "2020",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_population <- function(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2,
  nasa_earth_data_token = NULL
) {
  #### Retrieve NASA EarthData token
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

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

  #### Define URL base
  base <- paste0(
    paste0(
      "https://data.earthdata.nasa.gov/nasa-earth/human-dimensions/",
      "sedac-root/downloads/data/gpw-v4/"
    )
  )

  #### Define year
  year <- ifelse(year == "all", "totpop", as.character(year))

  #### Define data resolution
  resolution <- amadeus::process_sedac_codes(data_resolution)

  #### 30 second resolution not available for all years
  if (year == "totpop" && resolution == "30_sec") {
    resolution <- "2pt5_min"
    message(paste0(
      "30 second resolution not available for all years. Returning",
      " highest (2.5 minute) resolution.\n"
    ))
  }

  data_format <- match.arg(data_format)

  #### Define data format
  if (data_format == "GeoTIFF") {
    if (year != "totpop") {
      format <- "tif"
    } else {
      format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (data_format == "ASCII") {
    if (year != "totpop") {
      format <- "asc"
    } else {
      format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  } else if (data_format == "netCDF") {
    format <- "nc"
  }

  #### Build download URL
  download_url <- paste0(
    base,
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11/",
    "gpw-v4-population-density-adjusted-to-2015-unwpp-",
    "country-totals-rev11_",
    year,
    "_",
    resolution,
    "_",
    format,
    ".zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "gpw_v4_population_density_adjusted_to_2015_unwpp_",
    "country_totals_rev11_",
    year,
    "_",
    resolution,
    "_",
    format,
    ".zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = nasa_earth_data_token,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}

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
  if (length(date) == 1) {
    date <- c(date, date)
  }
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

#' Download climate classification data
#' @description
#' The \code{download_koppen_geiger()} function accesses and downloads
#' climate classification data.
#' @note Köppen-Geiger data does not require authentication.
#' @param data_resolution character(1). Available resolutions.
#' @param time_period character(1). "Present" (1980-2016) or "Future"
#' (2071-2100).
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
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_beck2023koppen}{amadeus}
#' \insertRef{article_beck2018present}{amadeus}
#' @examples
#' \dontrun{
#' download_koppen_geiger(
#'   data_resolution = "0.0083",
#'   time_period = "Present",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_koppen_geiger <- function(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
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

  #### Check for data resolution
  data_resolution <- match.arg(data_resolution)

  #### Check for valid time period
  time_period <- match.arg(time_period)

  #### Define time period
  period <- tolower(time_period)

  #### Define data resolution
  data_resolution <- gsub("\\.", "p", data_resolution)

  #### Define download URL
  download_url <- paste0(
    "https://s3-eu-west-1.amazonaws.com/",
    "pfigshare-u-files/12407516/Beck_KG_V1.zip"
  )

  #### Build download file name
  download_name <- paste0(
    directory_to_download,
    "koppen_geiger_",
    period,
    "_",
    data_resolution,
    ".zip"
  )

  #### Exit early if download=FALSE
  if (!isTRUE(download)) {
    message("Skipping download.\n")
    return(invisible(list(
      urls = download_url,
      destfiles = download_name,
      n_files = 1
    )))
  }

  #### Download file using httr2
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    message("File already exists. Skipping download.\n")
  }

  #### Unzip
  amadeus::download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(NULL))
  }
}
#' Download MODIS product files
#' @description Downloads MODIS data using httr2 with robust retry logic and
#' rate limiting. This function queries NASA's CMR API for available granules
#' and downloads relevant tiles based on the specified extent.
#' @note Due to NASA data access policies, downloads require a valid NASA
#' Earthdata token for authentication. For security, it's recommended to store
#' your token in an environment variable or file rather than in your code.
#' Use \code{setup_nasa_token()} for easy, secure token setup.
#' @note Both dates in \code{date} should be in the same year.
#'  Directory structure:
#'  input/modis/raw/\{version\}/\{product\}/\{year\}/\{day_of_year\}.
#' @param product character(1). MODIS product code
#' @param version character(1). Default is `"061"`, meaning v061.
#' @param nasa_earth_data_token character(1) or NULL. NASA EarthData
#' authentication token.
#'   For security, recommended options (in priority order):
#'   \itemize{
#'     \item NULL (default): Reads from NASA_EARTHDATA_TOKEN environment
#'     variable
#'     \item File path: e.g., "~/.nasa_earthdata_token"
#'     \item Token string: Direct token (not recommended for scripts)
#'   }
#'   Use \code{setup_nasa_token()} for interactive setup.
#' @param date character(1 or 2). Date range "YYYY-MM-DD" format
#' @param extent numeric(4). Bounding box `c(min_lon, max_lon, min_lat,
#' max_lat)`.
#' Default covers continental US: `c(-125, 22, -64, 50)`.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed with
#' download
#' @param download logical(1). DEPRECATED. Downloads now happen automatically.
#'   Set to FALSE to skip downloading (generates file list only).
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum download retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_mcd19a22021}{amadeus}
#' \insertRef{data_mod06l2_2017}{amadeus}
#' \insertRef{data_mod09ga2021}{amadeus}
#' \insertRef{data_mod11a12021}{amadeus}
#' \insertRef{data_mod13a22021}{amadeus}
#' \insertRef{article_roman2018vnp46}{amadeus}
#' @examples
#' \dontrun{
#' # RECOMMENDED: Set up token once (persists across sessions)
#' setup_nasa_token()
#'
#' # Then download without specifying token
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = "2024-01-01",
#'   extent = c(-80, 35, -75, 40),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # ALTERNATIVE: Token from file
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = "2024-01-01",
#'   extent = c(-80, 35, -75, 40),
#'   nasa_earth_data_token = "~/.nasa_earthdata_token",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#'
#' # ALTERNATIVE: Set token for current session
#' Sys.setenv(NASA_EARTHDATA_TOKEN = "your_token_here")
#' download_modis(
#'   product = "MOD09GA",
#'   date = "2024-01-01",
#'   acknowledgement = TRUE
#' )
#'
#' # Date range
#' download_modis(
#'   product = "MOD09GA",
#'   version = "061",
#'   date = c("2024-01-01", "2024-01-07"),
#'   extent = c(-80, 35, -75, 40),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_modis <- function(
  product = c(
    "MOD09GA",
    "MYD09GA",
    "MOD09GQ",
    "MYD09GQ",
    "MOD09A1",
    "MYD09A1",
    "MOD09Q1",
    "MYD09Q1",
    "MOD11A1",
    "MYD11A1",
    "MOD11A2",
    "MYD11A2",
    "MOD11B1",
    "MYD11B1",
    "MOD13A1",
    "MYD13A1",
    "MOD13A2",
    "MYD13A2",
    "MOD13Q1",
    "MYD13Q1",
    "MOD13A3",
    "MYD13A3",
    "MCD12Q1",
    "MOD14A1",
    "MYD14A1",
    "MOD14A2",
    "MYD14A2",
    "MOD14CM1",
    "MYD14CM1",
    "MOD16A2",
    "MYD16A2",
    "MCD64A1",
    "MCD64CMQ",
    "MOD06_L2",
    "MCD14ML",
    "MCD19A2",
    "VNP46A2",
    "VNP64A1"
  ),
  version = "061",
  nasa_earth_data_token = NULL,
  date = c("2023-09-01", "2023-09-01"),
  extent = c(-125, 22, -64, 50),
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

  #### 2. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. Check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 4. Check and retrieve NASA token (BEFORE null check)
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### 5. Check for null parameters (AFTER token retrieval)
  params_to_check <- mget(ls())
  params_to_check <- params_to_check
  amadeus::check_for_null_parameters(params_to_check)
  product <- match.arg(product)

  if (substr(date[1], 1, 4) != substr(date[2], 1, 4)) {
    if (
      !product %in%
        c(
          "MOD06_L2",
          "MOD14CM1",
          "MYD14CM1",
          "MCD14ML",
          "MCD64A1",
          "MCD64CMQ",
          "VNP64A1"
        )
    ) {
      stop("dates should be in the same year.\n")
    }
  }

  #### 7. Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after querying available files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 8. Warning for excessive query
  dt_date <- as.Date(date)
  if (diff(dt_date) > 31) {
    warning(
      "Date range is greater than 31 days. ",
      "The results may not include all dates in the range.",
      call. = FALSE
    )
  }

  #### 9. Version fix
  if (product == "MOD06_L2") {
    str_version <- "6.1"
  } else if (product %in% c("MOD14CM1", "MYD14CM1")) {
    str_version <- "005"
  } else if (product == "MCD64CMQ") {
    str_version <- "006"
  } else if (product == "MCD14ML") {
    str_version <- "6.1NRT"
  } else if (product %in% c("VNP46A2", "VNP64A1")) {
    str_version <- NULL
  } else {
    str_version <- version
  }

  #### 10. Query CMR
  message("Querying NASA CMR for available granules...\n")
  chr_extent <- paste(extent, collapse = ",")
  query_granules <- function(short_name, version) {
    resp <- tryCatch(
      httr2::request(
        "https://cmr.earthdata.nasa.gov/search/granules.json"
      ) |>
        httr2::req_url_query(
          short_name = short_name,
          version = version,
          temporal = paste(date[1], date[2], sep = ","),
          bounding_box = chr_extent,
          page_size = 2000
        ) |>
        httr2::req_options(connecttimeout = 30L) |>
        httr2::req_retry(retry_on_failure = TRUE, max_tries = 5L) |>
        httr2::req_timeout(120) |>
        httr2::req_perform(),
      error = function(e) {
        stop(
          "Failed to query NASA CMR (cmr.earthdata.nasa.gov). ",
          "Check network connectivity and NASA EarthData status at ",
          "https://status.earthdata.nasa.gov. Original error: ",
          conditionMessage(e)
        )
      }
    )
    resp |> httr2::resp_body_json()
  }
  granules <- query_granules(product, str_version)

  if (
    product %in% c("MOD14CM1", "MYD14CM1") && length(granules$feed$entry) == 0
  ) {
    product_fallback <- switch(
      product,
      MOD14CM1 = "MOD14A2",
      MYD14CM1 = "MYD14A2"
    )
    message(
      sprintf(
        "No granules found for %s; retrying with %s (v006).\n",
        product,
        product_fallback
      )
    )
    granules <- query_granules(product_fallback, "006")
  }

  # Extract product data URLs.
  granule_entries <- granules$feed$entry
  urls <- vapply(
    granule_entries,
    function(g) {
      links <- g$links
      data_links <- Filter(
        function(l) {
          grepl("data#", l$rel) &&
            if (product == "MCD14ML") {
              grepl("\\.txt$", l$href, ignore.case = TRUE)
            } else {
              grepl("\\.(hdf|h5)$", l$href, ignore.case = TRUE)
            }
        },
        links
      )
      if (length(data_links) > 0) data_links[[1]]$href else NA_character_
    },
    character(1)
  )

  keep <- !is.na(urls)
  urls <- urls[keep]

  if (length(urls) == 0) {
    stop("No granules found for the specified query parameters.\n")
  }

  #### 12. Filter by date range
  urls_filtered <- modis_filter_paths_by_date(urls, date = date)
  if (length(urls_filtered) == 0) {
    stop("No granules matched the requested date range.\n")
  }

  keep_date <- urls %in% urls_filtered
  urls <- urls[keep_date]
  download_names <- basename(urls)

  scale_detected <- modis_extract_temporal_scale(download_names[1])
  if (scale_detected == "monthly") {
    month_start <- as.Date(format(as.Date(date[1]), "%Y-%m-01"))
    month_end <- as.Date(format(as.Date(date[2]), "%Y-%m-01"))
    month_sequence <- seq(month_start, month_end, by = "month")
    message(sprintf(
      "Found %d / %d monthly files in the queried date range.\n",
      length(urls),
      length(month_sequence)
    ))
  } else {
    date_sequence <- vapply(
      download_names,
      modis_extract_temporal_key,
      character(1)
    )
    date_sequence <- unique(date_sequence[!is.na(date_sequence)])
    date_start_i <- as.integer(strftime(date[1], "%Y%j"))
    date_end_i <- as.integer(strftime(date[2], "%Y%j"))
    date_range_julian <- seq(date_start_i, date_end_i)
    date_sequence_i <- as.integer(date_sequence)
    date_sequence_in <- date_sequence_i %in% date_range_julian

    message(sprintf(
      "Found %d / %d days of data in the queried date range.\n",
      sum(date_sequence_in),
      length(date_range_julian)
    ))
  }

  #### 13. Prepare download paths
  destfiles <- paste0(directory_to_save, download_names)

  #### 14. Exit early if download=FALSE (deprecated behavior)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(urls)
    ))
    return(invisible(list(
      urls = urls,
      destfiles = destfiles,
      n_files = length(urls)
    )))
  }

  #### 15. Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = urls,
    destfiles = destfiles,
    token = nasa_earth_data_token,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  message("Download process complete.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}


#' Download toxic release data
#' @description
#' The \code{download_tri()} function accesses and downloads toxic release
#' data from the U.S. Environmental Protection Agency's (EPA) Toxic Release
#' Inventory (TRI) Program. The EPA TRI basic data files contain annual,
#' facility-reported toxic chemical release and waste management information.
#' EPA publishes TRI basic files in multiple annual variants under the same
#' service endpoint: a nationwide file (\code{"US"}), state-specific files
#' identified by two-letter postal abbreviations (for example \code{"AZ"} or
#' \code{"NC"}), and a tribal file (\code{"tbl"}).
#' @note TRI data does not require authentication. State and tribal downloads
#'   are saved with jurisdiction-specific file names, while the U.S.-wide
#'   download keeps the historical \code{tri_raw_<year>.csv} naming pattern.
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param jurisdiction character(1). TRI file variant to download. Use
#'   \code{"US"} for the nationwide file, a two-letter state or territory code
#'   such as \code{"AZ"} or \code{"NC"} for a jurisdiction-specific file, or
#'   \code{"tbl"} for the tribal file. Default is \code{"US"}.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mariana Kassien, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024tri}{amadeus}
#' @examples
#' \dontrun{
#' download_tri(
#'   year = 2021L,
#'   directory_to_save = tempdir(),
#'   jurisdiction = "NC",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_tri <- function(
  year = c(2018L, 2022L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  jurisdiction = "US",
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Check jurisdiction
  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      "`jurisdiction` must be a single character value such as ",
      "\"US\", \"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction <- trimws(jurisdiction)
  if (!nzchar(jurisdiction)) {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code, or \"tbl\".\n",
      call. = FALSE
    )
  }
  jurisdiction_upper <- toupper(jurisdiction)
  if (identical(jurisdiction_upper, "TBL")) {
    jurisdiction_url <- "tbl"
    jurisdiction_suffix <- "_tbl"
  } else if (
    identical(jurisdiction_upper, "US") ||
      grepl("^[A-Z]{2}$", jurisdiction_upper)
  ) {
    jurisdiction_url <- jurisdiction_upper
    jurisdiction_suffix <- if (identical(jurisdiction_upper, "US")) {
      ""
    } else {
      paste0("_", jurisdiction_upper)
    }
  } else {
    stop(
      "`jurisdiction` must be \"US\", a two-letter state code such as ",
      "\"AZ\", or \"tbl\".\n",
      call. = FALSE
    )
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

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### Define measurement data paths
  url_download <- paste0(
    # nolint: line_length_linter.
    "https://data.epa.gov/efservice/downloads/tri/",
    "mv_tri_basic_download/"
  )
  year_sequence <- seq(year[1], year[2], 1)
  download_urls <- paste0(
    url_download,
    year_sequence,
    "_",
    jurisdiction_url,
    "/csv"
  )
  download_names <- paste0(
    directory_to_save,
    "tri_raw_",
    year_sequence,
    jurisdiction_suffix,
    ".csv"
  )

  #### Filter to files that need downloading
  needs_download <- sapply(download_names, amadeus::check_destfile)
  download_urls_filtered <- download_urls[needs_download]
  download_names_filtered <- download_names[needs_download]

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
  download_result <- amadeus::download_run_method(
    urls = download_urls_filtered,
    destfiles = download_names_filtered,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  message("Requests were processed.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

#' Download road emissions data
#' @description
#' The \code{download_nei()} function accesses and downloads road emissions
#' data from the U.S Environmental Protection Agency's (EPA) National
#' Emissions Inventory (NEI).
#' @note NEI data does not require authentication.
#' @param epa_certificate_path TO BE DEPRECATED. Certificate path.
#' @param certificate_url TO BE DEPRECATED. Certificate URL.
#' @param year integer(1). Available years of NEI data.
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
#' @author Kyle Messier, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024nei}{amadeus}
#' @examples
#' \dontrun{
#' download_nei(
#'   year = c(2017L, 2020L),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_nei <- function(
  epa_certificate_path = NULL,
  certificate_url = paste0(
    # nolint: line_length_linter.
    "http://cacerts.digicert.com/",
    "DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ),
  year = c(2017L, 2020L),
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

  if (
    !is.null(epa_certificate_path) ||
      certificate_url !=
        "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt"
  ) {
    warning(
      "Parameters 'epa_certificate_path' and 'certificate_url'",
      " are deprecated.\n",
      "SSL certificates are now handled automatically by httr2.\n",
      call. = FALSE
    )
  }

  #### Define measurement data paths
  # Each NEI year has a distinct URL path suffix; use a named lookup so the
  # function works correctly whether `year` is a scalar (as dispatched from
  # the targets pipeline via pattern = map()) or a full c(2017, 2020) vector.
  url_download_base <- "https://gaftp.epa.gov/air/nei/%s/data_summaries/"
  nei_url_map <- c(
    "2017" = "2017v1/2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  nei_file_map <- c(
    "2017" = "2017neiApr_onroad_byregions.zip",
    "2020" = "2020nei_onroad_byregion.zip"
  )
  year_chr <- as.character(year)
  unknown <- setdiff(year_chr, names(nei_url_map))
  if (length(unknown) > 0) {
    stop(paste0(
      "NEI data is not available for year(s): ",
      paste(unknown, collapse = ", "),
      ". Available years: ",
      paste(names(nei_url_map), collapse = ", "),
      ".\n"
    ))
  }
  download_urls <- vapply(
    year_chr,
    function(y) paste0(sprintf(url_download_base, y), nei_url_map[y]),
    character(1)
  )
  download_names_file <- unname(nei_file_map[year_chr])
  download_names <- paste0(directory_to_download, download_names_file)

  #### Filter to files that need downloading
  needs_download <- sapply(download_names, amadeus::check_destfile)
  download_urls_filtered <- download_urls[needs_download]
  download_names_filtered <- download_names[needs_download]

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

  #### Unzip data (custom unzipping to separate directories per file)
  if (unzip) {
    dir_unzip <- paste0(
      directory_to_save,
      sub(".zip", "", download_names_file)
    )
    for (fn in seq_along(dir_unzip)) {
      if (file.exists(download_names[fn])) {
        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }
    }
  }

  #### Remove zip files
  if (remove_zip) {
    for (fn in download_names) {
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }
  }

  message("Requests were processed.\n")

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

#' Download gridMET data
#' @description
#' The \code{download_gridmet} function accesses and downloads gridded
#' surface meteorological data from the University of California Merced
#' Climatology Lab's gridMET dataset.
#' @note gridMET data does not require authentication.
#' @param variables character. Variable(s) name(s).
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_abatzoglou2013development}{amadeus}
#' @examples
#' \dontrun{
#' download_gridmet(
#'   variables = "pr",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_gridmet <- function(
  variables = NULL,
  year = c(2018, 2022),
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

  #### Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

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

  #### Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  years <- seq(year[1], year[2], 1)

  #### Define variables
  variables_list <- amadeus::process_variable_codes(
    variables = variables,
    source = "gridmet"
  )

  #### Define URL base
  base <- "https://www.northwestknowledge.net/metdata/data/"

  #### Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")

    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }

    for (y in seq_along(years)) {
      year_l <- years[y]
      url <- paste0(base, variable, "_", year_l, ".nc")

      # Validate first URL only
      if (v == 1 && y == 1) {
        if (!amadeus::check_url_status(url)) {
          stop(paste0(
            "Invalid year returns HTTP code 404. ",
            "Check `year` parameter.\n"
          ))
        }
      }

      destfile <- paste0(
        directory_to_save,
        variable,
        "/",
        variable,
        "_",
        year_l,
        ".nc"
      )

      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
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

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL,
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

#' Download TerraClimate data
#' @description
#' The \code{download_terraclimate} function accesses and downloads climate
#' and water balance data from the University of California Merced
#' Climatology Lab's TerraClimate dataset.
#' @note TerraClimate data does not require authentication.
#' @param variables character. Variable(s) name(s).
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
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
#' \insertRef{article_abatzoglou2018terraclimate}{amadeus}
#' @examples
#' \dontrun{
#' download_terraclimate(
#'   variables = "ppt",
#'   year = 2023,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_terraclimate <- function(
  variables = NULL,
  year = c(2018, 2022),
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

  #### Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

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

  #### Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  years <- seq(year[1], year[2], 1)

  #### Define variables
  variables_list <- amadeus::process_variable_codes(
    variables = variables,
    source = "terraclimate"
  )

  #### Define URL base
  base <- paste0(
    "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/",
    "TerraClimate_"
  )

  #### Collect all URLs and destination files
  all_urls <- character()
  all_destfiles <- character()

  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")

    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }

    for (y in seq_along(years)) {
      year_l <- years[y]
      url <- paste0(base, variable, "_", year_l, ".nc")

      # Validate first URL only
      if (v == 1 && y == 1) {
        if (!amadeus::check_url_status(url)) {
          stop(paste0(
            "Invalid year returns HTTP code 404. ",
            "Check `year` parameter.\n"
          ))
        }
      }

      destfile <- paste0(
        directory_to_save,
        variable,
        "/",
        variable,
        "_",
        year_l,
        ".nc"
      )

      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
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

  #### Download files using httr2
  download_result <- amadeus::download_run_method(
    urls = all_urls,
    destfiles = all_destfiles,
    token = NULL,
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

# nolint start
#' Download National Hydrography Dataset (NHD) data
#' @description
#' NHDPlus data provides the most comprehensive and high-resolution
#' hydrography data. This function downloads **national** dataset from
#' NHDPlus Version 2.1 on USGS Amazon S3 storage.
#' @note
#' For HUC, set `type = "Seamless"`. HUC12 layer presents in the seamless
#' geodatabase. Users can aggregate HUC12 layer to make HUC6, HUC8, HUC10, etc.
#' For whom wants to download a specific region,
#' please visit [Get NHDPlus Data](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#ListofAreas)
#' @param region character(1). One of `c("Lower48", "Islands")`.
#' When `"Islands"` is selected, the data will be downloaded for Hawaii,
#' Puerto Rico, and Virgin Islands.
#' @param type character(1). One of `c("Seamless", "OceanCatchment")`.
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
#' @param unzip logical(1). Unzip the downloaded compressed files.
#' Default is \code{FALSE}. Supports ".7z" extraction via \pkg{archive}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Downloaded files will be stored in \code{directory_to_save}.
#' @author Insang Song
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_usgs2023nhd}{amadeus}
#' @examples
#' \dontrun{
#' download_huc(
#'   region = "Lower48",
#'   type = "Seamless",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE,
#'   unzip = FALSE
#' )
#' }
#' @export
# @importFrom archive archive_extract
download_huc <-
  function(
    region = c("Lower48", "Islands"),
    type = c("Seamless", "OceanCatchment"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = TRUE,
    remove_command = FALSE,
    unzip = FALSE,
    hash = FALSE,
    show_progress = TRUE,
    max_tries = 20,
    rate_limit = 2
  ) {
    #### 1. check for data download acknowledgement
    amadeus::download_permit(acknowledgement = acknowledgement)
    #### 2. directory setup
    amadeus::download_setup_dir(directory_to_save)
    directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

    region <- match.arg(region)
    type <- match.arg(type)

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
    url_base <-
      "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NationalData/"

    if (region == "Lower48") {
      if (type == "Seamless") {
        url_template_nat <-
          "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
      }
      if (type == "OceanCatchment") {
        url_template_nat <-
          "NHDPlusV21_NationalData_OceanCatchment_Lower48_fgdb_01.7z"
      }
    }
    if (region == "Islands") {
      if (type == "Seamless") {
        url_template_nat <-
          "NHDPlusV21_NationalData_Seamless_Geodatabase_HI_PR_VI_PI_03.7z"
      }
      if (type == "OceanCatchment") {
        url_template_nat <-
          "NHDPlusV21_NationalData_OceanCatchment_Islands_fgdb_01.7z"
      }
    }
    download_urls <- paste0(url_base, url_template_nat)
    download_names <- paste0(directory_to_save, url_template_nat)

    #### Exit early if download=FALSE
    if (!isTRUE(download)) {
      message("Skipping download.\n")
      return(invisible(list(
        urls = download_urls,
        destfiles = download_names,
        n_files = 1
      )))
    }

    #### Download using httr2
    amadeus::download_run_method(
      urls = download_urls,
      destfiles = download_names,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
    sapply(
      download_names,
      amadeus::download_unzip,
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )

    message("Requests were processed.\n")
    return(amadeus::download_hash(hash, directory_to_save))
  }
# nolint end

################################################################################
# 2. Emissions Database for Global Atmospheric Research (EDGAR)
# nolint start
#' Download EDGAR Emissions Data
#'
#' Constructs and optionally downloads EDGAR emissions data URLs based on
#' user-specified inputs including species, temporal resolution, emission
#' sectors, and file formats.
#'
#' @param species Character vector. One or more species to download.
#'  Supported values: "BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10",
#'  "PM2.5", "SO2". Input is case-insensitive and supports "pm2.5" or "pm25".
#' @param version Character. EDGAR data version. Supported values: "8.1" for
#'  most recent version data or "8.1_voc" for VOC speciation data.
#' @param temp_res Character. Temporal resolution for specification with
#'  version 8.1. One of "yearly", "monthly", or "timeseries". temp_res is not
#'  needed for version=8.1_voc and will be ignored if specified.
#' @param sector_yearly Character vector or NULL. Emission sectors for yearly
#'  data. If NULL, totals will be used. Possible values include:
#'  "AGS", "AWB", "CHE", "ENE", "IND", "MNM", "NMM", "PRU_SOL", "RCO",
#'  "REF_TRF", "SWD_INC", "SWD_LDF", "TNR_Aviation_CDS", "TNR_Aviation_CRS",
#'  "TNR_Aviation_LTO", "TNR_Aviation_SPS", "TNR_Other",
#'  "TNR_Ship", "TRO", "WWT"
#' @param sector_monthly Character vector or NULL. Emission sectors for monthly
#'  data. If NULL, the function will use full-species files
#'  (not sector-specific).
#'  Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param sector_voc Character vector or NULL. Emission sectors for VOC
#'   speciation
#'  data. If NULL, the function will use full-species files
#'  (not sector-specific).
#'  Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param output Character. Output type. Supported values include "emi" for
#'  emissions and "flx" for fluxes.
#' @param format Character. File format to download. Typically "nc" (NetCDF)
#'  or "txt". Flux output and monthly outputs are only supported in .nc format
#' @param year_range Numeric vector of length 1, 2 or NULL. Year range, e.g.,
#' 2021, or c(2021, 2022). If NULL, uses all available years (1970-2022 for
#' yearly data, 2000-2022 for monthly and VOC speciation data)
#' @param voc Integer vector or NULL. Used for VOC speciation in version
#'  "8.1_voc". Accepts integers from 1 to 25. See:
#'  https://edgar.jrc.ec.europa.eu/dataset_ap81_VOC_spec#p3  for reference on
#'  speciation groups and VOC numbers.
#' @return A list of download URLs (character). Optionally downloads available
#'  files and warns about missing ones.
#' @param directory_to_save character(1). Directory to save data. Two
#'  sub-directories will be created for the downloaded zip files ("/zip_files")
#'  and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#'  user acknowledges that the data downloaded using this function may be very
#'  large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#'  containing all download commands. By setting \code{TRUE} the function
#'  will download all of the requested data files.
#' @param remove_command logical(1).
#'  Remove (\code{TRUE}) or keep (\code{FALSE})
#'  the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#'  Default is \code{FALSE}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#'  an \code{rlang::hash_file()} hash character corresponding to the
#'  downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @author Mariana Alifa Kassien
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#'
# nolint end
#' @examples
#' \dontrun{
#' download_edgar(
#' species = "CO",
#' acknowledgement = TRUE,
#' temp_res = "yearly",
#' sector_yearly = "ENE",
#' year_range = c(2021, 2022)
#' )
#' }
#' \dontrun{
#' download_edgar(
#' species = "PM2.5",
#' acknowledgement = TRUE,
#' temp_res = "monthly",
#' sector_monthly = c("TRANSPORT", "WASTE")
#' )
#' }
#' \dontrun{
#' download_edgar(
#' species = "SO2",
#' acknowledgement = TRUE,
#' temp_res = "timeseries"
#' )
#' }
#' @export
download_edgar <- function(
  species = c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM2.5", "SO2"),
  version = "8.1",
  temp_res = NULL,
  sector_yearly = NULL,
  sector_monthly = NULL,
  sector_voc = NULL,
  format = "nc",
  output = "emi",
  year_range = NULL,
  voc = NULL,
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
  # check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  # Handle deprecated parameters
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

  # directory setup
  directory_original <- amadeus::download_sanitize_path(directory_to_save)
  directories <- amadeus::download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  durl <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/"

  # Normalize species input
  species <- toupper(species)
  species <- gsub("NOX", "NOx", species)

  # Check for invalid combinations
  if (any(output == "flx" & format == "txt")) {
    stop("Output 'flux' is only supported for format 'nc'.")
  }
  if (any(temp_res == "monthly" & format == "txt")) {
    stop("Monthly resolution is only supported for format 'nc'.")
  }

  if (!is.null(year_range)) {
    if (length(year_range) == 1) {
      yearsvec <- year_range
    } else if (length(year_range) == 2) {
      yearsvec <- seq(year_range[1], year_range[2])
    } else {
      stop("year_range must be of length 1 or 2")
    }
  }

  urls <- character()

  if (version == "8.1") {
    vers <- "v81_FT2022_AP_new/"
    vers_file <- "v8.1_FT2022_AP"

    grid_species <- data.frame(species = species, stringsAsFactors = FALSE)
    grid_species$folder <- gsub(
      "(?i)pm2\\.5|pm25",
      "PM2.5",
      grid_species$species,
      perl = TRUE
    )
    grid_species$file <- gsub(
      "(?i)pm2\\.5|pm25",
      "PM25",
      grid_species$species,
      perl = TRUE
    )

    if (temp_res == "timeseries") {
      urls <- paste0(durl, vers, "EDGAR_", grid_species$file, "_1970_2022.zip")
      urls <- ifelse(
        grid_species$file == "SO2",
        gsub("2022.zip$", "2022_v2.zip", urls),
        urls
      )
    } else if (temp_res == "yearly") {
      if (!is.null(sector_yearly)) {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          sector = sector_yearly,
          year = if (!is.null(year_range)) yearsvec else NA,
          stringsAsFactors = FALSE
        )

        urls <- if (!is.null(year_range)) {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/",
            grid$sector,
            "/",
            output,
            "_",
            format,
            "/",
            vers_file,
            "_",
            grid_species$species[grid$species_idx],
            "_",
            grid$year,
            "_",
            grid$sector,
            "_",
            output,
            "_",
            format,
            ".zip"
          )
        } else {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/",
            grid$sector,
            "/",
            grid$sector,
            "_",
            output,
            "_",
            format,
            ".zip"
          )
        }
      } else {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          year = if (!is.null(year_range)) yearsvec else NA,
          stringsAsFactors = FALSE
        )

        urls <- if (!is.null(year_range)) {
          paste0(
            durl,
            vers,
            grid_species$folder[grid$species_idx],
            "/TOTALS/",
            output,
            "_",
            format,
            "/",
            vers_file,
            "_",
            grid_species$species[grid$species_idx],
            "_",
            grid$year,
            "_TOTALS_",
            output,
            "_",
            format,
            ".zip"
          )
        } else {
          paste0(
            durl,
            vers,
            grid_species$folder,
            "/TOTALS/TOTALS_",
            output,
            "_",
            format,
            ".zip"
          )
        }
      }
    } else if (temp_res == "monthly") {
      if (!is.null(sector_monthly)) {
        grid <- expand.grid(
          species_idx = seq_len(nrow(grid_species)),
          sector = sector_monthly,
          stringsAsFactors = FALSE
        )
        urls <- paste0(
          durl,
          vers,
          "monthly/",
          grid_species$folder[grid$species_idx],
          "/bkl_",
          grid$sector,
          "/bkl_",
          grid$sector,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      } else {
        urls <- paste0(
          durl,
          vers,
          "monthly/EDGAR_",
          grid_species$file,
          "_m_2000_2022.zip"
        )
        urls <- ifelse(
          grid_species$file == "SO2",
          gsub("2022.zip$", "2022_v2.zip", urls),
          urls
        )
      }
    } else {
      stop("Unsupported temp_res value")
    }
  } else if (version == "8.1_voc") {
    vers <- "v81_FT2022_VOC_spec/"
    vers_file <- "v8.1_FT2022_VOC_spec"

    if (!is.null(sector_voc)) {
      grid <- expand.grid(
        voc = voc,
        sector = sector_voc,
        year = if (!is.null(year_range)) yearsvec else NA,
        stringsAsFactors = FALSE
      )

      urls <- if (!is.null(year_range)) {
        paste0(
          durl,
          vers,
          "voc",
          grid$voc,
          "/bkl_",
          grid$sector,
          "/",
          output,
          "_",
          format,
          "/",
          vers_file,
          "_voc",
          grid$voc,
          "_",
          grid$year,
          "_bkl_",
          grid$sector,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      } else {
        paste0(
          durl,
          vers,
          "voc",
          voc,
          "/bkl_",
          sector_voc,
          "/bkl_",
          sector_voc,
          "_",
          output,
          "_",
          format,
          ".zip"
        )
      }
    } else {
      urls <- paste0(durl, vers, "EDGAR_voc", voc, "_1970_2022.zip")
    }
  } else {
    stop("Unsupported version")
  }
  # Check constructed urls
  message("Constructed URL(s): ", paste(urls, collapse = "\n"))

  #### 5. build download file name
  download_label <- temp_res
  if (is.null(download_label) || length(download_label) == 0) {
    download_label <- if (version == "8.1_voc") "voc" else "edgar"
  }
  download_names <- paste0(
    directory_to_download,
    "edgar_",
    download_label,
    "_",
    basename(urls)
  )

  #### Exit early if download=FALSE (return unvalidated URL list)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available for download.\n",
      length(urls)
    ))
    return(invisible(list(
      urls = urls,
      destfiles = download_names,
      n_files = length(urls)
    )))
  }

  # Validate and download
  download_urls <- c()
  download_names_valid <- c()
  missing_urls <- c()

  for (i in seq_along(urls)) {
    u <- urls[i]
    if (!(amadeus::check_url_status(u))) {
      missing_urls <- c(missing_urls, u)
    } else {
      download_urls <- c(download_urls, u)
      download_names_valid <- c(download_names_valid, download_names[i])
    }
  }
  # Stop function if no valid urls were created
  if (is.null(download_urls) || length(download_urls) == 0) {
    stop("No valid URLs were constructed.")
  }
  # Issue warning message for URLs not found
  if (length(missing_urls)) {
    warning(
      "Some URLs could not be accessed:",
      paste(missing_urls, collapse = "\n")
    )
  }

  #### Filter to files that need downloading
  needs_download <- !file.exists(download_names_valid) |
    file.size(download_names_valid) == 0
  download_urls_dl <- download_urls[needs_download]
  download_names_dl <- download_names_valid[needs_download]

  #### Download using httr2
  if (length(download_urls_dl) > 0) {
    amadeus::download_run_method(
      urls = download_urls_dl,
      destfiles = download_names_dl,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  }

  #### Unzip data
  sapply(
    download_names_valid,
    amadeus::download_unzip,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  amadeus::download_remove_zips(
    remove = remove_zip,
    download_name = download_names_valid
  )
  return(amadeus::download_hash(hash, directory_to_save))
}


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


###############################################################################
# 4. USDA Cropscape
# nolint start
#' Download CropScape data
#' @description
#' Accesses and downloads United States Department of Agriculture
#' CropScape Cropland Data Layer data from
#' the [USDA National Agricultural Statistics Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php) or the
#' [George Mason University website](https://nassgeodata.gmu.edu/CropScape/).
#' @param year integer(1). Year of the data to download.
#' @param source character(1). Data source, one of `c("USDA", "GMU")`.
#' * `"USDA"` will download the national data from the USDA website
#'   (available in 2008-last year).
#' * `"GMU"` will download the data from the George Mason University
#'   website (available in 1997-last year).
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
#' @param unzip logical(1). Unzip the downloaded compressed files.
#' Default is \code{FALSE}.
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @param show_progress logical(1). Show download progress.
#'   Default is \code{TRUE}.
#' @param max_tries integer(1). Maximum download retry attempts.
#'   Default is \code{20}.
#' @param rate_limit numeric(1). Minimum seconds between requests.
#'   Default is \code{2}.
#' @author Insang Song
#' @note JSON files should be found at STAC catalog of OpenLandMap
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Yearly comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @examples
#' \dontrun{
#' download_cropscape(
#'   year = 2020,
#'   source = "USDA",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = FALSE, # NOTE: download skipped for examples,
#'   remove_command = TRUE,
#'   unzip = FALSE
#' )
#' }
#' @importFrom archive archive_extract
#' @export
download_cropscape <- function(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
) {
  source <- match.arg(source)
  if (source == "GMU" && year < 1997) {
    stop("Year should be equal to or greater than 1997.")
  }
  if (source == "USDA" && year < 2008) {
    stop("Year should be equal to or greater than 2008.")
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
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 3. define measurement data paths
  url_download_base <- switch(
    source,
    USDA = "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/datasets/",
    GMU = "https://nassgeodata.gmu.edu/nass_data_cache/tar/"
  )
  filename_template <- switch(
    source,
    USDA = "%d_30m_cdls.zip",
    GMU = "%d_cdls.tar.gz"
  )
  url_download_template <- paste0(url_download_base, filename_template)

  download_urls <-
    sprintf(url_download_template, year)
  download_names_file <- sprintf(filename_template, year)
  download_names <- paste0(directory_to_save, download_names_file)

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
  amadeus::download_run_method(
    urls = download_urls,
    destfiles = download_names,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )

  #### Unzip data
  # nocov start
  if (unzip) {
    extension <- ifelse(source == "USDA", "\\.zip", "(\\.tar|\\.tar\\.gz)")
    dir_unzip <- gsub(extension, "", download_names)
    for (fn in seq_along(dir_unzip)) {
      if (!dir.exists(dir_unzip[fn])) {
        dir.create(dir_unzip[fn], recursive = TRUE)
      }
      archive::archive_extract(download_names[fn], dir = dir_unzip[fn])
    }
  }
  # nocov end
  message("Requests were processed.\n")
  return(amadeus::download_hash(hash, directory_to_save))
}
# nolint end

################################################################################
# nolint start
#' Download NOAA GOES ADP data
#' @description
#' The \code{download_goes()} function accesses and downloads NOAA GOES-16 or
#' GOES-18 Aerosol Detection Product (ADP) files from the
#' NOAA Open Data Dissemination (NODD) AWS S3 bucket. Files are in NetCDF
#' format and contain aerosol detection variables (e.g. \code{"Smoke"},
#' \code{"Dust"}) on the GOES fixed geostationary grid.
#' @note
#' \itemize{
#'   \item GOES data does not require authentication.
#'   \item GOES-16 (East) covers the Americas; GOES-18 (West) covers the
#'     western hemisphere and Pacific.
#'   \item ADP-C (CONUS) scans are produced approximately every 5 minutes.
#'     A single day may contain several hundred files.
#'   \item GOES ADP files use the GOES fixed geostationary projection. Use
#'     \code{process_goes()} to load and reproject to EPSG:4326.
#' }
#' @param date character(1 or 2). Date (YYYY-MM-DD) or start and end dates.
#' @param satellite character(1). GOES satellite number: \code{"16"} (East,
#'   default) or \code{"18"} (West).
#' @param product character(1). ADP scan sector: \code{"ADP-C"} (CONUS,
#'   default), \code{"ADP-F"} (Full Disk), or \code{"ADP-M"} (Mesoscale).
#' @param directory_to_save character(1). Directory to save downloaded files.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default \code{TRUE}).
#' @param hash logical(1). Return hash of downloaded files (default \code{FALSE}).
#' @param max_tries integer(1). Maximum retry attempts (default 20).
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2).
#' @author Mitchell Manware
#' @return invisible list with download results; or hash character if
#'   \code{hash = TRUE}
#' @importFrom httr2 request
#' @importFrom httr2 req_retry
#' @importFrom httr2 req_timeout
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_string
#' @importFrom httr2 resp_status
#' @examples
#' \dontrun{
#' download_goes(
#'   date = "2024-01-01",
#'   satellite = "16",
#'   product = "ADP-C",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE
#' )
#' }
#' @export
# nolint end
download_goes <- function(
  date = c("2024-01-01", "2024-01-01"),
  satellite = "16",
  product = "ADP-C",
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

  #### Validate satellite
  satellite <- as.character(satellite)
  if (!satellite %in% c("16", "18")) {
    stop("satellite must be '16' or '18'.\n")
  }

  #### Validate product
  product <- toupper(product)
  valid_products <- c("ADP-C", "ADP-F", "ADP-M")
  if (!product %in% valid_products) {
    stop(paste0(
      "product must be one of: ",
      paste(valid_products, collapse = ", "),
      ".\n"
    ))
  }
  product_prefix <- switch(
    product,
    "ADP-C" = "ABI-L2-ADPC",
    "ADP-F" = "ABI-L2-ADPF",
    "ADP-M" = "ABI-L2-ADPM"
  )

  #### Check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  # nolint start
  #### S3 bucket base URL (NOAA NODD public bucket, no auth required)
  bucket_url <- paste0("https://noaa-goes", satellite, ".s3.amazonaws.com/")
  # nolint end

  #### Iterate over each day and list available files from S3
  dates_seq <- seq(as.Date(date[1]), as.Date(date[2]), by = "day")
  all_urls <- character()
  all_destfiles <- character()

  for (d in seq_along(dates_seq)) {
    d_date <- dates_seq[d]
    year <- format(d_date, "%Y")
    doy <- sprintf("%03d", as.integer(format(d_date, "%j")))
    prefix <- paste0(product_prefix, "/", year, "/", doy, "/")

    # nolint start
    list_url <- paste0(
      bucket_url,
      "?list-type=2&prefix=",
      prefix,
      "&max-keys=1000"
    )
    # nolint end

    tryCatch(
      {
        resp <- httr2::request(list_url) |>
          httr2::req_retry(
            max_tries = 3,
            is_transient = function(r) {
              httr2::resp_status(r) %in% c(429, 503, 504)
            }
          ) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()

        xml_body <- httr2::resp_body_string(resp)
        key_matches <- gregexpr("<Key>[^<]+\\.nc</Key>", xml_body)
        keys <- regmatches(xml_body, key_matches)[[1]]
        keys <- gsub("<Key>|</Key>", "", keys)

        if (length(keys) > 0) {
          for (k in seq_along(keys)) {
            key <- keys[k]
            file_url <- paste0(bucket_url, key)
            destfile <- paste0(directory_to_save, key)
            all_urls <- c(all_urls, file_url)
            all_destfiles <- c(all_destfiles, destfile)
          }
        } else {
          message(sprintf(
            "No files found for %s on %s (DOY %s).\n",
            product,
            format(d_date, "%Y-%m-%d"),
            doy
          ))
        }
      },
      error = function(e) {
        warning(
          sprintf(
            "Failed to list GOES files for %s/%s/%s: %s\n",
            product,
            year,
            doy,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }

  if (length(all_urls) == 0) {
    message("No GOES ADP files found for the specified parameters.\n")
    return(invisible(list(success = 0, failed = 0, skipped = 0)))
  }

  #### Exit early if download=FALSE (deprecated path)
  if (!isTRUE(download)) {
    message(sprintf(
      "Skipping download. Found %d files available.\n",
      length(all_urls)
    ))
    return(invisible(list(
      urls = all_urls,
      destfiles = all_destfiles,
      n_files = length(all_urls)
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

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

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
  if (length(year) == 1) {
    year <- c(year, year)
  }
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

# nolint start
#' Download drought index data
#' @description
#' The \code{download_drought()} function downloads drought index data from
#' publicly available sources. Three source datasets are supported:
#' \itemize{
#'   \item \strong{SPEI} (Standardized Precipitation-Evapotranspiration Index):
#'     Multi-year netCDF files by timescale from
#'     \url{https://spei.csic.es}.
#'   \item \strong{EDDI} (Evaporative Demand Drought Index): Weekly raster
#'     files by timescale from
#'     \url{https://www.drought.gov/data-maps-tools/evaporative-demand-drought-index-eddi}.
#'   \item \strong{USDM} (U.S. Drought Monitor): Weekly drought class
#'     shapefiles from
#'     \url{https://droughtmonitor.unl.edu}.
#' }
# nolint end
#' @param source character(1). Drought data source. One of \code{"spei"},
#'   \code{"eddi"}, or \code{"usdm"}.
#' @param date character(1 or 2). Single date or start/end dates.
#'   Format \code{"YYYY-MM-DD"}. For SPEI/EDDI the year component selects
#'   the annual file(s); for USDM the full date is used to select weekly
#'   release(s).
#' @param timescale integer(1). Accumulation timescale in months (SPEI/EDDI
#'   only; ignored for USDM). Typical values are 1, 3, 6, 12, 24, 48.
#'   Default is \code{1L}.
#' @param directory_to_save character(1). Directory to save downloaded data.
#' @param acknowledgement logical(1). Must be \code{TRUE} to proceed.
#' @param hash logical(1). Return \code{rlang::hash_file()} hash of
#'   downloaded files. Default \code{FALSE}.
#' @param show_progress logical(1). Show download progress bar.
#'   Default \code{TRUE}.
#' @param max_tries integer(1). Maximum retry attempts. Default \code{3L}.
#' @param rate_limit numeric(1). Minimum seconds between HTTP requests.
#'   Default \code{2}.
#' @param unzip logical(1). Unzip downloaded zip archives (USDM only).
#'   Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip archives after unzipping
#'   (USDM only). Default \code{FALSE}.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item SPEI and EDDI are raster products; USDM is a polygon
#'     product (shapefile). Their \code{process_drought()} and
#'     \code{calculate_drought()} handling differ accordingly.
#'   \item No authentication is required for any of these sources.
#' }
#' @author Insang Song
#' @return \code{invisible(NULL)} when \code{hash = FALSE}; a character hash
#'   string when \code{hash = TRUE}.
#' @seealso \code{\link{process_drought}}, \code{\link{calculate_drought}}
#' @examples
#' \dontrun{
#' download_drought(
#'   source = "spei",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L,
#'   directory_to_save = "./data/drought",
#'   acknowledgement = TRUE
#' )
#' download_drought(
#'   source = "usdm",
#'   date = c("2020-01-07", "2020-03-31"),
#'   directory_to_save = "./data/drought",
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_drought <- function(
  source = c("spei", "eddi", "usdm"),
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 3L,
  rate_limit = 2,
  unzip = TRUE,
  remove_zip = FALSE,
  ...
) {
  #### Check acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### Validate source
  source <- match.arg(source)

  #### Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Validate date
  if (length(date) == 1L) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2L)
  date <- date[order(as.Date(date))]

  #### Validate unzip/remove_zip
  if (!unzip && remove_zip) {
    stop(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    )
  }

  #### Validate timescale (SPEI/EDDI only)
  timescale <- as.integer(timescale)
  if (source %in% c("spei", "eddi")) {
    if (is.na(timescale) || timescale < 1L) {
      stop("`timescale` must be a positive integer (months).\n")
    }
  }

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Source-specific download logic

  if (source == "spei") {
    # SPEI: one global multi-year netCDF per timescale
    # nolint start
    ts_str <- sprintf("%02d", timescale)
    spei_file <- paste0("spei", ts_str, ".nc")
    spei_url_candidates <- c(
      paste0("https://spei.csic.es/spei_database_2_11/nc/", spei_file),
      paste0("https://spei.csic.es/spei_database_2_10/nc/", spei_file),
      paste0("https://spei.csic.es/files/", spei_file)
    )
    # nolint end
    destfile <- paste0(directory_to_save, "spei", ts_str, ".nc")

    if (!amadeus::check_destfile(destfile)) {
      message("SPEI file already exists. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_save))
      }
      return(invisible(list(success = 0, failed = 0, skipped = 1)))
    }

    url <- NULL
    for (candidate_url in spei_url_candidates) {
      if (amadeus::check_url_status(candidate_url)) {
        url <- candidate_url
        break
      }
    }

    if (is.null(url)) {
      stop(sprintf(
        "SPEI timescale %s returned HTTP 404. Check `timescale` parameter.\n",
        ts_str
      ))
    }

    download_result <- amadeus::download_run_method(
      urls = url,
      destfiles = destfile,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    }
    return(invisible(download_result))
  }

  if (source == "eddi") {
    # EDDI: weekly CONUS rasters by timescale, organised in year folders
    # nolint start
    ts_str <- sprintf("%02d", timescale)
    eddi_base <- "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data"
    eddi_ext <- "asc"
    # nolint end

    week_dates <- amadeus::drought_weekly_dates(date[1], date[2])
    if (length(week_dates) == 0) {
      stop(
        "No Tuesday dates found in the specified date range. ",
        "EDDI is a weekly (Tuesday) product.\n"
      )
    }

    all_urls <- character(0)
    all_destfiles <- character(0)

    for (wd in week_dates) {
      year_str <- substr(wd, 1, 4)
      url <- sprintf(
        "%s/%s/EDDI_ETrs_%smn_%s.%s",
        eddi_base,
        year_str,
        ts_str,
        wd,
        eddi_ext
      )
      destfile <- sprintf(
        "%sEDDI_ETrs_%smn_%s.%s",
        directory_to_save,
        ts_str,
        wd,
        eddi_ext
      )
      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
    }

    if (length(all_urls) == 0L) {
      message("All EDDI files already exist. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_save))
      }
      return(invisible(list(
        success = 0,
        failed = 0,
        skipped = length(week_dates)
      )))
    }

    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_save))
    }
    return(invisible(download_result))
  }

  if (source == "usdm") {
    # USDM: weekly CONUS drought-class shapefiles (zip archives, Tuesdays)
    directories <- amadeus::download_setup_dir(directory_to_save, zip = TRUE)
    directory_to_download <- directories[1]
    directory_to_unzip <- directories[2]

    # nolint start
    base <- "https://droughtmonitor.unl.edu/data/shapefiles_m"
    # nolint end

    week_dates <- amadeus::drought_weekly_dates(date[1], date[2])
    if (length(week_dates) == 0) {
      stop(
        "No Tuesday dates found in the specified date range. ",
        "USDM is a weekly (Tuesday) product.\n"
      )
    }

    all_urls <- character(0)
    all_destfiles <- character(0)

    for (wd in week_dates) {
      url <- sprintf("%s/USDM_%s_M.zip", base, wd)
      destfile <- sprintf("%sUSDM_%s_M.zip", directory_to_download, wd)
      if (amadeus::check_destfile(destfile)) {
        all_urls <- c(all_urls, url)
        all_destfiles <- c(all_destfiles, destfile)
      }
    }

    if (length(all_urls) == 0L) {
      message("All USDM files already exist. Skipping download.\n")
      if (hash) {
        return(amadeus::download_hash(hash = TRUE, directory_to_unzip))
      }
      return(invisible(list(
        success = 0,
        failed = 0,
        skipped = length(week_dates)
      )))
    }

    if (length(all_urls) > 0 && !amadeus::check_url_status(all_urls[1])) {
      stop(
        "First USDM URL returned HTTP 404. ",
        "Check `date` parameter.\n"
      )
    }

    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )

    for (d in seq_along(all_destfiles)) {
      amadeus::download_unzip(
        file_name = all_destfiles[d],
        directory_to_unzip = directory_to_unzip,
        unzip = unzip
      )
    }

    amadeus::download_remove_zips(
      remove = remove_zip,
      download_name = all_destfiles
    )

    if (hash) {
      return(amadeus::download_hash(hash = TRUE, directory_to_unzip))
    }
    return(invisible(download_result))
  }
}
