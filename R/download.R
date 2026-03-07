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
#' @param ... Arguments passed to each download function.
#' @note
#' - All download function names are in \code{download_*} formats
#' @author Insang Song
#' @seealso
#' For details of each download function per dataset,
#' Please refer to:
#' * \code{\link{download_aqs}}: `"aqs"`, `"AQS"`
#' * \code{\link{download_ecoregion}}: `"ecoregions"`, `"ecoregion"`
#' * \code{\link{download_geos}}: `"geos"`
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
      "edgar"
    ),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    hash = FALSE,
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
      edgar = download_edgar
    )

    return <- tryCatch(
      {
        what_to_run(
          directory_to_save = directory_to_save,
          acknowledgement = acknowledgement,
          hash = hash,
          ...
        )
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
#' @param parameter_code integer(1). EPA pollutant parameter code.
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

    #### Check for valid URL
    if (!amadeus::check_url_status(download_urls[1])) {
      stop(paste0(
        "Invalid year returns HTTP code 404. ",
        "Check `year` parameter.\n"
      ))
    }

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
  max_tries = 20
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
      rate_limit = 2
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
  amadeus::check_for_null_parameters(mget(ls()))

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
#' meteorological and atmospheric collections from [NASA's Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).
#' @note Due to NASA data access policies, downloads require a valid NASA
#' Earthdata token for authentication. Use \code{setup_nasa_token()} for setup.
#' @param collection character(1). MERRA-2 data collection file name.
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
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_gmao_merra-inst1_2d_asm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst1_2d_int_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst1_2d_lfo_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_3d_asm_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_3d_aer_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_3d_asm_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_3d_chm_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_3d_gas_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst3_2d_gas_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst6_3d_ana_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-inst6_3d_ana_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-statD_2d_slv_Nx_m}{amadeus}
#'
#' \insertRef{data_gmao_merra-statD_2d_slv_Nx_d}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_adg_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_aer_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_chm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_csp_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_flx_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_int_Nx}{amadeus}
#'
#' \insertRef{pawson_merra-2_2020}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_lnd_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_ocn_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_rad_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg1_2d_slv_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Ne}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_trb_Ne}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_nav_Ne}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_cld_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_rad_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_tdt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_trb_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_udt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_odt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_qdt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_asm_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_cld_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_3d_rad_Nv}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavg3_2d_glc_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_2d_asm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_2d_int_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_2d_lfo_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_2d_gas_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_3d_asm_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-instM_3d_ana_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_adg_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_aer_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_chm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_csp_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_flx_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_int_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_lfo_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_lnd_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_ocn_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_rad_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_slv_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_2d_glc_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_cld_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_mst_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_rad_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_tdt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_trb_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_udt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_odt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgM_3d_qdt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-const_2d_asm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_2d_asm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_2d_int_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_2d_lfo_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_2d_gas_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_3d_asm_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-instU_3d_ana_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_adg_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_aer_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_chm_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_csp_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_flx_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_int_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_lfo_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_lnd_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_ocn_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_rad_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_slv_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_2d_glc_Nx}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_cld_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_mst_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_rad_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_tdt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_trb_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_udt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_odt_Np}{amadeus}
#'
#' \insertRef{data_gmao_merra-tavgU_3d_qdt_Np}{amadeus}
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
    "tavg3_2d_glc_Nx"
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
    return(invisible(list(urls = character(0),
                          destfiles = character(0),
                          n_files = 0L)))
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 4. Check and retrieve NASA token (BEFORE null check)
  nasa_earth_data_token <- amadeus::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )

  #### 5. Now check for null parameters - token is now set
  amadeus::check_for_null_parameters(mget(ls()))

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

  if (!all(collection %in% identifiers_df$collection_id)) {
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

          # Filter by date sequence
          list_urls_date_sequence <- list_urls_month[
            substr(basename(list_urls_month), 28, 35) %in% date_sequence
          ]
          list_xml_date_sequence <- list_xml_month[
            substr(basename(list_xml_month), 28, 35) %in% date_sequence
          ]

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
#' data from NOAA's North American Regional Reanalysis (NARR) model.
#' @note "Pressure levels" variables contain variable values at 29 atmospheric
#' levels, ranging from 1000 hPa to 100 hPa. All pressure levels data will be
#' downloaded for each variable.
#' @param variables character. Variable(s) name acronym. See the
#' \emph{Available NARR Variables} section below or
#' \href{https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf}{the NARR variable table}
#' for the full list.
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
#'   \item{\code{ulwrf.ntat}}{Upward longwave radiation flux at nominal top of atmosphere}
#'   \item{\code{ulwrf.sfc}}{Upward longwave radiation flux at surface}
#'   \item{\code{ustm}}{U-component of storm motion}
#'   \item{\code{uswrf.ntat}}{Upward shortwave radiation flux at nominal top of atmosphere}
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
  max_tries = 20
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
  if (amadeus::check_destfile(download_name)) {
    amadeus::download_run_method(
      urls = download_url,
      destfiles = download_name,
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = 2
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
#' @note gROADS data may require NASA EarthData authentication depending on
#' access method.
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
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20
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
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = 2
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
#' @author Mitchell Manware, Insang Song
#' @return invisible list with download results; or hash character if hash=TRUE
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2017gpwv4}{amadeus}
#' @examples
#' \dontrun{
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
  max_tries = 20
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
      token = NULL,
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = 2
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
  max_tries = 20
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
      rate_limit = 2
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
    "MOD13A3",
    "MYD13A3",
    "MOD06_L2",
    "MCD19A2",
    "VNP46A2"
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
  amadeus::check_for_null_parameters(mget(ls()))

  #### 6. Check product
  product <- match.arg(product)

  if (substr(date[1], 1, 4) != substr(date[2], 1, 4)) {
    if (product != "MOD06_L2") {
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

  #### 8. Check version
  if (is.null(version)) {
    stop("Please select a data version.\n")
  }

  #### 9. Warning for excessive query
  dt_date <- as.Date(date)
  if (diff(dt_date) > 31) {
    warning(
      "Date range is greater than 31 days. ",
      "The results may not include all dates in the range.",
      call. = FALSE
    )
  }

  #### 10. Version fix
  if (product == "MOD06_L2") {
    str_version <- "6.1"
  } else if (product == "VNP46A2") {
    str_version <- NULL
  } else {
    str_version <- version
  }

  #### 11. Query CMR
  message("Querying NASA CMR for available granules...\n")
  chr_extent <- paste(extent, collapse = ",")
  resp <-
    httr2::request(
      "https://cmr.earthdata.nasa.gov/search/granules.json"
    ) |>
    httr2::req_url_query(
      short_name = product,
      version = str_version,
      temporal = paste(date[1], date[2], sep = ","),
      bounding_box = chr_extent,
      page_size = 2000
    ) |>
    httr2::req_perform()
  granules <- resp |> httr2::resp_body_json()

  # Extract data URLs (HDF files only)
  urls <- sapply(granules$feed$entry, function(g) {
    links <- g$links
    # Filter for data links only (exclude metadata, browse images, etc.)
    data_links <- Filter(
      function(l) {
        grepl("data#", l$rel) && grepl("\\.hdf$", l$href, ignore.case = TRUE)
      },
      links
    )
    if (length(data_links) > 0) data_links[[1]]$href else NA
  })
  urls <- urls[!is.na(urls)]

  if (length(urls) == 0) {
    stop("No granules found for the specified query parameters.\n")
  }

  #### 12. Filter by date range
  list_available_d <- stringi::stri_extract(urls, regex = "A2[0-9]{6,6}")
  list_available_d <- unique(gsub("A", "", list_available_d))
  date_sequence <- list_available_d[!is.na(list_available_d)]
  date_sequence_i <- as.integer(date_sequence)

  date_start_i <- as.integer(strftime(date[1], "%Y%j"))
  date_end_i <- as.integer(strftime(date[2], "%Y%j"))
  date_range_julian <- seq(date_start_i, date_end_i)
  date_sequence_in <- (date_sequence_i %in% date_range_julian)

  message(sprintf(
    "Found %d / %d days of data in the queried date range.\n",
    sum(date_sequence_in),
    length(date_range_julian)
  ))

  date_sequence <- date_sequence[date_sequence_in]

  #### 13. Prepare download paths
  download_names <- basename(urls)
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
#' Inventory (TRI) Program.
#' @note TRI data does not require authentication.
#' @param year integer(1 or 2). Year or start/end years for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
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
#'   acknowledgement = TRUE
#' )
#' }
#' @export
download_tri <- function(
  year = c(2018L, 2022L),
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

  #### Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

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
  url_download <- paste0( # nolint: line_length_linter.
    "https://data.epa.gov/efservice/downloads/tri/",
    "mv_tri_basic_download/"
  )
  year_sequence <- seq(year[1], year[2], 1)
  download_urls <- sprintf(
    paste(url_download, "%.0f", "_US/csv", sep = ""),
    year_sequence
  )
  download_names <- sprintf(
    paste0(directory_to_save, "tri_raw_%.0f.csv"),
    year_sequence
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
#' @author Ranadeep Daw, Insang Song
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
  certificate_url = paste0( # nolint: line_length_linter.
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
  url_download_base <- "https://gaftp.epa.gov/air/nei/%d/data_summaries/"
  url_download_remain <-
    c("2017v1/2017neiApr_onroad_byregions.zip", "2020nei_onroad_byregion.zip")
  download_urls <-
    paste0(
      sprintf(url_download_base, year),
      url_download_remain
    )
  download_names_file <-
    c("2017neiApr_onroad_byregions.zip", "2020nei_onroad_byregion.zip")
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
#' Default is \code{FALSE}. Not working for this function since HUC data
#' is in 7z format.
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
    if (!isFALSE(unzip)) {
      warning(
        "Parameter 'unzip' is deprecated.",
        " HUC data is in 7z format and cannot be unzipped automatically.\n",
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
  download_names <- paste0(
    directory_to_download,
    "edgar_",
    temp_res,
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
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

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
      "https://services.nacse.org/prism/data/public",
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
    directory_to_save,
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

  #### Unzip downloaded zip files
  amadeus::download_unzip(
    file_name = download_names,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### Remove zip files (use file.remove directly; zip is in directory_to_save)
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
      archive::archive_extract(download_names[fn], exdir = dir_unzip[fn])
    }
  }
  # nocov end
  message("Requests were processed.\n")
  return(amadeus::download_hash(hash, directory_to_save))
}
# nolint end
