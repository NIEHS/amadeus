# download.R
#' Download raw data wrapper function
# nolint start
#' @description
#' The \code{download_data()} function accesses and downloads atmospheric, meteorological, and environmental data from various open-access data sources.
# nolint end
#' @param dataset_name character(1). Dataset to download.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
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
#' * \code{\link{download_sedac_groads}}: `"sedac_groads"`, `"groads"`
#' * \code{\link{download_sedac_population}}: `"sedac_population"`,
#'   `"population"`
#' * \code{\link{download_modis}}: `"modis"`, `"MODIS"`
#' * \code{\link{download_tri}}: `"tri"`, `"TRI"`
#' * \code{\link{download_nei}}: `"nei"`, `"NEI"`
#' * \code{\link{download_gridmet}}: `"gridMET"`, `"gridmet"`
#' * \code{\link{download_terraclimate}}: `"TerraClimate"`, `"terraclimate"`
#' * \code{\link{download_huc}}: `"huc"`
#' * \code{\link{download_cropscape}}: `"cropscape"`, `"cdl"`
#' * \code{\link{download_prism}}: `"prism"`
#' @return NULL
#' @examples
#' \dontrun{
#' download_data(
#'   dataset_name = "narr",
#'   variables = "weasd",
#'   year = c(2023, 2023),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_commands = TRUE
#' )
#' }
#' @export
download_data <-
  function(
    dataset_name = c("aqs", "ecoregion", "ecoregions",
                     "geos", "gmted", "koppen",
                     "koppengeiger", "merra2", "merra",
                     "modis", "narr", "nlcd", "noaa", "sedac_groads",
                     "sedac_population", "groads", "population",
                     "hms", "smoke", "tri", "nei",
                     "gridmet", "terraclimate", "huc", "cropscape", "cdl",
                     "prism"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    ...
  ) {

    dataset_name <- tolower(dataset_name)
    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    what_to_run <- switch(dataset_name,
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
      sedac_groads = download_sedac_groads,
      groads = download_sedac_groads,
      sedac_population = download_sedac_population,
      population = download_sedac_population,
      modis = download_modis,
      tri = download_tri,
      nei = download_nei,
      gridmet = download_gridmet,
      terraclimate = download_terraclimate,
      huc = download_huc,
      cropscape = download_cropscape,
      cdl = download_cropscape,
      prism = download_prism
    )

    tryCatch(
      {
        what_to_run(
          directory_to_save = directory_to_save,
          acknowledgement = acknowledgement,
          ...
        )
      },
      error = function(e) {
        print(e)
        print(args(what_to_run))
        stop(paste0("Please refer to the argument list and ",
                    "the error message above to rectify the error.\n"))
      }
    )
  }

# nolint start
#' Download air quality data
#' @description
#' The \code{download_aqs()} function accesses and downloads Air Quality System (AQS) data from the [U.S. Environmental Protection Agency's (EPA) Pre-Generated Data Files](https://aqs.epa.gov/aqsweb/airdata/download_files.html).
#' @param parameter_code integer(1). length of 5.
#'  EPA pollutant parameter code. For details, please refer to
#'  [AQS parameter codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html)
#' @param resolution_temporal character(1).
#'  Name of column containing POC values.
#'  Currently, no value other than `"daily"` works.
#' @param url_aqs_download character(1).
#'  URL to the AQS pre-generated datasets.
#' @param year character(2). length of 4 each. Start/end years for downloading data.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default \code{FALSE}.
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_usepa2023airdata}{amadeus}
#' @examples
#' \dontrun{
#' download_aqs(
#'   parameter_code = 88101,
#'   resolution_temporal = "daily",
#'   year = c(2022, 2023),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
# nolint end
#' @export
download_aqs <-
  function(
    parameter_code = 88101,
    resolution_temporal = "daily",
    year = c(2018, 2022),
    url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE
  ) {
    #### 1. check for data download acknowledgement
    download_permit(acknowledgement = acknowledgement)
    #### 2. check for null parameters
    check_for_null_parameters(mget(ls()))
    #### check years
    stopifnot(length(year) == 2)
    year <- year[order(year)]
    #### 3. directory setup
    directory_original <- download_sanitize_path(directory_to_save)
    directories <- download_setup_dir(directory_original, zip = TRUE)
    directory_to_download <- directories[1]
    directory_to_save <- directories[2]
    #### 4. define year sequence
    year_sequence <- seq(year[1], year[2], 1)
    #### 5. build URLs
    download_urls <- sprintf(
      paste(url_aqs_download,
        resolution_temporal,
        "_",
        parameter_code,
        "_%d.zip",
        sep = ""
      ),
      year_sequence
    )
    #### 6. check for valid URL
    if (!(check_url_status(download_urls[1]))) {
      stop(paste0(
        "Invalid year returns HTTP code 404. ",
        "Check `year` parameter.\n"
      ))
    }
    #### 5. build download file name
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
    #### 6. build download command
    download_commands <- paste0(
      "curl -s --url ",
      download_urls,
      " --output ",
      download_names,
      "\n"
    )
    #### filter commands to non-existing files
    download_commands <- download_commands[
      which(
        !file.exists(download_names)
      )
    ]
    #### 7. initiate "..._curl_commands.txt"
    commands_txt <- paste0(
      directory_original,
      "aqs_",
      parameter_code,
      "_",
      year[1], "_", year[2],
      "_",
      resolution_temporal,
      "_curl_commands.txt"
    )
    download_sink(commands_txt)
    #### 8. concatenate and print download commands to "..._curl_commands.txt"
    cat(download_commands)
    #### 9. finish "..._curl_commands.txt" file
    sink()
    #### 10. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 11. download data
    download_run(
      download = download,
      system_command = system_command
    )
    #### 12. unzip data
    sapply(
      download_names,
      download_unzip,
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
    download_remove_zips(
      remove = remove_zip,
      download_name = download_names
    )
    #### 13. remove command file
    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
  }



# nolint start
#' Download ecoregion data
#' @description
#' The \code{download_ecoregion()} function accesses and downloads United States Ecoregions data from the [U.S. Environmental Protection Agency's (EPA) Ecorgions](https://www.epa.gov/eco-research/ecoregions). Level 3 data, where all pieces of information in the higher levels are included, are downloaded.
# nolint end
#' @note
#' For EPA Data Commons certificate errors, follow the steps below:
#' 1. Click Lock icon in the address bar at https://gaftp.epa.gov
#' 2. Click Show Certificate
#' 3. Access Details
#' 4. Find URL with *.crt extension
#' Currently we bundle the pre-downloaded crt and its PEM (which is accepted
#' in wget command) file in ./inst/extdata. The instruction above is for
#' certificate updates in the future.
#' @param epa_certificate_path character(1). Path to the certificate file
#' for EPA DataCommons. Default is
#' 'extdata/cacert_gaftp_epa.pem' under the package installation path.
#' @param certificate_url character(1). URL to certificate file. See notes for
#' details.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from
#' \code{directory_to_download}. Default \code{FALSE}.
#' @author Insang Song
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom utils download.file
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_omernik2014ecoregions}{amadeus}
#' @examples
#' \dontrun{
#' download_ecoregion(
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_ecoregion <- function(
  epa_certificate_path =
    system.file("extdata/cacert_gaftp_epa.pem",
                package = "amadeus"),
  certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  #### 1. data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 5. define download URL
  download_epa_certificate(
    epa_certificate_path = epa_certificate_path,
    certificate_url = certificate_url
  )

  download_url <- paste0(
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/",
    "us_eco_l3_state_boundaries.zip"
  )
  #### 6. build download file name
  download_name <- file.path(
    directory_to_download,
    "us_eco_l3_state_boundaries.zip"
  )
  #### 7. build download command
  download_command <-
    paste0(
      "wget --ca-certificate=",
      epa_certificate_path,
      " ",
      download_url,
      " -O ",
      download_name,
      "\n"
    )
  #### 8. initiate "..._curl_commands.txt" file
  commands_txt <- paste0(
    directory_original,
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )
  #### 9. concatenate
  download_sink(commands_txt)
  if (!file.exists(download_name)) {
    #### 10. concatenate and print download commands to "..._wget_commands.txt"
    #### cat command only file does not already exist or
    #### if size does not match URL size
    cat(download_command)
  }
  #### 11. finish "...curl_commands.txt" file
  sink()
  #### 12. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 13. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 14. remove download command
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 15. unzip files
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  #### 16. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
}

# nolint start 
#' Download atmospheric composition data
#' @description
#' The \code{download_geos()} function accesses and downloads various
#' atmospheric composition collections from [NASA's Global Earth Observing System (GEOS) model](https://gmao.gsfc.nasa.gov/GEOS_systems/).
#' @param collection character(1). GEOS-CF data collection file name.
#' @param date character(2). length of 10 each. Start/end date for downloading data.
#' Format "YYYY-MM-DD" (ex. January 1, 2018 = `"2018-01-01"`).
#' @param directory_to_save character(1). Directory to save data.
#' Sub-directories will be created within \code{directory_to_save} for each
#' GEOS-CF collection.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL; netCDF (.nc4) files will be stored in a
#' collection-specific folder within \code{directory_to_save}.
#' @importFrom utils download.file
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{keller_description_2021}{amadeus}
#' @examples
#' \dontrun{
#' download_geos(
#'   collection = "aqc_tavg_1hr_g1440x721_v1",
#'   date = c("2024-01-01", "2024-01-05"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
# nolint start: cyclocomp
download_geos <- function(
    collection =
        c(
          "aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1",
          "met_tavg_1hr_g1440x721_x1", "xgc_tavg_1hr_g1440x721_x1",
          "chm_inst_1hr_g1440x721_p23", "met_inst_1hr_g1440x721_p23"
        ),
    date = c("2018-01-01", "2018-01-01"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check dates
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### 3. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. match collection
  collection <- match.arg(collection, several.ok = TRUE)
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### 7. define URL base
  base <- "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/"
  #### 8. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    "geos_",
    date[1],
    "_",
    date[2],
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 9. concatenate and print download commands to "..._wget_commands.txt"
  for (c in seq_along(collection)) {
    collection_loop <- collection[c]
    download_folder <- paste0(
      directory_to_save,
      collection_loop,
      "/"
    )
    if (!dir.exists(download_folder)) {
      dir.create(download_folder, recursive = TRUE)
    }
    for (d in seq_along(date_sequence)) {
      date <- date_sequence[d]
      year <- substr(date, 1, 4)
      month <- substr(date, 5, 6)
      day <- substr(date, 7, 8)
      time_sequence <- generate_time_sequence(collection_loop)
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
          date,
          "_",
          time_sequence[t],
          "z.nc4"
        )
        download_url <- paste0(
          download_url_base,
          download_name
        )
        if (t == 1) {
          if (!(check_url_status(download_url))) {
            sink()
            file.remove(commands_txt)
            stop(paste0(
              "Invalid date returns HTTP code 404. ",
              "Check `date` parameter.\n"
            ))
          }
        }
        download_folder_name <- paste0(
          download_folder,
          download_name
        )
        download_command <- paste0(
          "curl ",
          download_url,
          " -o ",
          download_folder_name,
          "\n"
        )
        if (!file.exists(download_folder_name)) {
          #### cat command only if file does not already exist
          cat(download_command)
        }
      }
    }
  }
  #### 9. finish "..._wget_commands.txt" file
  sink()
  #### 10. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 11. download data
  download_run(
    download = download,
    system_command = system_command
  )
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}
# nolint end: cyclocomp

# nolint start
#' Download elevation data
#' @description
#' The \code{download_gmted()} function accesses and downloads Global
#' Multi-resolution Terrain Elevation Data (GMTED2010) from
#' [U.S. Geological Survey and National Geospatial-Intelligence Agency](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010).
#' @param statistic character(1). Available statistics include `"Breakline Emphasis"`, `"Systematic Subsample"`, `"Median Statistic"`,
#' `"Minimum Statistic"`, `"Mean Statistic"`, `"Maximum Statistic"`, and
#' `"Standard Deviation Statistic"`.
#' @param resolution character(1). Available resolutions include `"7.5 arc-seconds"`, `"15 arc-seconds"`, and `"30 arc-seconds"`.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands. Default is FALSE.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default is \code{FALSE}.
#' @author Mitchell Manware, Insang Song
# nolint end
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{danielson_global_2011}{amadeus}
#' @examples
#' \dontrun{
#' download_gmted(
#'   statistic = "Breakline Emphasis",
#'   resolution = "7.5 arc-seconds",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_gmted <- function(
  statistic = c(
    "Breakline Emphasis", "Systematic Subsample",
    "Median Statistic", "Minimum Statistic",
    "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"
  ),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. check for valid statistic
  statistic <- match.arg(statistic)
  #### 5. check for valid resolution
  resolution <- match.arg(resolution)
  #### 6. define URL base
  base <- paste0(
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo",
    "/downloads/GMTED/Grid_ZipFiles/"
  )
  #### 7. define URL statistic code
  statistic_code <- process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  #### 8. define URL resolution code
  resolution_code <- process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  #### 9. build url
  download_url <- paste0(
    base,
    statistic_code,
    resolution_code,
    "_grd.zip"
  )
  #### 10. build download file name
  download_name <- paste0(
    directory_to_download,
    "gmted2010_",
    statistic_code,
    resolution_code,
    "_grd.zip"
  )
  #### 11. build download command
  download_command <- paste0(
    "curl -s -o ",
    download_name,
    " --url ",
    download_url,
    "\n"
  )
  #### 12. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "gmted_",
    gsub(" ", "", statistic),
    "_",
    gsub(" ", "", resolution),
    "_",
    Sys.Date(),
    "_curl_command.txt"
  )
  download_sink(commands_txt)
  #### 13. concatenate and print download command to "..._curl_commands.txt"
  if (!file.exists(download_name)) {
    #### cat command only if file does not already exist
    cat(download_command)
  }
  #### 14. finish "..._curl_commands.txt" file
  sink()
  #### 15. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 16. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 17. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
  #### 18. end if unzip == FALSE
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  #### 19. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
}

# nolint start
#' Download meteorological and atmospheric data
#' @description
#' The \code{download_merra2()} function accesses and downloads various
#' meteorological and atmospheric collections from [NASA's Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).
#' @param collection character(1). MERRA-2 data collection file name.
#' @param date character(2). length of 10 each. Start/end date for downloading data.
#' Format "YYYY-MM-DD" (ex. January 1, 2018 = `"2018-01-01"`).
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL; netCDF (.nc4) files will be stored in a
#' collection-specific folder within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_gmao_merra-inst1_2d_asm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-inst1_2d_int_Nx}{amadeus}
#' \insertRef{data_gmao_merra-inst1_2d_lfo_Nx}{amadeus}
#' \insertRef{data_gmao_merra-inst3_3d_asm_Np}{amadeus}
#' \insertRef{data_gmao_merra-inst3_3d_aer_Nv}{amadeus}
#' \insertRef{data_gmao_merra-inst3_3d_asm_Nv}{amadeus}
#' \insertRef{data_gmao_merra-inst3_3d_chm_Nv}{amadeus}
#' \insertRef{data_gmao_merra-inst3_3d_gas_Nv}{amadeus}
#' \insertRef{data_gmao_merra-inst3_2d_gas_Nx}{amadeus}
#' \insertRef{data_gmao_merra-inst6_3d_ana_Np}{amadeus}
#' \insertRef{data_gmao_merra-inst6_3d_ana_Nv}{amadeus}
#' \insertRef{data_gmao_merra-statD_2d_slv_Nx_m}{amadeus}
#' \insertRef{data_gmao_merra-statD_2d_slv_Nx_d}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_adg_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_aer_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_chm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_csp_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_flx_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_int_Nx}{amadeus}
#' \insertRef{pawson_merra-2_2020}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_lnd_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_ocn_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_rad_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg1_2d_slv_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Ne}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_trb_Ne}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_nav_Ne}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_cld_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_rad_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_tdt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_trb_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_udt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_odt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_qdt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_asm_Nv}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_cld_Nv}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_mst_Nv}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_3d_rad_Nv}{amadeus}
#' \insertRef{data_gmao_merra-tavg3_2d_glc_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instM_2d_asm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instM_2d_int_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instM_2d_lfo_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instM_2d_gas_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instM_3d_asm_Np}{amadeus}
#' \insertRef{data_gmao_merra-instM_3d_ana_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_adg_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_aer_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_chm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_csp_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_flx_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_int_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_lfo_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_lnd_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_ocn_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_rad_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_slv_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_2d_glc_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_cld_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_mst_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_rad_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_tdt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_trb_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_udt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_odt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgM_3d_qdt_Np}{amadeus}
#' \insertRef{data_gmao_merra-const_2d_asm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instU_2d_asm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instU_2d_int_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instU_2d_lfo_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instU_2d_gas_Nx}{amadeus}
#' \insertRef{data_gmao_merra-instU_3d_asm_Np}{amadeus}
#' \insertRef{data_gmao_merra-instU_3d_ana_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_adg_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_aer_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_chm_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_csp_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_flx_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_int_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_lfo_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_lnd_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_ocn_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_rad_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_slv_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_2d_glc_Nx}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_cld_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_mst_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_rad_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_tdt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_trb_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_udt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_odt_Np}{amadeus}
#' \insertRef{data_gmao_merra-tavgU_3d_qdt_Np}{amadeus}
#' @examples
#' \dontrun{
#' download_merra2(
#'   collection = "inst1_2d_int_Nx",
#'   date = c("2024-01-01", "2024-01-05"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
# nolint start: cyclocomp
download_merra2 <- function(
    collection = c(
      "inst1_2d_asm_Nx", "inst1_2d_int_Nx", "inst1_2d_lfo_Nx",
      "inst3_3d_asm_Np", "inst3_3d_aer_Nv", "inst3_3d_asm_Nv",
      "inst3_3d_chm_Nv", "inst3_3d_gas_Nv", "inst3_2d_gas_Nx",
      "inst6_3d_ana_Np", "inst6_3d_ana_Nv", "statD_2d_slv_Nx",
      "tavg1_2d_adg_Nx", "tavg1_2d_aer_Nx", "tavg1_2d_chm_Nx",
      "tavg1_2d_csp_Nx", "tavg1_2d_flx_Nx", "tavg1_2d_int_Nx",
      "tavg1_2d_lfo_Nx", "tavg1_2d_lnd_Nx", "tavg1_2d_ocn_Nx",
      "tavg1_2d_rad_Nx", "tavg1_2d_slv_Nx", "tavg3_3d_mst_Ne",
      "tavg3_3d_trb_Ne", "tavg3_3d_nav_Ne", "tavg3_3d_cld_Np",
      "tavg3_3d_mst_Np", "tavg3_3d_rad_Np", "tavg3_3d_tdt_Np",
      "tavg3_3d_trb_Np", "tavg3_3d_udt_Np", "tavg3_3d_odt_Np",
      "tavg3_3d_qdt_Np", "tavg3_3d_asm_Nv", "tavg3_3d_cld_Nv",
      "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx"
    ),
    date = c("2018-01-01", "2018-01-01"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### check dates
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check if collection is recognized
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
  colnames(identifiers_df) <- c("collection_id", "estd_name", "DOI")
  if (!all(collection %in% identifiers_df$collection_id)) {
    print(identifiers_df)
    stop(paste0("Requested collection is not recognized.\n
    Please refer to the table above to find a proper collection.\n"))
  }
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))
  #### initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    "merra2_",
    date[1],
    "_",
    date[2],
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  for (c in seq_along(collection)) {
    collection_loop <- collection[c]
    #### define ESDT name and DOI
    identifiers_df_requested <- subset(
      identifiers_df,
      subset = identifiers_df$collection_id == collection_loop
    )
    esdt_name <- identifiers_df_requested[, 2]
    #### define URL base
    #### NOTE: sorted and defined manually according to
    ####       https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/ \&
    ####       https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/
    esdt_name_4 <- c(
      "M2I1NXASM", "M2I1NXINT", "M2I1NXLFO", "M2I3NXGAS",
      "M2SDNXSLV", "M2T1NXADG", "M2T1NXAER", "M2T1NXCHM",
      "M2T1NXCSP", "M2T1NXFLX", "M2T1NXINT", "M2T1NXLFO",
      "M2T1NXLND", "M2T1NXOCN", "M2T1NXRAD", "M2T1NXSLV",
      "M2T3NXGLC"
    )
    esdt_name_5 <- c(
      "M2I3NPASM", "M2I3NVAER", "M2I3NVASM", "M2I3NVCHM",
      "M2I3NVGAS", "M2I6NPANA", "M2I6NVANA", "M2T3NEMST",
      "M2T3NENAV", "M2T3NETRB", "M2T3NPCLD", "M2T3NPMST",
      "M2T3NPODT", "M2T3NPQDT", "M2T3NPRAD", "M2T3NPTDT",
      "M2T3NPTRB", "M2T3NPUDT", "M2T3NVASM", "M2T3NVCLD",
      "M2T3NVMST", "M2T3NVRAD"
    )
    if (esdt_name %in% esdt_name_4) {
      base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    } else if (esdt_name %in% esdt_name_5) {
      base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    }
    #### identify download URLs
    list_urls <- NULL
    for (y in seq_along(yearmonth_sequence)) {
      year <- substr(yearmonth_sequence[y], 1, 4)
      month <- substr(yearmonth_sequence[y], 5, 6)
      if (y == 1) {
        base_url <- paste0(
          base,
          esdt_name,
          ".5.12.4/",
          year,
          "/",
          month,
          "/"
        )
        if (!(check_url_status(base_url))) {
          stop(paste0(
            "Invalid date returns HTTP code 404. ",
            "Check `date` parameter.\n"
          ))
        }
      }
      list_urls_month <- system(
        paste0(
          "wget -q -nH -nd ",
          "\"",
          base,
          esdt_name,
          ".5.12.4/",
          year,
          "/",
          month,
          "/\"",
          " -O - | grep .nc4 | awk -F'\"' ",
          "'{print $4}'"
        ),
        intern = TRUE
      )
      list_urls <- c(list_urls, list_urls_month)
    }
    #### match list_urls to date sequence
    list_urls_date_sequence <- list_urls[substr(list_urls, 28, 35) %in%
                                           date_sequence]
    #### separate data and metadata
    list_urls_data <- list_urls_date_sequence[grep(
      "*.xml",
      list_urls_date_sequence,
      invert = TRUE
    )]
    list_urls_metadata <- list_urls_date_sequence[grep(
      "*.xml",
      list_urls_date_sequence,
      invert = FALSE
    )]
    #### concatenate and print download commands to "..._wget_commands.txt"
    for (l in seq_along(date_sequence)) {
      year <- as.character(substr(date_sequence[l], 1, 4))
      month <- as.character(substr(date_sequence[l], 5, 6))
      download_url <- paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/",
        list_urls_data[l]
      )
      download_folder <- paste0(
        directory_to_save,
        collection_loop
      )
      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }
      download_name <- paste0(
        download_folder,
        "/",
        list_urls_data[l]
      )
      download_command <- paste0(
        "wget ",
        download_url,
        " -O ",
        download_name,
        "\n"
      )
      if (!file.exists(download_name)) {
        #### cat command only if file does not already exist
        cat(download_command)
      }
      download_url_metadata <- paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/",
        list_urls_metadata[l]
      )
      download_folder_metadata <- paste0(
        directory_to_save,
        collection_loop,
        "/metadata/"
      )
      if (!dir.exists(download_folder_metadata)) {
        dir.create(download_folder_metadata, recursive = TRUE)
      }
      download_name_metadata <- paste0(
        download_folder_metadata,
        list_urls_metadata[l]
      )
      download_command_metadata <- paste0(
        "wget ",
        download_url_metadata,
        " -O ",
        download_name_metadata,
        "\n"
      )
      if (!file.exists(download_name_metadata)) {
        #### cat command only if file does not already exist
        cat(download_command_metadata)
      }
    }
  }
  #### finish "..._wget_commands.txt"
  sink()
  #### build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}
# nolint end: cyclocomp

# nolint start
#' Download meteorological data
#' @description
#' The \code{download_narr} function accesses and downloads daily meteorological data from [NOAA's North American Regional Reanalysis (NARR) model](https://psl.noaa.gov/data/gridded/data.narr.html).
#' @note "Pressure levels" variables contain variable values at 29 atmospheric levels, ranging from 1000 hPa to 100 hPa. All pressure levels data will be downloaded for each variable.
#' @param variables character. Variable(s) name acronym. See [List of Variables in NARR Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
#' for variable names and acronym codes.
#' @param year character(2). length of 4 each. Start/end years for downloading data.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL; netCDF (.nc) files will be stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{mesinger_north_2006}{amadeus}
#' @examples
#' \dontrun{
#' download_narr(
#'   variables = c("weasd", "omega"),
#'   year = c(2022, 2023),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
# nolint start: cyclocomp
download_narr <- function(
    variables = NULL,
    year = c(2018, 2022),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check years
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  #### 3. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. define years and months sequence
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
  #### 5. define variables
  variables_list <- as.vector(variables)
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "narr_",
    year[1], "_", year[2],
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 8. concatenate and print download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    # implement variable sorting function
    base <- narr_variable(variable)[[1]]
    months <- narr_variable(variable)[[2]]
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
        command <- paste0(
          "curl -s -o ",
          destfile,
          " --url ",
          url,
          "\n"
        )
        if (!file.exists(destfile)) {
          #### cat command if file does not already exist or if local file size
          #### and the HTTP length (url file size) do not match
          cat(command)
        }
      }
    }
  }
  #### 9. finish "..._curl_commands.txt"
  sink()
  #### 10. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 11. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 12. remove command text file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}
# nolint end: cyclocomp

# nolint start
#' Download land cover data
#' @description
#' The \code{download_nlcd()} function accesses and downloads
#' land cover data from the
#' [Multi-Resolution Land Characteristics (MRLC) Consortium's National Land Cover Database (NLCD) products data base](https://www.mrlc.gov/data).
# nolint end
#' @param collection character(1). `"Coterminous United States"` or `"Alaska"`.
#' @param year integer(1). Available years for Coterminous United States
#' include `2001`, `2004`, `2006`, `2008`, `2011`, `2013`, `2016`,
#' `2019`, and `2021`.
#' Available years for Alaska include `2001`, `2011`, and `2016`.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped shapefiles ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default is \code{FALSE}.
#' @author Mitchell Manware, Insang Song
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{dewitz_national_2023}{amadeus}
#' \insertRef{dewitz_national_2024}{amadeus}
#' @examples
#' \dontrun{
#' download_nlcd(
#'   collection = "Coterminous United States",
#'   year = 2021,
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_nlcd <- function(
  collection = "Coterminous United States",
  year = 2021,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. check for valid years
  valid_years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021)
  if (!(year %in% valid_years)) {
    stop(paste0("Requested year is not recognized.\n"))
  }
  #### 5. define URL base
  base <- "https://s3-us-west-2.amazonaws.com/mrlc/"
  #### 6. define collection code
  if (collection == "Coterminous United States") {
    collection_code <- paste0(
      "nlcd_",
      as.character(year),
      "_land_cover_l48_"
    )
  } else if (collection == "Alaska") {
    collection_code <- paste0(
      "NLCD_",
      as.character(year),
      "_Land_Cover_AK_"
    )
  }
  #### 7. define release date
  #### NOTE: release dates identified by inspecting URLs on from
  ####       https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover
  if (year == 2021 && collection == "Coterminous United States") {
    release_date <- "20230630"
  } else if (!(year == 2021) && collection == "Coterminous United States") {
    release_date <- "20210604"
  } else if (collection == "Alaska") {
    release_date <- "20200724"
  }
  #### 8. build URL
  download_url <- paste0(
    base,
    collection_code,
    release_date,
    ".zip"
  )
  #### 9. build download file name
  download_name <- paste0(
    directory_to_download,
    tolower(collection_code),
    release_date,
    ".zip"
  )
  #### 10. build system command
  download_command <- paste0(
    "curl -o ",
    download_name,
    " --url ",
    download_url,
    "\n"
  )
  #### 11. initiate "..._curl_command.txt"
  commands_txt <- paste0(
    directory_original,
    tolower(collection_code),
    Sys.Date(),
    "_curl_command.txt"
  )
  download_sink(commands_txt)
  #### 12. concatenate and print download command to "..._curl_commands.txt"
  if (!file.exists(download_name)) {
    #### cat command only if file does not already exist
    cat(download_command)
  }
  #### 13. finish "..._curl_command.txt"
  sink()
  #### 14. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 15. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 16. end if unzip == FALSE
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  #### 17. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
  #### 18. remove command text
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}

# nolint start
#' Download roads data
#' @description
#' The \code{download_sedac_groads()} function accesses and downloads
#' roads data from [NASA's Global Roads Open Access Data Set (gROADS), v1 (1980-2010)](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/data-download).
#' @param data_region character(1). Data can be downloaded for `"Global"`,
#' `"Africa"`, `"Asia"`, `"Europe"`, `"Americas"`, `"Oceania East"`, and `"Oceania West"`.
#' @param data_format character(1). Data can be downloaded as `"Shapefile"` or
#' `"Geodatabase"`. (Only `"Geodatabase"` available for `"Global"` region).
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped shapefiles ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default is \code{FALSE}.
#' @author Mitchell Manware, Insang Song
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2013groads}{amadeus}
#' @examples
#' \dontrun{
#' download_sedac_groads(
#'   data_region = "Americas",
#'   data_format = "Shapefile",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_sedac_groads <- function(
    data_region = c("Americas", "Global", "Africa", "Asia", "Europe", "Oceania East", "Oceania West"),
    data_format = c("Shapefile", "Geodatabase"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE
    ) {
  # nolint end
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. check if region is valid
  data_format <- match.arg(data_format)
  data_region <- match.arg(data_region)
  #### 5. define URL base
  base <- paste0(
    "https://sedac.ciesin.columbia.edu/downloads/data/groads/",
    "groads-global-roads-open-access-v1/",
    "groads-v1-"
  )
  #### 6. define data format
  if (data_format == "Shapefile" && data_region == "Global") {
    message("Geodatabase format utilized for 'Global' dataset.\n")
    format <- "gdb"
  } else if (data_format == "Shapefile" && !(data_region == "Global")) {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gdb"
  }
  #### 7. coerce region to lower case
  region <- tolower(data_region)
  #### 8. build download URL
  download_url <- paste0(
    base,
    gsub(" ", "-", region),
    "-",
    format,
    ".zip"
  )
  #### 9. build download file name
  download_name <- paste0(
    directory_to_download,
    "groads_v1_",
    gsub(" ", "-", region),
    "_",
    format,
    ".zip"
  )
  #### 10. build system command
  download_command <- paste0(
    "curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
    " -o ",
    download_name,
    " --url ",
    download_url,
    "\n"
  )
  #### 11. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "sedac_groads_",
    gsub(" ", "_", region),
    "_",
    Sys.Date(),
    "_curl_command.txt"
  )
  download_sink(commands_txt)
  if (!file.exists(download_name)) {
    #### 12. concatenate and print download command to "..._curl_commands.txt"
    #### cat command if file does not already exist or is incomplete
    cat(download_command)
  }
  #### 13. finish "..._curl_commands.txt" file
  sink()
  #### 14. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 15. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 16. end if unzip == FALSE
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  #### 17. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
  #### 18. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
}

# nolint start
#' Download population density data
#' @description
#' The \code{download_sedac_population()} function accesses and downloads
#' population density data from [NASA's UN WPP-Adjusted Population Density, v4.11](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11).
#' @param data_resolution character(1). Available resolutions are 30 second
#' (approx. 1 km), 2.5 minute (approx. 5 km), 15 minute (approx. 30 km),
#' 30 minute (approx. 55 km), and 60 minute (approx. 110 km).
#' @param data_format character(1). Individual year data can be downloaded as
#' `"ASCII"` or `"GeoTIFF"`. "all" years is downloaded as `"netCDF"`.
#' @param year character(1). Available years are `2000`, `2005`, `2010`, `2015`, and
#' `2020`, or `"all"` for all years.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped shapefiles ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default is \code{FALSE}.
#' @author Mitchell Manware, Insang Song
# nolint end
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_ciesin2017gpwv4}{amadeus}
#' @examples
#' \dontrun{
#' download_sedac_population(
#'   data_resolution = "30 second",
#'   data_format = "GeoTIFF",
#'   year = "2020",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_sedac_population <- function(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/")
  #### 5. define year
  year <- ifelse(year == "all", "totpop", as.character(year))
  #### 6. define data resolution
  resolution <- process_sedac_codes(data_resolution)
  #### 7. 30 second resolution not available for all years
  if (year == "totpop" && resolution == "30_sec") {
    resolution <- "2pt5_min"
    message(paste0(
      "30 second resolution not available for all years. Returning",
      " highest (2.5 minute) resolution.\n"
    ))
  }

  data_format <- match.arg(data_format)
  #### 8. define data format
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
  }
  if (data_format == "ASCII") {
    if (year != "totpop") {
      format <- "asc"
    } else {
      format <- "nc"
      message(paste0(
        "Data for all years is only available in netCDF format. ",
        "Data will be downloaded as netCDF.\n"
      ))
    }
  }
  if (data_format == "netCDF") {
    format <- "nc"
  }
  #### 9. build download URL
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
  #### 10. build download file name
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
  #### 11. build system command
  download_command <- paste0(
    "curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
    " -o ",
    download_name,
    " --url ",
    download_url,
    "\n"
  )
  #### 12. initiate "..._curl_command.txt"
  commands_txt <- paste0(
    directory_original,
    "sedac_population_",
    year,
    "_",
    resolution,
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  if (!file.exists(download_name)) {
    #### 13. concatenate and print download command to "..._curl_commands.txt"
    #### cat command if file does not already exist or is incomplete
    cat(download_command)
  }
  #### 14. finish "..._curl_commands.txt" file
  sink()
  #### 15. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 16. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 17. end if unzip == FALSE
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )
  #### 18. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
  #### 19. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
}

# nolint start
#' Download wildfire smoke data
#' @description
#' The \code{download_hms()} function accesses and downloads
#' wildfire smoke plume coverage data from [NOAA's Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/products/land/hms.html#0).
#' @param data_format character(1). "Shapefile" or "KML".
#' @param date character(2). length of 10 each. Start/end date for downloading data.
# nolint end
#' @param directory_to_save character(1). Directory to save data. If
#' `data_format = "Shapefile"`, two sub-directories will be created for the
#' downloaded zip files ("/zip_files") and the unzipped shapefiles
#' ("/data_files"). If `data_format = "KML"`, a single sub-directory
#' ("/data_files") will be created.
#' @param acknowledgement logical(1).
#' By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}. (Ignored
#' if \code{data_format = "KML"}.)
#' @param remove_zip logical(1). Remove zip files from
#' directory_to_download. Default is \code{FALSE}.
#' (Ignored if \code{data_format = "KML"}.)
#' @importFrom utils head
#' @importFrom utils tail
#' @author Mitchell Manware, Insang Song
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_HMSabout}{amadeus}
#' @examples
#' \dontrun{
#' download_hms(
#'   data_format = "Shapefile",
#'   date = c("2024-01-01", "2024-01-05"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint start: cyclocomp
download_hms <- function(
    data_format = "Shapefile",
    date = c("2018-01-01", "2018-01-01"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check dates
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. check for unzip == FALSE && remove_zip == TRUE
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    ))
  }
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### 6. define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "hms_smoke_",
    utils::head(date_sequence, n = 1),
    "_",
    utils::tail(date_sequence, n = 1),
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 8. concatenate and print download commands to "..._curl_commands.txt"
  download_names <- NULL
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
    if (f == 1) {
      if (!(check_url_status(url))) {
        sink()
        file.remove(commands_txt)
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
    download_names <- c(download_names, destfile)
    command <- paste0(
      "curl -s -o ",
      destfile,
      " --url ",
      url,
      "\n"
    )
    if (!file.exists(destfile)) {
      #### cat command only if file does not already exist
      cat(command)
    }
  }
  #### 9. finish "..._curl_commands.txt"
  sink()
  #### 10. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 11. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 12. remove command file
  download_remove_command(
    remove = remove_command,
    commands_txt = commands_txt
  )
  #### 13. end if data_format == "KML"
  if (data_format == "KML") {
    unlink(directory_to_download, recursive = TRUE)
    message(paste0("KML files cannot be unzipped.\n"))
    return(TRUE)
  }
  #### 14. unzip downloaded zip files
  for (d in seq_along(download_names)) {
    download_unzip(
      file_name = download_names[d],
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
  }
  #### 15. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_names
  )
}
# nolint end: cyclocomp

# nolint start
#' Download climate classification data
#' @description
#' The \code{download_koppen_geiger()} function accesses and downloads
#' climate classification data from the \emph{Present and future
#' Köppen-Geiger climate classification maps at
#' 1-km resolution}([link for article](https://www.nature.com/articles/sdata2018214); [link for data](https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2)).
#' @param data_resolution character(1). Available resolutions are `"0.0083"`
#' degrees (approx. 1 km), `"0.083"` degrees (approx. 10 km), and
#' `"0.5"` degrees (approx. 50 km).
#' @param time_period character(1). Available times are `"Present"` (1980-2016)
#' and `"Future"` (2071-2100). ("Future" classifications are based on scenario
#' RCP8.5).
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped shapefiles ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip zip files. Default is \code{TRUE}.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default is \code{FALSE}.
#' @author Mitchell Manware, Insang Song
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
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
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
# nolint end
#' @export
download_koppen_geiger <- function(
    data_resolution = c("0.0083", "0.083", "0.5"),
    time_period = c("Present", "Future"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]
  #### 4. check for data resolution
  data_resolution <- match.arg(data_resolution)
  #### 5. check for valid time period
  time_period <- match.arg(time_period)
  #### 6. define time period
  period <- tolower(time_period)
  #### 7. define data resolution
  data_resolution <- gsub("\\.", "p", data_resolution)
  #### 8 define download URL
  download_url <- paste0(
    "https://s3-eu-west-1.amazonaws.com/",
    "pfigshare-u-files/12407516/Beck_KG_V1.zip"
  )
  #### 9 build download file name
  download_name <- paste0(
    directory_to_download,
    "koppen_geiger_",
    period,
    "_",
    data_resolution,
    ".zip"
  )
  #### 10. build download command
  download_command <- paste0(
    "wget ",
    download_url,
    " -O ",
    download_name,
    "\n"
  )
  #### 11. initiate "..._wget_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "koppen_geiger_",
    time_period,
    "_",
    data_resolution,
    "_",
    Sys.Date(),
    "_wget_command.txt"
  )
  download_sink(commands_txt)
  if (!file.exists(download_name)) {
    #### 12. concatenate and print download command to "..._wget_commands.txt"
    #### cat command if file does not already exist or is incomplete
    cat(download_command)
  }
  sink()
  #### 14. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 15. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 17. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
  #### 18. end if unzip == FALSE
  download_unzip(
    file_name = download_name,
    directory_to_unzip = directory_to_save,
    unzip = unzip
  )

  #### 19. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_name
  )
}



#' Download MODIS product files
# nolint start
#' @description Need maintenance for the directory path change
#' in NASA EOSDIS. This function first retrieves the all hdf download links
#' on a certain day, then only selects the relevant tiles from the retrieved
#' links. Download is only done at the queried horizontal-vertical tile number
#' combinations. An exception is MOD06_L2 product, which is produced
#' every five minutes every day.
#' @note Both dates in \code{date} should be in the same year.
#'  Directory structure looks like
#'  input/modis/raw/\{version\}/\{product\}/\{year\}/\{day_of_year\}.
#' @param product character(1).
#' One of `c("MOD09GA", "MOD11A1", "MOD06_L2", "MCD19A2", "MOD13A2", "VNP46A2")`
#' @param version character(1). Default is `"61"`, meaning v061.
#' @param horizontal_tiles integer(2). Horizontal tile numbers
#' `c({start}, {end})`. Default is `c(7, 13)`.
#' @param vertical_tiles integer(2). Vertical tile numbers
#' `c({start}, {end})`. Default is `c(3, 6)`.
#' @param nasa_earth_data_token character(1).
#'  Token for downloading data from NASA. Should be set before
#'  trying running the function.
#' @param mod06_links character(1). CSV file path to MOD06_L2 download links
#' from NASA LPDAAC. Default is `NULL`.
#' @param date character(2). length of 10 each. Start/end date for downloading data.
#' Format "YYYY-MM-DD" (ex. January 1, 2018 = `"2018-01-01"`). Note: ignored if
#' \code{product == "MOD06_L2"}.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). Download data or only save wget commands.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @import rvest
#' @return NULL; HDF (.hdf) files will be stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_mcd19a22021}{amadeus}
#' \insertRef{data_mod06l2_2017}{amadeus}
#' \insertRef{data_mod09ga2021}{amadeus}
#' \insertRef{data_mod11a12021}{amadeus}
#' \insertRef{data_mod13a22021}{amadeus}
#' \insertRef{article_roman2018vnp46}{amadeus}
# nolint end
#' @examples
#' \dontrun{
#' # example with MOD0GA product
#' download_modis(
#'   product = "MOD09GA",
#'   version = "61",
#'   horizontal_tiles = c(8, 10),
#'   vertical_tiles = c(4, 5),
#'   date = c("2024-01-01", "2024-01-10"),
#'   nasa_earth_data_token = readLines("~/pre_generated_token.txt"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' # example with MOD06_L2 product
#' download_modis(
#'   product = "MOD06_L2",
#'   version = "61",
#'   horizontal_tiles = c(8, 10),
#'   vertical_tiles = c(4, 5),
#'   mod06_links = "~/LAADS_query.2024-07-15T12_17.csv",
#'   date = c("2024-01-01", "2024-01-10"),
#'   nasa_earth_data_token = readLines("~/pre_generated_token.txt"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' # example with VNP46A2 product
#' download_modis(
#'   product = "VNP46A2",
#'   version = "61",
#'   horizontal_tiles = c(8, 10),
#'   vertical_tiles = c(4, 5),
#'   date = c("2024-01-01", "2024-01-10"),
#'   nasa_earth_data_token = readLines("~/pre_generated_token.txt"),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_modis <- function(
    product = c(
      "MOD09GA", "MOD11A1", "MOD06_L2",
      "MCD19A2", "MOD13A2", "VNP46A2"
    ),
    version = "61",
    horizontal_tiles = c(7, 13),
    vertical_tiles = c(3, 6),
    mod06_links = NULL,
    nasa_earth_data_token = NULL,
    date = c("2023-09-01", "2023-09-01"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### check dates
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]

  #### 3. check for NASA earth data token
  if (is.null(nasa_earth_data_token)) {
    stop("Please provide NASA EarthData Login token.\n")
  }
  #### 4. check for product
  product <- match.arg(product)

  if (substr(date[1], 1, 4) != substr(date[2], 1, 4)) {
    if (product != "MOD06_L2") {
      stop("dates should be in the same year.\n")
    }
  }

  #### 5. check for version
  if (is.null(version)) {
    stop("Please select a data version.\n")
  }

  #### 6. check for valid horizontal tiles
  if (!all(horizontal_tiles %in% seq(0, 35))) {
    stop("Horizontal tiles are not in the proper range [0, 35].\n")
  }
  if (!all(vertical_tiles %in% seq(0, 17))) {
    stop("Vertical tiles are not in the proper range [0, 17].\n")
  }

  #### 7. define horizontal and vertical tiles
  tiles_horizontal <-
    seq(
      horizontal_tiles[1],
      horizontal_tiles[2]
    )
  tiles_horizontal <-
    sprintf("h%02d", tiles_horizontal)

  tiles_vertical <-
    seq(
      vertical_tiles[1],
      vertical_tiles[2]
    )
  tiles_vertical <-
    sprintf("v%02d", tiles_vertical)

  #### 8. define requested tiles
  tiles_df <- expand.grid(
    h = tiles_horizontal,
    v = tiles_vertical
  )
  tiles_requested <-
    paste0(tiles_df$h, tiles_df$v)

  #### 9. Reuse ladsweb home url
  ladsurl <- "https://ladsweb.modaps.eosdis.nasa.gov/"
  version <- ifelse(startsWith(product, "VNP"), "5000", version)

  #### 10. MOD06_L2 manual input
  if (product == "MOD06_L2") {
    mod06l2_url1 <-
      "https://ladsweb.modaps.eosdis.nasa.gov/"
    mod06l2_url2 <-
      "search/order/4/MOD06_L2--61/"
    mod06l2_url3 <-
      "%s..%s/DNB/-130,52,-60,20"
    mod06l2_url_template <-
      paste0(mod06l2_url1, mod06l2_url2, mod06l2_url3)
    mod06l2_full <-
      sprintf(mod06l2_url_template, date[1], date[2])

    if (is.null(mod06_links)) {
      stop(paste(
        "Please provide a CSV file path to MOD06_L2 download links.
                 You may download it from the link:\n", mod06l2_full,
        "\nTime length up to one month is recommended.\n"
      ))
    }

    #### 10-1. Parse urls in csv
    file_url <- read.csv(mod06_links)
    file_url <- unlist(file_url[, 2])
    download_url <-
      paste0(
        substr(ladsurl, 1, nchar(ladsurl) - 1),
        file_url
      )

    #### 10-2. Parse dates from csv
    file_dates <-
      regmatches(
        file_url,
        regexpr("[2][0-2][0-9]{2,2}[0-3][0-9]{2,2}", file_url)
      )
    file_dates <- as.integer(file_dates)
    date_start <- as.Date(as.character(min(file_dates)), format = "%Y%j")
    date_end <- as.Date(as.character(max(file_dates)), format = "%Y%j")

    # Extract year and month from file_dates
    splitter <- paste0(
      substr(file_dates, 1, 4), "/", substr(file_dates, 5, 7), "/"
    )
    # Extract download names from file_url using splitter
    download_name <- sapply(strsplit(file_url, splitter), `[`, 2)

    #### 10-3. initiate "..._wget_commands.txt" file
    commands_txt <- paste0(
      directory_to_save,
      product,
      "_",
      date_start,
      "_",
      date_end,
      "_wget_commands.txt"
    )

    #### 10-4. write download_command
    download_command <- paste0(
      "wget -e robots=off -np -R .html,.tmp ",
      "-nH --cut-dirs=3 \"",
      download_url,
      "\" --header \"Authorization: Bearer ",
      nasa_earth_data_token,
      "\" -O ",
      directory_to_save,
      download_name,
      "\n"
    )

    #### filter commands to non-existing files
    download_command <- download_command[
      which(
        !file.exists(paste0(directory_to_save, download_name))
      )
    ]

    # avoid any possible errors by removing existing command files
    download_sink(commands_txt)
    #### cat command only if file does not already exist
    cat(download_command)
    sink()

    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    download_run(
      download = download,
      system_command = system_command
    )

    message("Requests were processed.\n")

    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
    return(NULL)
  }


  #### 11. define date sequence
  date_sequence <- generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = FALSE
  )
  # In a certain year, list all available dates
  year <- as.character(substr(date[1], 1, 4))
  filedir_year_url <-
    paste0(
      ladsurl,
      "archive/allData/",
      version,
      "/",
      product,
      "/",
      year
    )

  list_available_d <-
    rvest::read_html(filedir_year_url) |>
    rvest::html_elements("tr") |>
    rvest::html_attr("data-name")
  # no conditional assignment at this moment.

  # remove NAs
  # 12. Queried year's available days
  date_sequence <- list_available_d[!is.na(list_available_d)]
  date_sequence_i <- as.integer(date_sequence)
  # Queried dates to integer range
  date_start_i <- as.integer(strftime(date[1], "%j"))
  date_end_i <- as.integer(strftime(date[2], "%j"))
  date_range_julian <- seq(date_start_i, date_end_i)
  date_sequence_in <- (date_sequence_i %in% date_range_julian)

  message(sprintf(
    "%d / %d days of data available in the queried dates.\n",
    sum(date_sequence_in), length(date_range_julian)
  ))
  date_sequence <- date_sequence[date_sequence_in]


  #### 13. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    product,
    "_",
    date[1],
    "_",
    date[2],
    "_wget_commands.txt"
  )

  # avoid any possible errors by removing existing command files
  download_sink(commands_txt)
  #### 14. append download commands to text file
  for (d in seq_along(date_sequence)) {
    day <- date_sequence[d]
    filedir_url <-
      paste0(
        filedir_year_url,
        "/",
        day
      )

    filelist <-
      rvest::read_html(filedir_url) |>
      rvest::html_elements("tr") |>
      rvest::html_attr("data-path")

    filelist_sub <-
      grep(
        paste0("(", paste(tiles_requested, collapse = "|"), ")"),
        filelist,
        value = TRUE
      )
    download_url <- sprintf("%s%s", ladsurl, filelist_sub)

    download_name <- sapply(
      strsplit(download_url, paste0("/", day, "/")), `[`, 2
    )

    # Main wget run
    download_command <- paste0(
      "wget -e robots=off -np -R .html,.tmp ",
      "-nH --cut-dirs=3 \"",
      download_url,
      "\" --header \"Authorization: Bearer ",
      nasa_earth_data_token,
      "\" -O ",
      directory_to_save,
      download_name,
      "\n"
    )

    #### filter commands to non-existing files
    download_command <- download_command[
      which(
        !file.exists(
          paste0(directory_to_save, download_name)
        )
      )
    ]

    #### 15. concatenate and print download commands to "..._wget_commands.txt"
    #### cat command only if file does not already exist
    cat(download_command)
  }

  #### 16. finish "..._wget_commands.txt"
  sink(file = NULL)

  #### 17.
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  download_run(
    download = download,
    system_command = system_command
  )

  message("Requests were processed.\n")

  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}



# nolint start
#' Download toxic release data
#' @description
#' The \code{download_tri()} function accesses and downloads toxic release data from the [U.S. Environmental Protection Agency's (EPA) Toxic Release Inventory (TRI) Program](https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-action-0).
#' @param year character(2). length of 4 each. Start/end years for downloading data.
# nolint end
#' @param directory_to_save character(1). Directory to download files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mariana Kassien, Insang Song
#' @return NULL; Comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024tri}{amadeus}
#' @examples
#' \dontrun{
#' download_tri(
#'   year = c(2020L, 2021L),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_tri <- function(
  year = c(2018L, 2022L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### check years
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  #### 3. define measurement data paths
  url_download <-
    "https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/"
  year_sequence <- seq(year[1], year[2], 1)
  download_urls <- sprintf(
    paste(url_download, "%.0f", "_US/csv", sep = ""),
    year_sequence
  )
  download_names <-
    sprintf(paste0(directory_to_save,
                   "tri_raw_%.0f.csv"),
            year_sequence)

  #### 4. build download command
  download_commands <- paste0("curl -L ",
                              download_urls,
                              " --output ",
                              download_names,
                              "\n")
  #### filter commands to non-existing files
  download_commands <- download_commands[
    which(
      !file.exists(download_names)
    )
  ]
  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "TRI_",
    year[1], "_", year[2],
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 8. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 9. download data
  download_run(download = download,
               system_command = system_command)
  message("Requests were processed.\n")
  #### 10. remove download commands
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}


# nolint start
#' Download road emissions data
#' @description
#' The \code{download_nei()} function accesses and downloads road emissions data from the [U.S Environmental Protection Agency's (EPA) National Emissions Inventory (NEI)](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei).
# nolint end
#' @param epa_certificate_path character(1). Path to the certificate file
#' for EPA DataCommons. Default is
#' 'extdata/cacert_gaftp_epa.pem' under the package installation path.
#' @param certificate_url character(1). URL to certificate file. See notes for
#' details.
#' @param year Available years of NEI data.
#' Default is \code{c(2017L, 2020L)}.
#' @param directory_to_save character(1). Directory to save data. Two
#' sub-directories will be created for the downloaded zip files ("/zip_files")
#' and the unzipped data files ("/data_files").
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @param unzip logical(1). Unzip the downloaded zip files.
#' Default is \code{FALSE}.
#' @author Ranadeep Daw, Insang Song
#' @note
#' For EPA Data Commons certificate errors, follow the steps below:
#' 1. Click Lock icon in the address bar at https://gaftp.epa.gov
#' 2. Click Show Certificate
#' 3. Access Details
#' 4. Find URL with *.crt extension
#' Currently we bundle the pre-downloaded crt and its PEM (which is accepted
#' in wget command) file in ./inst/extdata. The instruction above is for
#' certificate updates in the future.
#' @return NULL; Zip and/or data files will be downloaded and stored in
#' respective sub-directories within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_usepa2024nei}{amadeus}
#' @examples
#' \dontrun{
#' download_nei(
#'   year = c(2017L, 2020L),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
download_nei <- function(
  epa_certificate_path =
    system.file("extdata/cacert_gaftp_epa.pem",
                package = "amadeus"),
  certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt",
  year = c(2017L, 2020L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  directory_original <- download_sanitize_path(directory_to_save)
  directories <- download_setup_dir(directory_original, zip = TRUE)
  directory_to_download <- directories[1]
  directory_to_save <- directories[2]

  #### 5. define download URL
  download_epa_certificate(
    epa_certificate_path = epa_certificate_path,
    certificate_url = certificate_url
  )

  #### 3. define measurement data paths
  url_download_base <- "https://gaftp.epa.gov/air/nei/%d/data_summaries/"
  url_download_remain <-
    c("2017v1/2017neiApr_onroad_byregions.zip",
      "2020nei_onroad_byregion.zip")
  download_urls <-
    paste0(
      sprintf(url_download_base, year),
      url_download_remain
    )
  download_names_file <-
    c("2017neiApr_onroad_byregions.zip",
      "2020nei_onroad_byregion.zip")
  download_names <- paste0(directory_to_download, download_names_file)
  #### 4. build download command
  download_commands <-
    paste0("wget --ca-certificate=",
           epa_certificate_path,
           " ",
           download_urls,
           " -O ",
           download_names,
           "\n")
  #### filter commands to non-existing files
  download_commands <- download_commands[
    which(
      !file.exists(download_names)
    )
  ]
  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "NEI_AADT_",
    paste(year, collapse = "-"),
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 8. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 9. download data
  download_run(download = download,
               system_command = system_command)

  #### 10. unzip data
  # note that this part does not utilize download_unzip
  # as duplicate file names are across multiple zip files
  if (download) {
    if (unzip) {
      dir_unzip <- paste0(
        directory_to_save,
        sub(".zip", "", download_names_file)
      )
      for (fn in seq_along(dir_unzip)) {
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }
    }
  }
  message("Requests were processed.\n")
  #### 10. remove download commands
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

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
#' When `"Islands"` is selected, the data will be downloaded for Hawaii, Puerto Rico, and Virgin Islands.
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
#' Default is \code{FALSE}. Not working for this function since HUC data is in 7z format.
#' @return NULL; Downloaded files will be stored in \code{directory_to_save}.
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
#'   download = TRUE,
#'   remove_command = TRUE
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
    download = FALSE,
    remove_command = FALSE,
    unzip = FALSE
  ) {
    #### 1. check for data download acknowledgement
    download_permit(acknowledgement = acknowledgement)
    #### 2. directory setup
    download_setup_dir(directory_to_save)
    directory_to_save <- download_sanitize_path(directory_to_save)

    region <- match.arg(region)
    type <- match.arg(type)

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

    #### 4. build download command
    download_commands <-
      paste0(
        "wget -e robots=off -np",
        " ",
        download_urls,
        " -O ",
        download_names,
        "\n"
      )

    #### 5. initiate "..._curl_commands.txt"
    commands_txt <- paste0(
      directory_to_save,
      "USGS_NHD_",
      region,
      "_",
      type,
      "_",
      Sys.Date(),
      "_wget_commands.txt"
    )
    download_sink(commands_txt)
    #### 6. concatenate and print download commands to "..._curl_commands.txt"
    writeLines(download_commands)
    #### 7. finish "..._curl_commands.txt" file
    sink()
    #### 8. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 9. download data
    download_run(download = download,
                 system_command = system_command)

    #### 10. unzip data
    # note that this part does not utilize download_unzip
    # as duplicate file names are across multiple zip files
    if (download) {
      if (unzip) {
        stop("Unzipping is not supported for 7z files. Please do it manually with 7-zip program")
        # dir_unzip <- gsub("(\\.7z)", "", download_names)
        # for (fn in seq_along(dir_unzip)) {
        #   archive::archive_extract(
        #     archive = download_names[fn],
        #     dir = dir_unzip[fn]
        #   )
        # }
      }
    }
    message("Requests were processed.\n")
    #### 10. remove download commands
    download_remove_command(commands_txt = commands_txt,
                            remove = remove_command)

  }
# nolint end



# nolint start
#' Download CropScape data
#' @description
#' Accesses and downloads United States Department of Agriculture
#' CropScape Cropland Data Layer data from 
#' the [USDA National Agricultural Statistics Service](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php) or the 
#' [George Mason University website](https://nassgeodata.gmu.edu/CropScape/).
#' @param year integer(1). Year of the data to download.
#' @param source character(1). Data source, one of `c("USDA", "GMU")`.
#' * `"USDA"` will download the national data from the USDA website (available in 2008-last year).
#' * `"GMU"` will download the data from the George Mason University website (available in 1997-last year).
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
#' @author Insang Song
#' @note JSON files should be found at STAC catalog of OpenLandMap
#' @return NULL; Yearly comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @examples
#' \dontrun{
#' download_cropscape(
#'   year = 2020,
#'   source = "USDA",
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @importFrom archive archive_extract
#' @export
download_cropscape <- function(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE
) {
  source <- match.arg(source)
  if (source == "GMU" && year < 1997) {
    stop("Year should be equal to or greater than 1997.")
  }
  if (source == "USDA" && year < 2008) {
    stop("Year should be equal to or greater than 2008.")
  }
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

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

  #### 4. build download command
  download_commands <-
    paste0("wget -e robots=off -np",
           " ",
           download_urls,
           " -O ",
           download_names,
           "\n")

  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "CropScape_CDL_",
    source,
    "_",
    year,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 8. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 9. download data
  download_run(download = download,
               system_command = system_command)

  #### 10. unzip data
  # note that this part does not utilize download_unzip
  # as duplicate file names are across multiple zip files
  if (download) {
    # nocov start
    if (unzip) {
      extension <- ifelse(source == "USDA", "\\.zip", "(\\.tar|\\.tar\\.gz)")
      dir_unzip <- gsub(extension, "", download_names)
      for (fn in seq_along(dir_unzip)) {
        archive::archive_extract(download_names[fn], exdir = dir_unzip[fn])
      }
    }
    # nocov end
  }
  message("Requests were processed.\n")
  #### 10. remove download commands
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
}
# nolint end

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
#' * `YYYYMMDD` for daily data (between yesterday and January 1st, 1981) – returns a single grid in a .zip file
#' * `YYYYMM` for monthly data (between last month and January 1981) – returns a single grid in a .zip file
#' * `YYYY` for annual data (between last year and 1981) - returns a single grid in a .zip file
#' * `YYYY` for historical data (between 1980 and 1895) - returns a single zip file containing 12 monthly grids for `YYYY` plus the annual.
#'
#' __Normals__:
#' * Monthly normal: date is `MM` (i.e., 04 for April) or the value 14, which returns the annual normal
#' * Daily normal: date is `MMDD` (i.e., 0430 for April 30)
#' @param element character(1). Data element.
#' One of `c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")`
#' For normals, `c("solslope", "soltotal", "solclear", "soltrans")` are also accepted.
#' @param data_type character(1). Data type.
#' * `"ts"`: 4km resolution time series.
#' * `"normals_800"`: 800m resolution normals.
#' * `"normals"`: 4km resolution normals.
#' @param format character(1). Data format. Only applicable for `data_type = "ts"`.
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
#' @author Insang Song
#' @return NULL; .bil (normals) or single grid files depending on the format
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
#'   download = TRUE,
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
  element = c("ppt", "tmin", "tmax", "tmean", "tdmean",
              "vpdmin", "vpdmax",
              "solslope", "soltotal", "solclear", "soltrans"),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
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

  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

  url_middle <-
    # ts: element-date-format
    # normals: element-date
    switch(data_type,
           "ts" = "4km/%s/%s?format=%s",
           "normals_800" = "normals/800m/%s/%s",
           "normals" = "normals/4km/%s/%s")
  #### 3. define measurement data paths
  url_download_template <-
    file.path(
      "https://services.nacse.org/prism/data/public",
      url_middle
    )

  download_urls <-
    ifelse(data_type == "ts",
           sprintf(url_download_template, element, time, format),
           sprintf(url_download_template, element, time))

  #### 4. build download command
  # --content-disposition flag is for web service retrieval
  # when using the URL does not end with the file name
  download_commands <-
    paste0("wget -e robots=off -np ",
           "--content-disposition ",
           download_urls,
           " -P ",
           directory_to_save,
           "\n")

  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "PRISM_",
    element, "_",
    data_type, "_",
    time,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 8. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 9. download data
  download_run(download = download,
               system_command = system_command)

  message("Requests were processed.\n")
  #### 10. remove download commands
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}

# nolint start
#' Download gridMET data
#' @description
#' The \code{download_gridmet} function accesses and downloads gridded surface meteorological data from the [University of California Merced Climatology Lab's gridMET dataset](https://www.climatologylab.org/gridmet.html).
#' @param variables character(1). Variable(s) name(s). See [gridMET Generate Wget File](https://www.climatologylab.org/wget-gridmet.html)
#' for variable names and acronym codes. (Note: variable "Burning Index" has code "bi" and variable
#' "Energy Release Component" has code "erc").
#' @param year character(2). length of 4 each. Start/end years for downloading data.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware
#' @return NULL; netCDF (.nc) files will be stored in a variable-specific
#' folder within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_abatzoglou2013development}{amadeus}
#' @examples
#' \dontrun{
#' download_gridmet(
#'   variables = "Precipitation",
#'   year = c(2023, 2024),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
download_gridmet <- function(
    variables = NULL,
    year = c(2018, 2022),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check years
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  #### directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[1]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  years <- seq(year[1], year[2], 1)
  #### define variables
  variables_list <- process_variable_codes(
    variables = variables,
    source = "gridmet"
  )
  #### define URL base
  base <- "https://www.northwestknowledge.net/metdata/data/"
  #### initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "gridmet_",
    year[1], "_", year[2],
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### concatenate and print download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year_l <- years[y]
      url <- paste0(
        base,
        variable,
        "_",
        year_l,
        ".nc"
      )
      if (y == 1) {
        if (!(check_url_status(url))) {
          sink()
          file.remove(commands_txt)
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
      command <- paste0(
        "curl -s -o ",
        destfile,
        " --url ",
        url,
        "\n"
      )
      if (!file.exists(destfile)) {
        #### cat command only if file does not already exist
        cat(command)
      }
    }
  }
  #### finish "..._curl_commands.txt"
  sink()
  #### build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### remove command text file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}

# nolint start
#' Download TerraClimate data
#' @description
#' The \code{download_terraclimate} function accesses and downloads climate and water balance data from the [University of California Merced Climatology Lab's TerraClimate dataset](https://www.climatologylab.org/terraclimate.html).
#' @param variables character(1). Variable(s) name(s). See [TerraClimate Direct Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
#' for variable names and acronym codes.
#' @param year character(2). length of 4 each. Start/end years for downloading data.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). \code{FALSE} will generate a *.txt file
#' containing all download commands. By setting \code{TRUE} the function
#' will download all of the requested data files.
#' @param remove_command logical(1).
#' Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL; netCDF (.nc) files will be stored in a variable-specific
#' folder within \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{article_abatzoglou2018terraclimate}{amadeus}
#' @examples
#' \dontrun{
#' download_terraclimate(
#'   variables = "Precipitation",
#'   year = c(2023, 2024),
#'   directory_to_save = tempdir(),
#'   acknowledgement = TRUE,
#'   download = TRUE,
#'   remove_command = TRUE
#' )
#' }
#' @export
# nolint end
download_terraclimate <- function(
    variables = NULL,
    year = c(2018, 2022),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### check years
  stopifnot(length(year) == 2)
  year <- year[order(year)]
  #### directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  years <- seq(year[1], year[2], 1)
  #### define variables
  variables_list <- process_variable_codes(
    variables = variables,
    source = "terraclimate"
  )
  #### define URL base
  base <-
    "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "terraclimate_",
    year[1], "_", year[2],
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### concatenate and print download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year_l <- years[y]
      url <- paste0(
        base,
        variable,
        "_",
        year_l,
        ".nc"
      )
      if (y == 1) {
        if (!(check_url_status(url))) {
          sink()
          file.remove(commands_txt)
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
      command <- paste0(
        "curl -s -o ",
        destfile,
        " --url ",
        url,
        "\n"
      )
      if (!file.exists(destfile)) {
        #### cat command only if file does not already exist
        cat(command)
      }
    }
  }
  #### finish "..._curl_commands.txt"
  sink()
  #### build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### remove command text file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}
