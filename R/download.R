# download.R
#' Download raw data from sources
#' @param dataset_name character(1). Dataset to download.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param ... Arguments passed to each download function.
#' @note
#' - All download function names are in \code{download_*_data} formats
#' @author Insang Song
#' @seealso
#' For details of each download function per dataset,
#' Please refer to:
#' * \link{download_aqs_data}: "aqs", "AQS"
#' * \link{download_ecoregion_data}: "ecoregion"
#' * \link{download_geos_data}: "geos"
#' * \link{download_gmted_data}: "gmted", "GMTED"
#' * \link{download_koppen_geiger_data}: "koppen", "koppengeiger"
#' * \link{download_merra2_data}: "merra2", "merra", "MERRA", "MERRA2"
#' * \link{download_narr_monolevel_data}: "narr_monolevel", "monolevel"
#' * \link{download_narr_p_levels_data}: "narr_p_levels", "p_levels", "plevels"
#' * \link{download_nlcd_data}: "nlcd", "NLCD"
#' * \link{download_hms_data}: "noaa", "smoke", "hms"
#' * \link{download_sedac_groads_data}: "sedac_groads", "groads"
#' * \link{download_sedac_population_data}: "sedac_population", "population"
#' * \link{download_modis_data}: "modis", "MODIS"
#' * \link{download_tri_data}: "tri", "TRI"
#' * \link{download_nei_data}: "nei", "NEI"
#' @returns NULL
#' @export
download_data <-
  function(
    dataset_name = c("aqs", "ecoregion", "geos", "gmted", "koppen",
                     "koppengeiger", "merra2", "merra", "narr_monolevel",
                     "modis", "narr_p_levels", "nlcd", "noaa", "sedac_groads",
                     "sedac_population", "groads", "population", "plevels",
                     "p_levels", "monolevel", "hms", "smoke", "tri", "nei"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    ...
  ) {

    dataset_name <- tolower(dataset_name)
    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    what_to_run <- switch(dataset_name,
      aqs = download_aqs_data,
      ecoregion = download_ecoregion_data,
      geos = download_geos_data,
      gmted = download_gmted_data,
      koppen = download_koppen_geiger_data,
      koppengeiger = download_koppen_geiger_data,
      merra2 = download_merra2_data,
      merra = download_merra2_data,
      narr_monolevel = download_narr_monolevel_data,
      monolevel = download_narr_monolevel_data,
      narr_p_levels = download_narr_p_levels_data,
      p_levels = download_narr_p_levels_data,
      plevels = download_narr_p_levels_data,
      nlcd = download_nlcd_data,
      noaa = download_hms_data,
      smoke = download_hms_data,
      hms = download_hms_data,
      sedac_groads = download_sedac_groads_data,
      groads = download_sedac_groads_data,
      sedac_population = download_sedac_population_data,
      population = download_sedac_population_data,
      modis = download_modis_data,
      tri = download_tri_data,
      nei = download_nei_data
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
#' Download daily data from AQS datamart
#' @param parameter_code integer(1). length of 5.
#'  EPA pollutant parameter code. For details, please refer to
#'  [AQS parameter codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html)
# nolint end
#' @param resolution_temporal character(1).
#'  Name of column containing POC values.
#'  Currently, no value other than `"daily"` works.
#' @param url_aqs_download character(1).
#'  URL to the AQS pre-generated datasets.
#' @param year_start integer(1). length of 4.
#'  Start year for downloading data.
#' @param year_end integer(1). length of 4.
#'  End year for downloading data.
#' @param directory_to_download character(1).
#'  Directory to download zip files from AQS data mart.
#' @param directory_to_save character(1).
#'  Directory to decompress zip files.
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
#' @returns NULL; Separate comma-separated value (CSV) files of
#'  monitors and the daily representative values
#'  will be stored in \code{directory_to_save}.
#' @export
download_aqs_data <-
  function(
    parameter_code = 88101,
    resolution_temporal = "daily",
    year_start = 2018,
    year_end = 2022,
    url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
    directory_to_download = NULL,
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE
  ) {
    #### 1. check for data download acknowledgement
    download_permit(
      acknowledgement =
        acknowledgement
    )
    #### 2. check for null parameters
    check_for_null_parameters(mget(ls()))
    #### 3. directory setup
    directory_to_download <- download_sanitize_path(directory_to_download)
    directory_to_save <- download_sanitize_path(directory_to_save)
    download_setup_dir(directory_to_download)
    download_setup_dir(directory_to_save)
    #### 4. define year sequence
    year_sequence <- seq(year_start, year_end, 1)
    #### 5. build URLs
    download_urls <- sprintf(
      paste(url_aqs_download,
        resolution_temporal,
        "_",
        parameter_code,
        "_%.0f.zip",
        sep = ""
      ),
      year_sequence
    )
    #### 6. check for valid URL
    if (!(check_url_status(download_urls[1]))) {
      stop(paste0(
        "Invalid year returns HTTP code 404. ",
        "Check `year_start` parameter.\n"
      ))
    }
    #### 5. build download file name
    download_names <- sprintf(
      paste(directory_to_download,
        "aqs_",
        resolution_temporal,
        "_",
        parameter_code,
        "_%.0f.zip",
        sep = ""
      ),
      year_sequence
    )
    #### 6. build download command
    download_commands <- paste0(
      "curl ",
      download_urls,
      " --output ",
      download_names,
      "\n"
    )
    #### 7. initiate "..._curl_commands.txt"
    commands_txt <- paste0(
      directory_to_download,
      "aqs_",
      parameter_code,
      "_",
      year_start, "_", year_end,
      "_",
      resolution_temporal,
      "_curl_commands.txt"
    )
    download_sink(commands_txt)
    #### 8. concatenate and print download commands to "..._curl_commands.txt"
    writeLines(download_commands)
    #### 9. finish "..._curl_commands.txt" file
    sink()
    #### 10. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 11. download data
    if (!any(file.exists(download_names))) {
      download_run(
        download = download,
        system_command = system_command
      )
    }
    #### 12. unzip data
    for (n in seq_along(download_names)) {
      download_unzip(
        file_name = download_names[n],
        directory_to_unzip = directory_to_save,
        unzip = unzip
      )
    }
    #### 13. remove command file
    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
    #### 14. remove zip files
    for (d in seq_along(download_names)) {
      download_remove_zips(
        remove = remove_zip,
        download_name = download_names[d]
      )
    }
  }




#' Download Ecoregion Shapefiles from EPA
#' @description
#' The \code{download_ecoregion_data()} function accesses and downloads
#' Ecoregions level 3 data, where all pieces of information in the higher
#' levels are included.
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
#' @param directory_to_download character(1). Directory to download zip file
#' of Ecoregion level 3 shapefiles
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default \code{FALSE}.
#' @author Insang Song
#' @returns NULL;
#' @importFrom utils download.file
#' @export
download_ecoregion_data <- function(
  epa_certificate_path =
    system.file("extdata/cacert_gaftp_epa.pem",
                package = "amadeus"),
  certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt",
  directory_to_download = NULL,
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
  download_setup_dir(directory_to_save)
  download_setup_dir(directory_to_download)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. Check the presence of file
  ## This part is hard-coded as the original file appears to
  ## be a misnomer. May need to be modified accordingly in the future.
  path_downloaded_file <- sprintf(
    "%sus_eco_l3_state_boundaries.shp",
    directory_to_save
  )
  if (file.exists(path_downloaded_file)) {
    message("Requested files exist in the target directory.\n")
    return(NULL)
  }
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
  download_name <- sprintf(
    "%sus_eco_l3_state_boundaries.zip",
    directory_to_download
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
    directory_to_download,
    "us_eco_l3_state_boundaries_",
    Sys.Date(),
    "_wget_command.txt"
  )
  #### 9. concatenate
  download_sink(commands_txt)
  #### 10. concatenate and print download commands to "..._wget_commands.txt"
  cat(download_command)
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
#' Download atmospheric composition data from the NASA Global Earth Observing System (GEOS) model.
#' @description
#' The \code{download_geos_data()} function accesses and downloads various
#' atmospheric composition collections from the [NASA Global Earth Observing System (GEOS) model](https://gmao.gsfc.nasa.gov/GEOS_systems/).
# nolint end
#' @param collection character(1). GEOS-CF data collection file name.
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = `"2023-09-01"`).
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = `"2023-09-01"`).
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
#' @return NULL; Hourly netCDF (.nc4) files will be stored in
#' \code{directory_to_save}.
#' @export
download_geos_data <- function(
    collection =
        c(
          "aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1",
          "met_tavg_1hr_g1440x721_x1", "xgc_tavg_1hr_g1440x721_x1",
          "chm_inst_1hr_g1440x721_p23", "met_inst_1hr_g1440x721_p23"
        ),
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. match collection
  collection <- match.arg(collection)
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### 6. define time sequence
  time_sequence <- generate_time_sequence(collection)
  #### 7. define URL base
  base <- "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/"
  #### 8. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    collection,
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )

  download_sink(commands_txt)
  #### 9. concatenate and print download commands to "..._wget_commands.txt"
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    year <- substr(date, 1, 4)
    month <- substr(date, 5, 6)
    day <- substr(date, 7, 8)
    for (t in seq_along(time_sequence)) {
      download_url <- paste0(
        base,
        "Y",
        year,
        "/M",
        month,
        "/D",
        day,
        "/GEOS-CF.v01.rpl.",
        collection,
        ".",
        date,
        "_",
        time_sequence[t],
        "z.nc4"
      )
      if (t == 1) {
        if (!(check_url_status(download_url))) {
          sink()
          file.remove(commands_txt)
          stop(paste0(
            "Invalid date returns HTTP code 404. ",
            "Check `date_start` parameter.\n"
          ))
        }
      }
      download_folder <- paste0(
        directory_to_save,
        collection
      )
      download_command <- paste0(
        "wget ",
        download_url,
        " -P ",
        download_folder,
        "\n"
      )
      cat(download_command)
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

# nolint start
#' Download global elevation data from the Global Multi-resolution Terrain Elevation Data (GMTED2010).
#' @description
#' The \code{download_gmted_data()} function acesses and downloads Global
#' Multi-resolution Terrain Elevation Data (GMTED2010) from
#' [U.S. Geological Survey and National Geospatial-Intelligence Agency](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010).
#' @param statistic character(1). Available statistics include `"Breakline Emphasis"`, `"Systematic Subsample"`, `"Median Statistic"`,
#' `"Minimum Statistic"`, `"Mean Statistic"`, `"Maximum Statistic"`, and
#' `"Standard Deviation Statistic"`.
#' @param resolution character(1). Available resolutions include `"7.5 arc-seconds"`, `"15 arc-seconds"`, and `"30 arc-seconds"`.
#' @param directory_to_download character(1). Directory to download zip files
#' from Global Multi-resolution Terrain Elevation Data (GMTED2010).
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @return NULL; Statistic and resolution-specific zip files will be stored in
#' \code{directory_to_download}, and directories containing raw ASCII Grid data
#'will be stored in \code{directory_to_save}.
#' @export
download_gmted_data <- function(
  statistic = c(
    "Breakline Emphasis", "Systematic Subsample",
    "Median Statistic", "Minimum Statistic",
    "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"
  ),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
  directory_to_download = NULL,
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
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
    directory_to_download,
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
  cat(download_command)
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
#' Download meteorological and atmospheric data from the Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2) model.
#' @description
#' The \code{download_merra2_data()} function accesses and downloads various
#' meteorological and atmospheric collections from the [Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2)](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).
#' @param collection character(1). MERRA-2 data collection file name.
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 is `"2023-09-01"`).
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 is `"2023-09-01"`).
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
#' @return NULL; Daily netCDF (.nc4) files will be stored in
#' \code{directory_to_save}.
#' @export
# nolint end
download_merra2_data <- function(
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
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 3. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 4. check if collection is recognized
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
  if (!(collection %in% identifiers_df$collection_id)) {
    print(identifiers_df)
    stop(paste0("Requested collection is not recognized.\n
    Please refer to the table above to find a proper collection.\n"))
  }
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### 6. define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))
  #### 7. define ESDT name and DOI
  identifiers_df_requested <- subset(identifiers_df,
    subset =
      identifiers_df$collection_id ==
      collection
  )
  esdt_name <- identifiers_df_requested[, 2]
  cat(paste0(
    "Collection: ",
    collection,
    " | ESDT Name: ",
    esdt_name,
    " | DOI: ",
    identifiers_df_requested[, 3],
    "\n"
  ))
  #### 8. define URL base
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
  #### 9. identify download URLs
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
          "Check `date_start` parameter.\n"
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
  #### 10. match list_urls to date sequence
  list_urls_date_sequence <- list_urls[substr(list_urls, 28, 35) %in%
                                         date_sequence]
  #### 11. separate data and metadata
  list_urls_data <- list_urls_date_sequence[grep("*.xml",
    list_urls_date_sequence,
    invert = TRUE
  )]
  list_urls_metadata <- list_urls_date_sequence[grep("*.xml",
    list_urls_date_sequence,
    invert = FALSE
  )]
  #### 12. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    collection,
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )
  download_sink(commands_txt)
  #### 13. concatenate and print download commands to "..._wget_commands.txt"
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
      collection
    )
    download_command <- paste0(
      "wget ",
      download_url,
      " -P ",
      download_folder,
      "\n"
    )
    cat(download_command)
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
      collection,
      "/metadata/"
    )
    download_command_metadata <- paste0(
      "wget ",
      download_url_metadata,
      " -P ",
      download_folder_metadata,
      "\n"
    )
    cat(download_command_metadata)
  }
  #### 14. finish "..._wget_commands.txt"
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
}

# nolint start
#' download_narr_monolevel_data: download monolevel meteorological data from NOAA NCEP North American Regional Reanalysis (NARR) model.
#' @description
#' The \code{download_narr_monolevel_data} function accesses and downloads
#' monolevel meteorological data from [NOAA NCEP North American Regional Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html).
# nolint end
#' @param variables character. Variable(s) name acronym.
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
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
#' @return NULL; Yearly netCDF (.nc) files will be stored in
#' \code{directory_to_save}.
#' @export
download_narr_monolevel_data <- function(
    variables = NULL,
    year_start = 2022,
    year_end = 2022,
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. define years sequence
  if (any(nchar(year_start) != 4, nchar(year_end) != 4)) {
    stop("year_start and year_end should be 4-digit integers.\n")
  }
  years <- seq(year_start, year_end, 1)
  #### 5. define variables
  variables_list <- as.vector(variables)
  #### 6. define URL base
  base <- "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "narr_monolevel_",
    year_start, "_", year_end,
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 8. concatenate and print download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year <- years[y]
      url <- paste0(
        base,
        variable,
        ".",
        year,
        ".nc"
      )
      if (y == 1) {
        if (!(check_url_status(url))) {
          sink()
          file.remove(commands_txt)
          stop(paste0(
            "Invalid year returns HTTP code 404. ",
            "Check `year_start` parameter.\n"
          ))
        }
      }
      destfile <- paste0(
        directory_to_save,
        variable,
        "/",
        variable,
        ".",
        year,
        ".nc"
      )
      command <- paste0(
        "curl -s -o ",
        destfile,
        " --url ",
        url,
        "\n"
      )
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
  #### 12. remove command text file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}

# nolint start
#' Download pressure level meteorological data from NOAA NCEP North American Regional Reanalysis (NARR) model.
# nolint end
#' @description
#' The \code{download_narr_p_levels_data} function accesses and downloads
#' pressure level meteorological data from [NOAA NCEP North American Regional
#' Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html).
#' @param variables character(1). Variable(s) name acronym.
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
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
#' @return NULL; Monthly netCDF (.nc) files will be stored in
#' \code{directory_to_save}.
#' @export
download_narr_p_levels_data <- function(
    variables = NULL,
    year_start = 2022,
    year_end = 2022,
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. define years sequence
  years <- seq(year_start, year_end, 1)
  #### 5. define months sequence
  months <- sprintf("%02d", seq(1, 12, by = 1))

  #### 6. define variables
  variables_list <- as.vector(variables)
  #### 7. define URL base
  base <- "https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/"
  #### 8. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "narr_p_levels_",
    year_start,
    "_",
    year_end,
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 9. concatenate download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year <- years[y]
      for (m in seq_along(months)) {
        month <- months[m]
        url <- paste0(
          base,
          variable,
          ".",
          year,
          month,
          ".nc"
        )
        if (m == 1) {
          if (!(check_url_status(url))) {
            sink()
            file.remove(commands_txt)
            stop(paste0(
              "Invalid year returns HTTP code 404. ",
              "Check `year_start` parameter.\n"
            ))
          }
        }
        destfile <- paste0(
          directory_to_save,
          variable,
          "/",
          variable,
          ".",
          year,
          month,
          ".nc"
        )
        command <- paste0(
          "curl -s -o ",
          destfile,
          " --url ",
          url,
          "\n"
        )
        cat(command)
      }
    }
  }
  #### 10. finish "..._curl_commands.txt"
  sink()
  #### 11. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 12. download data
  download_run(
    download = download,
    system_command = system_command
  )
  #### 13. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
}


# nolint start
#' Download land cover data from the National Land Cover Database Science Research Products.
# nolint end
#' @description
#' The \code{download_nlcd_data()} function accesses and downloads
#' land cover data from the
#' [NLCD Science Research Products](https://www.mrlc.gov/data) data base.
#' @param collection character(1). `"Coterminous United States"` or `"Alaska"`.
#' @param year integer(1). Available years for Coterminous United States
#' include `2001`, `2004`, `2006`, `2008`, `2011`, `2013`, `2016`,
#' `2019`, and `2021`.
#' Available years for Alaska include `2001`, `2011`, and `2016`.
#' @param directory_to_download character(1). Directory to download zip files
#' from National Land Cover Database Science Research Products.
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @returns NULL; Zip file will be stored in \code{directory_to_download}, and
#' selected GeoTIFF (.tif) files will be stored in \code{directory_to_save}.
#' @export
download_nlcd_data <- function(
  collection = "Coterminous United States",
  year = 2021,
  directory_to_download = NULL,
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
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
    directory_to_download,
    tolower(collection_code),
    Sys.Date(),
    "_curl_command.txt"
  )
  download_sink(commands_txt)
  #### 12. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
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
#' Download Global Roads Open Access Data Set (gROADS), v1 (1980-2010) data from NASA Socioeconomic Data and Applications Center (SEDAC).
#' @description
#' The \code{download_sedac_groads_data()} function accesses and downloads
#' roads data from the National Aeronautics and Space
#' Administration's (NASA) [Global Roads Open Access Data Set](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1/data-download).
#' @param data_region character(1). Data can be downloaded for `"Global"`,
#' `"Africa"`, `"Asia"`, `"Europe"`, `"Americas"`, `"Oceania East"`, and `"Oceania West"`.
#' @param data_format character(1). Data can be downloaded as `"Shapefile"` or
#' `"Geodatabase"`. (Only `"Geodatabase"` available for `"Global"` region).
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA Global Roads Open Access Data Set.
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @returns NULL; Zip file will be stored in \code{directory_to_download}, and
#' selected Shapefile (.shp) or Geodatabase (.gdb) files will be stored in 
#' \code{directory_to_save}.
#' @export
download_sedac_groads_data <- function(
    data_region = c("Americas", "Global", "Africa", "Asia", "Europe", "Oceania East", "Oceania West"),
    data_format = c("Shapefile", "Geodatabase"),
    directory_to_download = NULL, 
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
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
    directory_to_download,
    "sedac_groads_",
    gsub(" ", "_", region),
    "_",
    Sys.Date(),
    "_curl_command.txt"
  )
  download_sink(commands_txt)
  #### 12. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
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
#' Download UN WPP-Adjusted population density data from NASA Socioeconomic Data and Applications Center (SEDAC)
#' @description
#' The \code{download_sedac_population_data()} function accesses and downloads
#' population density data from the National Aeronatuics and Space
#' Administration's (NASA) [UN WPP-Adjusted Population Density, v4.11]( https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11).
#' @param data_resolution character(1). Available resolutions are 30 second
#' (approx. 1 km), 2.5 minute (approx. 5 km), 15 minute (approx. 30 km),
#' 30 minute (approx. 55 km), and 60 minute (approx. 110 km).
#' @param data_format character(1). Individual year data can be downloaded as
#' `"ASCII"` or `"GeoTIFF"`. "all" years is downloaded as `"netCDF"`.
#' @param year character(1). Available years are `2000`, `2005`, `2010`, `2015`, and
#' `2020`, or `"all"` for all years.
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA UN WPP-Adjusted Population Density, v4.11.
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @returns NULL; Zip file will be stored in \code{directory_to_download}, and
#' selected GeoTIFF (.tif) files will be stored in \code{directory_to_save}.
#' @export
download_sedac_population_data <- function(
  data_resolution = "60 minute",
  data_format = c("GeoTIFF", "ASCII", "netCDF"),
  year = "2020",
  directory_to_download = NULL,
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/")
  #### 5. define year
  year <- ifelse(year == "all", "totpop", as.character(year))
  #### 6. define data resolution
  resolution <- process_sedac_codes(data_resolution)
  #### 7. 30 second resolution not available for all years
  if (year == "totpop" && resolution == "30_sec") {
    resolution <- "2pt5_min"
    cat(paste0(
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
      cat(paste0(
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
      cat(paste0(
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
    directory_to_download,
    "sedac_population_",
    year,
    "_",
    resolution,
    "_",
    Sys.Date(),
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 13. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
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
#' Download daily wildfire smoke plume data from NOAA Hazard Mapping System Fire and Smoke Product
#' @description
#' The \code{download_hms_data()} function accesses and downloads
#' wildfire smoke plume coverage data from
#' the National Oceanic and Atmospheric Administration's (NOAA)
#' [Hazard Mapping System Fire and Smoke Product](https://www.ospo.noaa.gov/Products/land/hms.html#0).
# nolint end
#' @param data_format character(1). "Shapefile" or "KML".
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 is `"2023-09-01"`).
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 10, 2023 is `"2023-09-10"`).
#' @param directory_to_download character(1). Directory to download zip files
#' from NOAA Hazard Mapping System Fire and Smoke Product. (Ignored if
#' \code{data_format = "KML"}.)
#' @param directory_to_save character(1). Directory to save unzipped shapefiles
#' and KML files.
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
#' @returns NULL; Zip file will be stored in \code{directory_to_download}, and
#' Shapefiles (.shp) or KML files (.kml) will be stored in
#' \code{directory_to_save}.
#' @export
download_hms_data <- function(
    data_format = "Shapefile",
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    directory_to_download = NULL,
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. check for unzip == FALSE && remove_zip == TRUE
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0(
      "Arguments unzip = FALSE and remove_zip = TRUE are not ",
      "acceptable together. Please change one.\n"
    ))
  }
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### 6. define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_download,
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
    if (data_format == "Shapefile") {
      suffix <- ".zip"
      directory_to_cat <- directory_to_download
    } else if (data_format == "KML") {
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
          "Check `date_start` parameter.\n"
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
    cat(command)
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
    return(cat(paste0("KML files cannot be unzipped.\n")))
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


# nolint start
#' Download climate classification data from the present and future Köppen-Geiger climate classification maps.
#' @description
#' The \code{download_koppen_geiger_data()} function accesses and downloads
#' climate classification data from the Present and future
#' Köppen-Geiger climate classification maps at
#'  1-km resolution ([link for article](https://www.nature.com/articles/sdata2018214); [link for data](https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2)).
# nolint end
#' @param data_resolution character(1). Available resolutions are `"0.0083"`
#' degrees (approx. 1 km), `"0.083"` degrees (approx. 10 km), and
#' `"0.5"` degrees (approx. 50 km).
#' @param time_period character(1). Available times are `"Present"` (1980-2016)
#' and `"Future"` (2071-2100). ("Future" classifications are based on scenario
#' RCP8.5).
#' @param directory_to_download character(1). Directory to download zip files
#' from Present and future Köppen-Geiger climate classification maps at 1-km
#' resolution.
#' @param directory_to_save character(1). Directory to decompress zip files.
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
#' @returns NULL; Zip file will be stored in \code{directory_to_download}, and
#' selected GeoTIFF (.tif) files will be stored in \code{directory_to_save}.
#' @export
download_koppen_geiger_data <- function(
    data_resolution = c("0.0083", "0.083", "0.5"),
    time_period = c("Present", "Future"),
    directory_to_download = NULL,
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
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. check for data resolution
  data_resolution <- match.arg(data_resolution)
  #### 5. check for valid time period
  time_period <- match.arg(time_period)
  #### 6. define time period
  period <- tolower(time_period)
  #### 7. define data resolution
  data_resolution <- gsub("\\.", "p", data_resolution)
  #### 8 define download URL
  download_url <- "https://figshare.com/ndownloader/files/12407516"
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
    directory_to_download,
    "koppen_geiger_",
    time_period,
    "_",
    data_resolution,
    "_",
    Sys.Date(),
    "_wget_command.txt"
  )
  download_sink(commands_txt)
  #### 12. concatenate and print download command to "..._wget_commands.txt"
  cat(download_command)
  #### 13. finish "..._wget_commands.txt" file
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

  if (unzip) {
    #### 17. remove unwanted files
    unwanted_names <- list.files(
      path = directory_to_save,
      pattern = "Beck_KG",
      full.names = TRUE
    )
    unwanted_names <- as.vector(c(
      unwanted_names,
      paste0(
        directory_to_save,
        "KoppenGeiger.m"
      )
    ))
    tif <- paste0(
      directory_to_save,
      "/Beck_KG_V1_",
      period,
      "_",
      data_resolution,
      ".tif"
    )
    unwanted_names <- unwanted_names[grep(
      pattern = tif,
      unwanted_names,
      invert = TRUE
    )]
    file.remove(unwanted_names)
  }

  #### 18. Remove command file
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove_command
  )
  if (download) {
    #### 16. end if unzip == FALSE
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
}


# nolint start
#' Download MODIS product files
# nolint end
#' @description Need maintenance for the directory path change
#' in NASA EOSDIS. This function first retrieves the all hdf download links
#' on a certain day, then only selects the relevant tiles from the retrieved
#' links. Download is only done at the queried horizontal-vertical tile number
#' combinations. An exception is MOD06_L2 product, which is produced
#' every five minutes every day.
#' @note \code{date_start} and \code{date_end} should be in the same year.
#'  Directory structure looks like
#'  input/modis/raw/\{version\}/\{product\}/\{year\}/\{day_of_year\}
#'  Please note that \code{date_start} and \code{date_end} are
#'  ignored if \code{product == 'MOD06_L2'}.
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
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 is `"2023-09-01"`).
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 is `"2023-09-01"`).
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledges that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). Download data or only save wget commands.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @import rvest
#' @return NULL; Raw HDF (.hdf) files will be stored in
#' \code{directory_to_save}.
#' @export
download_modis_data <- function(
    product = c(
      "MOD09GA", "MOD11A1", "MOD06_L2",
      "MCD19A2", "MOD13A2", "VNP46A2"
    ),
    version = "61",
    horizontal_tiles = c(7, 13),
    vertical_tiles = c(3, 6),
    mod06_links = NULL,
    nasa_earth_data_token = NULL,
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for NASA earth data token
  if (is.null(nasa_earth_data_token)) {
    stop("Please provide NASA EarthData Login token.\n")
  }
  #### 4. check for product
  product <- match.arg(product)

  if (substr(date_start, 1, 4) != substr(date_end, 1, 4)) {
    if (product != "MOD06_L2") {
      stop("date_start and date_end should be in the same year.\n")
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
      sprintf(mod06l2_url_template, date_start, date_end)

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
      "wget -e robots=off -m -np -R .html,.tmp ",
      "-nH --cut-dirs=3 \"",
      download_url,
      "\" --header \"Authorization: Bearer ",
      nasa_earth_data_token,
      "\" -P ",
      directory_to_save,
      "\n"
    )

    # avoid any possible errors by removing existing command files
    download_sink(commands_txt)
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
    date_start,
    date_end,
    sub_hyphen = FALSE
  )
  # In a certain year, list all available dates
  year <- as.character(substr(date_start, 1, 4))
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
  date_start_i <- as.integer(strftime(date_start, "%j"))
  date_end_i <- as.integer(strftime(date_end, "%j"))
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
    date_start,
    "_",
    date_end,
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
    # Main wget run
    download_command <- paste0(
      "wget -e robots=off -m -np -R .html,.tmp ",
      "-nH --cut-dirs=3 \"",
      download_url,
      "\" --header \"Authorization: Bearer ",
      nasa_earth_data_token,
      "\" -P ",
      directory_to_save,
      "\n"
    )
    #### 15. concatenate and print download commands to "..._wget_commands.txt"
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


#' Download data from EPA toxic release inventory
#' @param year_start integer(1). length of 4. Start year for downloading data.
#' @param year_end integer(1). length of 4. End year for downloading data.
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
#' @returns NULL; Yearly comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @export
download_tri_data <- function(
  year_start = 2018L,
  year_end = 2022L,
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

  #### 3. define measurement data paths
  url_download <-
    "https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/"
  year_sequence <- seq(year_start, year_end, 1)
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
  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "TRI_",
    year_start, "_", year_end,
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
#' Download data from EPA National Emission Inventory aggregated on-road emission data
# nolint end
#' @param epa_certificate_path character(1). Path to the certificate file
#' for EPA DataCommons. Default is
#' 'extdata/cacert_gaftp_epa.pem' under the package installation path.
#' @param certificate_url character(1). URL to certificate file. See notes for
#' details.
#' @param year_target Available years of NEI data.
#' Default is \code{c(2017L, 2020L)}.
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
#' @returns NULL; Yearly comma-separated value (CSV) files will be stored in
#' \code{directory_to_save}.
#' @export
download_nei_data <- function(
  epa_certificate_path =
    system.file("extdata/cacert_gaftp_epa.pem",
                package = "amadeus"),
  certificate_url =
    "http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt",
  year_target = c(2017L, 2020L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE
) {
  #### 1. check for data download acknowledgement
  download_permit(acknowledgement = acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

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
      sprintf(url_download_base, year_target),
      url_download_remain
    )
  download_names_file <-
    c("2017neiApr_onroad_byregions.zip",
      "2020nei_onroad_byregion.zip")
  download_names <- paste0(directory_to_save, download_names_file)

  #### 4. build download command
  download_commands <-
    paste0("wget --ca-certificate=",
           epa_certificate_path,
           " ",
           download_urls,
           " -O ",
           download_names,
           "\n")

  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "NEI_AADT_",
    paste(year_target, collapse = "-"),
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
      dir_unzip <- sub(".zip", "", download_names)
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
