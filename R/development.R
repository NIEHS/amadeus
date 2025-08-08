################################################################################
# Functions which are being developed but do not currently have a complete
# series of `download_*`, `process_*`, and `calculate_*` functions.
# 1. National Hydrography Dataset (HUC) (function suffix `_huc`)
# 2. Emissions Database for Global Atmospheric Research (EDGAR) (function suffix
#    `_edgar`)
# 3. PRISM Climate Group Web Service (function suffix `_prism`)
# 4. United States Department of Agriculture (USDA) Cropscape Cropland Data
#    Layer (function suffix `_cropscape`)

################################################################################
# 1. National Hydrography Dataset (HUC)
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
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Downloaded files will be stored in \code{directory_to_save}.
#' @author Insang Song
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{data_usgs2023nhd}{amadeus}
#' @keywords internal
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
    download = FALSE,
    remove_command = FALSE,
    unzip = FALSE,
    hash = FALSE
  ) {
    #### 1. check for data download acknowledgement
    amadeus::download_permit(acknowledgement = acknowledgement)
    #### 2. directory setup
    amadeus::download_setup_dir(directory_to_save)
    directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

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
    amadeus::download_sink(commands_txt)
    #### 6. concatenate and print download commands to "..._curl_commands.txt"
    writeLines(download_commands)
    #### 7. finish "..._curl_commands.txt" file
    sink()
    #### 9. download data
    amadeus::download_run(
      download = download,
      commands_txt = commands_txt,
      remove = remove_command
    )

    #### 10. unzip data
    # note that this part does not utilize download_unzip
    # as duplicate file names are across multiple zip files
    if (download) {
      if (unzip) {
        stop(
          "Unzipping is not supported for 7z files. Please do it manually with 7-zip program"
        )
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
    return(amadeus::download_hash(hash, directory_to_save))
  }
# nolint end

#' Retrieve Hydrologic Unit Code (HUC) data
#' @author Insang Song
#' @param path character. Path to the file or the directory containing HUC data.
#' @param layer_name character(1). Layer name in the `path`
#' @param huc_level character(1). Field name of HUC level
#' @param huc_header character(1). The upper level HUC code header to extract
#'  lower level HUCs.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Arguments passed to `nhdplusTools::get_huc()`
#' @return a `SpatVector` object
#' @seealso [`nhdplusTools::get_huc`]
#' @importFrom terra vect
#' @importFrom terra vector_layers
#' @importFrom rlang inject
#' @importFrom nhdplusTools get_huc
#' @examples
#' ## NOTE: Examples are wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' library(terra)
#' getf <- "WBD_National_GDB.gdb"
#' # check the layer name to read
#' terra::vector_layers(getf)
#' test1 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test2 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test3 <- process_huc(
#'   "",
#'   layer_name = NULL,
#'   huc_level = NULL,
#'   huc_header = NULL,
#'   id = "030202",
#'   type = "huc06"
#' )
#' }
#' @keywords internal
#' @export
process_huc <-
  function(
    path,
    layer_name = NULL,
    huc_level = NULL,
    huc_header = NULL,
    extent = NULL,
    ...
  ) {
    # exclude the coverage due to write permission related to memoization
    #nocov start
    if (missing(path) || (!file.exists(path) && !dir.exists(path))) {
      hucpoly <- try(
        rlang::inject(nhdplusTools::get_huc(!!!list(...)))
      )
      if (inherits(hucpoly, "try-error")) {
        stop("HUC data was not found.")
      }
      hucpoly <- terra::vect(hucpoly)
    }
    #nocov end
    if (file.exists(path) || dir.exists(path)) {
      if (!is.null(huc_header)) {
        querybase <-
          sprintf(
            "SELECT * FROM %s WHERE %s LIKE '%s%%'",
            layer_name,
            huc_level,
            huc_header
          )
      } else {
        querybase <-
          sprintf("SELECT * FROM %s", layer_name)
      }
      if (!layer_name %in% terra::vector_layers(path)) {
        stop(
          paste0(
            "Layer ",
            layer_name,
            " not found in ",
            path
          )
        )
      }

      hucpoly <- try(
        terra::vect(
          path,
          query = querybase,
          extent = extent
        )
      )
    }
    return(hucpoly)
  }

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
#'  "TNR_Aviation_LTO", "TNR_Aviation_SPS", "TNR_Other", "TNR_Ship", "TRO", "WWT"
#' @param sector_monthly Character vector or NULL. Emission sectors for monthly
#'  data. If NULL, the function will use full-species files (not sector-specific).
#'  Supported values: "AGRICULTURE", "BUILDINGS", "FUEL_EXPLOITATION",
#' "IND_COMBUSTION", "IND_PROCESSES", "POWER_INDUSTRY", "TRANSPORT", "WASTE".
#' @param sector_voc Character vector or NULL. Emission sectors for VOC speciation
#'  data. If NULL, the function will use full-species files (not sector-specific).
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
#' @author Mariana Alifa Kassien
#' @return
#' * For \code{hash = FALSE}, NULL
#' * For \code{hash = TRUE}, an \code{rlang::hash_file} character.
#' * Zip and/or data files will be downloaded and stored in
#' \code{directory_to_save}.
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{web_edgarv8_1ap}{amadeus}
#' \insertRef{web_edgarv8_1voc}{amadeus}
#' # nolint end
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
#' @keywords internal
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
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE
) {
  # check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

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

  # Validate and download
  download_urls <- c()
  missing_urls <- c()

  for (u in urls) {
    if (!(amadeus::check_url_status(u))) {
      missing_urls <- c(missing_urls, u)
    } else {
      download_urls <- c(download_urls, u)
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

  #### 5. build download file name
  download_names <- paste0(
    directory_to_download,
    "edgar_",
    temp_res,
    "_",
    basename(download_urls)
  )

  #### build download command
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
      !file.exists(download_names) | file.size(download_names) == 0
    )
  ]
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_original,
    "edgar_",
    temp_res,
    "_curl_commands.txt"
  )
  amadeus::download_sink(commands_txt)
  #### 8. concatenate and print download commands to "..._curl_commands.txt"
  cat(download_commands)
  #### 9. finish "..._curl_commands.txt" file
  sink()
  #### 11. download data
  amadeus::download_run(
    download = download,
    commands_txt = commands_txt,
    remove = remove_command
  )
  #### 12. unzip data
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
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
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
#' @keywords internal
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
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
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

  #### 4. build download command
  # --content-disposition flag is for web service retrieval
  # when using the URL does not end with the file name
  download_commands <-
    paste0(
      "wget -e robots=off -np ",
      "--content-disposition ",
      download_urls,
      " -P ",
      directory_to_save,
      "\n"
    )

  #### 5. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_save,
    "PRISM_",
    element,
    "_",
    data_type,
    "_",
    time,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  amadeus::download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 9. download data
  amadeus::download_run(
    download = download,
    commands_txt = commands_txt,
    remove = remove_command
  )
  message("Requests were processed.\n")
  return(amadeus::download_hash(hash, directory_to_save))
}

#' Process PRISM data
#' @description
#' This function imports and cleans raw PRISM data,
#' returning a single `SpatRaster` object.
#' @param path character giving PRISM data path
#' Both file and directory path are acceptable.
#' @param element character(1). PRISM element name
#' @param time character(1). PRISM time name.
#' Should be character in length of 2, 4, 6, or 8.
#' "annual" is acceptable.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads time series or 30-year normal PRISM data.
#' @return a `SpatRaster` object with metadata of time and element.
#' @seealso [`terra::rast`], [`terra::metags`]
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' prism <- process_prism(
#'   path = "./data/PRISM_ppt_stable_4kmM3_202104_nc.nc",
#'   element = "ppt",
#'   time = "202104"
#' )
#' }
#' @keywords internal
#' @export
# nolint start
process_prism <-
  function(
    path = NULL,
    element = NULL,
    time = NULL,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (
      !element %in%
        c(
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
        )
    ) {
      stop("element is not a valid PRISM element.")
    }
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!nchar(time) %in% seq(2, 8, 2)) {
      stop("time does not have valid length.")
    }

    if (dir.exists(path)) {
      pattern <- "PRISM_%s*.*_%s_*.*(bil|nc|grib2|asc)$"
      pattern <- sprintf(pattern, element, time)
      prism_file <-
        list.files(
          path,
          pattern = pattern,
          full.names = TRUE
        )
    } else {
      prism_file <- path
    }
    prism <- terra::rast(prism_file, win = extent)
    terra::metags(prism) <- cbind(
      c("time", "element"),
      c(time, element)
    )
    return(prism)
  }
# nolint end

################################################################################
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
#' @param hash logical(1). By setting \code{TRUE} the function will return
#' an \code{rlang::hash_file()} hash character corresponding to the
#' downloaded files. Default is \code{FALSE}.
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
#' @keywords internal
#' @importFrom archive archive_extract
#' @export
download_cropscape <- function(
  year = seq(1997, 2023),
  source = c("USDA", "GMU"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  hash = FALSE
) {
  source <- match.arg(source)
  if (source == "GMU" && year < 1997) {
    stop("Year should be equal to or greater than 1997.")
  }
  if (source == "USDA" && year < 2008) {
    stop("Year should be equal to or greater than 2008.")
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
    "CropScape_CDL_",
    source,
    "_",
    year,
    "_",
    Sys.Date(),
    "_wget_commands.txt"
  )
  amadeus::download_sink(commands_txt)
  #### 6. concatenate and print download commands to "..._curl_commands.txt"
  writeLines(download_commands)
  #### 7. finish "..._curl_commands.txt" file
  sink()
  #### 9. download data
  amadeus::download_run(
    download = download,
    commands_txt = commands_txt,
    remove = remove_command
  )

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
  return(amadeus::download_hash(hash, directory_to_save))
}
# nolint end

#' Process CropScape data
#' @description
#' This function imports and cleans raw CropScape data,
#' returning a single `SpatRaster` object.
#' @param path character giving CropScape data path
#' @param year numeric giving the year of CropScape data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads CropScape file of selected `year`.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' cropscape <- process_cropscape(
#'   path = "./data/cropscape_example.tif",
#'   year = 2020
#' )
#' }
#' @keywords internal
#' @export
process_cropscape <-
  function(
    path = NULL,
    year = 2021,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    # open cdl file corresponding to the year
    if (dir.exists(path)) {
      cdl_file <-
        list.files(
          path,
          pattern = paste0("cdl_30m_*.*", year, "_*.*.tif$"),
          full.names = TRUE
        )
    } else {
      cdl_file <- path
    }
    cdl <- terra::rast(cdl_file, win = extent)
    terra::metags(cdl) <- c(year = year)
    return(cdl)
  }
