################################################################################
# {amadeus} functions for Climatology Lab gridMET.
################################################################################

################################################################################
# {download} tier
################################################################################
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
#' @details
#' EDDI uses the same native GRIDMET multi-year workflow as SPEI.
#' Use \code{eddi14d}, \code{eddi30d}, \code{eddi90d}, \code{eddi180d},
#' \code{eddi270d}, \code{eddi1y}, \code{eddi2y}, or \code{eddi5y};
#' \code{eddi} defaults to \code{eddi30d}. Both indices retain the native
#' 1/24-degree grid and five-day observation dates. Select dataset
#' \code{gridmet} in the wrapper functions to use these products; the
#' standalone drought source aliases continue to select their separate products.
#' GRIDMET SPEI is available directly from the provider as multi-year files
#' (one file per accumulation period), at native five-day intervals on the
#' approximately 4-km grid. Use \code{"spei14d"}, \code{"spei30d"},
#' \code{"spei90d"}, \code{"spei180d"}, \code{"spei270d"}, \code{"spei1y"},
#' \code{"spei2y"}, or \code{"spei5y"}; \code{"spei"} aliases \code{"spei30d"}.
#' For SPEI, \code{year} does not subset the download: the complete multi-year
#' file is saved under its resolved code directory (e.g., spei30d/spei30d.nc).
#' Subset dates with \code{process_gridmet()}. These files are several GB each.
#' This is distinct from the global CSIC SPEI product in
#' \code{download_drought()}.
#' See \url{https://www.northwestknowledge.net/metdata/data/}.
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
  year <- if (length(year) == 1) rep(year, 2) else year
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

    # SPEI is a single multi-year file per accumulation period.
    variable_years <- if (grepl("^(spei|eddi)", variable)) years[1] else years
    for (y in seq_along(variable_years)) {
      year_l <- variable_years[y]
      filename <- if (grepl("^(spei|eddi)", variable)) {
        paste0(variable, ".nc")
      } else {
        paste0(variable, "_", year_l, ".nc")
      }
      url <- paste0(base, filename)

      # Validate first URL only
      if (v == 1 && y == 1) {
        if (!amadeus::check_url_status(url)) {
          warning(
            paste0(
              "Preflight URL check failed; attempting direct download ",
              "for year validation.\n"
            ),
            call. = FALSE
          )
        }
      }

      destfile <- paste0(folder, filename)

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
  if (length(all_urls) > 0) {
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
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
      skipped = sum(ifelse(
        grepl("^(spei|eddi)", variables_list),
        1L,
        length(years)
      ))
    )
  }

  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process gridMET data
#' @description
#' The \code{process_gridmet()} function imports and cleans raw gridded surface meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [gridMET Generate Wget File](https://www.climatologylab.org/wget-gridmet.html)
#' for variable names and acronym codes. (Note: variable "Burning Index" has code "bi" and variable
#' "Energy Release Component" has code "erc").
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @details
#' EDDI uses the same native GRIDMET multi-year workflow as SPEI.
#' Use \code{eddi14d}, \code{eddi30d}, \code{eddi90d}, \code{eddi180d},
#' \code{eddi270d}, \code{eddi1y}, \code{eddi2y}, or \code{eddi5y};
#' \code{eddi} defaults to \code{eddi30d}. Both indices retain the native
#' 1/24-degree grid and five-day observation dates. Select dataset
#' \code{gridmet} in the wrapper functions to use these products; the
#' standalone drought source aliases continue to select their separate products.
#' GRIDMET SPEI uses native five-day observations, not daily values.
#' Use \code{variable = "spei"} (alias for \code{"spei30d"}) or an explicit
#' accumulation code: \code{"spei14d"}, \code{"spei30d"}, \code{"spei90d"},
#' \code{"spei180d"}, \code{"spei270d"}, \code{"spei1y"}, \code{"spei2y"},
#' or \code{"spei5y"}. Set \code{path} to the code directory created by
#' \code{download_gridmet()} (e.g., spei30d). The multi-year file is subset
#' using its observation dates; no daily interpolation is performed.
#' Layer names retain the accumulation period (e.g., spei30d_20200105).
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' gridmet <- process_gridmet(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/pr"
#' )
#' }
#' @export
# nolint end
process_gridmet <- function(
  date = c("2023-09-01", "2023-09-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  variable_checked <- amadeus::process_variable_codes(
    variables = variable,
    source = "gridmet"
  )
  variable_checked_long <- amadeus::process_gridmet_codes(
    variable_checked,
    invert = TRUE
  )
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable_checked,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### years of interest
  yoi <- unique(
    substr(
      date_sequence,
      1,
      4
    )
  )
  #### SPEI files span all years; annual covariates encode years in filenames.
  if (grepl("^(spei|eddi)", variable_checked)) {
    data_paths <- data_paths[
      basename(data_paths) == paste0(variable_checked, ".nc")
    ]
  } else {
    #### subset file paths to only dates of interest
    data_paths <- unique(
      grep(
        paste(yoi, collapse = "|"),
        data_paths,
        value = TRUE
      )
    )
  }
  if (length(data_paths) == 0) {
    stop(
      paste0(
        "No gridMET NetCDF files for variable '",
        variable_checked,
        "' and requested year(s) were found in `path`: ",
        path
      ),
      call. = FALSE
    )
  }
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    # Native drought files also contain a category raster. Read only the
    # continuous index, which carries its own CRS and time coordinates.
    if (grepl("^(spei|eddi)", variable_checked)) {
      index_name <- sub("[0-9].*$", "", variable_checked)
      data_year <- terra::rast(data_paths[p], subds = index_name, win = extent)
    } else {
      data_year <- terra::rast(data_paths[p], win = extent)
    }
    message(paste0(
      "Cleaning ",
      variable_checked,
      " data from ",
      basename(data_paths[p]),
      "...\n"
    ))

    existing_time <- terra::time(data_year)

    if (
      length(existing_time) == terra::nlyr(data_year) &&
        !anyNA(existing_time)
    ) {
      terra::time(data_year) <- existing_time
    } else {
      terra::time(data_year) <- amadeus::process_parse_ncdf_day_codes(
        layer_names = names(data_year),
        source = "gridmet",
        origin = "1900-01-01"
      )
    }

    names(data_year) <- paste0(
      variable_checked,
      "_",
      gsub(
        "-",
        "",
        terra::time(data_year)
      )
    )
    terra::varnames(data_year) <- variable_checked
    terra::longnames(data_year) <- variable_checked_long
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  selected_layers <- which(
    substr(
      names(data_full),
      nchar(names(data_full)) - 7,
      nchar(names(data_full))
    ) %in%
      date_sequence
  )
  if (length(selected_layers) == 0) {
    stop(
      paste0(
        "No gridMET layers fall within the requested date range ",
        date[1],
        " to ",
        date[2],
        "."
      ),
      call. = FALSE
    )
  }
  data_return <- terra::subset(data_full, selected_layers)
  message(paste0(
    "Returning ",
    variable_checked_long,
    " data from ",
    as.Date(
      date_sequence[1],
      format = "%Y%m%d"
    ),
    " to ",
    as.Date(
      date_sequence[length(date_sequence)],
      format = "%Y%m%d"
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

################################################################################
#' Process gridMET variable codes
#' @description
#' Convert gridMET variable names to/from variable codes.
#' @param string character(1). gridMET variable name or variable code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides variable code and returns full length variable name.
#' @keywords internal auxiliary
#' @return character
#' @export
process_gridmet_codes <-
  function(
    string,
    invert = FALSE
  ) {
    names <- c(
      paste0("Evaporative Demand Drought Index (",
             c("14 day", "30 day", "90 day", "180 day", "270 day",
               "1 year", "2 year", "5 year"), ")"),
      paste0(
        "Standardized Precipitation Evapotranspiration Index (",
        c("14 day", "30 day", "90 day", "180 day", "270 day",
          "1 year", "2 year", "5 year"),
        ")"
      ),
      "Near-Surface Specific Humidity",
      "Mean Vapor Pressure Deficit",
      "Precipitation",
      "Minimum Near-Surface Relative Humidity",
      "Maximum Near-Surface Relative Humidity",
      "Surface Downwelling Solar Radiation",
      "Minimum Near-Surface Air Temperature",
      "Maximum Near-Surface Air Temperature",
      "Wind speed at 10 m",
      "Wind direction at 10 m",
      "Palmer Drought Severity Index",
      "Reference grass evaportranspiration",
      "Reference alfalfa evaportranspiration",
      "Energy Release Component",
      "Burning Index",
      "100-hour dead fuel moisture",
      "1000-hour dead fuel moisture"
    )
    codes <- c(
      "eddi14d", "eddi30d", "eddi90d", "eddi180d", "eddi270d",
      "eddi1y", "eddi2y", "eddi5y",
      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d",
      "spei1y", "spei2y", "spei5y",
      "sph",
      "vpd",
      "pr",
      "rmin",
      "rmax",
      "srad",
      "tmmn",
      "tmmx",
      "vs",
      "th",
      "pdsi",
      "pet",
      "etr",
      "erc",
      "bi",
      "fm100",
      "fm1000"
    )
    names_codes <- cbind(tolower(names), codes)
    if (string == "all") {
      return(names_codes)
    }
    if (invert == FALSE) {
      name_code <- names_codes[names_codes[, 1] == tolower(string), ]
      return(name_code[2])
    } else if (invert == TRUE) {
      name_code <- names_codes[
        tolower(names_codes[, 2]) == tolower(string),
        ,
        drop = FALSE
      ]
      return(name_code[1])
    }
  }
################################################################################
