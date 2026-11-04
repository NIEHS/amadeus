################################################################################
# {amadeus} functions for Climatology Lab TerraClimate.
################################################################################

################################################################################
# {download} tier
################################################################################
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
          warning(
            paste0(
              "Preflight URL check failed; attempting direct download ",
              "for year validation.\n"
            ),
            call. = FALSE
          )
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

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process TerraClimate data
#' @description
#' The \code{process_terraclimate()} function imports and cleans climate and water balance
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [TerraClimate Direct Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym, year,
#' and month.
#' @note
#' TerraClimate data has monthly temporal resolution, so the first day of each month
#' is used as a placeholder temporal value.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' terraclimate <- process_terraclimate(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/ppt"
#' )
#' }
#' @export
# nolint end
process_terraclimate <- function(
  date = c("2023-09-01", "2023-09-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  date <- if (length(date) == 1) rep(date, 2) else date
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  variable_checked <- amadeus::process_variable_codes(
    variables = variable,
    source = "terraclimate"
  )
  variable_checked_long <- amadeus::process_terraclimate_codes(
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
  #### year-months of interest
  ymoi <- unique(
    substr(
      date_sequence,
      1,
      6
    )
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        yoi,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p], win = extent)
    message(paste0(
      "Cleaning monthly ",
      variable_checked_long,
      " data for ",
      substr(
        terra::time(data_year)[1],
        1,
        4
      ),
      "...\n"
    ))
    names(data_year) <- paste0(
      variable_checked,
      "_",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)
        ),
        1,
        6
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
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 5,
        nchar(names(data_full))
      ) %in%
        ymoi
    )
  )
  message(paste0(
    "Returning monthly ",
    variable_checked_long,
    " data from ",
    month.name[
      as.numeric(
        substr(
          ymoi[1],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[1],
      1,
      4
    ),
    " to ",
    month.name[
      as.numeric(
        substr(
          ymoi[length(ymoi)],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[length(ymoi)],
      1,
      4
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

################################################################################
#' Process terraClimate variable codes
#' @description
#' Convert terraClimate variable names to/from variable codes.
#' @param string character(1). terraClimate variable name or variable code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides variable code and returns full length variable name.
#' @keywords internal auxiliary
#' @return character
#' @export
process_terraclimate_codes <-
  function(
    string,
    invert = FALSE
  ) {
    names <- c(
      "Actual Evapotranspiration",
      "Climate Water Deficit",
      "Potential evapotranspiration",
      "Precipitation",
      "Runoff",
      "Soil Moisture",
      "Downward surface shortwave radiation",
      "Snow water equivalent - at end of month",
      "Max Temperature",
      "Min Temperature",
      "Vapor pressure",
      "Wind speed",
      "Vapor Pressure Deficit",
      "Palmer Drought Severity Index"
    )
    codes <- c(
      "aet",
      "def",
      "pet",
      "ppt",
      "q",
      "soil",
      "srad",
      "swe",
      "tmax",
      "tmin",
      "vap",
      "ws",
      "vpd",
      "PDSI"
    )
    names_codes <- cbind(tolower(names), codes)
    if (string == "all") {
      return(names_codes)
    }
    if (invert == FALSE) {
      name_code <- names_codes[names_codes[, 1] == tolower(string), ]
      return(name_code[2])
    } else if (invert == TRUE) {
      name_code <- names_codes[names_codes[, 2] == tolower(string), ]
      return(name_code[1])
    }
  }
################################################################################
