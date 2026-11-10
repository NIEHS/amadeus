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
#' @param year integer(1 or more). One year, two endpoints, or a non-empty
#'   vector of years. Every year from the minimum through the maximum is
#'   requested, including gaps: `c(2018, 2023)` requests `2018:2023`.
#'   Default is `c(2018, 2022)`. Missing, fractional, and future years are
#'   rejected.
#' @param directory_to_save character(1). Directory to save data.
#' @param acknowledgement logical(1). Must be TRUE to proceed.
#' @param download logical(1). DEPRECATED. Downloads happen automatically.
#' @param remove_command logical(1). Deprecated, ignored.
#' @param show_progress logical(1). Show download progress (default TRUE)
#' @param hash logical(1). Return hash of downloaded files (default FALSE)
#' @param max_tries integer(1). Maximum retry attempts (default 20)
#' @param rate_limit numeric(1). Minimum seconds between requests (default 2)
#' @author Mitchell Manware, Insang Song
#' @return With `download = FALSE`, an invisible preview list containing
#' `urls`, `destfiles`, and `n_files`. With `download = TRUE` and
#' `hash = FALSE`, an invisible summary from [download_run_method()] containing
#' `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`. The two
#' failure vectors are `character(0)` when no downloads failed, including when
#' every file was skipped. With `download = TRUE` and `hash = TRUE`, a visible
#' character scalar containing the combined MD5 hash of the output directory.
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
  # ----------------------------------------------------------------------------
  # Check common arguments.
  amadeus:::download_check(
    dataset_name = "terraclimate",
    acknowledgement = acknowledgement,
    download = download,
    remove_command = remove_command,
    unzip = FALSE,
    remove_zip = FALSE,
    directory_to_save = directory_to_save,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit,
    hash = hash
  )
  # ----------------------------------------------------------------------------
  # Check source-specific arguments.
  checkmate::assert_character(variables, min.char = 1L)
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- amadeus:::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Switch variables names to codes.
  chr_variables <- amadeus::process_variable_codes(
    variables = variables,
    source = "terraclimate"
  )
  # ----------------------------------------------------------------------------
  # Apply URL formula for all variables.
  df_download <- expand.grid(
    base = paste0(
      "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/",
      "TerraClimate_"
    ),
    variable = chr_variables,
    years = int_years,
    dir = chr_dir_to_save
  )
  # ----------------------------------------------------------------------------
  # Generate download URLs, file names, and variable-specific directories.
  df_download$file <- with(df_download, paste0(variable, "_", years, ".nc"))
  df_download$url <- with(df_download, paste0(base, file))
  df_download$vdir <- with(df_download, paste0(dir, variable, "/"))
  df_download$destfile <- with(df_download, paste0(vdir, file))
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = NULL,
    show_progress = show_progress,
    max_tries = max_tries,
    rate_limit = rate_limit
  )
  # ----------------------------------------------------------------------------
  # Return results or hash.
  amadeus:::download_return(download_result, hash, chr_dir_to_save)
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
  path <- amadeus:::download_sanitize_path(path)
  #### check for variable
  amadeus:::check_for_null_parameters(mget(ls()))
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
# {calculate} tier
################################################################################
#' Calculate TerraClimate covariates
#' @description
#' Extract TerraClimate values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and TerraClimate variable. TerraClimate
#' variable column name reflects the TerraClimate variable and
#' circular buffer radius. The `$time` column will contain the year and month
#' ("YYYYMM") as TerraClimate products have monthly temporal resolution.
#' @param from SpatRaster(1). Output from \code{process_terraclimate()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @note
#' TerraClimate data has monthly temporal resolution, so the `$time` column
#' will contain the year and month in YYYYMM format (ie. January, 2018 =
#' 201801).
#' @author Mitchell Manware
#' @seealso [`process_terraclimate()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_terraclimate <- function(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "terraclimate",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = 2,
    time_type = "yearmonth",
    weights = weights,
    ...
  )
  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}
################################################################################
