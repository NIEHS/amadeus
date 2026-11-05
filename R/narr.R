################################################################################
# {amadeus} functions for NOAA North American Regional Reanalysis (NARR).
################################################################################

################################################################################
# {download} tier
################################################################################
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
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeus:::download_check(
    dataset_name = "narr",
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
  checkmate::expect_character(variables, min.char = 1L)
  # ----------------------------------------------------------------------------
  # Check `years` and generate sequence.
  int_years <- amadeus:::sequence_years(year)
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- amadeus::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Store variables as `list`.
  list_variables <- as.vector(variables)
  # ----------------------------------------------------------------------------
  # Generate download URLs, file names, and variable-specific directories.
  df_download <- do.call(
    rbind,
    lapply(
      list_variables,
      function(x) {
        # ----------------------------------------------------------------------
        # Extract variable-specific URL base and months.
        list_narrvar <- amadeus::narr_variable(x)
        # ----------------------------------------------------------------------
        # Apply URL formula for all variables.
        # Multi-pressure level variables have monthly files.
        df_x <- expand.grid(
          base = list_narrvar[[1]],
          variable = x,
          year = int_years,
          month = list_narrvar[[2]],
          dir = chr_dir_to_save
        )
        df_x$file <- with(df_x, paste0(variable, ".", year, month, ".nc"))
        df_x$url <- with(df_x, paste0(base, file))
        df_x$vdir <- with(df_x, paste0(dir, variable, "/"))
        df_x$destfile <- with(df_x, paste0(vdir, file))
        df_x
      }
    )
  )
  # ----------------------------------------------------------------------------
  # Create missing directories.
  invisible(suppressWarnings(sapply(unique(df_download$vdir), dir.create)))
  # ----------------------------------------------------------------------------
  # Check first URL for validity. First only because sequential `int_years` will
  # ensure first URL is earliest year requested or only non-temporal SPEI/EDDI
  # request.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus::download_run_method(
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
#' Sort NOAA NARR variables
#' @description
#' Determine whether a NOAA NARR variable selected for download is a
#' monolevel or pressure level variable. Monolevel variables are derived
#' from https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/,
#' and pressure level variables are derived from
#' https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/.
#' @param variable character(1). User-selected NARR variable
#' @return list with URL base and vector of months (blank for monolevel)
#' @keywords internal auxiliary
#' @export
narr_variable <- function(variable) {
  stopifnot(length(variable) == 1)
  mono <- c(
    "acpcp",
    "air.2m",
    "air.sfc",
    "albedo",
    "apcp",
    "bgrun",
    "bmixl.hl1",
    "cape",
    "ccond",
    "cdcon",
    "cdlyr",
    "cfrzr",
    "cicep",
    "cin",
    "cnwat",
    "crain",
    "csnow",
    "dlwrf",
    "dpt.2m",
    "dswrf",
    "evap",
    "gflux",
    "hcdc",
    "hgt.tropo",
    "hlcy",
    "hpbl",
    "lcdc",
    "lftx4",
    "lhtfl",
    "mcdc",
    "mconv.hl1",
    "mslet",
    "mstav",
    "pevap",
    "pottmp.hl1",
    "pottmp.sfc",
    "prate",
    "pres.sfc",
    "pres.tropo",
    "prmsl",
    "pr_wtr",
    "rcq",
    "rcs",
    "rcsol",
    "rct",
    "rhum.2m",
    "shtfl",
    "shum.2m",
    "snod",
    "snohf",
    "snom",
    "snowc",
    "soilm",
    "ssrun",
    "tcdc",
    "tke.hl1",
    "ulwrf.ntat",
    "ulwrf.sfc",
    "ustm",
    "uswrf.ntat",
    "uswrf.sfc",
    "uwnd.10m",
    "veg",
    "vis",
    "vstm",
    "vvel.hl1",
    "vwnd.10m",
    "vwsh.tropo",
    "wcconv",
    "wcinc",
    "wcuflx",
    "wcvflx",
    "weasd",
    "wvconv",
    "wvinc",
    "wvuflx",
    "wvvflx"
  )
  pressure <- c("air", "hgt", "omega", "shum", "tke", "uwnd", "vwnd")
  soil <- c("soill", "soilw", "tsoil")
  base <- "https://downloads.psl.noaa.gov/NARR/Dailies/"
  if (variable %in% mono) {
    base <- paste0(base, "monolevel/")
    months <- ""
  } else {
    months <- sprintf("%02d", seq(1, 12, by = 1))
    if (variable %in% pressure) {
      base <- paste0(base, "pressure/")
    } else if (variable %in% soil) {
      base <- paste0(base, "subsurface/")
    } else {
      stop(
        paste0(
          "Selected variable \"",
          variable,
          "\" is not available.\n"
        )
      )
    }
  }
  return(list(base, months))
}

################################################################################
# {process} tier
################################################################################
# nolint start
#' Process meteorological data
#' @description
#' The \code{process_narr()} function imports and cleans raw meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name acronym. See [List of Variables in NARR Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' pressure level, and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @importFrom stringi stri_pad
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' process_narr(
#'   date = c("2018-01-01", "2018-01-10"),
#'   variable = "weasd",
#'   path = "./tests/testdata/narr/weasd"
#' )
#' }
#' @export
# nolint end
process_narr <- function(
  date = "2023-09-01",
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
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
    recursive = TRUE,
    full.names = TRUE
  )
  data_paths <- grep(
    sprintf("%s*.*.nc", variable),
    data_paths,
    value = TRUE
  )
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### path ncar
  path_ncar <- nchar(
    strsplit(
      data_paths[1],
      paste0(
        variable,
        "."
      )
    )[[1]][3]
  )
  if (path_ncar == 9) {
    #### define year-months of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        6
      )
    )
  } else if (path_ncar == 7) {
    #### define years of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        4
      )
    )
  }
  #### subset file paths to only dates of interest
  data_paths_ym <- unique(
    grep(
      paste(
        ym_of_interest,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths_ym)) {
    #### import data
    data_year <- terra::rast(data_paths_ym[p], win = extent)
    #### check for mono or pressure levels
    if (grepl("level", names(data_year)[1])) {
      #### pressure levels data
      message(paste0("Detected pressure levels data...\n"))
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
        month.name[
          as.numeric(
            substr(
              gsub(
                "-",
                "",
                terra::time(data_year)[1]
              ),
              5,
              6
            )
          )
        ],
        ", ",
        substr(
          gsub(
            "-",
            "",
            terra::time(data_year)[1]
          ),
          1,
          4
        ),
        "...\n"
      ))
      days <- sapply(
        strsplit(
          names(data_year),
          "_"
        ),
        function(x) x[3]
      )
      terra::time(data_year) <- as.Date(
        paste0(
          substr(
            terra::time(data_year),
            1,
            4
          ),
          substr(
            gsub(
              "-",
              "",
              terra::time(data_year)
            ),
            5,
            6
          ),
          days
        ),
        format = "%Y%m%d"
      )
      names(data_year) <- paste0(
        variable,
        "_",
        sapply(
          strsplit(
            names(data_year),
            "_"
          ),
          function(x) x[2]
        ),
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    } else {
      #### mono level data
      message(paste0("Detected monolevel data...\n"))
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
        substr(
          gsub(
            "-",
            "",
            terra::time(data_year)[1]
          ),
          1,
          4
        ),
        "...\n"
      ))

      names(data_year) <- paste0(
        variable,
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    }
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
        nchar(names(data_full)) - 7,
        nchar(names(data_full))
      ) %in%
        date_sequence
    )
  )
  message(paste0(
    "Returning daily ",
    variable,
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
# {calculate} tier
################################################################################
#' Calculate meteorological covariates
#' @description
#' Extract meteorological values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, date, vertical pressure level, and
#' meteorological variable. Meteorological variable column name reflects
#' variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_narr()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#' when \code{.by_time} is provided.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [`process_narr`]
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
#' calculate_narr(
#'   from = narr, # derived from process_narr() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_narr <- function(
  from,
  locs,
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
  #### identify pressure level or monolevel data
  if (grepl("level", names(from)[1])) {
    narr_time <- 3
    narr_level <- 2
  } else {
    narr_time <- 2
    narr_level <- NULL
  }
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "narr",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = narr_time,
    time_type = "date",
    level = narr_level,
    weights = weights,
    ...
  )
  narr_group_extra <- if (!is.null(narr_level)) "level" else NULL
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = narr_group_extra
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
    }
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}
################################################################################
