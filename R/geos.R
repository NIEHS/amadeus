################################################################################
# {amadeus} functions for NASA Global Earth Observing System (GEOS).
################################################################################

################################################################################
# {download} tier
################################################################################
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
#' @param hours integer(1 to 24). Hours of day to download for each date,
#' in UTC. Supply values from 0 through 23. Default is `0:23`, which selects
#' all hours. The collection determines the minute within each selected hour.
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
#' @details
#' Hour selection uses the timestamps of the requested collection. Hourly
#' time-averaged collections (`_tavg_1hr_`) use timestamps at 30 minutes past
#' the hour, while hourly instantaneous collections (`_inst_1hr_`) use
#' timestamps on the hour. For example, `hours = c(0, 6)` selects `00:30`
#' and `06:30` UTC for time-averaged collections, or `00:00` and `06:00` UTC
#' for instantaneous collections. The same hour selection applies to every
#' date in the requested range.
#' @examples
#' \dontrun{
#' download_geos(
#'   collection = "aqc_tavg_1hr_g1440x721_v1",
#'   date = "2024-01-01",
#'   hours = c(0, 6, 12, 18),
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
  hours = 0:23,
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
    dataset_name = "geos",
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
  checkmate::assert_character(collection, min.char = 1L)
  checkmate::assert_subset(
    collection,
    c(
      "aqc_tavg_1hr_g1440x721_v1",
      "chm_tavg_1hr_g1440x721_v1",
      "met_tavg_1hr_g1440x721_x1",
      "xgc_tavg_1hr_g1440x721_x1",
      "chm_inst_1hr_g1440x721_p23",
      "met_inst_1hr_g1440x721_p23"
    )
  )
  # ----------------------------------------------------------------------------
  # Check NASA EarthData token.
  chr_nasatoken <- amadeus:::get_token(
    token = nasa_earth_data_token,
    env_var = "NASA_EARTHDATA_TOKEN"
  )
  checkmate::assert_string(chr_nasatoken, min.chars = 1L)
  # ----------------------------------------------------------------------------
  # Check `date` and generate sequence.
  chr_dates <- amadeus:::sequence_dates(date, hyphen = FALSE)
  # ----------------------------------------------------------------------------
  # Generate saving directory.
  chr_dir_to_save <- amadeus:::download_setup_dir(directory_to_save)
  # ----------------------------------------------------------------------------
  # Store collections as a `list`.
  list_collections <- as.list(collection)
  # ----------------------------------------------------------------------------
  # Generate download URLs, file names, and collection-specific directories.
  df_download <- do.call(
    rbind,
    lapply(
      list_collections,
      function(x) {
        # ----------------------------------------------------------------------
        # Set collection-specific time sequence (e.g. 0000 vs 0030).
        int_offset <- if (grepl("_tavg_1hr_", x)) 30L else 0L
        chr_times <- amadeus:::sequence_times(
          by = 60L,
          offset = int_offset,
          hours = hours
        )
        # ----------------------------------------------------------------------
        # Apply URL formula for all variables.
        df_x <- expand.grid(
          base = "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/",
          collection = x,
          date = chr_dates,
          time = chr_times,
          dir = chr_dir_to_save
        )
        df_x$file <- with(
          df_x,
          paste0("GEOS-CF.v01.rpl.", collection, ".", date, "_", time, "z.nc4")
        )
        df_x$url <- with(
          df_x,
          paste0(
            base,
            "Y",
            substr(date, 1, 4),
            "/M",
            substr(date, 5, 6),
            "/D",
            substr(date, 7, 8),
            "/",
            file
          )
        )
        df_x$vdir <- with(df_x, paste0(chr_dir_to_save, collection, "/"))
        df_x$destfile <- with(df_x, paste0(vdir, file))
        df_x
      }
    )
  )
  # ----------------------------------------------------------------------------
  # Create missing directories.
  invisible(suppressWarnings(sapply(unique(df_download$vdir), dir.create)))
  # ----------------------------------------------------------------------------
  # Check first URL for validity.
  amadeus:::check_preflight(df_download$url[1])
  # ----------------------------------------------------------------------------
  # Exit early if download = FALSE. !! INVESTIGATE FOR DEPRECATION. !!
  if (!isTRUE(download)) {
    return(invisible(amadeus:::download_preview(
      df_download$url,
      df_download$destfile
    )))
  }
  # ----------------------------------------------------------------------------
  # Run download.
  download_result <- amadeus:::download_run_method(
    urls = df_download$url,
    destfiles = df_download$destfile,
    token = chr_nasatoken,
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
#' Process atmospheric composition data
#' @description
#' The \code{process_geos()} function imports and cleans raw atmospheric
#' composition data, returning a single `SpatRaster` object.
#' @details
#' GEOS-CF netCDF collections currently supported by \code{download_geos()} are:
#' \code{"aqc_tavg_1hr_g1440x721_v1"},
#' \code{"chm_tavg_1hr_g1440x721_v1"},
#' \code{"met_tavg_1hr_g1440x721_x1"},
#' \code{"xgc_tavg_1hr_g1440x721_x1"},
#' \code{"chm_inst_1hr_g1440x721_p23"}, and
#' \code{"met_inst_1hr_g1440x721_p23"}.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). GEOS-CF variable name(s). See \emph{Notes}
#'   for collection-specific variable-name guidance.
#' @param path character(1). Directory with downloaded netCDF (.nc4) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves the original hourly output.
#'   Aggregation groups layers by variable/level and date so that pressure-level
#'   structure is preserved. Not meaningful for collections that are already
#'   daily.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
#'   `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour when `daily_agg = FALSE` (default). When
#' `daily_agg = TRUE`, layer names contain the variable, pressure level, and
#' date only, and `terra::time()` is set to midnight UTC of each date.
#'
#' Collection-specific variable names accepted by \code{variable}:
#' \tabular{ll}{
#' \strong{Collection}\tab \strong{Variables}\cr
#' \code{aqc_tavg_1hr_g1440x721_v1}\tab
#' \code{no2}, \code{co}, \code{so2}, \code{pm25_rh35_gcc}, \code{o3}\cr
#' \code{chm_tavg_1hr_g1440x721_v1}\tab
#' \code{ocpi}, \code{bcpo}, \code{pm25soa_rh35_gc}, \code{dst4}, \code{prpe},
#' \code{macr}, \code{pm25ss_rh35_gcc}, \code{hno4}, \code{ch4}, \code{nh3},
#' \code{h2o2}, \code{rcho}, \code{hno3}, \code{dst1}, \code{pan},
#' \code{pm25oc_rh35_gcc}, \code{c3h8}, \code{soas}, \code{no}, \code{tolu},
#' \code{mvk}, \code{xyle}, \code{isop}, \code{noy}, \code{sala}, \code{so2},
#' \code{co}, \code{n2o5}, \code{eoh}, \code{o3}, \code{acet}, \code{c2h6},
#' \code{mek}, \code{nit}, \code{benz}, \code{soap}, \code{alk4}, \code{ocpo},
#' \code{ald2}, \code{hcho}, \code{pm25_rh35_gocar}, \code{dst3},
#' \code{pm25su_rh35_gcc}, \code{pm25_rh35_gcc}, \code{pm25ni_rh35_gcc},
#' \code{pm25bc_rh35_gcc}, \code{dst2}, \code{pm25du_rh35_gcc}, \code{bcpi},
#' \code{no2}, \code{salc}, \code{nh4}\cr
#' \code{met_tavg_1hr_g1440x721_x1}\tab
#' \code{zl}, \code{zpbl}, \code{ps}, \code{v2m}, \code{v}, \code{q2m},
#' \code{u}, \code{t2m}, \code{troppb}, \code{q}, \code{t}, \code{v10m},
#' \code{t10m}, \code{u2m}, \code{q10m}, \code{ts}, \code{slp}, \code{cldtt},
#' \code{phis}, \code{tprec}, \code{u10m}, \code{rh}\cr
#' \code{xgc_tavg_1hr_g1440x721_x1}\tab
#' \code{wetdepflx_nh4}, \code{aod550_dst6}, \code{wetdepflx_dst1},
#' \code{tropcol_io}, \code{totcol_o3}, \code{tropcol_hcho},
#' \code{drydepflx_bcpi}, \code{aod550_cloud}, \code{aod550_dst5},
#' \code{wetdepflx_hcho}, \code{aod550_salc}, \code{aod550_dust},
#' \code{wetdepflx_so2}, \code{wetdepflx_salc}, \code{wetdepflx_dst3},
#' \code{drydepflx_nit}, \code{wetdepflx_so4}, \code{aod550_sala},
#' \code{aod550_dst1}, \code{tropcol_co}, \code{wetdepflx_bcpi},
#' \code{drydepflx_sala}, \code{wetdepflx_nh3}, \code{tropcol_no2},
#' \code{wetdepflx_nit}, \code{aod550_sulfate}, \code{wetdepflx_ocpi},
#' \code{drydepflx_hcho}, \code{drydepflx_dst4}, \code{tropcol_so2},
#' \code{drydepflx_ocpi}, \code{tropcol_o3}, \code{drydepflx_nh4},
#' \code{aod550_dst7}, \code{totcol_co}, \code{totcol_so2}, \code{totcol_io},
#' \code{drydepflx_nh3}, \code{wetdepflx_sala}, \code{wetdepflx_dst4},
#' \code{drydepflx_o3}, \code{drydepflx_hno3}, \code{aod550_dst4},
#' \code{aod550_oc}, \code{totcol_no2}, \code{drydepflx_dst2},
#' \code{tropcol_bro}, \code{wetdepflx_bcpo}, \code{drydepflx_bcpo},
#' \code{wetdepflx_dst2}, \code{drydepflx_dst1}, \code{aod550_dst2},
#' \code{aod550_bc}, \code{aod550_dst3}, \code{wetdepflx_ocpo},
#' \code{drydepflx_dst3}, \code{drydepflx_salc}, \code{wetdepflx_hno3},
#' \code{drydepflx_ocpo}, \code{drydepflx_no2}, \code{totcol_hcho},
#' \code{totcol_bro}\cr
#' \code{chm_inst_1hr_g1440x721_p23}\tab
#' \code{pm25soa_rh35_gc}, \code{pm25ss_rh35_gcc}, \code{so2}, \code{co},
#' \code{o3}, \code{pm25oc_rh35_gcc}, \code{pm25du_rh35_gcc}, \code{noy},
#' \code{no2}, \code{pm25ni_rh35_gcc}, \code{pm25bc_rh35_gcc},
#' \code{pm25_rh35_gcc}, \code{pm25su_rh35_gcc}\cr
#' \code{met_inst_1hr_g1440x721_p23}\tab
#' \code{omega}, \code{t}, \code{eth}, \code{q}, \code{epv}, \code{rh},
#' \code{slp}, \code{airdens}, \code{ps}, \code{h}, \code{th}, \code{v},
#' \code{u}, \code{airvol_chem}\cr
#' }
#'
#' \code{variable} matching is case-insensitive (for example, \code{"o3"}
#' matches \code{"O3"}).
#'
#' Reference: NASA GEOS-CF OpenDAP catalog
#' <https://opendap.nccs.nasa.gov/dods/gmao/geos-cf/assim>.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra tapp
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' geos <- process_geos(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "O3",
#'   path = "./data/aqc_tavg_1hr_g1440x721_v1"
#' )
#' ## daily mean across all sub-daily layers per variable/level
#' geos_daily <- process_geos(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "O3",
#'   path = "./data/aqc_tavg_1hr_g1440x721_v1",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
process_geos <-
  function(
    date = c("2018-01-01", "2018-01-10"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    daily_agg = FALSE,
    fun = "mean",
    ...
  ) {
    #### directory setup
    path <- amadeus:::download_sanitize_path(path)
    #### check for variable
    amadeus:::check_for_null_parameters(mget(ls()))
    if (!is.character(variable) || length(variable) != 1 || !nzchar(variable)) {
      stop("`variable` must be a single non-empty character string.\n")
    }
    variable <- trimws(variable)
    #### check dates
    date <- if (length(date) == 1) rep(date, 2) else date
    stopifnot(length(date) == 2)
    date <- date[order(as.Date(date))]
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "GEOS-CF.v01.rpl",
      full.names = TRUE,
      recursive = TRUE
    )
    paths <- grep(".nc4$", paths, value = TRUE)

    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
    )
    #### subset file paths to only dates of interest
    data_paths <- unique(
      grep(
        paste(
          dates_of_interest,
          collapse = "|"
        ),
        paths,
        value = TRUE
      )
    )
    #### identify collection
    collection <- amadeus::process_collection(
      data_paths[1],
      source = "geos",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_list <- vector("list", length(data_paths))
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p])
      data_datetime <- amadeus::process_collection(
        data_paths[p],
        source = "geos",
        datetime = TRUE
      )
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        "...\n"
      ))
      #### subset to user-selected variable
      data_variable_idx <- grep(
        tolower(variable),
        tolower(names(data_raw)),
        fixed = TRUE
      )
      if (length(data_variable_idx) == 0) {
        available_base <- sort(unique(sub("_lev=.*$", "", names(data_raw))))
        stop(
          paste0(
            "Variable '",
            variable,
            "' was not found in collection ",
            collection,
            ".\n",
            "Available case-sensitive variables include: ",
            paste(available_base, collapse = ", "),
            ".\n"
          )
        )
      }
      data_variable <- terra::subset(
        data_raw,
        subset = data_variable_idx
      )
      matched_base <- sort(unique(sub("_lev=.*$", "", names(data_variable))))
      if (!(variable %in% matched_base)) {
        message(
          "Requested variable '",
          variable,
          "' matched case-sensitive variable(s): ",
          paste(matched_base, collapse = ", "),
          ".\n"
        )
      }
      #### define variable time
      terra::time(data_variable) <- rep(
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        terra::nlyr(data_variable)
      )
      #### define variable name based on date and time
      names(data_variable) <- paste0(
        names(data_variable),
        "_",
        gsub(
          ":",
          "",
          gsub(
            "-",
            "",
            gsub(" ", "_", terra::time(data_variable))
          )
        )
      )
      if (substr(data_datetime, 9, 12) == "0000") {
        names(data_variable) <- paste0(
          names(data_variable),
          "_000000"
        )
      }
      #### store for one-shot stack creation
      data_list[[p]] <- data_variable
    }
    #### combine all layers in one call
    data_return <- terra::rast(data_list)
    #### set coordinate reference system
    terra::crs(data_return) <- "EPSG:4326"
    #### optional daily aggregation
    if (isTRUE(daily_agg)) {
      t <- terra::time(data_return)
      if (!anyNA(t) && length(t) == terra::nlyr(data_return)) {
        date_str <- format(as.Date(t), "%Y%m%d")
      } else {
        # nocov start
        date_str <- regmatches(
          names(data_return),
          regexpr(
            "(?<![0-9])[0-9]{8}(?![0-9])",
            names(data_return),
            perl = TRUE
          )
        )
        if (length(date_str) != terra::nlyr(data_return)) {
          stop("daily_agg: cannot determine dates from layer times or names.\n")
        }
      } # nocov end
      var_prefix <- sub("_[0-9]{8}.*$", "", names(data_return))
      tapp_index <- paste(var_prefix, date_str, sep = "_")
      if (length(unique(tapp_index)) == 1L) {
        out_name <- unique(tapp_index)
        data_return <- terra::app(data_return, fun = fun, na.rm = TRUE)
        names(data_return) <- out_name
      } else {
        data_return <- terra::tapp(
          data_return,
          tapp_index,
          fun = fun,
          na.rm = TRUE
        )
      }
      terra::crs(data_return) <- "EPSG:4326"
      out_dates <- regmatches(
        names(data_return),
        regexpr("[0-9]{8}$", names(data_return))
      )
      terra::time(data_return) <- as.POSIXct(
        paste0(out_dates, " 00:00:00"),
        format = "%Y%m%d %H:%M:%S",
        tz = "UTC"
      )
    }
    message(paste0(
      "Returning ",
      ifelse(isTRUE(daily_agg), "daily ", "hourly "),
      variable,
      " data from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ".\n"
    ))
    #### return SpatRaster
    return(data_return)
  }

################################################################################
#' Get GEOS variable lookup information
#' @description
#' Returns a lookup table of available GEOS collection and variable selectors
#' from locally downloaded GEOS-CF netCDF files. This helper inspects layer
#' metadata only and does not read raster values into memory.
#' @param path character(1+) Path(s) to GEOS file(s) and/or directory(ies)
#'   containing GEOS-CF `.nc4` files.
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each collection-variable row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with GEOS collection and variable selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_geos_info(path = "./data/geos")
#' get_geos_info(path = "./data/geos", include_file = TRUE)
#' }
#' @importFrom terra rast
#' @export
get_geos_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "GEOS-CF\\.v01\\.rpl.*\\.nc4$",
    source_name = "GEOS-CF .nc4"
  )

  out_rows <- lapply(files, function(f) {
    data_raw <- terra::rast(f)
    vars <- info_normalize_layer_variables(names(data_raw))
    collection <- unique(amadeus::process_collection(
      f,
      source = "geos",
      collection = TRUE
    ))
    if (length(vars) == 0L || length(collection) == 0L) {
      return(NULL)
    }
    row <- data.frame(
      collection = rep(collection[1], length(vars)),
      variable = vars,
      file = rep(f, length(vars)),
      stringsAsFactors = FALSE
    )
    row
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop("No GEOS collection-variable metadata could be derived from `path`.\n")
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("collection", "variable"), drop = FALSE])
    out <- out[order(out$collection, out$variable), , drop = FALSE]
  } else {
    out <- unique(out[, c("collection", "variable", "file"), drop = FALSE])
    out <- out[order(out$collection, out$variable, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

################################################################################
# {calculate} tier
################################################################################
#' Calculate atmospheric composition covariates
#' @description
#' Extract atmospheric composition values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and atmospheric composition variable. Atmospheric
#' composition variable column name reflects variable and circular buffer
#' radius.
#' @param from SpatRaster(1). Output of \code{process_geos()}.
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
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @seealso [process_geos()]
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect
#' @importFrom terra buffer
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
#' calculate_geos(
#'   from = geos, # derived from process_geos() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_geos <- function(
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
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "geos",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = c(3, 4),
    time_type = "hour",
    level = 2,
    weights = weights,
    ...
  )
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = "level"
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }
  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
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
