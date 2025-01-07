#' Set column names
#' @description
#' Apply standard column names to calculated covariates consistent with
#' the requirements of the \code{beethoven} package. Column names follow fixed
#' format of 3 character data genre, 2 - 15 character variable code, 1 digit
#' temporal lag, and 5 digit buffer radius (in meters). Variable code character
#' range is required to retain interpretable column names across datasets.
#' @note
#' \code{beethoven} utilizes point, 1km, and 10km radius buffer distance for
#' covariate calculation, and therefore the buffer radius column is
#' padded to 5 digits. If provided a buffer radius greater than 5 digits,
#'  \code{calc_setcolumns()} will expand to the number of digits. (ie. buffer
#' radius of 100km = CCC_CCCCC_I_100000).
#' @param from data.frame(1) or SpatVector(1). Calculated covariates as
#' returned from \code{calc_covariates()} or a source specific covariate
#' function.
#' @param lag integer(1). Temporal lag.
#' @param dataset character(1). Covariate parent dataset.
#' @param locs_id character(1). Column containing identifier for each unique
#' coordinate location.
#' @keywords internal auxiliary
#' @importFrom stringi stri_pad
#' @return a data.frame or SpatVector object (depending on `from`)
#' @export
calc_setcolumns <- function(
    from,
    lag,
    dataset,
    locs_id) {
  #### check from is data.frame
  stopifnot(class(from) %in% c("data.frame", "SpatVector"))
  #### original names
  names_from <- colnames(from)
  #### copy
  names_return <- names_from
  #### identifier
  id_index <- which(names_from == locs_id)
  names_return[id_index] <- locs_id
  #### time
  time_index <- which(
    names_from %in% c("time", "year", "date", "hour")
  )
  stopifnot(length(time_index) <= 1)
  names_return[time_index] <- "time"
  #### description (for time period coverage)
  description_index <- which(names_from == "description")
  stopifnot(length(description_index) <= 1)
  names_return[description_index] <- "description"
  #### geometry
  geometry_index <- which(names_from == "geometry")
  stopifnot(length(geometry_index) <= 1)
  names_return[geometry_index] <- "geometry"
  #### latitude and longitude
  lat_index <- which(
    tolower(names_from) %in% c("lat", "latitude")
  )
  stopifnot(length(lat_index) <= 1)
  names_return[lat_index] <- "lat"
  lon_index <- which(
    tolower(names_from) %in% c("lon", "longitude")
  )
  stopifnot(length(lon_index) <= 1)
  names_return[lon_index] <- "lon"
  #### vertical pressure level
  level_index <- which(names_from == "level")
  stopifnot(length(level_index) <= 1)
  names_return[level_index] <- "level"
  #### dataset and genre
  datasets <- c(
    "aqs", "ecoregions", "geos", "gmted", "koppen geiger", "merra2", "modis",
    "narr", "nlcd", "hms", "groads", "pop", "tri", "nei", "gridmet",
    "terraclimate"
  )
  stopifnot(dataset %in% datasets)
  genre <- substr(dataset, 1, 3)
  #### covariates
  cov_index <- which(
    !(c(names_from %in% c(
      locs_id, "geometry", "time", "lat", "lon", "level", "description"
    ))
    )
  )
  for (c in seq_along(cov_index)) {
    name_covariate <- names_return[cov_index[c]]
    name_split <- strsplit(name_covariate, "_")[[1]]
    name_variable <- tolower(name_split[1])
    name_variable <- gsub(
      "\\.",
      "",
      gsub(
        "-",
        "",
        gsub(
          " ",
          "",
          name_variable
        )
      )
    )
    nchar_radius <- nchar(as.character(name_split[2]))
    width_radius <- ifelse(nchar_radius > 5, nchar_radius, 5)
    name_new <- paste0(
      genre,
      "_",
      strsplit(
        name_variable,
        1,
        15
      ),
      "_",
      lag,
      "_",
      stringi::stri_pad(
        name_split[2],
        width = width_radius,
        pad = 0,
        side = "left"
      )
    )
    names_return[cov_index[c]] <- toupper(name_new)
  }
  #### check for unique names
  stopifnot(length(names_return) == length(unique(names_return)))
  return(from)
}

#' Send progress messages
#' @description
#' Send messages updating covariate extraction progress.
#' @param dataset character(1). Data source.
#' @param variable placeholder
#' @param time placeholder
#' @param time_type placeholder
#' @param level placeholder
#' @return NULL; provides progress messages to R console.
#' @keywords internal auxiliary
#' @export
calc_message <- function(
    dataset,
    variable,
    time,
    time_type,
    level) {
  message_time <- calc_time(time, time_type)
  if (dataset == "skip") {
    return()
  }
  if (dataset == "gmted") {
    return_message <- paste0(
      "Calculating ",
      process_gmted_codes(
        substr(
          variable,
          1,
          2
        ),
        statistic = TRUE,
        invert = TRUE
      ),
      " covariates with ",
      process_gmted_codes(
        substr(
          variable,
          3,
          4
        ),
        resolution = TRUE,
        invert = TRUE
      ),
      " resolution data.\n"
    )
  } else {
    if (is.null(level)) {
      return_message <- paste0(
        "Calculating ",
        variable,
        " covariates for ",
        message_time,
        "...\n"
      )
    } else {
      return_message <- paste0(
        "Calculating ",
        variable,
        " covariates at ",
        level,
        " for ",
        message_time,
        "...\n"
      )
    }
  }
  message(return_message)
}

#' Prepare extraction locations
#' @description
#' Prepare the point locations for extracting data by transforming `locs` to
#' a `SpatVector`, projecting to the coordinate reference system of `from`,
#' and creating a `data.frame` containing `locs_id` for retaining extracted
#' values.
#' @param from SpatRaster(1) or SpatVector(1). Output from
#' \code{process_\*()}. Passed from \code{calc_\*()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' Passed from \code{calc_\*()}.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' Passed from \code{calc_\*()}.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0). Passed from \code{calc_\*()}.
#' @param geom logical(1). Should the geometry of `locs` be returned in the
#' `data.frame`? Default is `FALSE`, options "sf" or "terra" will preserve
#' geometry, but will use `terra` for extraction.
#' @param keep_cols logical(1). Should all columns in `locs` be retained in the
#' returned `data.frame`? Default is `FALSE`.
#' @return A `list` containing `SpatVector` and `data.frame` objects
#' @seealso [`process_locs_vector()`], [`check_for_null_parameters()`]
#' @keywords internal auxiliary
#' @importFrom terra as.data.frame
#' @importFrom terra crs
#' @export
calc_prepare_locs <- function(
    from,
    locs,
    locs_id,
    radius,
    geom = FALSE,
    keep_cols = FALSE) {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n",
                 locs_id)
    )
  }

  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )

  #### check geom
  check_geom(geom)
  if (geom %in% c("sf", "terra")) geom <- TRUE

  #### retaine or drop columns
  stopifnot(methods::is(keep_cols, "logical"))
  if (keep_cols) {
    keep_names <- names(locs)
  } else {
    keep_names <- locs_id
  }

  #### site identifiers and geometry
  if (geom) {
    sites_id <- subset(
      terra::as.data.frame(sites_e, geom = "WKT"),
      select = c(keep_names, "geometry")
    )
  } else {
    #### site identifiers only
    sites_id <- subset(
      terra::as.data.frame(sites_e),
      select = keep_names
    )
  }

  return(list(sites_e, sites_id))
}

#' Prepare time values
#' @description
#' Prepare the time values for covariate calculation based on type of time
#' value.
#' @param time Time value
#' @param format Type of time to return in the `$time` column. Can be
#' "timeless" (ie. Ecoregions data), "date" (ie. NARR data), "hour"
#' (ie. GEOS data), "year" (ie. SEDAC population data), or "yearmonth"
#' (ie. TerraClimate data).
#' @return a `Date`, `POSIXt`, or `integer` object based on `format =`
#' @keywords internal auxiliary
#' @export
calc_time <- function(
    time,
    format) {
  if (format == "timeless") {
    return(time)
  } else if (format == "date") {
    return_time <- as.POSIXlt(
      time,
      format = "%Y%m%d",
      tz = "UTC"
    )
  } else if (format == "hour") {
    return_time <- as.POSIXlt(
      ISOdatetime(
        year = substr(time[1], 1, 4),
        month = substr(time[1], 5, 6),
        day = substr(time[1], 7, 8),
        hour = substr(time[2], 1, 2),
        min = substr(time[2], 3, 4),
        sec = substr(time[2], 5, 6),
        tz = "UTC"
      )
    )
  } else if (format %in% c("yearmonth", "year")) {
    return_time <- as.integer(time)
  }
  return(return_time)
}

#' Check time values
#' @description
#' Check the time values within calculated covariates `data.frame`
#' @param covar data.frame(1). Calculated covariates `data.frame`.
#' @param POSIXt logical(1). Should the time values in `covar` be of class
#' `POSIXt`? If `FALSE`, the time values will be checked for integer class
#' (year and year-month).
#' @return NULL; returns a stop error if `time` is wrong class
#' @keywords internal auxiliary
#' @export
# nolint start
calc_check_time <- function(
  covar,
  POSIXt = TRUE
) {
  stopifnot(methods::is(covar, "data.frame"))
  if ("time" %in% names(covar)) {
    if (POSIXt) {
      stopifnot(all(sapply(covar$time, methods::is, "POSIXt")))
    } else {
      stopifnot(all(sapply(covar$time, methods::is, "integer")))
    }
  } else {
    message(
      "`$time` not detected in `data.frame` provided.\n"
    )
  }
}
# nolint end

#' Perform covariate extraction
#' @description
#' Extract covariate values from `SpatRaster` object passed from
#' \code{process_*()}.
#' @param dataset character(1). Dataset name.
#' @param from SpatRaster(1). Cleaned `SpatRaster` object.
#' @param locs_vector SpatVector(1). Cleaned `SpatVector` object passed
#' from \code{calc_prepare_locs()}. Contains location point/polygon values.
#' @param locs_df data.frame(1). Cleaned `data.frame` object passed from
#' \code{calc_prepare_locs()}. Contains location identifiers.
#' @param fun character(1). Summary function. Passed to \code{terra::extract()}.
#' @param variable integer. Position within the layer name containing the
#' variable name/code.
#' @param time integer. Position within the layer name containing the time
#' value(s).
#' @param time_type character(1). Type of time observation. One of "date",
#' "hour", "year", "yearmonth", "timeless".
#' @param level integer. Position within the layer name containing the vertical
#' pressure level value (if applicable). Default = `NULL`.
#' @param radius integer(1). Buffer distance (m). Passed from
#' \code{calc_prepare_locs()}. Used in column naming.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param ... Placeholders.
#' @importFrom terra nlyr
#' @importFrom terra extract
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_as_sf
#' @return a `data.frame` object
#' @keywords internal auxiliary
#' @export
calc_worker <- function(
    dataset,
    from,
    locs_vector,
    locs_df,
    fun,
    variable = 1,
    time,
    time_type = c("date", "hour", "year", "yearmonth", "timeless"),
    radius,
    level = NULL,
    max_cells = 1e8,
    ...) {
  #### empty location data.frame
  sites_extracted <- NULL
  time_type <- match.arg(time_type)
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    #### split layer name
    data_split <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### extract variable
    data_name <- data_split[variable]
    if (!is.null(time)) {
      #### extract time
      data_time <- calc_time(
        data_split[time],
        time_type
      )
    }
    #### extract level (if applicable)
    if (!is.null(level)) {
      data_level <- data_split[level]
    } else {
      data_level <- ""
    }
    #### message
    calc_message(
      dataset = dataset,
      variable = data_name,
      time = data_split[time],
      time_type = time_type,
      level = data_level
    )
    #### extract layer data at sites
    if (terra::geomtype(locs_vector) == "polygons") {
      ### apply exactextractr::exact_extract for polygons
      sites_extracted_layer <- exactextractr::exact_extract(
        data_layer,
        sf::st_as_sf(locs_vector),
        progress = FALSE,
        force_df = TRUE,
        fun = fun,
        max_cells_in_memory = max_cells
      )
    } else if (terra::geomtype(locs_vector) == "points") {
      #### apply terra::extract for points
      sites_extracted_layer <- terra::extract(
        data_layer,
        locs_vector,
        method = "simple",
        ID = FALSE,
        bind = FALSE,
        na.rm = TRUE
      )
    }
    # merge with site_id, time, and pressure levels (if applicable)
    if (time_type == "timeless") {
      sites_extracted_layer <- cbind(
        locs_df,
        sites_extracted_layer
      )
      colnames(sites_extracted_layer) <- c(
        colnames(locs_df),
        paste0(
          data_name,
          "_",
          radius
        )
      )
    } else {
      if (is.null(level)) {
        sites_extracted_layer <- cbind(
          locs_df,
          data_time,
          sites_extracted_layer
        )
        colnames(sites_extracted_layer) <- c(
          colnames(locs_df),
          "time",
          paste0(
            data_name,
            "_",
            radius
          )
        )
      } else {
        sites_extracted_layer <- cbind(
          locs_df,
          data_time,
          gsub(
            "level=|lev=",
            "",
            data_level
          ),
          sites_extracted_layer
        )
        colnames(sites_extracted_layer) <- c(
          colnames(locs_df),
          "time",
          "level",
          paste0(
            tolower(data_name),
            "_",
            radius
          )
        )
      }
    }
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  #### finish message
  message(
    paste0(
      "Returning extracted covariates.\n"
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}

#' Prepare covariates for return
#' @description
#' Check the time column for proper class and, if `geom = TRUE`,
#' transform `data.frame` into a `SpatVector` object.
#' @param covar data.frame(1). Calculated covariates `data.frame`.
#' @param POSIXt logical(1). Should the time values in `covar` be of class
#' `POSIXt`? If `FALSE`, the time values will be checked for integer class
#' (year and year-month).
#' @param geom FALSE/"sf"/"terra". Should `covar` be returned as a
#' `data.frame`? Default is `FALSE`, options with geometry are "sf" or "terra".
#' @param crs terra::crs(1). Coordinate reference system (inherited from
#' `from`).
#' @importFrom terra vect
#' @keywords internal auxiliary
#' @author Mitchell Manware
#' @return a data.frame or SpatVector object (depending on `geom` paramter)
#' @export
# nolint start
calc_return_locs <- function(
  covar,
  POSIXt = TRUE,
  geom,
  crs
) {
  # time value check
  if ("time" %in% names(covar)) {
    calc_check_time(covar = covar, POSIXt = POSIXt)
  }
  # nolint end
  # if geom, convert to and return SpatVector
  if (geom %in% c("terra", "sf")) {
    if ("geometry" %in% names(covar)) {
      covar_return <- terra::vect(
        covar,
        geom = "geometry",
        crs = crs
      )
    } else if (all(c("lon", "lat") %in% names(covar))) {
      covar_return <- terra::vect(
        covar,
        geom = c("lon", "lat"),
        crs = crs
      )
    }
    if (geom == "terra") {
      return(covar_return)
    } else if (geom == "sf") {
      return(sf::st_as_sf(covar_return))
    }
  } else {
    return(data.frame(covar))
  }
}

#' Check that `geom` value is one of `FALSE`, `"sf"`, or `"terra"`
#' @description Check that `geom` value is one of `FALSE`, `"sf"`,
#' or `"terra"`.
#' @param geom FALSE/"sf"/"terra".'
#' @keywords internal auxiliary
#' @author Mitchell Manware
#' @return NULL; will stop if `geom` is not one of the three options
#' @export
check_geom <- function(geom) {
  if (!geom %in% c(FALSE, "sf", "terra")) {
    stop("`geom` must be one of FALSE, 'sf', or 'terra'.")
  }
}


#' A single-date MODIS worker
#' @param from SpatRaster. Preprocessed objects.
#' @param locs SpatVector/sf/sftime object. Locations where MODIS values
#' are summarized.
#' @param locs_id character(1). Field name where unique site identifiers
#' are stored. Default is `"site_id"`
#' @param radius numeric. Radius to generate circular buffers.
#' @param date Date(1). date to query.
#' @param name_extracted character. Names of calculated covariates.
#' @param fun_summary function. Summary function for
#' multilayer rasters. Passed to `foo`. See [`exactextractr::exact_extract`]
#' for details.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' See [`exactextractr::exact_extract`] for details.
#' @param ... Placeholders.
#' @description The function operates at MODIS/VIIRS products
#' on a daily basis. Given that the raw hdf files are downloaded from
#' NASA, standard file names include a data retrieval date flag starting
#' with letter "A". Leveraging that piece of information, the function will
#' select files of scope on the date of interest.
#' Please note that this function does not provide a function to filter
#' swaths or tiles, so it is strongly recommended to check and pre-filter
#' the file names at users' discretion.
#' @seealso
#' * Preprocessing: [process_modis_merge()], [process_modis_swath()],
#'     [process_blackmarble()]
#' @keywords auxiliary
#' @author Insang Song
#' @return a data.frame or SpatVector object.
#' @importFrom terra extract project vect nlyr describe
#' @importFrom methods is
#' @importFrom sf st_as_sf st_drop_geometry
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' calculate_modis_daily(
#'   from = mod06l2_warp, # dervied from process_modis() example
#'   locs = locs,
#'   locs_id = "id",
#'   radius = 0,
#'   date = "2024-01-01",
#'   name_extracted = "cloud_fraction_0",
#'   fun_summary = "mean",
#'   max_cells = 3e7
#' )
#' }
#' @export
calculate_modis_daily <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = 0L,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  max_cells = 3e7,
  geom = FALSE,
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs))
    if (inherits(locs, "try-error")) {
      stop("locs should be a SpatVector or convertible object.")
    }
  }
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n",
                 locs_id))
  }

  extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    func = "mean",
    maxcells = NULL
  ) {
    # generate buffers
    if (radius == 0) radius <- 1e-6 # approximately 1 meter in degree
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # extract raster values
    surf_at_bufs <-
      exactextractr::exact_extract(
        x = surf,
        y = sf::st_as_sf(bufs),
        fun = func,
        force_df = TRUE,
        stack_apply = TRUE,
        append_cols = id,
        progress = FALSE,
        max_cells_in_memory = maxcells
      )
    return(surf_at_bufs)
  }

  ## NaN to NA
  from[is.nan(from)] <- NA

  # raster used to be vrt_today
  extracted <-
    extract_with_buffer(
      points = locs,
      surf = from,
      id = locs_id,
      radius = radius,
      func = fun_summary,
      maxcells = max_cells
    )
  # cleaning names
  # assuming that extracted is a data.frame
  name_offset <- terra::nlyr(from)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset + 1, ncol(extracted), 1)
  colnames(extracted)[name_range] <- name_extracted
  extracted$time <- as.POSIXlt(date)
  check_geom(geom)
  if (geom %in% c("sf", "terra")) {
    # convert to base date, as terra::vect does not like class "POSIXlt"
    extracted$time <- as.Date.POSIXlt(extracted$time)
    # location ID with geometry
    locs_geom_id <- suppressMessages(calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )[[2]]
    )
    # merge
    extracted_merge <- merge(
      locs_geom_id,
      extracted,
      by = locs_id
    )
    # re-convert to POSIXlt after creating the vect
    extracted_merge$time <- as.POSIXlt(extracted_merge$time)
    extracted_return <- calc_return_locs(
      covar = extracted_merge,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )
  } else {
    calc_check_time(covar = extracted, POSIXt = TRUE)
    extracted_return <- extracted
  }
  gc()
  return(extracted_return)
}
