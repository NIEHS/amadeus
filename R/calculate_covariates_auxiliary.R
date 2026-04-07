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
  locs_id
) {
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
    "aqs",
    "ecoregions",
    "geos",
    "gmted",
    "koppen geiger",
    "merra2",
    "modis",
    "narr",
    "nlcd",
    "hms",
    "groads",
    "pop",
    "tri",
    "nei",
    "gridmet",
    "terraclimate"
  )
  stopifnot(dataset %in% datasets)
  genre <- substr(dataset, 1, 3)
  #### covariates
  cov_index <- which(
    !(c(
      names_from %in%
        c(
          locs_id,
          "geometry",
          "time",
          "lat",
          "lon",
          "level",
          "description"
        )
    ))
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
  level
) {
  message_time <- calc_time(time, time_type)
  if (dataset == "skip") {
    return()
  }
  if (dataset == "gmted") {
    return_message <- paste0(
      "Calculating ",
      amadeus::process_gmted_codes(
        substr(
          variable,
          1,
          2
        ),
        statistic = TRUE,
        invert = TRUE
      ),
      " covariates with ",
      amadeus::process_gmted_codes(
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
  geom = FALSE
) {
  #### check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n", locs_id))
  }
  locs_id_values <- as.data.frame(locs)[[locs_id]]
  #### prepare sites
  sites_e <- amadeus::process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers and geometry
  # check geom
  amadeus::check_geom(geom)
  if (geom %in% c("sf", "terra")) {
    geom <- TRUE
  }
  sites_df <- if (geom) {
    terra::as.data.frame(sites_e, geom = "WKT")
  } else {
    terra::as.data.frame(sites_e)
  }
  if (!locs_id %in% names(sites_df)) {
    if (nrow(sites_df) != length(locs_id_values)) {
      stop(
        paste0(
          "`locs_id` was not retained in prepared locations and could not ",
          "be reconstructed because row counts differ."
        )
      )
    }
    sites_df[[locs_id]] <- locs_id_values
  }
  if (geom) {
    sites_id <- subset(
      sites_df,
      select = c(locs_id, "geometry")
    )
  } else {
    #### site identifiers only
    sites_id <- subset(
      sites_df,
      select = locs_id
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
  format
) {
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
  ...
) {
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
      data_level <- NULL
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
    if (nrow(covar) == 0) {
      if ("geometry" %in% names(covar)) {
        covar_sf <- sf::st_as_sf(covar, wkt = "geometry", crs = crs)
      } else if (all(c("lon", "lat") %in% names(covar))) {
        covar_sf <- sf::st_as_sf(
          covar,
          coords = c("lon", "lat"),
          crs = crs,
          remove = FALSE
        )
      } else {
        warning(
          paste(
            "`geom` was requested but no geometry columns were found in",
            "`covar`; returning data.frame."
          )
        )
        return(data.frame(covar))
      }
      if (geom == "sf") {
        return(covar_sf)
      }
      return(suppressWarnings(terra::vect(covar_sf)))
    }
    covar_return <- NULL
    if ("geometry" %in% names(covar)) {
      covar_sf <- sf::st_as_sf(covar, wkt = "geometry", crs = crs)
      covar_return <- if (geom == "sf") {
        covar_sf
      } else {
        suppressWarnings(terra::vect(covar_sf))
      }
    } else if (all(c("lon", "lat") %in% names(covar))) {
      covar_return <- terra::vect(
        covar,
        geom = c("lon", "lat"),
        crs = crs
      )
    }
    if (is.null(covar_return)) {
      warning(
        paste(
          "`geom` was requested but no geometry columns were found in",
          "`covar`; returning data.frame."
        )
      )
      return(data.frame(covar))
    }
    if (geom == "terra") {
      return(covar_return)
    } else if (geom == "sf") {
      return(
        if (inherits(covar_return, "sf")) {
          covar_return
        } else {
          sf::st_as_sf(covar_return)
        }
      )
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


#' Validate the `fun_temporal` parameter
#' @description
#' Validates the `fun_temporal` argument used by covariate
#' extraction functions.  When \code{NULL} (the default), no
#' temporal aggregation is applied and existing per-layer
#' extraction behavior is preserved.  When non-\code{NULL},
#' the value must be one of \code{"mean"}, \code{"median"},
#' \code{"sum"}, \code{"max"}, or \code{"min"}.
#' @param fun_temporal NULL or character(1). Name of the
#'   temporal summary function. \code{NULL} means no temporal
#'   aggregation (default / backward-compatible behavior).
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops with an informative
#'   error if the value is invalid.
#' @export
check_fun_temporal <- function(fun_temporal) {
  if (is.null(fun_temporal)) {
    return(invisible(NULL))
  }
  allowed <- c("mean", "median", "sum", "max", "min")
  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        "`fun_temporal` must be NULL or a single ",
        "character string.\n"
      )
    )
  }
  if (!fun_temporal %in% allowed) {
    stop(
      sprintf(
        paste0(
          "`fun_temporal` must be one of: %s.\n"
        ),
        paste(allowed, collapse = ", ")
      )
    )
  }
  invisible(NULL)
}


#' Classify the type of a `.by` summarization argument
#' @description
#' Internal helper used by \code{check_by()} to determine which dispatch
#' branch the \code{.by} value falls into.  The four recognised classes are:
#' \describe{
#'   \item{\code{"null"}}{No additional summarization (\code{.by = NULL}).}
#'   \item{\code{"time_unit"}}{Temporal bucketing: \code{.by} is a single
#'     character string naming a calendar unit such as \code{"day"},
#'     \code{"week"}, \code{"month"}, \code{"quarter"}, or \code{"year"}
#'     (singular and plural forms accepted, plus \code{"hour"} /
#'     \code{"minute"} for sub-daily data).}
#'   \item{\code{"col_name"}}{Group-by column: \code{.by} is a single
#'     character string that is \emph{not} a recognised time-unit token, and
#'     is therefore interpreted as the name of a grouping column in the
#'     extracted covariate \code{data.frame}.}
#'   \item{\code{"spatial_obj"}}{Spatial grouping: \code{.by} is an \code{sf}
#'     or \code{SpatVector} object used to spatially aggregate results.}
#' }
#' @param .by NULL, character(1), \code{sf}, or \code{SpatVector}.
#'   The \code{.by} value to classify.
#' @keywords internal
#' @author Insang Song
#' @return character(1). One of \code{"null"}, \code{"time_unit"},
#'   \code{"col_name"}, or \code{"spatial_obj"}.
#' @export
classify_by <- function(.by) {
  if (is.null(.by)) {
    return("null")
  }
  if (inherits(.by, c("sf", "SpatVector"))) {
    return("spatial_obj")
  }
  if (is.character(.by) && length(.by) == 1L) {
    time_units <- c(
      "minute", "minutes",
      "hour",   "hours",
      "day",    "days",
      "week",   "weeks",
      "month",  "months",
      "quarter", "quarters",
      "year",   "years"
    )
    if (.by %in% time_units) {
      return("time_unit")
    }
    return("col_name")
  }
  stop(
    paste0(
      "`.by` must be NULL, a single character string (time unit or column ",
      "name), or an sf/SpatVector object for spatial grouping.\n"
    )
  )
}


#' Validate the `.by` summarization argument
#' @description
#' Checks that the \code{.by} argument conforms to the expected contract
#' for optional summarization in covariate calculation functions:
#' \describe{
#'   \item{\code{NULL}}{No additional summarization (backward-compatible
#'     default).  Returned invisibly as \code{"null"}.}
#'   \item{Time-unit string}{A single character string naming a recognised
#'     calendar unit.  Accepted tokens (singular and plural):
#'     \code{"minute"}/\code{"minutes"},
#'     \code{"hour"}/\code{"hours"},
#'     \code{"day"}/\code{"days"},
#'     \code{"week"}/\code{"weeks"},
#'     \code{"month"}/\code{"months"},
#'     \code{"quarter"}/\code{"quarters"},
#'     \code{"year"}/\code{"years"}.
#'     When \code{.by} is a time-unit string, \code{.by_time} may optionally
#'     name the time column to bucket (default falls back to \code{"time"}).}
#'   \item{Column-name string}{A single character string that is \emph{not}
#'     a recognised time-unit token.  Interpreted as the name of a grouping
#'     column in the extracted covariate \code{data.frame}.  When \code{data}
#'     is supplied the column is validated to exist.}
#'   \item{Spatial object}{An \code{sf} or \code{SpatVector} object used
#'     for spatial-overlay grouping of extracted values.}
#' }
#' @param .by NULL, character(1), \code{sf}, or \code{SpatVector}.
#'   Summarization specification.  See Description for accepted forms.
#' @param .by_time NULL or character(1). Name of the time column in the
#'   extracted covariate table.  Only meaningful when \code{.by} is a
#'   time-unit string.  \code{NULL} (default) defers column lookup to the
#'   calling function (typically \code{"time"}).
#' @param data NULL or data.frame. When provided and \code{.by} is a
#'   column-name string, the column is validated to exist in \code{data}.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return character(1) invisibly — the \code{classify_by()} result for
#'   \code{.by}.  Stops with an informative error on invalid input.
#' @seealso \code{\link{classify_by}}, \code{\link{check_by_time}}
#' @export
check_by <- function(.by, .by_time = NULL, data = NULL) {
  kind <- classify_by(.by)
  if (kind == "col_name" && !is.null(data)) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data.frame when provided to `check_by()`.\n")
    }
    if (!.by %in% names(data)) {
      stop(sprintf(
        "`.by` column '%s' not found in `data`.\n",
        .by
      ))
    }
  }
  if (!is.null(.by_time) && kind == "null") {
    warning(
      "`.by_time` is ignored when `.by` is NULL.\n"
    )
  }
  invisible(kind)
}


#' Validate the `.by_time` explicit time-column argument
#' @description
#' Validates the \code{.by_time} argument used alongside \code{.by} in
#' covariate extraction functions.  When non-\code{NULL}, \code{.by_time}
#' must be a single character string naming the time column in the extracted
#' covariate \code{data.frame}.  A \code{NULL} value (the default) defers
#' time-column selection to the calling function (typically \code{"time"}).
#' @param .by_time NULL or character(1). Name of the time column.
#'   \code{NULL} means use the default time column inferred by the
#'   calling function.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops with an informative error if the
#'   value is invalid.
#' @seealso \code{\link{check_by}}
#' @export
check_by_time <- function(.by_time) {
  if (is.null(.by_time)) {
    return(invisible(NULL))
  }
  if (!is.character(.by_time) || length(.by_time) != 1L) {
    stop(
      paste0(
        "`.by_time` must be NULL or a single character string naming ",
        "the time column.\n"
      )
    )
  }
  invisible(NULL)
}

#' Normalize `.by` time-unit aliases
#' @description Internal helper that maps singular/plural `.by` tokens
#' to canonical units.
#' @param unit character(1). Time-unit alias.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return character(1). Canonical unit.
#' @export
normalize_by_time_unit <- function(unit) {
  aliases <- c(
    minute = "minute",
    minutes = "minute",
    hour = "hour",
    hours = "hour",
    day = "day",
    days = "day",
    week = "week",
    weeks = "week",
    month = "month",
    months = "month",
    quarter = "quarter",
    quarters = "quarter",
    year = "year",
    years = "year"
  )
  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop("`unit` must be one valid `.by` time-unit token.\n")
  }
  aliases[[unit]]
}


#' Bucket a time column to a `.by` unit
#' @description Buckets time values to one of the supported `.by` units.
#' @param time_vals vector. Time values to bucket.
#' @param unit character(1). A valid `.by` time-unit token.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return vector. Bucketed values as POSIXct (minute/hour) or Date.
#' @export
bucket_time_by_unit <- function(time_vals, unit) {
  unit_norm <- normalize_by_time_unit(unit)
  if (unit_norm %in% c("minute", "hour")) {
    breaks <- if (unit_norm == "minute") "min" else "hour"
    return(as.POSIXct(cut(as.POSIXct(time_vals, tz = "UTC"), breaks = breaks)))
  }
  time_vals_chr <- as.character(time_vals)
  if (all(grepl("^[0-9]{8}$", time_vals_chr))) {
    time_date <- as.Date(time_vals_chr, format = "%Y%m%d")
  } else if (all(grepl("^[0-9]{6}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "01"), format = "%Y%m%d")
  } else if (all(grepl("^[0-9]{4}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "-01-01"))
  } else {
    time_date <- as.Date(time_vals)
  }
  switch(
    unit_norm,
    day = time_date,
    week = as.Date(cut(time_date, breaks = "week")),
    month = as.Date(cut(time_date, breaks = "month")),
    quarter = as.Date(cut(time_date, breaks = "quarter")),
    year = as.Date(cut(time_date, breaks = "year"))
  )
}


#' Summarize extracted covariates using `.by` semantics
#' @description
#' Generic summarizer following `chopin::summarize_st`-style `.by`
#' semantics for covariate tables.
#' @param covar data.frame. Extracted covariates.
#' @param .by NULL, character(1), \code{sf}, or \code{SpatVector}.
#'   Summarization mode:
#'   \itemize{
#'     \item \code{NULL}: no-op
#'     \item time-unit character: summarize by \code{locs_id} and bucketed
#'       time
#'     \item column-name character: summarize by that column
#'     \item \code{sf}/\code{SpatVector}: summarize by overlay target feature
#'   }
#' @param fun_summary character(1) or function. Summary function
#'   (e.g., \code{"mean"}, \code{"sum"}).
#' @param locs_id character(1). Location-id column.
#' @param time_col character(1). Time column in `covar`.
#' @param .by_time NULL or character(1). Optional time grouping spec:
#'   either a column name in `covar` or a time-unit token.
#' @param group_cols_extra character or NULL. Extra grouping columns.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return a data.frame.
#' @importFrom dplyr across all_of group_by summarize left_join
#' @importFrom sf st_as_sf st_drop_geometry st_as_text st_geometry
#'   st_crs st_join st_transform
#' @export
calc_summarize_by <- function(
  covar,
  .by = NULL,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  .by_time = NULL,
  group_cols_extra = NULL
) {
  if (is.null(.by)) {
    return(covar)
  }
  stopifnot(is.data.frame(covar))
  check_by_time(.by_time)
  by_kind <- check_by(.by, .by_time, data = covar)

  if (is.character(fun_summary)) {
    if (length(fun_summary) != 1L) {
      stop("`fun_summary` must be a single function name.\n")
    }
    fun_r <- match.fun(fun_summary)
  } else if (is.function(fun_summary)) {
    fun_r <- fun_summary
  } else {
    stop("`fun_summary` must be a character string or function.\n")
  }

  covar2 <- covar
  has_geom <- "geometry" %in% names(covar2)
  geom_source <- NULL
  group_cols <- character(0)

  append_by_time <- function(df, group_cols_now) {
    if (is.null(.by_time)) {
      return(list(data = df, group_cols = group_cols_now))
    }
    if (.by_time %in% names(df)) {
      return(list(data = df, group_cols = c(group_cols_now, .by_time)))
    }
    unit_aliases <- c(
      "minute", "minutes", "hour", "hours", "day", "days",
      "week", "weeks", "month", "months", "quarter", "quarters",
      "year", "years"
    )
    if (.by_time %in% unit_aliases) {
      if (!time_col %in% names(df)) {
        stop(sprintf("`time_col` '%s' not found in `covar`.\n", time_col))
      }
      df[[time_col]] <- bucket_time_by_unit(df[[time_col]], .by_time)
      return(list(data = df, group_cols = c(group_cols_now, time_col)))
    }
    stop(
      "`.by_time` must be either a column name in `covar` ",
      "or a valid time-unit token.\n"
    )
  }

  if (by_kind == "time_unit") {
    if (!locs_id %in% names(covar2)) {
      stop(sprintf("`locs_id` column '%s' not found in `covar`.\n", locs_id))
    }
    time_col_use <- if (is.null(.by_time)) time_col else .by_time
    if (!time_col_use %in% names(covar2)) {
      stop(sprintf("time column '%s' not found in `covar`.\n", time_col_use))
    }
    covar2[[time_col]] <- bucket_time_by_unit(covar2[[time_col_use]], .by)
    group_cols <- c(locs_id, time_col, group_cols_extra)
  } else if (by_kind == "col_name") {
    group_cols <- c(.by, group_cols_extra)
    by_time_out <- append_by_time(covar2, group_cols)
    covar2 <- by_time_out$data
    group_cols <- by_time_out$group_cols
  } else if (by_kind == "spatial_obj") {
    if (!has_geom) {
      stop(
        "`covar` must include a `geometry` WKT column when `.by` ",
        "is an sf/SpatVector object.\n"
      )
    }
    by_sf <- if (inherits(.by, "SpatVector")) sf::st_as_sf(.by) else .by
    geom_col <- attr(by_sf, "sf_column")
    by_cols <- setdiff(names(by_sf), geom_col)
    if (length(by_cols) == 0L) {
      stop("`.by` spatial object must have at least one identifier column.\n")
    }
    by_id <- by_cols[1]
    by_only <- by_sf[, by_id, drop = FALSE]
    covar_sf <- sf::st_as_sf(covar2, wkt = "geometry")
    if (!is.na(sf::st_crs(by_sf)) && is.na(sf::st_crs(covar_sf))) {
      sf::st_crs(covar_sf) <- sf::st_crs(by_sf)
    }
    if (
      !is.na(sf::st_crs(by_sf)) &&
        !is.na(sf::st_crs(covar_sf)) &&
        sf::st_crs(covar_sf) != sf::st_crs(by_sf)
    ) {
      covar_sf <- sf::st_transform(covar_sf, sf::st_crs(by_sf))
    }
    joined <- sf::st_join(
      covar_sf,
      by_only,
      left = FALSE,
      join = sf::st_intersects
    )
    covar2 <- sf::st_drop_geometry(joined)
    group_cols <- c(by_id, group_cols_extra)
    by_time_out <- append_by_time(covar2, group_cols)
    covar2 <- by_time_out$data
    group_cols <- by_time_out$group_cols
    geom_source <- data.frame(
      stats::setNames(list(by_sf[[by_id]]), by_id),
      geometry = sf::st_as_text(sf::st_geometry(by_sf)),
      stringsAsFactors = FALSE
    )
    has_geom <- TRUE
  }

  group_cols <- unique(stats::na.omit(group_cols))
  missing_group <- setdiff(group_cols, names(covar2))
  if (length(missing_group) > 0L) {
    stop(
      sprintf(
        "Grouping column(s) not found in `covar`: %s.\n",
        paste(missing_group, collapse = ", ")
      )
    )
  }

  cov_cols <- names(covar2)[vapply(covar2, is.numeric, logical(1))]
  cov_cols <- setdiff(cov_cols, group_cols)
  if (length(cov_cols) == 0L) {
    stop("No numeric covariate columns found to summarize.\n")
  }

  summary_df <- covar2 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(cov_cols), \(x) fun_r(x, na.rm = TRUE)),
      .groups = "drop"
    )

  if (by_kind == "spatial_obj" && !is.null(geom_source)) {
    summary_df <- dplyr::left_join(summary_df, geom_source, by = group_cols[1])
  } else if (has_geom && "geometry" %in% names(covar2)) {
    geom_first <- covar2[
      !duplicated(covar2[, group_cols, drop = FALSE]),
      c(group_cols, "geometry"),
      drop = FALSE
    ]
    summary_df <- dplyr::left_join(summary_df, geom_first, by = group_cols)
  }

  col_order <- c(group_cols, cov_cols)
  if ("geometry" %in% names(summary_df)) {
    col_order <- c(col_order, "geometry")
  }
  data.frame(summary_df[, unique(col_order), drop = FALSE])
}


#' Summarize extracted covariates by temporal bucket
#' @description
#' Applies a named summary function across covariate columns
#' after bucketing the \code{time} column to a coarser temporal
#' resolution (daily by default).  When \code{fun_temporal} is
#' \code{NULL}, the input is returned unchanged
#' (backward-compatible default).  A WKT \code{"geometry"} column
#' produced by \code{calc_prepare_locs()} is preserved by
#' carrying forward the first observed geometry per group.
#' @param covar data.frame. Extracted covariate table, typically
#'   the output of \code{calc_worker()} or a
#'   \code{calculate_*()} function before the
#'   \code{calc_return_locs()} call. Must contain the columns
#'   named by \code{locs_id} and \code{time_col}.
#' @param fun_temporal NULL or character(1). Name of the summary
#'   function.  One of \code{"mean"}, \code{"median"},
#'   \code{"sum"}, \code{"max"}, \code{"min"}, or \code{NULL}
#'   (no aggregation; backward-compatible default).
#' @param locs_id character(1). Name of the location-identifier
#'   column in \code{covar}. Default \code{"site_id"}.
#' @param time_col character(1). Name of the time column in
#'   \code{covar}. Default \code{"time"}.
#' @param time_bucket character(1). Temporal resolution to
#'   summarise to.  One of \code{"day"} (default),
#'   \code{"week"}, \code{"month"}, or \code{"year"}.
#' @param group_cols_extra character or NULL. Additional column
#'   names to include in the grouping key (e.g. \code{"level"}
#'   for pressure-level data). Default \code{NULL}.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return a data.frame.  When \code{fun_temporal} is
#'   \code{NULL}, \code{covar} is returned as-is.  Otherwise
#'   each row represents one unique group / time-bucket
#'   combination with covariate columns aggregated by
#'   \code{fun_temporal}.
#' @importFrom dplyr across all_of group_by summarize left_join
#' @export
calc_summarize_temporal <- function(
  covar,
  fun_temporal,
  locs_id = "site_id",
  time_col = "time",
  time_bucket = "day",
  group_cols_extra = NULL
) {
  if (is.null(fun_temporal)) {
    return(covar)
  }
  stopifnot(is.data.frame(covar))
  if (!locs_id %in% names(covar)) {
    stop(sprintf(
      "`locs_id` column '%s' not found in `covar`.",
      locs_id
    ))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(
      "`time_col` column '%s' not found in `covar`.",
      time_col
    ))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(sprintf(
        paste0(
          "Extra grouping column(s) not found in `covar`: %s."
        ),
        paste(missing_extra, collapse = ", ")
      ))
    }
  }
  time_bucket <- match.arg(
    time_bucket,
    c("day", "week", "month", "year")
  )
  grp_cols <- c(locs_id, group_cols_extra, time_col)
  skip_cols <- c(grp_cols, "geometry")
  cov_cols <- setdiff(names(covar), skip_cols)
  has_geom <- "geometry" %in% names(covar)
  if (length(cov_cols) == 0L) {
    stop(paste0(
      "No covariate columns found in `covar` to summarize."
    ))
  }
  covar2 <- covar
  covar2[[time_col]] <- switch(
    time_bucket,
    day = as.Date(covar[[time_col]]),
    week = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "week")
    ),
    month = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "month")
    ),
    year = as.Date(
      cut(as.Date(covar[[time_col]]), breaks = "year")
    )
  )
  fun_r <- match.fun(fun_temporal)
  force(fun_r)
  summary_df <- covar2 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(cov_cols),
        \(x) fun_r(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  if (has_geom) {
    geom_first <- covar2[
      !duplicated(covar2[, grp_cols, drop = FALSE]),
      c(grp_cols, "geometry"),
      drop = FALSE
    ]
    summary_df <- dplyr::left_join(
      summary_df,
      geom_first,
      by = grp_cols
    )
  }
  col_order <- c(grp_cols, cov_cols)
  if (has_geom) {
    col_order <- c(col_order, "geometry")
  }
  data.frame(summary_df[, col_order, drop = FALSE])
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
#' @param scale character(1). Scale expression to be applied to the raw values.
#' It is crucial that users review the technical documentation of the MODIS
#' product
#' they are using to ensure proper scale.
#' An example for the MOD11A1 product's LST_Day_1km variable (land surface
#' temperature)
#' would be `scale = "* 0.02"`.
#' Default is `NULL`, which applies no scale.
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
#' @keywords internal
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
  scale = NULL,
  ...
) {
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs))
    if (inherits(locs, "try-error")) {
      stop("locs should be a SpatVector or convertible object.")
    }
  }
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n", locs_id))
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
    if (radius == 0) {
      radius <- 1e-6
    } # approximately 1 meter in degree
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

  # apply scale as expression to `from` values
  chr_scale <- paste0("from ", scale)
  # Evaluate the scale expression
  from_scale <- eval(parse(text = chr_scale)[[1]])

  # from_scale[is.nan(from_scale)] <- NA

  # raster used to be vrt_today
  extracted <-
    extract_with_buffer(
      points = locs,
      surf = from_scale,
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
    )[[2]])
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

#' Collapse listed NLCD values while filling in NA for sites outside data.
#' @param data Buffered values from NLCD data.
#' @param mode "exact" or "terra"
#' @param locs extraction locations.
#' @param locs_id character(1). Name of unique identifier.
#' @keywords internal auxiliary
#' @importFrom collapse rowbind
#' @export
collapse_nlcd <- function(
  data,
  mode = c("terra", "exact"),
  locs = NULL,
  locs_id = "site_id"
) {
  # Add mode matching
  mode <- match.arg(mode)

  # Filter NULL values
  data_nonnull <- Filter(Negate(is.null), data)

  # Check for empty data
  if (length(data_nonnull) == 0) {
    warning("No non-null data provided to collapse_nlcd")
    return(data.frame())
  }

  # Combine data
  data_rbind <- collapse::rowbind(data_nonnull, fill = TRUE)

  # Check if result is empty
  if (is.null(data_rbind) || nrow(data_rbind) == 0) {
    warning("Row binding resulted in empty data frame")
    return(data.frame())
  }

  if (mode == "terra") {
    # Now safe to create NA row
    na_row <- data_rbind[1, , drop = FALSE]
    na_row[] <- NA

    # Replace all NULL elements with the NA row
    data_na <- lapply(data, function(x) if (is.null(x)) na_row else x)

    # Combine into a single data frame
    data_filled <- collapse::rowbind(data_na, fill = TRUE)
  } else {
    stopifnot(!is.null(locs))

    sites_wdata <- unlist(lapply(data, function(x) x[[locs_id]]))
    sites_missing <- setdiff(unlist(locs[[locs_id]]), sites_wdata)

    df_missing <- data.frame(setNames(list(sites_missing), locs_id))

    data_filled <- collapse::rowbind(data_rbind, df_missing, fill = TRUE)
  }

  return(data_filled)
}
