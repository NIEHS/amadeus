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
#' @param from data.frame(1). Calculated covariates as returned from 
#' \code{calc_covariates()} or a source specific covariate function.
#' @param lag integer(1). Temporal lag.
#' @param dataset character(1). Covariate parent dataset.
#' @param locs_id character(1). Column containing identifier for each unique
#' coordinate location.
#' @keywords internal
#' @importFrom stringi stri_pad
#' @export
calc_setcolumns <- function(
    from,
    lag,
    dataset,
    locs_id) {
  #### check from is data.frame
  stopifnot(is.data.frame(from))
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
    !(c(names_from %in% c(locs_id, "time", "lat", "lon", "level")))
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
    names_return[cov_index[c]] <- name_new
  }
  #### check for unique names
  stopifnot(length(names_return) == length(unique(names_return)))
  colnames(from) <- names_return
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
#' @return NULL
#' @keywords internal
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
  cat(return_message)
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
#' @return A `list` containing `SpatVector` and `data.frame` objects
#' @seealso [`process_sites_vector()`], [`check_for_null_parameters()`]
#' @keywords internal
#' @importFrom terra as.data.frame
#' @importFrom terra crs
#' @export
calc_prepare_locs <- function(
    from,
    locs,
    locs_id,
    radius) {
  #### check for null parameters
  check_for_null_parameters(mget(ls()))
  #### prepare sites
  sites_e <- process_locs_vector(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers only
  sites_id <- subset(
    terra::as.data.frame(sites_e),
    select = locs_id
  )
  return(list(sites_e, sites_id))
}

#' Prepare time values
#' @description
#' Prepare the time values for covariate calculation based on type of time
#' value.
#' @param time Time value
#' @param format Type of time to return in the `$time` column. Can be
#' "timeless" (ie. GMTED data), "date" (ie. NARR data), "hour", (ie. GEOS data),
#' "year" (ie. SEDAC population data), or "yearmonth" (ie. TerraClimate data).
#' @return a `Date`, `POSIXt`, or `integer` object based on `format =`
#' @keywords internal
#' @export
calc_time <- function(
    time,
    format) {
  if (format == "timeless") {
    return()
  } else if (format == "date") {
    return_time <- as.Date(
      time,
      format = "%Y%m%d"
    )
  } else if (format == "hour") {
    return_time <- ISOdatetime(
      year = substr(time[1], 1, 4),
      month = substr(time[1], 5, 6),
      day = substr(time[1], 7, 8),
      hour = substr(time[2], 1, 2),
      min = substr(time[2], 3, 4),
      sec = substr(time[2], 5, 6),
      tz = "UTC"
    )
  } else if (format %in% c("yearmonth", "year")) {
    return_time <- as.integer(time)
  }
  return(return_time)
}

#' Peform covariate extraction
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
#' @importFrom terra nlyr
#' @importFrom terra extract
#' @return a `data.frame` object
#' @keywords internal
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
    level = NULL) {
  #### empty location data.frame
  sites_extracted <- NULL
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
    if (!(is.null(level))) {
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
    sites_extracted_layer <- terra::extract(
      data_layer,
      locs_vector,
      fun = fun,
      method = "simple",
      ID = FALSE,
      bind = FALSE,
      na.rm = TRUE
    )
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
          data_level,
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
  cat(
    paste0(
      "Returning extracted covariates.\n"
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}
