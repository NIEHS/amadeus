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
    return_time <- time
  }
  return(return_time)
}

calc_worker <- function(
    from,
    locs_vector,
    locs_df,
    fun,
    variable = 1, # position within names() which contains variable (always 1)
    time, # position within the layer names which contains the date
    # vector c(3, 4) for hour (YYYYMMDD_HH)
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
    #### extract time
    data_time <- calc_time(
      data_split[time],
      time_type
    )
    #### extract level (if applicable)
    if (!(is.null(level))) {
      data_level <- data_split[level]
    }
    #### message
    # calc_message(
    #   time = time,
    #   time_type = time_type,
    #   level = level
    # )
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
    #### cat finishing message
    if (l == terra::nlyr(from)) {
      cat(
        paste0(
          "Returning ",
          data_name,
          " covariates.\n"
        )
      )
    }
  }
  #### return data.frame
  return(data.frame(sites_extracted))
}
