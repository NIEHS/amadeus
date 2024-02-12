#' Create circular buffer around site points.
#' @param sites SpatVector(1). SpatVector object with point geometry
#' @param buffer integer(1). Circular buffer size (meters).
#' @description Creates a circular buffer around points if `buffer` is > 0.
#' Returns points if `buffer` is 0.
#' @returns SpatVector.
#' @importFrom terra buffer
#' @export
sites_buffer <-
  function(
    sites,
    buffer
  ) {
    cat(paste0(
      "Utilizing ",
      buffer,
      " meter buffer for covariate calculations.\n"
    ))
    if (buffer == 0) {
      return(sites)
    } else if (buffer > 0) {
      sites_buffer <- terra::buffer(
        sites,
        buffer
      )
      return(sites_buffer)
    }
  }

#' Import and prepare site point locations for covariate calculations.
#' @param sites data.frame(1). Data frame containing columns for unique
#' identifier, latitute, and longitude. Latitude and longitude columns **must**
#' be named "lat" and "lon", respectively.
#' @param crs Coordinate reference system (CRS) description utilizing
#' `terra::crs()`.
#' @param buffer integer(1). Circular buffer size (meters).
#' @returns SpatVector
#' @importFrom terra crs
#' @importFrom terra vect
#' @importFrom terra project
#' @export
sites_vector <-
  function(
    sites,
    crs,
    buffer
  ) {
    #### sites as data frame
    if ("data.table" %in% class(sites)) {
      cat(paste0(
        "Converting data.table to data.frame...\n"
      ))
      sites_df <- data.frame(sites)
    } else if (class(sites) == "data.frame") {
      cat(paste0(
        "Sites are class data.frame...\n"
      ))
      sites_df <- sites
    } else if (!(class(sites) == "data.frame") ||
        "data.table" %in% class(sites)) {
      stop(
        paste0(
          "Detected a ",
          class(sites)[1],
          " object. Sites must be class data.frame or data.table.\n"
        )
      )
    }
    #### columns
    if (any(!(c("lon", "lat") %in% colnames(sites)))) {
      stop(paste0(
        "Sites data is missing 'lon', 'lat', or both.\n"
      ))
    }
    #### as SpatVector
    sites_v <- terra::vect(
      sites_df,
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    #### project SpatVector
    cat(paste0(
      "Projecting data to desired coordinate reference system...\n"
    ))
    sites_p <- terra::project(
      sites_v,
      crs
    )
    #### buffer SpatVector
    sites_b <- sites_buffer(
      sites_p,
      buffer
    )
    return(sites_b)
  }
