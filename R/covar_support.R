#' Create circular buffer around site points.
#' @param locs SpatVector(1). SpatVector object with point geometry
#' @param radius integer(1). Circular buffer size (meters).
#' @description Creates a circular buffer around points if `radius` is > 0.
#' Returns points if `radius` is 0.
#' @returns SpatVector.
#' @importFrom terra buffer
#' @export
process_locs_radius <-
  function(
    locs,
    radius
  ) {
    cat(paste0(
      "Utilizing ",
      radius,
      " meter buffer for covariate calculations.\n"
    ))
    if (radius == 0) {
      return(locs)
    } else if (radius > 0) {
      sites_buffer <- terra::buffer(
        locs,
        radius
      )
      return(sites_buffer)
    }
  }

#' Import and prepare site point locations for covariate calculations.
#' @param locs data.frame(1). Data frame containing columns for unique
#' identifier, latitute, and longitude. Latitude and longitude columns **must**
#' be named "lat" and "lon", respectively.
#' @param crs Coordinate reference system (CRS) description utilizing
#' `terra::crs()`.
#' @param radius integer(1). Circular buffer size (meters).
#' @returns SpatVector
#' @importFrom terra crs
#' @importFrom terra vect
#' @importFrom terra project
#' @export
process_locs_vector <-
  function(
    locs,
    crs,
    radius
  ) {
    #### sites as data frame
    if ("data.table" %in% class(locs)) {
      cat(paste0(
        "Converting data.table to data.frame...\n"
      ))
      sites_df <- data.frame(locs)
    } else if (class(locs) == "data.frame") {
      cat(paste0(
        "Sites are class data.frame...\n"
      ))
      sites_df <- locs
    } else if (!(class(locs) == "data.frame") ||
        "data.table" %in% class(locs)) {
      stop(
        paste0(
          "Detected a ",
          class(locs)[1],
          " object. Sites must be class data.frame or data.table.\n"
        )
      )
    }
    #### columns
    if (any(!(c("lon", "lat") %in% colnames(locs)))) {
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
    sites_b <- process_locs_radius(
      sites_p,
      radius
    )
    return(sites_b)
  }
