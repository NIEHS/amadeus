#' Check \code{sftime} object
#' @description
#' Check an \code{sftime} object for its class, \code{$time} column,
#' \code{$geometry} column, and geometry class.
#' @param x an `sftime` object
#' @import sftime
#' @author Eva Marques
#' @keywords spacetime
#' @export
check_mysftime <- function(x) {
  stopifnot(
    "x is not a sftime" = class(x)[1] == "sftime",
    "x is not inherited from a data.table" =
      class(x)[3] == "data.table",
    "time column should be called time" =
      attributes(x)$time_column == "time",
    "geometry column should be called geometry" =
      attributes(x)$sf_column == "geometry",
    "geometry is not a sfc_POINT" = class(x$geometry)[1] == "sfc_POINT"
  )
}

#' Check \code{sf} object
#' @description
#' Check an \code{sf} object for its class, \code{$geometry} column, and
#' geometry class.
#' @param x an `sf` object
#' @import sf
#' @author Eva Marques
#' @keywords spacetime
#' @export
check_mysf <- function(x) {
  stopifnot(
    "x is not a sf" = class(x)[1] == "sf",
    "x is not inherited from a data.table" =
      class(x)[2] == "data.table",
    "geometry column should be called geometry" =
      attributes(x)$sf_column == "geometry",
    "geometry is not a sfc_POINT" = class(x$geometry)[1] == "sfc_POINT"
  )
}

#' Rename \code{$time}
#' @description
#' Rename the \code{$time} column of an \code{sftime} object.
#' @param x an `sftime` object
#' @param newname character for new time column name
#' @return an `sftime` object
#' @import sftime
#' @author Eva Marques
#' @keywords spacetime
#' @export
rename_time <- function(x, newname) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  oldname <- attributes(x)$time_column
  output <- st_sftime(x, time_column_name = oldname)
  attributes(output)$time_column <- newname
  colnames(output)[which(colnames(output) == oldname)] <- newname
  return(st_sftime(output, time_column_name = newname))
}


#' Convert a \code{data.table} to an \code{sftime}
#' @description
#' Convert a \code{data.table} object to an \code{sftime}. \code{x} must be
#' a \code{data.table} object with "lon", "lat", and "time" columns to
#' describe the longitude, latitude, and time-orientation, respectively, of
#' \code{x}.
#' @param x a `data.table`
#' @param lonname character for longitude column name
#' @param latname character for latitude column name
#' @param timename character for time column name
#' @param crs coordinate reference system
#' @return an `sftime` object
#' @import sftime
#' @author Eva Marques
#' @keywords spacetime
#' @export
dt_as_mysftime <- function(x, lonname, latname, timename, crs) {
  stopifnot("x is not a data.table" = class(x)[1] == "data.table")
  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop("Some of lon, lat, time columns missing or mispelled")
  }

  mysft <-
    sftime::st_as_sftime(
      x,
      coords = c(lonname, latname),
      time_column_name = timename,
      crs = crs
    )
  mysft <- rename_time(mysft, timename)
  return(mysft)
}


#' Convert an \code{sf} to an \code{sftime}
#' @description
#' Convert an \code{sf} object to an \code{sftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sf` object
#' @param timename character: name of time column in x
#' @return an `sftime` object
#' @author Eva Marques
#' @export
sf_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- st_as_sftime(x, time_column_name = timename)
  output <- rename_time(output, "time")
  return(output)
}

#' Convert a \code{SpatVector} to an \code{sftime}
#' @description
#' Convert a \code{SpatVector} object to an \code{sftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x a `SpatVector` object
#' @param timename character for time column name in x
#' (default: "time")
#' @return an `sftime` object
#' @import sftime
#' @seealso [terra::vect]
#' @author Eva Marques
#' @export
spatvector_as_sftime <- function(x, timename = "time") {
  stopifnot("timename column missing or mispelled" = timename %in% names(x))
  crs <- terra::crs(x)
  output <- as.data.frame(x, geom = "XY") |>
    data.table::as.data.table() |>
    dt_as_mysftime("x", "y", timename, crs = crs)
  return(output)
}

#' Convert a \code{SpatRaster} to an \code{sftime}
#' @description
#' Convert a \code{SpatRaster} object to an \code{sftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x a `SpatRaster` object
#' @param varname character for variable column name in the sftime
#' @param timename character for time column name in the sftime
#' (default: "time")
#' @return a `sftime` object
#' @import sftime
#' @seealso [terra::rast]
#' @author Eva Marques
#' @keywords spacetime
#' @export
spatraster_as_sftime <- function(x, varname, timename = "time") {
  date_correct <- TRUE
  tryCatch(
    {
      as.POSIXct(names(x))
    },
    error = function(e) {
      date_correct <<- FALSE
    }
  )
  stopifnot("x layers might not be time" = date_correct)
  df <- as.data.frame(x, xy = TRUE)
  output <- df |>
    data.table::as.data.table() |>
    data.table::melt(
      measure.vars = names(df)[-1:-2],
      variable.name = "time",
      value.name = varname
    ) |>
    st_as_sftime(
      coords = c("x", "y"),
      time_column_name = "time",
      crs = terra::crs(x)
    )
  output <- rename_time(output, timename)
  return(output)
}

#' Convert a \code{SpatRasterDataset} to an \code{sftime}
#' @description
#' Convert a \code{SpatRasterDataset} object to an \code{sftime} object.
#' \code{x} must contain a time-defining column, identified in \code{timename}.
#' @param x a `SpatRasterDataset` object (~ list of named SpatRasters)
#' @param timename character for time column name in the sftime
#' (default: "time")
#' @return an `sftime` object
#' @import sftime
#' @seealso [terra::sds]
#' @author Eva Marques
#' @keywords spacetime
#' @export
spatrds_as_sftime <- function(x, timename = "time") {
  stopifnot(
    "x is not a SpatRasterDataset" =
      class(x)[1] == "SpatRasterDataset"
  )
  variables <- names(x)
  newsft <- spatraster_as_sftime(x[[variables[1]]],
    varname = variables[1],
    timename = timename
  )
  for (var in variables[2:length(variables)]) {
    s <- spatraster_as_sftime(x[[var]],
                              varname = var,
                              timename = timename)
    newsft[, var] <- st_drop_geometry(s[, var])
  }
  return(st_sftime(newsft, time_column_name = timename))
}


#' Create an \code{sftime} object
#' @description
#' Create a \code{sftime} object from one of \code{data.frame},
#' \code{data.table}, \code{sf}, \code{sftime}, \code{SpatRaster},
#' \code{SpatRasterDataset}, \code{SpatVector}
#' @param x an object of class `data.frame`, `data.table`, `sf`, `sftime`,
#' `SpatRaster`, `SpatRasterDataset` or `SpatVector`
#' @param ... if x is a data.frame or data.table: lonname, latname, timename and
#' crs arguments are required. If x is a sf or sftime, timename argument is
#' required. If x is a terra::SpatRaster, varname argument is required.
#' @return an `sftime` object with constrained time column name
#' @import sf
#' @seealso [check_mysftime], [sf_as_sftime], [data.frame],
#' [data.table::data.table], [terra::rast], [terra::sds], [terra::vect]
#' @author Eva Marques
#' @keywords spacetime
#' @export
as_mysftime <- function(x, ...) {
  format <- class(x)[1]
  if (format == "data.frame") {
    output <- x |>
      data.table::data.table() |>
      dt_as_mysftime(...)
  } else if (format == "data.table") {
    output <- x |>
      dt_as_mysftime(...)
  } else if (format == "sf") {
    output <- x |>
      sf_as_mysftime(...)
  } else if (format == "sftime") {
    output <- x |>
      sftime_as_mysftime(...)
  } else if (format == "SpatRaster") {
    output <- x |>
      spatraster_as_sftime(timename = "time", ...)
  } else if (format == "SpatVector") {
    output <- x |>
      spatvector_as_sftime(...)
    output <- rename_time(output, "time")
  } else if (format == "SpatRasterDataset") {
    crs_dt <- terra::crs(x)
    stdf <- as.data.frame(x[1], xy = TRUE)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"
    # -- tranform from wide to long format
    stdf <- stdf |>
      data.table::as.data.table() |>
      data.table::melt(
        measure.vars = names(stdf)[-1:-2],
        variable.name = "time",
        value.name = names(x)[1]
      )
    for (var in seq(2, length(names(x)))) {
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(x[var]), names(x[1])))) {
        stop("time series differ from 1 variable to the other")
      }
      varname_original <- names(x)[var]
      df_var <- as.data.frame(x[var], xy = TRUE)
      # -- tranform from wide to long format
      df_var <- df_var |>
        data.table::as.data.table() |>
        data.table::melt(
          measure.vars = names(df_var)[-1:-2],
          variable.name = "time",
          value.name = varname_original
        ) |>
        as.data.frame()
      stdf[, varname_original] <- df_var[, 4]
    }
    output <- data.table::as.data.table(stdf) |>
      dt_as_mysftime("lon", "lat", "time", crs_dt)
  } else {
    stop("x class not accepted")
  }
  return(output)
}


#' Convert an \code{sftime} to an \code{sf}
#' @description
#' Convert an \code{sftime} object to an \code{sf} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sftime` object
#' @param keeptime boolean: TRUE if user wants to keep time column
#' as simple column (default = TRUE)
#' @return an `sf` object
#' @author Eva Marques
#' @export
sftime_as_sf <- function(x, keeptime = TRUE) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  if (keeptime) {
    timecol <- attributes(x)$time_column
    output <- x[, !(colnames(x) %in% c(timecol))]
    output[, timecol] <- as.data.table(x)[, get(timecol)]
  } else {
    output <- x
    st_time(output) <- NULL
  }
  return(output)
}


#' Convert an \code{sftime} to a \code{mysftime}
#' @description
#' Convert an \code{sftime} object to a \code{mysftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sftime` object
#' @param timename character: name of time column in `x`
#' @return an `sftime` object with specific format
#' @seealso [check_mysftime]
#' @author Eva Marques
#' @export
sftime_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- rename_time(x, timename)
  return(output)
}


#' Convert an \code{sftime} to a \code{SpatVector}
#' @description
#' Convert an \code{sftime} object to a \code{SpatVector} object.
#' @param x an `sftime` object
#' @return a `SpatVector` object
#' @seealso [terra::vect]
#' @import sftime
#' @author Eva Marques
#' @keywords spacetime
#' @export
sftime_as_spatvector <- function(x) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  timecol <- attributes(x)$time_column
  tosf <- x[, !(colnames(x) %in% c(timecol))]
  tosf[, timecol] <- as.data.table(x)[, get(timecol)]
  return(terra::vect(tosf))
}


#' Convert an \code{sftime} to a \code{SpatRaster}
#' @description
#' Convert an \code{sftime} object to a \code{SpatRaster} object. Returns a
#' \code{SpatRatser} with one layer for each time step in \code{x}.
#' @note
#' Running \code{sftime_as_spatraster} can take a long time if \code{x} is not
#' spatially structured.
#' @param x an `sftime` object
#' @param varname variable to rasterize
#' @return a `SpatRaster` object
#' @import sftime
#' @import stars
#' @seealso [terra::rast]
#' @author Eva Marques
#' @keywords spacetime
#' @export
sftime_as_spatraster <- function(x, varname) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(x))
  dates <- unique(sftime::st_time(x))
  layers <- list()
  for (d in dates) {
    newrast <- stars::st_rasterize(x[which(st_time(x) == d), varname]) |>
      terra::rast()
    layers[[d]] <- newrast
  }
  return(terra::rast(layers))
}


#' Convert an \code{sftime} to a \code{SpatRasterDataset}
#' @description
#' Convert an \code{sftime} object to a \code{SpatRasterDataset} object.
#' @note
#' Running \code{sftime_as_spatrds} can take a long time if \code{x} is not
#' spatially and temporally structured.
#' @param x an `sftime` object
#' @import sftime
#' @return an `SpatRasterDataset` object
#' @seealso [terra::sds]
#' @author Eva Marques
#' @keywords spacetime
#' @export
sftime_as_spatrds <- function(x) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  timecol <- attributes(x)$time_column
  mysft <- x
  coords <- sf::st_coordinates(mysft)
  mysft$lon <- coords[, 1]
  mysft$lat <- coords[, 2]
  df <- as.data.frame(sf::st_drop_geometry(mysft))
  col <- colnames(df)
  variables <- col[!(col %in% c("lon", "lat", timecol))]
  rast_list <- list()
  for (var in variables) {
    newdf <- stats::reshape(
      df[, c("lon", "lat", timecol, var)],
      idvar = c("lon", "lat"),
      timevar = timecol,
      direction = "wide"
    )
    colnames(newdf) <- gsub(
      paste0(var, "."),
      "",
      colnames(newdf)
    )
    var_rast <- terra::rast(newdf,
      type = "xyz",
      crs = attributes(x$geometry)$crs
    )
    rast_list[[var]] <- var_rast
  }
  output <- terra::sds(rast_list)
  return(output)
}
