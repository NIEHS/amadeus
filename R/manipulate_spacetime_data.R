# nolint start
#' Convert spatio-temporal object to a datatable with lon, lat, time, predictors columns. It also returns the crs.
# nolint end
#' @param stobj object containing space-time data. It can be a data.frame,
#' a data.table, an sf or sftime, a SpatVector or a SpatRastDataset.
#' @return a list with a "stdt" a data.table of locations identified by
#' lat, lon, time columns and "crs_dt" the crs of the data in well-known text
#' format.
#' @importFrom data.table as.data.table
#' @importFrom terra crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_coordinates
#' @importFrom data.table melt
#' @importFrom data.table .SD
#' @importFrom sf st_crs
#' @author Eva Marques, Insang Song
#' @export
convert_stobj_to_stdt <- function(stobj) {
  format <- class(stobj)[[1]]
  names_stcols <- c("lon", "lat")

  if (format == "data.frame" || format == "data.table") {
    if (any(!(c("lon", "lat", "time") %in% colnames(stobj)))) {
      stop("Error: stobj does not contain lon, lat, time columns")
    }
    stdt <- data.table::as.data.table(stobj)
    crs_dt <- NA
  } else if (format == "sf" || format == "sftime") {
    if (any(!(c("geometry", "time") %in% colnames(stobj)))) {
      stop("Error: stobj does not contain geometry and time columns")
    }

    crs_dt <- sf::st_crs(stobj)$wkt

    stobj[, names_stcols] <- sf::st_coordinates(stobj)
    stobj <- sf::st_drop_geometry(stobj)
    stdt <- data.table::as.data.table(stobj)
  } else if (format == "SpatVector") {
    if (!("time") %in% names(stobj)) {
      stop("stobj does not contain time column")
    }

    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj, geom = "XY")
    names(stdf)[names(stdf) == "x"] <- "lon"
    names(stdf)[names(stdf) == "y"] <- "lat"

    stdt <- data.table::as.data.table(stdf)
  } else if (format == "SpatRasterDataset") {
    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj[1], xy = TRUE)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"

    # -- tranform from wide to long format
    stdf <- stdf |>
      data.table::as.data.table() |>
      data.table::melt(
        measure.vars = names(stdf)[-1:-2],
        variable.name = "time",
        value.name = names(stobj)[1]
      )

    for (var in seq(2, length(names(stobj)))) {
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(stobj[var]), names(stobj[1])))) {
        stop("Error in SpatRastDataset:
        time series is different for at least
             2 variables - or not ordered for one of these.")
      }

      varname_original <- names(stobj)[var]
      df_var <- as.data.frame(stobj[var], xy = TRUE)
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
    stdt <- data.table::as.data.table(stdf)
  } else {
    stop("Error: stobj class not accepted")
  }

  names_vars <- names(stdt)[!names(stdt) %in% c(names_stcols, "time")]
  names_sorted <- c(names_stcols, "time", names_vars)
  stdt <- stdt[, .SD, .SDcols = names_sorted]

  # sort stdt
  stdt <- data.table::setorderv(stdt, cols = c("lon", "lat", "time"))
  stdt <- stdt[order(stdt, na.last = TRUE)]

  stdt_result <- list("stdt" = stdt, "crs_stdt" = crs_dt)
  class(stdt_result) <- c("list", "stdt")
  return(stdt_result)
}

#' Boolean to know if an object correspond to a stdtobj
#'
#' @param obj an object
#' @return a boolean to know if obj is from newly created class "stdt"
#' @author Eva Marques
#' @export
is_stdt <- function(obj) {
  if (!(identical(class(obj), c("list", "stdt")))) {
    return(FALSE)
  } else if (!(identical(names(obj), c("stdt", "crs_stdt")))) {
    return(FALSE)
  } else if (!(identical(class(obj$stdt)[1], "data.table"))) {
    return(FALSE)
  } else if (!(all(c("lon", "lat", "time") %in% colnames(obj$stdt)))) {
    return(FALSE)
  } else if (!is.character(obj$crs_stdt)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check if the sftime object is formated on a specific way
#'
#' @param x a sftime object
#' @import sftime
#' @author Eva Marques
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

#' Check if the sf object is formated on a specific way
#'
#' @param x a sf object
#' @import sf
#' @author Eva Marques
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

#' Create a sftime from a datatable
#'
#' @param x a data.table
#' @param lonname character for longitude column name
#' @param latname character for latitude column name
#' @param timename character for time column name
#' @param crs coordinate reference system
#' @return a sftime object
#' @import sftime
#' @author Eva Marques
#' @export
dt_as_mysftime <- function(x, lonname, latname, timename, crs) {
  stopifnot("x is not a data.table" = class(x)[1] == "data.table")
  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop("Some of lon, lat, time columns missing or mispelled")
  }
  mysft <- st_as_sftime(x,
    coords = c(lonname, latname),
    time_column_name = timename,
    crs = crs
  ) |>
    dplyr::rename("time" = timename)
  return(mysft)
}

#' Create a sftime from a terra::SpatRaster
#'
#' @param x a terra::SpatRaster
#' @param varname character for variable column name in the sftime
#' @param timename character for time column name in the sftime
#' (default: "time")
#' @return a sftime object
#' @import sftime
#' @author Eva Marques
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
  names(output)[names(output) == "time"] <- timename
  attributes(output)$time_column <- timename
  return(output)
}

#' Create a sftime from a terra::SpatRasterDataset
#'
#' @param x a terra::SpatRasterDataset
#' @param timename character for time column name in the sftime
#' (default: "time")
#' @return a sftime object
#' @import sftime
#' @author Eva Marques
#' @export
spatrds_as_sftime <- function(x, timename = "time") {
  stopifnot("x is not a SpatRasterDataset" =
              class(x)[1] == "SpatRasterDataset")
  variables <- names(x)
  sftime_list <- list()
  newsft <- spatraster_as_sftime(x[[variables[1]]],
                                 varname = variables[1],
                                 timename = timename) 
  for (var in variables[2:length(variables)]) {
    newsft[, var] <- st_drop_geometry(
      spatraster_as_sftime(x[[var]],
                           varname = var,
                           timename = timename)[, var])
  }
  return(newsft)
}

#' Simplify an sftime to sf class
#'
#' @param x a sftime
#' @param keeptime boolean: TRUE if user wants to keep time column
#' as simple column (default = TRUE)
#' @return a sf object
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

#' Convert a sf object to mysftime
#'
#' @param x a sf
#' @param timename character: name of time column in x
#' @return a sftime object
#' @author Eva Marques
#' @export
sf_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- st_as_sftime(x, time_column_name = timename)
  attributes(output)$time_column <- "time"
  output <- dplyr::rename(output, "time" = timename)
  return(output)
}

#' Convert a sftime object to mysftime
#'
#' @param x a sftime
#' @param timename character: name of time column in x
#' @return a sftime object with specific format (see check_mysftime() function)
#' @author Eva Marques
#' @export
sftime_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- x
  attributes(output)$time_column <- "time"
  output <- dplyr::rename(output, "time" = timename)
  return(output)
}

#' Create a sftime from a terra::SpatVector
#'
#' @param x a terra::SpatVector
#' @param timename character for time column name in x
#' (default: "time")
#' @return a sftime object
#' @import sftime
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


#' Convert to sftime object on the form adapted to beethoven code
#'
#' @param x a data.frame, data.table, SpatVector or SpatRasterDataset
#' @param ... if x is a data.frame or data.table: lonname, latname, timename and
#' crs arguments are required. If x is a sf or sftime, timename argument is
#' required. If x is a terra::SpatRaster, varname argument is required.
#' @return a sftime object with constrained time column name
#' (see check_mysftime() function)
#' @import sf
#' @author Eva Marques
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
    attributes(output)$time_column <- "time"
    output <- dplyr::rename(output, "time" = timename)
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

#' Convert sftime object to SpatVector
#'
#' @param x a sftime
#' @return a terra::SpatVector
#' @import sftime
#' @author Eva Marques
#' @export
sftime_as_spatvector <- function(x) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  timecol <- attributes(x)$time_column
  tosf <- x[, !(colnames(x) %in% c(timecol))]
  tosf[, timecol] <- as.data.table(x)[, get(timecol)]
  return(terra::vect(tosf))
}

#' Convert sftime object to SpatRaster
#' /!\ can be very time consuming if sftime is not spatially structured
#'
#' @param x a sftime
#' @param varname variable to rasterize
#' @return a SpatRaster with layers corresponding to timestamps
#' @import sftime
#' @import stars
#' @author Eva Marques
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

#' Convert sftime object to SpatVector
#'
#' @param x a sftime
#' @import sftime
#' @author Eva Marques
#' @export
sftime_as_spatrds <- function(x) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  df <- as.data.frame(x)
  col <- colnames(df)
  variables <- col[!(col %in% c("lon", "lat", "time"))]
  rast_list <- list()
  for (var in variables) {
    newdf <- stats::reshape(
      df[, c("lon", "lat", "time", var)],
      idvar = c("lon", "lat"),
      timevar = "time",
      direction = "wide"
    )
    colnames(newdf) <- gsub(
      paste0(var, "."),
      "",
      colnames(newdf)
    )
    var_rast <- terra::rast(newdf, type = "xyz",
                            crs = attributes(x$geometry)$crs)
    rast_list[[var]] <- var_rast
  }
  output <- terra::sds(rast_list)
  return(output)
}



#' Convert a stdt to sf/sftime/SpatVector
#' @param stdt A stdt object
#' @param class_to character(1). Should be one of
#' `"sf"`, `"sftime"`, `"SpatRasterDataset"`, or `"SpatVector"`
#' @return a sf/sftime/SpatRasterDataset/SpatVector
#' @importFrom sf st_as_sf
#' @importFrom terra vect
#' @importFrom terra sds
#' @author Insang Song
#' @export
convert_stdt <- function(
    stdt,
    class_to = c("sf", "sftime", "SpatVector", "SpatRasterDataset")) {
  if (!is_stdt(stdt)) {
    stop("The input for stdt argument is not an stdt object")
  }
  class_to <- match.arg(class_to)

  converted <- switch(class_to,
    sf = sf::st_as_sf(convert_stdt_sftime(stdt)),
    sftime = convert_stdt_sftime(stdt),
    SpatRasterDataset = convert_stdt_spatrastdataset(stdt),
    SpatVector = convert_stdt_spatvect(stdt)
  )
  return(converted)
}


#' Convert a stdt to SpatVector
#' @param stdt A stdt object
#' @return a SpatVector
#' @author Eva Marques
#' @importFrom terra vect
#' @export
convert_stdt_spatvect <- function(stdt) {
  if (!is_stdt(stdt)) {
    stop("The input for stdt argument is not an stdt object")
  }
  vect_obj <-
    terra::vect(stdt$stdt,
      geom = c("lon", "lat"),
      crs = stdt$crs_stdt,
      keepgeom = FALSE
    )
  return(vect_obj)
}


#' Convert a stdtobj to sftime
#' @param stdt A stdt object
#' @return a sftime object
#' @author Eva Marques
#' @export
convert_stdt_sftime <- function(stdt) {
  if (!is_stdt(stdt)) {
    stop("The input for stdt argument is not an stdt object")
  }
  stdt_stdt <- stdt$stdt
  stdt_stdt$time <- as.Date(stdt_stdt$time)
  sftime_obj <-
    sftime::st_as_sftime(
      stdt_stdt,
      coords = c("lon", "lat"),
      time_column_name = "time",
      crs = stdt$crs_stdt
    )
  return(sftime_obj)
}


#' Convert a stdtobj to SpatRasterDataset
#'
#' @param stdt A stdt object
#' @return a SpatRasterDataset with each raster corresponding to one variable
#' (layers are the time series)
#' @author Eva Marques
#' @importFrom stats reshape
#' @importFrom terra sds
#' @export
convert_stdt_spatrastdataset <- function(stdt) {
  if (!is_stdt(stdt)) {
    stop("The input for stdt argument is not an stdt object")
  }
  df <- as.data.frame(stdt$stdt)
  col <- colnames(df)
  variables <- col[!(col %in% c("lon", "lat", "time"))]
  rast_list <- list()
  for (var in variables) {
    newdf <- stats::reshape(
      df[, c("lon", "lat", "time", var)],
      idvar = c("lon", "lat"),
      timevar = "time",
      direction = "wide"
    )
    colnames(newdf) <- gsub(
      paste0(var, "."),
      "",
      colnames(newdf)
    )

    var_rast <- terra::rast(newdf, type = "xyz", crs = stdt$crs_stdt)
    rast_list[[var]] <- var_rast
  }
  rastdt_obj <- terra::sds(rast_list)
  return(rastdt_obj)
}

#' Create a sf object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the original crs
#' @author Eva Marques
#' @importFrom sf st_as_sf
#' @return an sf object
dt_as_sf <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!is.character(crs)) {
    stop("crs is not a character")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }

  data_sf <- sf::st_as_sf(
    datatable,
    coords = c("lon", "lat"),
    remove = FALSE,
    crs = crs
  )
  return(data_sf)
}


#' Create a sftime object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon", "time"
#' @param crs A character containing the original crs
#' @note "time" column in datatable argument should be in date format,
#' e.g., "2023-01-01", "01/01/2023", etc.
#' @author Eva Marques
#' @return an sftime object
dt_as_sftime <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!is.character(crs)) {
    stop("crs is not a character")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("time" %in% colnames(datatable))) {
    stop("datatable does not contain time column")
  }

  datatable$date <- as.Date(datatable$time)
  data_sft <- sftime::st_as_sftime(
    datatable,
    coords = c("lon", "lat"),
    remove = FALSE,
    crs = crs,
    time_column_name = "time"
  )
  return(data_sft)
}


#' Project coordinates in a datatable from crs_ori to crs_dest
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs_ori A character containing the original crs of spatial data
#' @param crs_dest A character containing the destination crs of spatial data
#' @note This function assumes that users have point geometry.
#' @author Eva Marques
#' @importFrom sf st_coordinates
#' @importFrom sf st_transform
#' @importFrom sf st_drop_geometry
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @importFrom methods is
#' @return same datatable object with "lon", "lat",
#' "lon_ori", "lat_ori" columns
project_dt <- function(datatable, crs_ori, crs_dest) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!is.character(crs_ori) || !is.character(crs_dest)) {
    stop("crs are not characters")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }

  loc <- unique(datatable[, c("lon", "lat")])
  loc_sf <- dt_as_sf(loc, crs_ori)
  loc_sf <- sf::st_transform(loc_sf, crs_dest)
  colnames(loc_sf)[colnames(loc_sf) == "lon"] <- "lon_ori"
  colnames(loc_sf)[colnames(loc_sf) == "lat"] <- "lat_ori"
  loc_sf_coords <- sf::st_coordinates(loc_sf)
  loc_sf[["lon"]] <- loc_sf_coords[, 1]
  loc_sf[["lat"]] <- loc_sf_coords[, 2]

  loc_proj <- sf::st_drop_geometry(loc_sf)
  loc_proj <- data.table::as.data.table(loc_proj)

  # renaming is only valid within the function
  colnames(datatable)[colnames(datatable) == "lon"] <- "lon_ori"
  colnames(datatable)[colnames(datatable) == "lat"] <- "lat_ori"
  datatable_proj <- merge(
    datatable,
    loc_proj,
    by = c("lon_ori", "lat_ori")
  )

  return(datatable_proj)
}
