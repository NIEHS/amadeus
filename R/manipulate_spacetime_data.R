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
dt_to_sf <- function(datatable, crs) {
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
dt_to_sftime <- function(datatable, crs) {
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
  loc_sf <- dt_to_sf(loc, crs_ori)
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
