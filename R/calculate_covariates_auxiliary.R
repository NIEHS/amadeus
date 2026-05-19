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
  level,
  layer_time = NULL
) {
  message_time <- calc_time(
    time = time,
    format = time_type,
    dataset = dataset,
    layer_time = layer_time
  )
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

#' Validate extents overlap
#' @param x SpatRaster(1)
#' @param y SpatRaster(1)
#' @return logical(1)
#' @keywords internal auxiliary
#' @export
calc_extents_overlap <- function(x, y) {
  ext_x <- as.vector(terra::ext(x))
  ext_y <- as.vector(terra::ext(y))
  !(ext_x[2] < ext_y[1] ||
      ext_y[2] < ext_x[1] ||
      ext_x[4] < ext_y[3] ||
      ext_y[4] < ext_x[3])
}

#' Prepare optional weighting raster
#' @param from SpatRaster(1). Template raster.
#' @param weights NULL, SpatRaster, SpatVector/sf polygon, or file path.
#' @return NULL or single-layer SpatRaster aligned to `from`.
#' @keywords internal auxiliary
#' @export
calc_prepare_weights <- function(from, weights = NULL) {
  if (is.null(weights)) {
    return(NULL)
  }
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster when `weights` are supplied.")
  }

  normalize_vector_weights <- function(vect_weights) {
    if (terra::geomtype(vect_weights)[1] != "polygons") {
      stop(
        "`weights` vector input must contain polygons when supplied as ",
        "SpatVector/sf."
      )
    }
    from_crs <- terra::crs(from)
    if (is.na(from_crs) || from_crs == "") {
      stop("`from` is missing CRS; cannot validate weighted extraction CRS.")
    }
    vect_weights <- terra::project(vect_weights, from_crs)
    vect_df <- terra::as.data.frame(vect_weights)
    val_cols <- names(vect_weights)
    val_cols <- val_cols[sapply(val_cols, function(x) {
      is.numeric(vect_df[[x]])
    })]
    if (length(val_cols) == 0L) {
      vect_weights$.amadeus_weight <- 1
      val_col <- ".amadeus_weight"
    } else {
      val_col <- val_cols[1]
      if (length(val_cols) > 1L) {
        message(
          "Multiple numeric columns found in `weights`; using first column: ",
          val_col
        )
      }
    }
    if (any(vect_df[[val_col]] < 0, na.rm = TRUE)) {
      stop("`weights` values must be non-negative.")
    }
    weights_r <- terra::rasterize(
      vect_weights,
      from[[1]],
      field = val_col,
      background = NA,
      touches = TRUE
    )
    if (all(is.na(terra::values(weights_r)))) {
      stop("`weights` polygons do not overlap `from` extent.")
    }
    weights_r
  }

  weights_obj <- weights
  if (is.character(weights) && length(weights) == 1L) {
    weights_obj <- try(terra::rast(weights), silent = TRUE)
    if (inherits(weights_obj, "try-error")) {
      weights_obj <- try(terra::vect(weights), silent = TRUE)
      if (inherits(weights_obj, "try-error")) {
        stop("`weights` path could not be read as raster or vector data.")
      }
    }
  }

  if (inherits(weights_obj, c("sf", "sfc"))) {
    weights_obj <- terra::vect(weights_obj)
  }

  if (inherits(weights_obj, "SpatVector")) {
    return(normalize_vector_weights(weights_obj))
  }
  if (!inherits(weights_obj, "SpatRaster")) {
    stop(
      "`weights` must be NULL, SpatRaster, polygon SpatVector/sf, ",
      "or a file path to one of those."
    )
  }
  if (terra::nlyr(weights_obj) != 1L) {
    stop("`weights` raster must have exactly one layer.")
  }
  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop("`weights` raster values must be numeric.")
  }
  if (any(terra::values(weights_obj)[, 1] < 0, na.rm = TRUE)) {
    stop("`weights` values must be non-negative.")
  }

  from_crs <- terra::crs(from)
  weights_crs <- terra::crs(weights_obj)
  if (is.na(from_crs) || from_crs == "") {
    stop("`from` is missing CRS; cannot validate weighted extraction CRS.")
  }
  if (is.na(weights_crs) || weights_crs == "") {
    stop("`weights` is missing CRS; cannot validate weighted extraction CRS.")
  }

  weights_re <- terra::project(weights_obj, from[[1]], method = "bilinear")
  if (!calc_extents_overlap(from[[1]], weights_re)) {
    stop("`weights` extent does not overlap `from` extent.")
  }
  terra::resample(weights_re, from[[1]], method = "bilinear")
}

#' Convert point extractions to tiny polygons for exact extraction
#' @param locs_vector SpatVector(1)
#' @param radius numeric(1)
#' @return sf object for exactextractr
#' @keywords internal auxiliary
#' @export
calc_prepare_exact_geoms <- function(locs_vector, radius) {
  geom_type <- terra::geomtype(locs_vector)[1]
  if (geom_type == "polygons") {
    return(sf::st_as_sf(locs_vector))
  }
  if (geom_type != "points") {
    stop("Unsupported location geometry for weighted extraction.")
  }
  width <- as.numeric(radius)
  if (!is.finite(width) || width <= 0) {
    if (terra::is.lonlat(locs_vector)) {
      width <- 1e-6
    } else {
      width <- 1
    }
  }
  sf::st_as_sf(terra::buffer(locs_vector, width = width, quadsegs = 90L))
}

#' Resolve weighted summary function names for exactextractr
#' @param fun character(1)
#' @param weighted logical(1)
#' @return character(1)
#' @keywords internal auxiliary
#' @export
calc_weighted_fun <- function(fun, weighted = FALSE) {
  if (!weighted) {
    return(fun)
  }
  switch(
    fun,
    mean = "weighted_mean",
    sum = "weighted_sum",
    fun
  )
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
  format,
  dataset = NULL,
  layer_name = NULL,
  layer_time = NULL
) {
  parse_gridmet_day_code <- function(x) {
    x_chr <- as.character(x)
    if (!grepl("^[0-9]+$", x_chr)) {
      return(as.Date(NA))
    }
    as.Date(as.numeric(x_chr), origin = "1900-01-01")
  }
  extract_ymd_from_text <- function(x) {
    x_chr <- as.character(x)
    x_digits <- gsub("[^0-9]", "", x_chr)
    if (grepl("^[0-9]{8}$", x_digits)) {
      return(as.Date(x_digits, format = "%Y%m%d"))
    }
    if (grepl("^[0-9]{7}$", x_digits)) {
      return(as.Date(x_digits, format = "%Y%j"))
    }
    as.Date(NA)
  }
  to_posixlt_utc <- function(x) {
    as.POSIXlt(as.POSIXct(x, tz = "UTC"))
  }
  extract_digits <- function(x) {
    token <- as.character(x)[1]
    if (is.na(token)) {
      return("")
    }
    gsub("[^0-9]", "", token)
  }
  has_layer_time <- !is.null(layer_time) &&
    length(layer_time) > 0 &&
    !all(is.na(layer_time))

  if (format == "timeless") {
    return(time)
  }

  if (has_layer_time) {
    if (format == "hour") {
      return(to_posixlt_utc(layer_time[1]))
    }
    if (format == "date") {
      return(to_posixlt_utc(as.Date(layer_time[1])))
    }
    if (format == "year") {
      return(as.integer(format(as.Date(layer_time[1]), "%Y")))
    }
    if (format == "yearmonth") {
      return(as.integer(format(as.Date(layer_time[1]), "%Y%m")))
    }
  }

  if (format == "date") {
    time_chr <- as.character(time[1])
    parsed <- extract_ymd_from_text(time_chr)
    if (!is.na(parsed)) {
      return(to_posixlt_utc(parsed))
    }
    if (!is.null(layer_name) && grepl("=[0-9]+$", layer_name)) {
      day_code <- sub(".*=([0-9]+)$", "\\1", layer_name)
      parsed <- parse_gridmet_day_code(day_code)
      if (!is.na(parsed)) {
        return(to_posixlt_utc(parsed))
      }
    }
    stop(
      sprintf(
        "Unable to parse date for dataset '%s' from token '%s' (layer '%s').\n",
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "hour") {
    time_chr <- as.character(time)
    if (length(time_chr) >= 2) {
      date_digits <- gsub("[^0-9]", "", time_chr[1])
      hour_digits <- gsub("[^0-9]", "", time_chr[2])
      if (
        !is.na(date_digits) &&
          !is.na(hour_digits) &&
          nchar(date_digits) == 8 &&
          nchar(hour_digits) >= 2
      ) {
        hour_digits <- sprintf("%06d", as.integer(substr(hour_digits, 1, 6)))
        return(
          to_posixlt_utc(ISOdatetime(
            year = substr(date_digits, 1, 4),
            month = substr(date_digits, 5, 6),
            day = substr(date_digits, 7, 8),
            hour = substr(hour_digits, 1, 2),
            min = substr(hour_digits, 3, 4),
            sec = substr(hour_digits, 5, 6),
            tz = "UTC"
          ))
        )
      }
    }
    full_digits <- gsub("[^0-9]", "", paste(time_chr, collapse = ""))
    if (nchar(full_digits) >= 10) {
      dt <- as.POSIXct(
        substr(full_digits, 1, 14),
        format = "%Y%m%d%H%M%S",
        tz = "UTC"
      )
      if (!is.na(dt)) {
        return(to_posixlt_utc(dt))
      }
    }
    stop(
      sprintf(
        paste0(
          "Unable to parse datetime for dataset '%s' from token '%s' ",
          "(layer '%s').\n"
        ),
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "yearmonth") {
    digits <- extract_digits(time)
    if (nchar(digits) >= 6) {
      return(as.integer(substr(digits, 1, 6)))
    }
    stop(
      sprintf(
        paste0(
          "Unable to parse year-month for dataset '%s' from token '%s' ",
          "(layer '%s').\n"
        ),
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  if (format == "year") {
    digits <- extract_digits(time)
    if (nchar(digits) >= 4) {
      return(as.integer(substr(digits, 1, 4)))
    }
    stop(
      sprintf(
        "Unable to parse year for dataset '%s' from token '%s' (layer '%s').\n",
        ifelse(is.null(dataset), "unknown", dataset),
        paste(time, collapse = "_"),
        ifelse(is.null(layer_name), "unknown", layer_name)
      )
    )
  }

  stop("Unsupported time format.\n")
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
#' @param weights NULL, SpatRaster, polygon SpatVector/sf, or file path.
#'   Optional weighting surface used for weighted extraction.
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
  fun = "mean",
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  level = NULL,
  max_cells = 1e8,
  weights = NULL,
  ...
) {
  #### empty location data.frame
  sites_extracted <- NULL
  time_type <- match.arg(time_type)
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = fun,
    weighted = !is.null(weights_prepared)
  )
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    layer_time <- NULL
    #### split layer name
    data_split <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### extract variable
    data_name <- data_split[variable]
    if (!is.null(time)) {
      layer_time <- try(terra::time(data_layer), silent = TRUE)
      if (inherits(layer_time, "try-error")) {
        layer_time <- NULL
      }
      #### extract time
      data_time <- calc_time(
        time = data_split[time],
        format = time_type,
        dataset = dataset,
        layer_name = names(data_layer),
        layer_time = layer_time
      )
    }
    #### extract level (if applicable)
    if (!is.null(level)) {
      data_level <- data_split[level]
    } else {
      data_level <- NULL
    }
    layer_time_msg <- if (!is.null(time)) data_split[time] else NA_character_
    #### message
    calc_message(
      dataset = dataset,
      variable = data_name,
      time = layer_time_msg,
      time_type = time_type,
      level = data_level,
      layer_time = layer_time
    )
    #### extract layer data at sites
    if (terra::geomtype(locs_vector) == "polygons") {
      ### apply exactextractr::exact_extract for polygons
      extract_args <- list(
        x = data_layer,
        y = sf::st_as_sf(locs_vector),
        progress = FALSE,
        force_df = TRUE,
        fun = fun_extract,
        max_cells_in_memory = max_cells
      )
      if (!is.null(weights_prepared)) {
        extract_args$weights <- weights_prepared
      }
      sites_extracted_layer <- do.call(
        exactextractr::exact_extract,
        extract_args
      )
    } else if (terra::geomtype(locs_vector) == "points") {
      if (is.null(weights_prepared)) {
        #### apply terra::extract for points
        sites_extracted_layer <- terra::extract(
          data_layer,
          locs_vector,
          method = "simple",
          ID = FALSE,
          bind = FALSE,
          na.rm = TRUE
        )
      } else {
        weighted_geoms <- amadeus::calc_prepare_exact_geoms(
          locs_vector = locs_vector,
          radius = radius
        )
        sites_extracted_layer <- exactextractr::exact_extract(
          x = data_layer,
          y = weighted_geoms,
          weights = weights_prepared,
          progress = FALSE,
          force_df = TRUE,
          fun = fun_extract,
          max_cells_in_memory = max_cells
        )
      }
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


#' Build standardized error for legacy grouping usage
#' @keywords internal
#' @noRd
stop_legacy_by_error <- function() {
  stop(
    paste0(
      "`.by` is no longer supported in calculate APIs. ",
      "Use `.by_time` for temporal summarization (e.g., 'day', ",
      "'week', 'month', or 'year').\n"
    )
  )
}


#' Reject deprecated legacy grouping argument in dots
#' @description
#' Internal helper for calculate APIs that now support temporal
#' summarization via \code{.by_time} only. Stops immediately when a
#' deprecated legacy grouping argument is supplied through \code{...}.
#' @param ... Placeholders.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops on deprecated legacy grouping input.
#' @export
check_unsupported_by <- function(..., .call = NULL) {
  dots <- list(...)
  call_names <- character(0)
  if (!is.null(.call)) {
    call_names <- names(as.list(.call)[-1])
  }
  if (".by" %in% names(dots) || ".by" %in% call_names) {
    stop_legacy_by_error()
  }
  invisible(NULL)
}


#' Validate the `.by_time` temporal summarization argument
#' @description
#' Validates the \code{.by_time} argument used by covariate extraction
#' functions for temporal summarization. When non-\code{NULL},
#' \code{.by_time} must be a single character string naming a supported
#' temporal unit token (singular or plural): \code{"minute"},
#' \code{"hour"}, \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"quarter"}, or \code{"year"}.
#' @param .by_time NULL or character(1). Temporal summarization unit.
#'   \code{NULL} means no temporal summarization.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return \code{NULL} invisibly; stops with an informative error if the
#'   value is invalid.
#' @export
check_by_time <- function(.by_time) {
  if (is.null(.by_time)) {
    return(invisible(NULL))
  }
  if (!is.character(.by_time) || length(.by_time) != 1L) {
    stop(
      paste0(
        "`.by_time` must be NULL or a single character string naming ",
        "a temporal unit.
"
      )
    )
  }
  allowed <- c(
    "minute",
    "minutes",
    "hour",
    "hours",
    "day",
    "days",
    "week",
    "weeks",
    "month",
    "months",
    "quarter",
    "quarters",
    "year",
    "years"
  )
  if (!.by_time %in% allowed) {
    stop(
      paste0(
        "`.by_time` must be one of: ",
        paste(allowed, collapse = ", "),
        ".
"
      )
    )
  }
  invisible(NULL)
}

#' Normalize `.by_time` time-unit aliases
#' @description Internal helper that maps singular/plural `.by_time` tokens
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
    stop("`unit` must be one valid `.by_time` time-unit token.\n")
  }
  aliases[[unit]]
}


#' Bucket a time column to a `.by_time` unit
#' @description Buckets time values to one of the supported `.by_time` units.
#' @param time_vals vector. Time values to bucket.
#' @param unit character(1). A valid `.by_time` time-unit token.
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
  } else if (all(grepl("^[0-9]{7}$", time_vals_chr))) {
    time_date <- as.Date(time_vals_chr, format = "%Y%j")
  } else if (all(grepl("^[0-9]{6}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "01"), format = "%Y%m%d")
  } else if (all(grepl("^[0-9]{4}$", time_vals_chr))) {
    time_date <- as.Date(paste0(time_vals_chr, "-01-01"))
  } else {
    time_date <- as.Date(time_vals, tz = "UTC")
    if (any(is.na(time_date))) {
      stop(
        "Unable to bucket time values. Provide parseable Date/POSIXct values ",
        "or recognized numeric encodings (YYYYDDD, YYYYMMDD, YYYYMM, YYYY).\n"
      )
    }
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


#' Summarize extracted covariates by `.by_time` temporal unit
#' @description
#' Generic temporal summarizer for covariate tables. When
#' \code{.by_time} is \code{NULL}, the input is returned unchanged.
#' Otherwise, numeric covariates are summarized by
#' \code{locs_id + bucketed time + group_cols_extra}.
#' @param covar data.frame. Extracted covariates.
#' @param fun_summary character(1) or function. Summary function
#'   (e.g., \code{"mean"}, \code{"sum"}).
#' @param locs_id character(1). Location-id column.
#' @param time_col character(1). Time column in \code{covar}.
#' @param .by_time NULL or character(1). Temporal unit token.
#' @param group_cols_extra character or NULL. Extra grouping columns.
#' @param ... Placeholders.
#' @keywords internal auxiliary
#' @author Insang Song
#' @return a data.frame.
#' @importFrom dplyr across all_of group_by summarize left_join
#' @export
calc_summarize_by <- function(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  .by_time = NULL,
  group_cols_extra = NULL,
  ...
) {
  check_unsupported_by(..., .call = sys.call())
  if (is.null(.by_time)) {
    return(covar)
  }
  stopifnot(is.data.frame(covar))
  check_by_time(.by_time)

  if (!locs_id %in% names(covar)) {
    stop(sprintf("`locs_id` column '%s' not found in `covar`.\n", locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf("`time_col` column '%s' not found in `covar`.\n", time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(
        sprintf(
          "Grouping column(s) not found in `covar`: %s.\n",
          paste(missing_extra, collapse = ", ")
        )
      )
    }
  }

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

  group_cols <- c(locs_id, time_col, group_cols_extra)
  group_cols <- unique(stats::na.omit(group_cols))
  covar2 <- covar
  covar2[[time_col]] <- bucket_time_by_unit(covar2[[time_col]], .by_time)

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

  if ("geometry" %in% names(covar2)) {
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

#' Summarize extracted covariates at native temporal grain
#' @description Internal helper that summarizes numeric covariates by
#' \code{locs_id + time + group_cols_extra} while preserving the original time
#' representation.
#' @param covar data.frame. Extracted covariates.
#' @param fun_summary character(1) or function. Summary function.
#' @param locs_id character(1). Location-id column.
#' @param time_col character(1). Time column in \code{covar}.
#' @param group_cols_extra character or NULL. Extra grouping columns.
#' @return a data.frame.
#' @keywords internal auxiliary
#' @export
calc_summarize_native_time <- function(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
) {
  stopifnot(is.data.frame(covar))
  if (!locs_id %in% names(covar)) {
    stop(sprintf("`locs_id` column '%s' not found in `covar`.\n", locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf("`time_col` column '%s' not found in `covar`.\n", time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra <- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) > 0L) {
      stop(
        sprintf(
          "Grouping column(s) not found in `covar`: %s.\n",
          paste(missing_extra, collapse = ", ")
        )
      )
    }
  }
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

  group_cols <- unique(stats::na.omit(c(locs_id, time_col, group_cols_extra)))
  cov_cols <- names(covar)[vapply(covar, is.numeric, logical(1))]
  cov_cols <- setdiff(cov_cols, group_cols)
  if (length(cov_cols) == 0L) {
    stop("No numeric covariate columns found to summarize.\n")
  }

  summary_df <- covar |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(cov_cols), \(x) fun_r(x, na.rm = TRUE)),
      .groups = "drop"
    )

  if ("geometry" %in% names(covar)) {
    geom_first <- covar[
      !duplicated(covar[, group_cols, drop = FALSE]),
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

#' Apply default/native or explicit temporal summarization
#' @param covar data.frame.
#' @param .by_time NULL or character(1).
#' @param fun_summary character(1) or function.
#' @param locs_id character(1).
#' @param time_col character(1).
#' @param group_cols_extra character or NULL.
#' @return data.frame
#' @keywords internal auxiliary
#' @export
calc_apply_time_summary <- function(
  covar,
  .by_time = NULL,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
) {
  if (is.null(.by_time)) {
    return(
      calc_summarize_native_time(
        covar = covar,
        fun_summary = fun_summary,
        locs_id = locs_id,
        time_col = time_col,
        group_cols_extra = group_cols_extra
      )
    )
  }
  calc_summarize_by(
    covar = covar,
    fun_summary = fun_summary,
    locs_id = locs_id,
    time_col = time_col,
    .by_time = .by_time,
    group_cols_extra = group_cols_extra
  )
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
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
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
  weights = NULL,
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
    weights = NULL,
    maxcells = NULL
  ) {
    # generate buffers
    if (radius == 0) {
      radius <- 1e-6
    } # approximately 1 meter in degree
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # extract raster values
    weights_norm <- amadeus::calc_prepare_weights(
      from = surf[[1]],
      weights = weights
    )
    func_extract <- amadeus::calc_weighted_fun(
      fun = func,
      weighted = !is.null(weights_norm)
    )
    extract_args <- list(
      x = surf,
      y = sf::st_as_sf(bufs),
      fun = func_extract,
      force_df = TRUE,
      stack_apply = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = maxcells
    )
    if (!is.null(weights_norm)) {
      extract_args$weights <- weights_norm
    }
    surf_at_bufs <- do.call(exactextractr::exact_extract, extract_args)
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
      weights = weights,
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
