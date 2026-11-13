################################################################################
#' Check data download acknowledgement
#' @description
#' Return an error if the \code{acknowledgement = FALSE}.
#' @param acknowledgement logical(1). Whether to
#' start downloading
#' @note
#' The \code{acknowledgement} parameter is designed to help users avoid
#' accidentally initiating a very large data download that may take a very long
#' time to run or exceed machine capabilities.
#' @return NULL; returns a stop error if the acknowledgement is FALSE
#' @keywords internal
#' @export
download_permit <-
  function(acknowledgement) {
    # --------------------------------------------------------------------------
    # Stop when the download acknowledgement has not been given.
    if (!acknowledgement) {
      stop(paste0(
        "Data download acknowledgement is set to FALSE. ",
        "Please acknowledge that the data downloaded using this ",
        "function may be very large and use lots of machine storage ",
        "and memory.\n"
      ))
    }
  }

################################################################################
#' Check parameters
#' @description
#' Check that all parameters have been assigned a value.
#' @param parameters parameters passed to function (called by
#' \code{mget(ls())}.)
#' @return NULL; returns a stop error if one or more function
#' parameters other than 'extent' are NULL
#' @keywords internal
#' @export
check_for_null_parameters <-
  function(
    parameters
  ) {
    # --------------------------------------------------------------------------
    # Exclude the optional spatial extent from required parameters.
    if ("extent" %in% names(parameters)) {
      parameters <- parameters[-grep("extent", names(parameters))]
    }
    # --------------------------------------------------------------------------
    # Stop if any remaining parameter is NULL.
    parameters_status <- any(unlist(lapply(parameters, is.null)))
    if (parameters_status) {
      stop(paste0("One or more parameters are NULL\n"))
    }
  }

################################################################################
#' Sink download commands
#' @description
#' Open connection to \code{command_txt} file to store download commands.
#' @param command_txt character(1). file path to export commands.
#' @return NULL; creates and opens connection to text file to store
#' download commands
#' @keywords internal
#' @export
download_sink <-
  function(command_txt) {
    # --------------------------------------------------------------------------
    # Remove an existing download command file.
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    # --------------------------------------------------------------------------
    # Redirect subsequent output to a new command file.
    sink(file = command_txt, append = FALSE)
  }

################################################################################
#' Import download commands
#' @description
#' Read download commands from .txt file and convert to character vector.
#' @param commands_path file path with wget/curl commands
#' @return character vector containing download commands
#' @importFrom utils read.csv
#' @keywords internal
#' @export
read_commands <- function(
  commands_path = commands_path
) {
  # ----------------------------------------------------------------------------
  # Read the command file and return its rows as a vector.
  commands <- utils::read.csv(commands_path, header = FALSE)
  commands <- commands[seq_len(nrow(commands)), ]
  return(commands)
}

################################################################################
#' Extract download URLs
#' @description
#' Extract download URLs from multi-argument download commands.
#' @param commands character vector containing download commands
#' @param position URL position in the vector
#' @return character vector containing download URLs
#' @keywords internal
#' @export
extract_urls <- function(
  commands = commands,
  position = NULL
) {
  # ----------------------------------------------------------------------------
  # Return early when the URL position is unspecified.
  if (is.null(position)) {
    message("URL position in command is not defined.")
    return(NULL)
  }
  # ----------------------------------------------------------------------------
  # Extract the requested space-delimited field from each command.
  urls <- sapply(
    strsplit(
      trimws(commands),
      " "
    ),
    function(x, l) x[l],
    l = position
  )
  return(urls)
}

################################################################################
#' Legacy download_run function for backwards compatibility
#' @description
#' **DEPRECATED**: This function is maintained for backwards compatibility.
#' New code should use `download_run_method()` directly.
#'
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (\code{TRUE}) or skip (\code{FALSE})
#' download.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' command.
#' @return NULL; runs download commands with shell (Unix/Linux) or
#' command prompt (Windows) and removes \code{commands_txt} file if
#' \code{remove = TRUE}.
#' @keywords internal
#' @export
download_run <- function(
  download = FALSE,
  commands_txt = NULL,
  remove = FALSE
) {
  # ----------------------------------------------------------------------------
  # Warn about the legacy command runner once per session.
  if (!isTRUE(getOption("amadeus.download_run.warned"))) {
    warning(
      "download_run() is deprecated. Use download_run_method() instead.\n",
      "  Old: download_modis(..., download = TRUE)\n",
      "  New: download_modis(...) uses httr2 by default\n",
      call. = FALSE
    )
    options(amadeus.download_run.warned = TRUE)
  }
  # ----------------------------------------------------------------------------
  # Prepare the command file and runner for the current operating system.
  if (tolower(.Platform$OS.type) == "windows") {
    # nocov start
    runner <- ""
    commands_bat <- gsub(".txt", ".bat", commands_txt)
    file.rename(commands_txt, commands_bat)
    commands_txt <- commands_bat
    # nocov end
  } else {
    runner <- ". "
    if (!grepl("(^/|^\\./|^\\.\\./)", commands_txt)) {
      commands_txt <- paste0("./", commands_txt)
    }
  }
  # ----------------------------------------------------------------------------
  # Execute or skip the saved download commands.
  system_command <- paste0(runner, shQuote(commands_txt))
  if (download == TRUE) {
    message(paste0("Downloading requested files...\n"))
    system(command = system_command, intern = TRUE)
    message(paste0("Requested files have been downloaded.\n"))
  } else {
    message(paste0("Skipping data download.\n"))
  }
  # ----------------------------------------------------------------------------
  # Remove the command file when requested.
  download_remove_command(
    commands_txt = commands_txt,
    remove = remove
  )
}

################################################################################
#' Remove download commands
#' @description
#' Remove or retain the .txt file storing all download commands.
#' @param commands_txt character(1). Path of download commands
#' @param remove logical(1). Remove (\code{TRUE}) or
#'  keep (\code{FALSE}) commands
#' @return NULL; removes .txt/.bat file storing all download commands.
#' @keywords internal
#' @export
download_remove_command <-
  function(commands_txt = NULL, remove = FALSE) {
    # --------------------------------------------------------------------------
    # Remove the command file when requested.
    if (remove) {
      file.remove(commands_txt)
    }
  }

################################################################################
#' Generate date sequence
#' @description
#' Generate a sequence of dates from \code{date_start} to \code{date_end}.
#' @param date_start character(1). Beginning of date sequence.
#' @param date_end character(1). End of date sequence.
#' @param sub_hyphen logical(1). Substitute hyphen in dates. If `TRUE`, returns
#' date sequence as "YYYYMMDD". If `FALSE`, returns date sequence as
#' "YYYY-MM-DD".
#' @return vector
#' @keywords internal auxiliary
#' @export
generate_date_sequence <-
  function(
    date_start,
    date_end,
    sub_hyphen = TRUE
  ) {
    # --------------------------------------------------------------------------
    # Generate daily dates between the supplied endpoints.
    dates_original <- seq(
      as.Date(date_start, format = "%Y-%m-%d"),
      as.Date(date_end, format = "%Y-%m-%d"),
      "day"
    )
    # --------------------------------------------------------------------------
    # Return compact date strings or retain the Date vector.
    if (sub_hyphen) {
      dates_sub_hyphen <- gsub("-", "", as.character(dates_original))
      return(dates_sub_hyphen)
    } else {
      return(dates_original)
    }
  }

################################################################################
#' Generate time sequence
#' @description
#' Generate a sequence of time values based on the GEOS-CF collection.
#' @param collection character(1). GEOS-CF data collection
#' @return vector
#' @note
#' GEOS-CF hourly values are observed on the hour (ie. 0000 = 12:00:00 AM, 0100
#' = 01:00:00 AM) or the half hour (ie. 0030 = 12:30:00 AM, 0130 = 01:30:00 AM).
#' Typically, 2-dimensional collections (latitude and longitude only) utilize
#' half hour, and 3-dimensional collections (latitude, longitude, and time)
#' utilize hour.
#' @keywords internal auxiliary
#' @export
generate_time_sequence <-
  function(
    collection
  ) {
    # --------------------------------------------------------------------------
    # Select whole-hour or half-hour values using the collection suffix.
    collection_end <- substr(collection, nchar(collection), nchar(collection))
    if (collection_end == "1") {
      ts <- seq(from = 30, to = 2330, by = 100)
    } else if (collection_end == "3") {
      ts <- seq(from = 0, to = 2300, by = 100)
    }
    # --------------------------------------------------------------------------
    # Format the sequence as four-digit clock times.
    time_sequence <- sprintf("%04d", ts)
    return(time_sequence)
  }

################################################################################
#' Check \code{sftime} object
#' @description
#' Check an \code{sftime} object for its class, \code{$time} column,
#' \code{$geometry} column, and geometry class.
#' @param x an `sftime` object
#' @import sftime
#' @author Eva Marques
#' @keywords internal spacetime
#' @return NULL; returns stop error if \code{x} does not match class
#' and column expectations
#' @export
check_mysftime <- function(x) {
  stopifnot(
    "x is not a sftime" = class(x)[1] == "sftime",
    "x is not inherited from a data.table" = class(x)[3] == "data.table",
    "time column should be called time" = attributes(x)$time_column == "time",
    "geometry column should be called geometry" = attributes(x)$sf_column ==
      "geometry",
    "geometry is not a sfc_POINT" = class(x$geometry)[1] == "sfc_POINT"
  )
}

################################################################################
#' Check \code{sf} object
#' @description
#' Check an \code{sf} object for its class, \code{$geometry} column, and
#' geometry class.
#' @param x an `sf` object
#' @import sf
#' @author Eva Marques
#' @keywords internal spacetime
#' @return NULL; returns stop error if \code{x} does not match class
#' and/or column expectations
#' @export
check_mysf <- function(x) {
  stopifnot(
    "x is not a sf" = class(x)[1] == "sf",
    "x is not inherited from a data.table" = class(x)[2] == "data.table",
    "geometry column should be called geometry" = attributes(x)$sf_column ==
      "geometry",
    "geometry is not a sfc_POINT" = class(x$geometry)[1] == "sfc_POINT"
  )
}

################################################################################
#' Rename \code{$time}
#' @description
#' Rename the \code{$time} column of an \code{sftime} object.
#' @param x an `sftime` object
#' @param newname character for new time column name
#' @return an `sftime` object
#' @import sftime
#' @author Eva Marques
#' @keywords internal spacetime
#' @export
rename_time <- function(x, newname) {
  stopifnot("x is not a sftime" = class(x)[1] == "sftime")
  oldname <- attributes(x)$time_column
  output <- st_sftime(x, time_column_name = oldname)
  attributes(output)$time_column <- newname
  colnames(output)[which(colnames(output) == oldname)] <- newname
  return(st_sftime(output, time_column_name = newname))
}

################################################################################
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

################################################################################
#' Convert an \code{sf} to an \code{sftime}
#' @description
#' Convert an \code{sf} object to an \code{sftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sf` object
#' @param timename character: name of time column in x
#' @return an `sftime` object
#' @author Eva Marques
#' @keywords spacetime
#' @export
sf_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- st_as_sftime(x, time_column_name = timename)
  output <- rename_time(output, "time")
  return(output)
}

################################################################################
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
#' @keywords spacetime
#' @export
spatvector_as_sftime <- function(x, timename = "time") {
  stopifnot("timename column missing or mispelled" = timename %in% names(x))
  crs <- terra::crs(x)
  output <- as.data.frame(x, geom = "XY") |>
    data.table::as.data.table() |>
    dt_as_mysftime("x", "y", timename, crs = crs)
  return(output)
}

################################################################################
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

################################################################################
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
    "x is not a SpatRasterDataset" = class(x)[1] == "SpatRasterDataset"
  )
  variables <- names(x)
  newsft <- spatraster_as_sftime(
    x[[variables[1]]],
    varname = variables[1],
    timename = timename
  )
  for (var in variables[2:length(variables)]) {
    s <- spatraster_as_sftime(x[[var]], varname = var, timename = timename)
    newsft[, var] <- st_drop_geometry(s[, var])
  }
  return(st_sftime(newsft, time_column_name = timename))
}

################################################################################
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
#' @seealso [check_mysftime], [sf_as_mysftime], [data.frame],
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

################################################################################
#' Convert an \code{sftime} to an \code{sf}
#' @description
#' Convert an \code{sftime} object to an \code{sf} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sftime` object
#' @param keeptime boolean: TRUE if user wants to keep time column
#' as simple column (default = TRUE)
#' @return an `sf` object
#' @author Eva Marques
#' @keywords spacetime
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

################################################################################
#' Convert an \code{sftime} to a \code{mysftime}
#' @description
#' Convert an \code{sftime} object to a \code{mysftime} object. \code{x} must
#' contain a time-defining column, identified in \code{timename}.
#' @param x an `sftime` object
#' @param timename character: name of time column in `x`
#' @return an `sftime` object with specific format
#' @seealso [check_mysftime]
#' @author Eva Marques
#' @keywords spacetime
#' @export
sftime_as_mysftime <- function(x, timename) {
  if (!(timename %in% colnames(x))) {
    stop("time column missing or mispelled")
  }
  output <- rename_time(x, timename)
  return(output)
}

################################################################################
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

################################################################################
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

################################################################################
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
    var_rast <- terra::rast(
      newdf,
      type = "xyz",
      crs = attributes(x$geometry)$crs
    )
    rast_list[[var]] <- var_rast
  }
  output <- terra::sds(rast_list)
  return(output)
}

################################################################################
