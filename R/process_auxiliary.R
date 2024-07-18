# Process auxiliary and internal functions

#' Check input assumptions
#' @param locs Data. [sf][sf::st_as_sf],
#' [SpatVector][terra::vect], or [data.frame]
#' @param check_time logical(1). Whether `"time"` exists in column names.
#' @param locs_epsg character(1). `"{authority}:{code}"` or
#' Well-Known Text format for coordinate reference system definition.
#' @description Check if all of `"lon"`, `"lat"`, and `"time"`
#' (only if `check_time = TRUE`) then convert inputs into a
#' `SpatVector` object.
#' @returns a `SpatVector` object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom terra vect
#' @export
process_conformity <-
  function(
      locs = NULL,
      check_time = FALSE,
      locs_epsg = "EPSG:4326") {
    keyword <- c("lon", "lat", "time")
    if (!check_time) {
      keyword <- keyword[-3]
    }
    if (!all(keyword %in% names(locs))) {
      stop("locs should have 'lon', 'lat', (and 'time') fields.\n")
    }
    if (!methods::is(locs, "SpatVector")) {
      if (methods::is(locs, "sf")) {
        locs <- terra::vect(locs)
      }
      if (is.data.frame(locs)) {
        locs <-
          terra::vect(
            locs,
            geom = c("lon", "lat"),
            keepgeom = TRUE,
            crs = locs_epsg
          )
      }
    }
    return(locs)
  }

#' Process GEOS-CF and MERRA2 collection codes
#' @description
#' Identify the GEOS-CF or MERRA2 collection based on the file path.
#' @param path character(1). File path to data file.
#' @param source character(1). "geos" for GEOS-CF or "merra2" for MERRA2
#' @param collection logical(1). Identifies and returns collection
#' name(s) based on provided file path(s).
#' @param date logical(1). Identifies and returns date sequence (YYYYMMDD) based
#' on provided file path(s).
#' @param datetime logical(1). Identifies and returns date time sequence
#' (YYYYMoMoDDHHMiMi) based on provided file path(s).
#' @keywords auxiliary
#' @return character
#' @export
process_collection <-
  function(
      path,
      source,
      collection = FALSE,
      date = FALSE,
      datetime = FALSE) {
    #### check for more than one true
    parameters <- c(collection, date, datetime)
    if (length(parameters[parameters == TRUE]) > 1) {
      stop(
        paste0(
          "Select one of 'collection', 'date', or 'datetime'.\n"
        )
      )
    }
    #### source names
    geos <- c("geos", "GEOS", "geos-cf", "GEOS-CF")
    merra2 <- c("merra", "merra2", "MERRA", "MERRA2")
    #### string split point
    if (source %in% merra2) {
      code <- "MERRA2_400."
    } else if (source %in% geos) {
      code <- "GEOS-CF.v01.rpl."
    }
    #### split full file path based on unique GEOS-CF character
    split_source <- unlist(
      strsplit(
        path,
        code
      )
    )
    #### split file path into collection, datetime, and "nc4"
    split_period <- unlist(
      strsplit(
        split_source[
          which(
            endsWith(split_source, ".nc4")
          )
        ],
        "\\."
      )
    )
    #### remove "nc4"
    split_wo_nc4 <- split_period[!split_period == "nc4"]
    #### return merra2 collection information
    if (source %in% merra2) {
      #### return only collection name
      if (collection == TRUE) {
        return(split_wo_nc4[1])
      }
      #### return date sequence
      if (date == TRUE) {
        return(split_wo_nc4[2])
      }
    }
    #### create data frame
    split_df <- data.frame(
      split_wo_nc4[
        which(
          !(endsWith(
            split_wo_nc4,
            "z"
          ))
        )
      ],
      split_wo_nc4[
        which(
          endsWith(
            split_wo_nc4,
            "z"
          )
        )
      ]
    )
    #### colnames
    colnames(split_df) <- c("collection", "datetime")
    #### return only collection name
    if (collection == TRUE) {
      return(split_df$collection)
    }
    #### return date sequence
    if (date == TRUE) {
      split_dates <- substr(
        split_df$datetime,
        1,
        8
      )
      return(split_dates)
    }
    #### return datetime sequence
    if (datetime == TRUE) {
      split_datetime <- gsub(
        "_",
        "",
        gsub(
          "z",
          "",
          split_df$datetime
        )
      )
      return(split_datetime)
    }
  }

#' Process MERRA2 time steps
#' @description
#' Identify the time step of data observations based on MERRA2 collection and
#' filter to time values in `from`.
#' @param collection character(1). MERRA2 collection name.
#' @param from SpatRaster(1). Object to extract time values from.
#' @importFrom stringi stri_pad
#' @keywords auxiliary
#' @return character
#' @export
process_merra2_time <-
  function(collection, from) {
    split <- unlist(strsplit(collection, "_"))
    code <- split[1]
    if (code == "inst1") {
      step <- seq(from = 0, to = 2300, by = 100)
    } else if (code == "inst3") {
      step <- seq(from = 0, to = 2100, by = 300)
    } else if (code == "inst6") {
      step <- seq(from = 0, to = 1800, by = 600)
    } else if (code == "statD") {
      step <- 1200
    } else if (code == "tavg1") {
      step <- seq(from = 0030, to = 2330, by = 100)
    } else if (code == "tavg3") {
      step <- seq(from = 0130, to = 2330, by = 300)
    }
    pad_l <- stringi::stri_pad(step, side = "left", width = 4, pad = 0)
    pad_r <- stringi::stri_pad(pad_l, side = "right", width = 6, pad = 0)
    time_f <- gsub(
      " ",
      "",
      gsub(
        "-",
        "",
        gsub(
          ":",
          "",
          terra::time(from)
        )
      )
    )
    for (f in seq_along(time_f)) {
      if (nchar(time_f[f]) == 8) {
        time_f[f] <- paste0(time_f[f], "000000")
      }
    }
    time_return <- pad_r[
      pad_r %in% unique(substr(time_f, 9, 14))
    ]
    return(time_return)
  }

#' Process elevation statistic and resolution codes
#' @description
#' Identify the GMTED statistic and resolution based on the file path. Convert
#' statistic and resolution to/from full string to/from statistic and
#' resolution code.
#' @param string character(1). File path to GMTED data file.
#' @param statistic logical(1). Matches statistic to statistic code.
#' @param resolution logical(1). Matches resolution to resolution code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides statistic or resolution code, and returns full length statistic
#' or resolution.
#' @keywords auxiliary
#' @return character
#' @export
process_gmted_codes <-
  function(
      string,
      statistic = FALSE,
      resolution = FALSE,
      invert = FALSE) {
    statistics <- c(
      "Breakline Emphasis", "Systematic Subsample",
      "Median Statistic", "Minimum Statistic",
      "Mean Statistic", "Maximum Statistic",
      "Standard Deviation Statistic"
    )
    statistic_codes <- c("be", "ds", "md", "mi", "mn", "mx", "sd")
    statistic_codes <- cbind(statistics, statistic_codes)
    resolutions <- c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds")
    resolution_codes <- c("75", "15", "30")
    resolution_codes <- cbind(resolutions, resolution_codes)
    if (statistic == TRUE && invert == FALSE) {
      code <- statistic_codes[statistic_codes[, 1] == string][2]
    } else if (statistic == TRUE && invert == TRUE) {
      code <- statistic_codes[statistic_codes[, 2] == string][1]
    }
    if (resolution == TRUE && invert == FALSE) {
      code <- resolution_codes[resolution_codes[, 1] == string][2]
    } else if (resolution == TRUE && invert == TRUE) {
      code <- resolution_codes[resolution_codes[, 2] == string][1]
    }
    return(code)
  }

#' Process population resolution code
#' @description
#' Convert full length resolution name to/from resolution code.
#' @param string character(1). Resolution name or code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides resolution code, and returns full length resolution.
#' @keywords auxiliary
#' @export
process_sedac_codes <-
  function(
      string,
      invert = FALSE) {
    resolution_namecodes <- cbind(
      c(
        "60 minute", "30 second", "2.5 minute",
        "15 minute", "30 minute"
      ),
      c(
        "1_deg", "30_sec", "2pt5_min",
        "15_min", "30_min"
      )
    )
    if (invert == FALSE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 1] == string][2]
    } else if (invert == TRUE) {
      resolution <-
        resolution_namecodes[resolution_namecodes[, 2] == string][1]
    }
    return(resolution)
  }

#' Process locations buffer
#' @description
#' Create circular buffer around locations based on user defined radius.
#' @param locs SpatVector(1). SpatVector object with point geometry
#' @param radius integer(1). Circular buffer size (meters).
#' @description Creates a circular buffer around points if `radius` is > 0.
#' Returns points if `radius` is 0.
#' @keywords internal
#' @returns a `SpatVector` object
#' @importFrom terra buffer
#' @export
process_locs_radius <-
  function(
      locs,
      radius) {
    if (radius == 0) {
      return(locs)
    } else if (radius > 0) {
      sites_buffer <- terra::buffer(
        locs,
        radius,
        quadsegs = 180L
      )
      return(sites_buffer)
    }
  }

#' Process locations as `SpatVector`
#' @description
#' Detect `SpatVector` object, or convert locations from class \code{sf},
#' \code{data.frame} or \code{data.table} to
#' `SpatVector` object, project to coordinate reference system, and apply
#' circular buffer.
#' @param locs data.frame(1). Data frame containing columns for unique
#' identifier, latitude, and longitude. Latitude and longitude columns **must**
#' be named "lat" and "lon", respectively.
#' @param crs Coordinate reference system (CRS) description utilizing
#' `terra::crs()`.
#' @param radius integer(1). Circular buffer size (meters).
#' @keywords internal
#' @returns a `SpatVector` object
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
    #### detect SpatVector
    if (methods::is(locs, "SpatVector")) {
      message(
        paste0(
          "Detected `SpatVector` (",
          terra::geomtype(locs),
          ") extraction locations...\n"
        )
      )
      sites_v <- locs
      #### detect sf object
    } else if (methods::is(locs, "sf")) {
      cat("Detected `sf` extraction locations...\n")
      sites_v <- terra::vect(locs)
      ### detect data.frame object
    } else if (methods::is(locs, "data.frame")) {
      cat("Detected `data.frame` extraction locations...\n")
      #### columns
      if (any(!(c("lon", "lat") %in% colnames(locs)))) {
        stop(paste0(
          "`locs` is missing 'lon', 'lat', or both.\n"
        ))
      }
      sites_v <- terra::vect(
        data.frame(locs),
        geom = c("lon", "lat"),
        crs = "EPSG:4326",
        keepgeom = TRUE
      )
    } else {
      stop(
        paste0(
          "`locs` is not a `SpatVector`, `sf`, or `data.frame` object.\n"
        )
      )
    }
    ##### project to desired coordinate reference system
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

#' Process gridMET variable codes
#' @description
#' Convert gridMET variable names to/from variable codes.
#' @param string character(1). gridMET variable name or variable code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides variable code and returns full length variable name.
#' @keywords auxiliary
#' @return character
#' @export
process_gridmet_codes <-
  function(
      string,
      invert = FALSE) {
    names <- c(
      "Near-Surface Specific Humidity", "Mean Vapor Pressure Deficit",
      "Precipitation", "Minimum Near-Surface Relative Humidity",
      "Maximum Near-Surface Relative Humidity",
      "Surface Downwelling Solar Radiation",
      "Minimum Near-Surface Air Temperature",
      "Maximum Near-Surface Air Temperature",
      "Wind speed at 10 m", "Wind direction at 10 m",
      "Palmer Drought Severity Index", "Reference grass evaportranspiration",
      "Reference alfalfa evaportranspiration", "Energy Release Component",
      "Burning Index", "100-hour dead fuel moisture",
      "1000-hour dead fuel moisture"
    )
    codes <- c(
      "sph", "vpd", "pr", "rmin", "rmax", "srad", "tmmn", "tmmx", "vs",
      "th", "pdsi", "pet", "etr", "ERC", "BI", "FM100", "FM1000"
    )
    names_codes <- cbind(tolower(names), codes)
    if (string == "all") {
      return(names_codes)
    }
    if (invert == FALSE) {
      name_code <- names_codes[names_codes[, 1] == tolower(string), ]
      return(name_code[2])
    } else if (invert == TRUE) {
      name_code <- names_codes[names_codes[, 2] == tolower(string), ]
      return(name_code[1])
    }
  }

#' Process terraClimate variable codes
#' @description
#' Convert terraClimate variable names to/from variable codes.
#' @param string character(1). terraClimate variable name or variable code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides variable code and returns full length variable name.
#' @keywords auxiliary
#' @return character
#' @export
process_terraclimate_codes <-
  function(
      string,
      invert = FALSE) {
    names <- c(
      "Actual Evapotranspiration", "Climate Water Deficit",
      "Potential evapotranspiration", "Precipitation", "Runoff",
      "Soil Moisture", "Downward surface shortwave radiation",
      "Snow water equivalent - at end of month", "Max Temperature",
      "Min Temperature", "Vapor pressure", "Wind speed",
      "Vapor Pressure Deficit", "Palmer Drought Severity Index"
    )
    codes <- c(
      "aet", "def", "pet", "ppt", "q", "soil", "srad", "swe", "tmax", "tmin",
      "vap", "ws", "vpd", "PDSI"
    )
    names_codes <- cbind(tolower(names), codes)
    if (string == "all") {
      return(names_codes)
    }
    if (invert == FALSE) {
      name_code <- names_codes[names_codes[, 1] == tolower(string), ]
      return(name_code[2])
    } else if (invert == TRUE) {
      name_code <- names_codes[names_codes[, 2] == tolower(string), ]
      return(name_code[1])
    }
  }

#' Filter gridMET and terraClimate variable names and variable codes
#' @description
#' Check user defined variables for gridMET and TerraClimate functions.
#' @param variables character(1). Data variables. (Passed from download_* or
#' process_*).
#' @param source character(1). Data source for selected variables ("gridMET" or
#' "TerraClimate").
#' @keywords auxiliary
#' @return character
#' @export
process_variable_codes <-
  function(
      variables,
      source = c("gridmet", "terraclimate")) {
    if (tolower(source) == "gridmet") {
      code_function <- process_gridmet_codes
    } else if (tolower(source) == "terraclimate") {
      code_function <- process_terraclimate_codes
    }
    names_codes <- do.call(code_function, list(string = "all"))
    if (all(variables %in% names_codes[, 2]) == TRUE) {
      return(variables)
    } else {
      if (all(tolower(variables) %in% names_codes[, 1]) == TRUE) {
        codes_return <- lapply(
          tolower(variables),
          function(var) {
            do.call(code_function, list(var, invert = FALSE))
          }
        )
        return(as.vector(unlist(codes_return)))
      } else {
        stop(
          paste0(
            "Unable to identify requested variables.\n"
          )
        )
      }
    }
  }


#' Check date format
#' @description
#' Check date input strings conform to the required format.
#' @param instr character(1). String to check.
#' @param format character(1). Matching format to be checked.
#' Default is `"%Y-%m-%d"`, which can detect `"%Y/%m/%d`.
#' See [`strftime`] for details of formatting this string.
#' @returns No returning value. It stops the function if `instr` doesn't
#' conform to the `format`.
#' @author Insang Song
#' @keywords internal
#' @export
is_date_proper <- function(
  instr = NULL,
  format = "%Y-%m-%d"
) {
  # the results are alphabetically ordered
  argnames <- mget(ls())
  datestr <- try(strftime(instr, format = format))
  if (inherits(datestr, "try-error")) {
    stop(sprintf("%s does not conform to the required format
         \"YYYY-MM-DD\".\n", names(argnames)[2]))
  }
}


#' Apply extent to the processed data
#'
#' User-defined extent is used to filter the data.
#' 
#' @param data sf/terra object.
#' @param extent numeric(4). Extent to filter the data.
#'   Should be ordered as c(xmin, xmax, ymin, ymax).
#' @param geom character(1 or 2). Geometry type for if `data` is `data.frame`.
#' One of "geometry" or c("lon", "lat").
#' @importFrom sf st_as_sfc st_bbox st_crs
#' @importFrom terra ext
#' @returns sf/terra object with the extent applied.
#' @keywords internal
apply_extent <-
  function(data, extent, geom) {
    extent <- terra::ext(extent)
    if (inherits(data, "sf")) {
      extent <- sf::st_as_sfc(sf::st_bbox(extent))
      sf::st_crs(extent) <- sf::st_crs(data)
    } else if (inherits(data, "data.frame")) {
      data <- terra::vect(data, geom = geom, crs = "EPSG:4326")
    }
    data <- data[extent, ]
    return(data)
  }
