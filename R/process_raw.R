#' @description
#' Import and clean population data from NASA Socioeconomic Data and
#' Applications Center (SEDAC).
#' @param path character(1). Path to GeoTIFF or netCDF file.
#' @author Mitchell Manware
#' @return a SpatRaster object;
#' @importFrom package function
#' @importFrom package function
#' @export
process_sedac_population <- function(
    path = "./input/sedac_population/raw/"
) {
  if (substr(path, nchar(path) - 2, nchar(path)) == ".nc") {
    cat(paste0("netCDF functionality for SEDAC data is under construction.\n"))
    return()
  }
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### import data
  data <- terra::rast(path)
  #### identify names
  names_raw <- names(data)
  #### create new names
  for (r in seq_along(names_raw)) {
    split1 <- strsplit(
      names_raw[r],
      "_rev11_",
    )[[1]][[2]]
    split2 <- strsplit(
      split1,
      "_"
    )[[1]]
    names(data[[r]]) <- paste0(
      "gpw_v4_population_",
      split2[1],
      "_",
      split2[2],
      "_",
      split2[3]
    )
    cat(paste0(
      "Cleaning ",
      process_sedac_codes(
        paste0(
          split2[2],
          "_",
          split2[3]
        ),
        invert = TRUE
      ),
      " population data for year ",
      split2[1],
      "...\n"
    ))
  }
  return(data)
}

#' @description
#' Import and clean wildfire smoke plume coverage data from NOAA Hazard
#' Mapping System Fire and Smoke Product.
#' @param date_start character(1). length of 10. Start date of downloaded data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data.
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param variable character(1). "Light", "Medium", or "Heavy".
#' @param path character(1). Directory with downloaded NOAA HMS data files.
#' @author Mitchell Manware.
#' @return a SpatVector object;
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra subset
#' @export
process_hms <- function(
    date_start = "2018-01-01",
    date_end = "2018-01-01",
    variable = c("Light", "Medium", "Heavy"),
    path = "./input/noaa_hms/raw/") {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  paths <- list.files(
    path,
    pattern = "hms_smoke",
    full.names = TRUE
  )
  paths <- paths[grep(
    ".shp",
    paths
  )]
  #### identify dates based on user input
  dates_of_interest <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        dates_of_interest,
        collapse = "|"
      ),
      paths,
      value = TRUE
    )
  )
  #### process data
  data_return <- terra::vect()
  for (d in seq_along(data_paths)) {
    data_date <- terra::vect(data_paths[d])
    data_date_p <- terra::project(
      data_date,
      "EPSG:4326"
    )
    #### subset to density of interest
    data_density <- data_date_p[data_date_p$Density == variable]
    #### absent polygons (ie. December 31, 2018)
    if (nrow(data_density) == 0) {
      cat(paste0(
        variable,
        " smoke plume polygons absent for date ",
        as.Date(
          dates_of_interest[d],
          format = "%Y%m%d"
        ),
        ". Returning empty SpatVector.\n"
      ))
      data_missing <- data_density
      data_missing$Density <- ""
      data_missing$Date <- ""
      data_return <- rbind(data_return, data_missing)
    } else {
      date <- as.Date(
        substr(
          data_density$Start[1],
          1,
          7
        ),
        format = "%Y%j"
      )
      cat(paste0(
        "Cleaning ",
        tolower(variable),
        " data for date ",
        date,
        "...\n"
      ))
      #### zero buffer to avoid self intersection
      data_0_buffer <- terra::buffer(
        data_density,
        width = 0
      )
      #### aggregate polygons
      data_aggregate <- terra::aggregate(
        data_0_buffer,
        by = "Density",
        dissolve = TRUE
      )
      #### factorize
      data_aggregate$Date <- paste0(
        gsub(
          "-",
          "",
          date
        )
      )
      #### select "Density" and "Date"
      data_aggregate <- data_aggregate[
        seq_len(nrow(data_aggregate)), c("Density", "Date")
      ]
      #### merge with other data
      data_return <- rbind(data_return, data_aggregate) 
    }
  }
  #### if no polygons
  if (nrow(data_return) == 0) {
    cat(paste0(
      variable,
      " smoke plume polygons absent from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ". Returning vector of dates.\n"
    ))
    return(c(variable, dates_of_interest))
  } else if (nrow(data_return) > 0) {
    cat(paste0(
      "Returning daily ",
      tolower(variable),
      " data from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ".\n"
    ))
    #### return SpatVector
    return(data_return)
  }
}

#' @description
#' Import and clean Global Multi-resolution Terrain Elevation Data (GMTED2010)
#' downloaded with `download_gmted` or `download_data(dataset_name = "gmted")`.
#' Function returns a SpatRast object containing the user-defined variable
#' of interest at specified resolution. Layer name indicates variable and
#' resolution.
#' @param variable vector(1). Vector containing the GMTED statistic first and
#' the resolution second. (Example: variable = c("Breakline Emphasis",
#' "7.5 arc-seconds")).
#' @param path character(1). Directory with downloaded GEOS-CF
#' the "*_grd" folder containing .adf files.
#' @author Mitchell Manware
#' @return a SpatRaster object
#' @importFrom terra rast
#' @export
process_gmted <- function(
    variable = NULL,
    path = "../../data/covariates/gmted/") {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### check for length of variable
  if (!(length(variable) == 2)) {
    stop(
      paste0(
        "Please provide a vector with the statistic and resolution.\n"
      )
    )
  }
  #### identify statistic and resolution
  statistic <- variable[1]
  statistic_code <- process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  resolution <- variable[2]
  resolution_code <- process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  cat(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  #### identify file path
  paths <- list.files(
    path,
    full.names = TRUE
  )
  #### select only the folder containing data
  data_paths <- unique(
    grep(
      paste0(
        statistic_code,
        resolution_code,
        "_grd",
        collapse = "|"
      ),
      paths,
      value = TRUE
    )
  )
  data_path <- data_paths[endsWith(data_paths, "_grd")]
  #### import data
  data <- terra::rast(data_path)
  #### set coordinate reference system
  return(data)
}

#' @description
#' Import and clean NOAA NCEP North American Regional Reanalysis (NARR) data
#' downloaded with `download_narr` or `download_data(dataset_name = "NARR")`.
#' Function returns a SpatRast object containing the user-defined variable
#' of interest. Layer names indicate the variable, pressure level, and date
#' (YYYYMMDD).
#' @param date_start character(1). length of 10. Format "YYYY-MM-DD".
#' @param date_end character(1). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). NARR variable name(s).
#' @param path character(1). Directory with downloaded GEOS-CF
#' netCDF files.
#' @author Mitchell Manware
#' @return a SpatRaster object
#' @importFrom terra rast
#' @export
process_narr <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    variable = NULL,
    path = "../../data/covariates/narr/") {
  #### directory setup
  path <- download_sanitize_path(path)
  #### check for variable
  check_for_null_parameters(mget(ls()))
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p])
    cat(paste0(
      "Cleaning ",
      variable,
      " data for year ",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)[1]
        ),
        1,
        4
      ),
      "...\n"
    ))
    #### check for mono or pressure levels
    if (grepl("level", names(data_year)[1])) {
      if (length(unique(terra::time(data_year))) == 1) {
        cat(paste0("Detected test data set.\n"))
        days <- sapply(
          strsplit(
            names(data_year),
            "_"
          ),
          function(x) x[3]
        )
        terra::time(data_year) <- as.Date(
          paste0(
            substr(
              terra::time(data_year),
              1,
              4
            ),
            stringi::stri_pad(
              days,
              width = 3,
              pad = "0",
              side = "left"
            )
          ),
          format = "%Y%j"
        )
      }
      #### pressure levels data
      names(data_year) <- paste0(
        variable,
        "_",
        sapply(
          strsplit(
            names(data_year),
            "_"
          ),
          function(x) x[2]
        ),
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    } else {
      #### mono level data
      names(data_year) <- paste0(
        variable,
        "_",
        gsub(
          "-",
          "",
          terra::time(data_year)
        )
      )
    }
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }
  #### subset years to dates of interest
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        names(data_full),
        nchar(names(data_full)) - 7,
        nchar(names(data_full))
      ) %in% date_sequence
    )
  )
  cat(paste0(
    "Returning daily ",
    variable,
    " data from ",
    as.Date(
      date_sequence[1],
      format = "%Y%m%d"
    ),
    " to ",
    as.Date(
      date_sequence[length(date_sequence)],
      format = "%Y%m%d"
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

#' @description
#' Import and clean GEOS-CF data downloaded with
#' `download_geos_cf_data` or `download_data(dataset_name = "geos")`. Function
#' returns a SpatRast object containing the user-defined variables of interest.
#' Layer names indicate the variable, pressure level, date (YYYYMMDD), and, if
#' applicable, the hour (HHMMSS).
#' @param date_start character(1). length of 10. Format "YYYY-MM-DD".
#' @param date_end character(1). length of 10. Format "YYYY-MM-DD".
#' @param variable character(1). GEOS-CF variable name(s).
#' @param directory_with_data character(1). Directory with downloaded GEOS-CF
#' netCDF files.
#' @author Mitchell Manware
#' @return a SpatRaster object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra timeInfo
#' @importFrom terra hasValues
#' @importFrom terra subset
#' @export
process_geos <-
  function(date_start = "2018-01-01",
           date_end = "2018-01-01",
           variable = NULL,
           path = "../../data/covariates/geos_cf/") {
    #### directory setup
    path <- download_sanitize_path(path)
    #### check for variable
    check_for_null_parameters(mget(ls()))
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "GEOS-CF.v01.rpl",
      full.names = TRUE
    )
    paths <- paths[grep(
      ".nc4",
      paths
    )]
    #### identify dates based on user input
    dates_of_interest <- generate_date_sequence(
      date_start,
      date_end,
      sub_hyphen = TRUE
    )
    #### subset file paths to only dates of interest
    data_paths <- unique(
      grep(
        paste(
          dates_of_interest,
          collapse = "|"
        ),
        paths,
        value = TRUE
      )
    )
    #### identify collection
    collection <- process_geos_collection(
      data_paths[1],
      collection = TRUE
    )
    cat(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_return <- terra::rast()
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p])
      data_datetime <- process_geos_collection(data_paths[p], datetime = TRUE)
      cat(paste0(
        "Cleaning ",
        variable,
        " data for ",
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        "...\n"
      ))
      #### subset to user-selected variable
      data_variable <- terra::subset(
        data_raw,
        subset = grep(
          variable,
          names(data_raw)
        )
      )
      #### define variable time
      terra::time(data_variable) <- rep(
        ISOdate(
          year = substr(data_datetime, 1, 4),
          month = substr(data_datetime, 5, 6),
          day = substr(data_datetime, 7, 8),
          hour = substr(
            data_datetime,
            9,
            10
          ),
          min = substr(
            data_datetime,
            11,
            12
          ),
          sec = 00,
          tz = "UTC"
        ),
        terra::nlyr(data_variable)
      )
      #### define variable name based on date and time
      names(data_variable) <- paste0(
        names(data_variable),
        "_",
        gsub(
          ":", "",
          gsub(
            "-", "",
            gsub(" ", "_", terra::time(data_variable))
          )
        )
      )
      if (substr(data_datetime, 9, 12) == "0000") {
        names(data_variable) <- paste0(
          names(data_variable),
          "_000000"
        )
      }
      #### combine data with same date
      data_return <- c(
        data_return,
        data_variable,
        warn = FALSE
      )
    }
    #### set coordinate refernce system
    terra::crs(data_return) <- "EPSG:4326"
    cat(paste0(
      "Returning hourly ",
      variable,
      " data from ",
      as.Date(
        dates_of_interest[1],
        format = "%Y%m%d"
      ),
      " to ",
      as.Date(
        dates_of_interest[length(dates_of_interest)],
        format = "%Y%m%d"
      ),
      ".\n"
    ))
    #### return SpatRaster
    return(data_return)
  }

#' Identify GEOS-CF collection based on user-defined file paths
#' @param path character(1). File path to GEOS-CF data file.
#' @param collection logical(1). Identifies and returns GEOS-CF collection
#' name(s) based on provided file path(s).
#' @param date logical(1). Identifies and returns date sequence (YYYYMMDD) based
#' on provided file path(s).
#' @param datetime logical(1). Identifies and returns date time sequence
#' (YYYYMoMoDDHHMiMi) based on provided file path(s).
#' @return character
#' @export
process_geos_collection <-
  function(
    path,
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
    #### split full file path based on unique GEOS-CF character
    split_geos <- unlist(
      strsplit(
        path,
        "GEOS-CF.v01.rpl."
      )
    )
    #### split file path into collection, datetime, and "nc4"
    split_period <- unlist(
      strsplit(
        split_geos[
          which(
            endsWith(split_geos, ".nc4")
          )
        ],
        "\\."
      )
    )
    #### remove "nc4"
    split_wo_nc4 <- split_period[!split_period == "nc4"]
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

#' Generate statistic and resolution codes based on GMTED statistic and
#' resolution.
#' @param string character(1). File path to GEOS-CF data file.
#' @param statistic logical(1). Matches statistic to statistic code.
#' @param resolution logical(1). Matches resolution to resolution code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides statistic or resolution code, and returns full length statistic
#' or resolution.
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

#' Generate resolution codes based on NASA SEDAC population resolution.
#' @param string character(1). Resolution name or code.
#' @param invert logical(1). Default = FALSE. `invert = TRUE` assumes `string`
#' provides resolution code, and returns full length resolution.
#' @export
process_sedac_codes <-
  function(
    string,
    invert = FALSE
  ) {
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
