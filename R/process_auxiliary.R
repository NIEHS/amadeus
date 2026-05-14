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
#' @return a `SpatVector` object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom terra vect
#' @keywords internal auxiliary
#' @export
process_conformity <-
  function(
    locs = NULL,
    check_time = FALSE,
    locs_epsg = "EPSG:4326"
  ) {
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_collection <-
  function(
    path,
    source,
    collection = FALSE,
    date = FALSE,
    datetime = FALSE
  ) {
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
    #### handle GlobalFWI daily corrected MERRA2 files
    if (source %in% merra2 && grepl("^FWI\\.", basename(path))) {
      split_period <- unlist(
        strsplit(
          basename(path),
          "\\."
        )
      )
      if (collection == TRUE) {
        return("fwi")
      }
      if (date == TRUE) {
        return(split_period[length(split_period) - 1])
      }
      if (datetime == TRUE) {
        return(split_period[length(split_period) - 1])
      }
    }
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
#' @keywords internal auxiliary
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
    } else if (collection == "fwi") {
      step <- 0000
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_gmted_codes <-
  function(
    string,
    statistic = FALSE,
    resolution = FALSE,
    invert = FALSE
  ) {
    statistics <- c(
      "Breakline Emphasis",
      "Systematic Subsample",
      "Median Statistic",
      "Minimum Statistic",
      "Mean Statistic",
      "Maximum Statistic",
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_sedac_codes <-
  function(
    string,
    invert = FALSE
  ) {
    resolution_namecodes <- cbind(
      c(
        "60 minute",
        "30 second",
        "2.5 minute",
        "15 minute",
        "30 minute"
      ),
      c(
        "1_deg",
        "30_sec",
        "2pt5_min",
        "15_min",
        "30_min"
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
#' @keywords internal auxiliary
#' @return a `SpatVector` object
#' @importFrom terra buffer
#' @export
process_locs_radius <-
  function(
    locs,
    radius
  ) {
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
#' @keywords internal auxiliary
#' @return a `SpatVector` object
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
      message("Detected `sf` extraction locations...\n")
      sites_v <- terra::vect(locs)
      ### detect data.frame object
    } else if (methods::is(locs, "data.frame")) {
      message("Detected `data.frame` extraction locations...\n")
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_gridmet_codes <-
  function(
    string,
    invert = FALSE
  ) {
    names <- c(
      "Near-Surface Specific Humidity",
      "Mean Vapor Pressure Deficit",
      "Precipitation",
      "Minimum Near-Surface Relative Humidity",
      "Maximum Near-Surface Relative Humidity",
      "Surface Downwelling Solar Radiation",
      "Minimum Near-Surface Air Temperature",
      "Maximum Near-Surface Air Temperature",
      "Wind speed at 10 m",
      "Wind direction at 10 m",
      "Palmer Drought Severity Index",
      "Reference grass evaportranspiration",
      "Reference alfalfa evaportranspiration",
      "Energy Release Component",
      "Burning Index",
      "100-hour dead fuel moisture",
      "1000-hour dead fuel moisture"
    )
    codes <- c(
      "sph",
      "vpd",
      "pr",
      "rmin",
      "rmax",
      "srad",
      "tmmn",
      "tmmx",
      "vs",
      "th",
      "pdsi",
      "pet",
      "etr",
      "ERC",
      "BI",
      "FM100",
      "FM1000"
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_terraclimate_codes <-
  function(
    string,
    invert = FALSE
  ) {
    names <- c(
      "Actual Evapotranspiration",
      "Climate Water Deficit",
      "Potential evapotranspiration",
      "Precipitation",
      "Runoff",
      "Soil Moisture",
      "Downward surface shortwave radiation",
      "Snow water equivalent - at end of month",
      "Max Temperature",
      "Min Temperature",
      "Vapor pressure",
      "Wind speed",
      "Vapor Pressure Deficit",
      "Palmer Drought Severity Index"
    )
    codes <- c(
      "aet",
      "def",
      "pet",
      "ppt",
      "q",
      "soil",
      "srad",
      "swe",
      "tmax",
      "tmin",
      "vap",
      "ws",
      "vpd",
      "PDSI"
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
#' @keywords internal auxiliary
#' @return character
#' @export
process_variable_codes <-
  function(
    variables,
    source = c("gridmet", "terraclimate")
  ) {
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

# Internal TRI helper: read and normalize TRI csv column names
tri_read_raw <- function(path = NULL) {
  csvs_tri_from <-
    list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  if (length(csvs_tri_from) < 1) {
    stop("No TRI CSV files found in `path`.\n")
  }
  csvs_tri <- lapply(csvs_tri_from, read.csv)
  dt_tri <- data.table::rbindlist(csvs_tri)
  tri_cns <- colnames(dt_tri)
  tri_cns <- sub(".*?\\.\\.", "", tri_cns)
  tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
  tri_cns <- gsub("\\.", "_", tri_cns)
  dt_tri <- stats::setNames(dt_tri, tri_cns)
  dt_tri <- as.data.frame(dt_tri, stringsAsFactors = FALSE)
  return(dt_tri)
}

#' Get TRI lookup information for chemicals or industries
#' @description
#' Returns a lookup table from local TRI files. By default it returns chemical
#' information (`TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, `CASN`). Set
#' `type = "industries"` to return industry sector information
#' (`INDUSTRY_SECTOR_CODE`, `INDUSTRY_SECTOR`).
#' @param path character(1). Path to the directory with TRI CSV files
#'   (from `download_tri`).
#' @param type character(1). Lookup table to return. One of `"chemicals"`
#'   (default) or `"industries"`.
#' @param year `NULL` or integer(1). Optional single year filter. If `NULL`
#'   (default), all years in `path` are included.
#' @param include_na logical(1). If `FALSE` (default), rows where lookup fields
#'   are all missing are removed.
#' @param ... Placeholders.
#' @return a `data.frame` containing the requested TRI lookup table.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_tri_info(path = "./data")
#' get_tri_info(path = "./data", type = "industries")
#' get_tri_info(path = "./data", year = 2020)
#' }
#' @export
get_tri_info <- function(
  path = NULL,
  type = c("chemicals", "industries"),
  year = NULL,
  include_na = FALSE,
  ...
) {
  type <- match.arg(type)
  dt_tri <- tri_read_raw(path = path)
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      stop("`year` must be NULL or a single numeric value.\n")
    }
    if (!("YEAR" %in% names(dt_tri))) {
      stop("TRI input is missing `YEAR` column needed for filtering.\n")
    }
    dt_tri <- dt_tri[dt_tri$YEAR == year, , drop = FALSE]
  }

  if (type == "chemicals") {
    required_cols <- c("TRI_CHEMICAL_COMPOUND_ID", "CHEMICAL")
    missing_cols <- setdiff(required_cols, names(dt_tri))
    if (length(missing_cols) > 0) {
      stop(
        "TRI input is missing required chemical lookup columns: ",
        paste(missing_cols, collapse = ", "),
        "\n"
      )
    }
    cas_col <- if ("CAS" %in% names(dt_tri)) {
      "CAS"
    } else if ("CAS_" %in% names(dt_tri)) {
      "CAS_"
    } else {
      NULL
    }
    cas_vals <- if (is.null(cas_col)) {
      rep(NA_character_, nrow(dt_tri))
    } else {
      as.character(dt_tri[[cas_col]])
    }
    out <- data.frame(
      TRI_CHEMICAL_COMPOUND_ID = as.character(dt_tri$TRI_CHEMICAL_COMPOUND_ID),
      CHEMICAL = as.character(dt_tri$CHEMICAL),
      CASN = cas_vals,
      stringsAsFactors = FALSE
    )
    if (!include_na) {
      out <- out[
        !(is.na(out$TRI_CHEMICAL_COMPOUND_ID) &
            is.na(out$CHEMICAL) &
            is.na(out$CASN)),
        ,
        drop = FALSE
      ]
    }
    out <- unique(out)
    out <- out[
      order(out$CHEMICAL, out$TRI_CHEMICAL_COMPOUND_ID, out$CASN),
      ,
      drop = FALSE
    ]
  } else {
    required_cols <- c("INDUSTRY_SECTOR_CODE", "INDUSTRY_SECTOR")
    missing_cols <- setdiff(required_cols, names(dt_tri))
    if (length(missing_cols) > 0) {
      stop(
        "TRI input is missing required industry lookup columns: ",
        paste(missing_cols, collapse = ", "),
        "\n"
      )
    }
    out <- data.frame(
      INDUSTRY_SECTOR_CODE = as.character(dt_tri$INDUSTRY_SECTOR_CODE),
      INDUSTRY_SECTOR = as.character(dt_tri$INDUSTRY_SECTOR),
      stringsAsFactors = FALSE
    )
    if (!include_na) {
      out <- out[
        !(is.na(out$INDUSTRY_SECTOR_CODE) &
            is.na(out$INDUSTRY_SECTOR)),
        ,
        drop = FALSE
      ]
    }
    out <- unique(out)
    out <- out[
      order(out$INDUSTRY_SECTOR_CODE, out$INDUSTRY_SECTOR),
      ,
      drop = FALSE
    ]
  }

  rownames(out) <- NULL
  return(out)
}

# Internal helper: resolve and filter metadata-inspection file paths
info_resolve_paths <- function(
  path = NULL,
  pattern = NULL,
  source_name = "files"
) {
  if (is.null(path) || !is.character(path) || length(path) < 1 || anyNA(path)) {
    stop("`path` must be a non-empty character vector.\n")
  }
  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop("`pattern` must be a single non-empty character string.\n")
  }

  path_entries <- unique(path)
  expanded_paths <- unlist(
    lapply(path_entries, function(p) {
      if (dir.exists(p)) {
        list.files(
          path = p,
          recursive = TRUE,
          full.names = TRUE
        )
      } else {
        p
      }
    }),
    use.names = FALSE
  )
  expanded_paths <- unique(expanded_paths[file.exists(expanded_paths)])
  matched <- grep(pattern, expanded_paths, ignore.case = TRUE, value = TRUE)
  matched <- unique(matched)
  if (length(matched) == 0L) {
    stop(sprintf("No %s files were found in `path`.\n", source_name))
  }
  matched
}

# Internal helper: normalize variable selectors from raster layer names
info_normalize_layer_variables <- function(layer_names) {
  vars <- as.character(layer_names)
  vars <- trimws(vars)
  vars <- vars[nzchar(vars)]
  vars <- sub("_[0-9]+$", "", vars)
  vars <- sub("_lev=.*$", "", vars)
  sort(unique(vars))
}

#' Get GEOS variable lookup information
#' @description
#' Returns a lookup table of available GEOS collection and variable selectors
#' from locally downloaded GEOS-CF netCDF files. This helper inspects layer
#' metadata only and does not read raster values into memory.
#' @param path character(1+) Path(s) to GEOS file(s) and/or directory(ies)
#'   containing GEOS-CF `.nc4` files.
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each collection-variable row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with GEOS collection and variable selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_geos_info(path = "./data/geos")
#' get_geos_info(path = "./data/geos", include_file = TRUE)
#' }
#' @importFrom terra rast
#' @export
get_geos_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "GEOS-CF\\.v01\\.rpl.*\\.nc4$",
    source_name = "GEOS-CF .nc4"
  )

  out_rows <- lapply(files, function(f) {
    data_raw <- terra::rast(f)
    vars <- info_normalize_layer_variables(names(data_raw))
    collection <- unique(amadeus::process_collection(
      f,
      source = "geos",
      collection = TRUE
    ))
    if (length(vars) == 0L || length(collection) == 0L) {
      return(NULL)
    }
    row <- data.frame(
      collection = rep(collection[1], length(vars)),
      variable = vars,
      file = rep(f, length(vars)),
      stringsAsFactors = FALSE
    )
    row
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop("No GEOS collection-variable metadata could be derived from `path`.\n")
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("collection", "variable"), drop = FALSE])
    out <- out[order(out$collection, out$variable), , drop = FALSE]
  } else {
    out <- unique(out[, c("collection", "variable", "file"), drop = FALSE])
    out <- out[order(out$collection, out$variable, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Get MERRA2 variable lookup information
#' @description
#' Returns a lookup table of available MERRA2 collection and variable selectors
#' from locally downloaded MERRA2 netCDF files. This helper inspects layer
#' metadata only and does not read raster values into memory.
#' @param path character(1+) Path(s) to MERRA2 file(s) and/or directory(ies)
#'   containing MERRA2 `.nc4` files (and optional FWI `.nc` files).
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each collection-variable row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with MERRA2 collection and variable selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_merra2_info(path = "./data/merra2")
#' get_merra2_info(path = "./data/merra2", include_file = TRUE)
#' }
#' @importFrom terra rast
#' @export
get_merra2_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "(MERRA2_400\\..*\\.nc4$|FWI\\..*\\.nc$)",
    source_name = "MERRA2 netCDF"
  )

  out_rows <- lapply(files, function(f) {
    data_raw <- terra::rast(f)
    collection <- unique(amadeus::process_collection(
      f,
      source = "merra2",
      collection = TRUE
    ))
    vars <- info_normalize_layer_variables(names(data_raw))
    if (length(collection) == 1L && collection == "fwi") {
      vars <- sub("^MERRA2\\.CORRECTED_", "", vars)
    }
    vars <- sort(unique(vars[nzchar(vars)]))
    if (length(vars) == 0L || length(collection) == 0L) {
      return(NULL)
    }
    data.frame(
      collection = rep(collection[1], length(vars)),
      variable = vars,
      file = rep(f, length(vars)),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop(
      "No MERRA2 collection-variable metadata could be derived from `path`.\n"
    )
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("collection", "variable"), drop = FALSE])
    out <- out[order(out$collection, out$variable), , drop = FALSE]
  } else {
    out <- unique(out[, c("collection", "variable", "file"), drop = FALSE])
    out <- out[order(out$collection, out$variable, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# Internal helper: derive MODIS subdataset labels without loading raster values
info_modis_subdatasets <- function(path = NULL) {
  sds_desc <- try(terra::describe(path, sds = TRUE), silent = TRUE)
  if (!inherits(sds_desc, "try-error") && nrow(sds_desc) > 0) {
    candidate_col <- if ("var" %in% names(sds_desc)) "var" else "name"
    if (!is.null(candidate_col) && candidate_col %in% names(sds_desc)) {
      sds <- trimws(as.character(sds_desc[[candidate_col]]))
      sds <- sds[!is.na(sds) & nzchar(sds)]
      if (length(sds) > 0L) {
        return(sort(unique(sds)))
      }
    }
  }
  sds_read <- try(terra::rast(path, raw = TRUE), silent = TRUE)
  if (inherits(sds_read, "try-error")) {
    return(character(0))
  }
  sds <- trimws(as.character(names(sds_read)))
  sds <- sds[!is.na(sds) & nzchar(sds)]
  sort(unique(sds))
}

#' Get MODIS product subdataset lookup information
#' @description
#' Returns a lookup table of available MODIS product and subdataset selectors
#' from locally downloaded MODIS/VIIRS-style HDF/H5 files. This helper uses
#' metadata inspection (`terra::describe(..., sds = TRUE)` and layer names) and
#' does not read raster values into memory.
#' @param path character(1+) Path(s) to MODIS file(s) and/or directory(ies)
#'   containing `.hdf`/`.h5` files.
#' @param include_file logical(1). If `TRUE`, include a `file` column showing
#'   the source file for each product-subdataset row. Default `FALSE`.
#' @param ... Placeholders.
#' @return a `data.frame` with MODIS product and subdataset selectors.
#' @author Kyle Messier
#' @examples
#' \dontrun{
#' get_modis_info(path = "./data/modis")
#' get_modis_info(path = "./data/modis", include_file = TRUE)
#' }
#' @importFrom terra describe
#' @importFrom terra rast
#' @export
get_modis_info <- function(
  path = NULL,
  include_file = FALSE,
  ...
) {
  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop("`include_file` must be a single logical value (TRUE/FALSE).\n")
  }
  files <- info_resolve_paths(
    path = path,
    pattern = "\\.(hdf|h5)$",
    source_name = "MODIS HDF/H5"
  )
  out_rows <- lapply(files, function(f) {
    sds <- info_modis_subdatasets(path = f)
    product <- sub("\\..*$", "", basename(f))
    if (length(sds) == 0L || !nzchar(product)) {
      return(NULL)
    }
    data.frame(
      product = rep(product, length(sds)),
      subdataset = sds,
      file = rep(f, length(sds)),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(out_rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    stop("No MODIS product-subdataset metadata could be derived from `path`.\n")
  }
  if (!isTRUE(include_file)) {
    out <- unique(out[, c("product", "subdataset"), drop = FALSE])
    out <- out[order(out$product, out$subdataset), , drop = FALSE]
  } else {
    out <- unique(out[, c("product", "subdataset", "file"), drop = FALSE])
    out <- out[order(out$product, out$subdataset, out$file), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}


#' Check date format
#' @description
#' Check date input strings conform to the required format.
#' @param instr character(1). String to check.
#' @param format character(1). Matching format to be checked.
#' Default is `"%Y-%m-%d"`, which can detect `"%Y/%m/%d`.
#' See [`strftime`] for details of formatting this string.
#' @return No returning value. It stops the function if `instr` doesn't
#' conform to the `format`.
#' @author Insang Song
#' @keywords internal auxiliary
#' @export
is_date_proper <- function(
  instr = NULL,
  format = "%Y-%m-%d"
) {
  # the results are alphabetically ordered
  argnames <- mget(ls())
  datestr <- try(strftime(instr, format = format))
  if (inherits(datestr, "try-error")) {
    stop(sprintf(
      "%s does not conform to the required format
         \"YYYY-MM-DD\".\n",
      names(argnames)[2]
    ))
  }
}

#' Parse netCDF day codes from layer names
#' @description Parse day-code suffixes from netCDF layer names such as
#' \code{"precipitation_amount_day=43101"} and convert to \code{Date}.
#' @param layer_names character. Layer names.
#' @param source character(1). Source label used in error messages.
#' @param origin character(1). Date origin for numeric day codes.
#' @return Date vector.
#' @keywords internal auxiliary
#' @export
process_parse_ncdf_day_codes <- function(
  layer_names,
  source = "gridmet",
  origin = "1900-01-01"
) {
  stopifnot(is.character(layer_names))
  day_codes <- sub(".*=([0-9]+)$", "\\1", layer_names)
  valid_code <- grepl("^[0-9]+$", day_codes)
  if (!all(valid_code)) {
    bad_layers <- paste(layer_names[!valid_code], collapse = ", ")
    stop(
      sprintf(
        "Unable to parse %s layer time from: %s.\n",
        source,
        bad_layers
      )
    )
  }
  as.Date(as.numeric(day_codes), origin = origin)
}


#' Apply extent to the processed data
#' @description
#' User-defined extent is used to filter the data.
#' @param data sf/terra object.
#' @param extent numeric(4). Extent to filter the data.
#'   Should be ordered as c(xmin, xmax, ymin, ymax).
#' @param geom character(1 or 2). Geometry type for if `data` is `data.frame`.
#' One of "geometry" or c("lon", "lat").
#' @importFrom sf st_as_sfc st_bbox st_crs
#' @importFrom terra ext
#' @return sf/terra object with the extent applied.
#' @keywords internal auxiliary
#' @export
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
