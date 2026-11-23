################################################################################
# Support and internal {process} functions.
################################################################################

################################################################################
# Check and normalize inputs.
################################################################################

################################################################################
#' Check whether date input can be formatted
#' @description
#' Attempt to format date input with [base::strftime()] and stop if that
#' operation raises an error.
#' @param instr character, Date, or POSIXt. Date values passed to
#'   [base::strftime()]. The default is `NULL`.
#' @param format character(1). Output format passed to [base::strftime()].
#'   Defaults to `"%Y-%m-%d"`.
#' @details
#' This helper checks whether formatting raises an error. It does not enforce
#' an exact input spelling, compare the formatted output with the input, or
#' reject missing results produced without an error.
#' @return `NULL`, invisibly, when formatting succeeds; otherwise an error.
#' @author Insang Song
#' @seealso [sequence_dates()], [base::strftime()]
#' @keywords internal auxiliary
#' @examples
#' is_date_proper("2021-05-04")
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

################################################################################
#' Check location columns and convert to a SpatVector
#' @description
#' Require longitude, latitude, and optionally time columns before converting
#' supported location inputs to a `SpatVector`.
#' @param locs sf, SpatVector, or data.frame. Locations containing `lon` and
#'   `lat` columns, plus `time` when `check_time = TRUE`. These columns are
#'   required even when the input already has a spatial geometry.
#' @param check_time logical(1). Require a column named `time`.
#'   Defaults to `FALSE`; time values are not parsed or validated.
#' @param locs_epsg character(1). Coordinate reference system assigned when
#'   converting a data frame. Defaults to `"EPSG:4326"`. Existing spatial
#'   inputs retain their coordinate reference system.
#' @return A `SpatVector` for supported inputs. Missing required columns
#'   raise an error.
#' @author Insang Song
#' @seealso [process_locs_vector()], [terra::vect()]
#' @importFrom methods is
#' @importFrom terra vect
#' @keywords internal auxiliary
#' @examples
#' process_conformity(data.frame(lon = -78.8, lat = 35.9))
#' @export
process_conformity <- function(
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

################################################################################
#' Resolve gridMET and TerraClimate variable codes
#' @description
#' Match variable codes or descriptive names against the source-specific
#' lookup table for download and processing functions.
#' @param variables character. A vector of variable codes or a vector of
#'   descriptive names. Matching is case-insensitive; names and codes should
#'   not be mixed in the same call.
#' @param source character(1). Supply either `"gridmet"` or `"terraclimate"`.
#' @details
#' For gridMET, the code aliases `"spei"` and `"eddi"` select `"spei30d"` and
#' `"eddi30d"`, respectively. Unknown variables raise an error. Input order
#' and repeated selections are preserved.
#' @return A character vector of variable codes. Code inputs use the lookup
#'   table's capitalization; codes resolved from descriptive names are
#'   converted to lowercase.
#' @author amadeus contributors
#' @seealso [process_gridmet_codes()], [process_terraclimate_codes()]
#' @keywords internal auxiliary
#' @examples
#' process_variable_codes(c("pr", "tmmx"), source = "gridmet")
#' process_variable_codes("Precipitation", source = "terraclimate")
#' @export
process_variable_codes <- function(
  variables,
  source = c("gridmet", "terraclimate")
) {
  if (tolower(source) == "gridmet") {
    code_function <- process_gridmet_codes
  } else if (tolower(source) == "terraclimate") {
    code_function <- process_terraclimate_codes
  }
  names_codes <- do.call(code_function, list(string = "all"))
  variables <- as.character(variables)
  if (tolower(source) == "gridmet") {
    variables[tolower(variables) == "spei"] <- "spei30d"
    variables[tolower(variables) == "eddi"] <- "eddi30d"
  }
  code_matches <- match(
    tolower(variables),
    tolower(names_codes[, 2])
  )
  if (all(!is.na(code_matches))) {
    return(unname(names_codes[code_matches, 2]))
  } else {
    if (all(tolower(variables) %in% names_codes[, 1]) == TRUE) {
      codes_return <- lapply(
        tolower(variables),
        function(var) {
          do.call(code_function, list(var, invert = FALSE))
        }
      )
      return(tolower(as.vector(unlist(codes_return))))
    } else {
      stop(
        paste0(
          "Unable to identify requested variables.\n"
        )
      )
    }
  }
}

################################################################################
# Resolve local file paths.
################################################################################

################################################################################
#' Resolve local paths by filename pattern and date range
#' @description
#' Search immediate entries in input directories for names matching a regular
#' expression, then retain paths containing a date in the requested range.
#' @param date character(1) or character(2). A single date or inclusive range
#'   endpoints in `"YYYY-MM-DD"` format. Endpoints may be supplied in either
#'   order. Dates must be between `"1950-01-01"` and [base::Sys.Date()],
#'   inclusive.
#' @param path character. Directory path or paths passed to
#'   [base::list.files()].
#'   Supply directories, not individual file paths.
#' @param pattern character(1). Regular expression used by [base::list.files()]
#'   to select names. Matching is case-sensitive.
#' @param hyphen logical(1). Whether the date strings matched against paths
#'   contain hyphens. Defaults to `TRUE`, matching `"2024-01-01"`; use `FALSE`
#'   to match `"20240101"`.
#' @details
#' [sequence_dates()] validates the date range, orders its endpoints, and
#' includes every day. Date strings are matched against the full returned
#' paths, including directory names, without inspecting file contents.
#'
#' Subdirectories are not searched. Directory entries can be returned if
#' their names match `pattern` and their paths contain a requested date.
#' Hidden entries are excluded by the defaults of [base::list.files()].
#' Repeated paths are removed while preserving their first occurrence.
#' @return A non-empty character vector of matching paths, including their
#'   input directory prefix. Paths are not normalized to absolute paths.
#'   An error is raised when no paths match both the pattern and date range.
#' @author amadeus contributors
#' @seealso [sequence_dates()], [base::list.files()]
#' @keywords internal auxiliary
#' @examples
#' \dontrun{
#' resolve_paths(
#'   date = "2021-05-04",
#'   path = "data/hrrr/conus",
#'   pattern = "^hrrr\\.[0-9]{8}\\.t[0-9]{2}z\\.wrfsfcf00\\.grib2$",
#'   hyphen = FALSE
#' )
#' }
#' @export
resolve_paths <- function(
  date,
  path,
  pattern,
  hyphen = TRUE
) {
  # ----------------------------------------------------------------------------
  # Identify file paths from all files in the path.
  chr_paths <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = FALSE
  )
  # ----------------------------------------------------------------------------
  # Generate sequence from user supplied dates.
  chr_datesofinterest <- sequence_dates(date = date, hyphen = hyphen)
  # ----------------------------------------------------------------------------
  # Select only those file paths which align with user supplied date sequence.
  chr_pathsdoi <- unique(
    grep(
      paste(
        chr_datesofinterest,
        collapse = "|"
      ),
      chr_paths,
      value = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # Error if no files are identified.
  if (!length(chr_pathsdoi) > 0) {
    stop("No files matched to dates and arguments at set `path`.")
  } else {
    chr_pathsdoi
  }
}

################################################################################
#' Resolve local files for metadata inspection
#' @description
#' Expand directory inputs recursively and retain existing paths matching a
#' source-specific regular expression.
#' @param path character. Non-empty vector of file or directory paths without
#'   missing values. Directory contents are listed recursively; individual
#'   file paths are considered directly.
#' @param pattern character(1). Non-empty regular expression matched against
#'   full paths, ignoring case.
#' @param source_name character(1). Source label included in the error when
#'   no matching paths are found. Defaults to `"files"`.
#' @details
#' Duplicate paths are removed. Nonexistent paths are discarded, and hidden
#' entries inside directories are excluded by [base::list.files()] defaults.
#' @return A non-empty character vector of existing matching paths. An error
#'   is raised for invalid inputs or when no paths match.
#' @seealso [resolve_paths()], [base::list.files()]
#' @keywords internal auxiliary
#' @noRd
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

################################################################################
# Identify collection, variable, and time metadata.
################################################################################

################################################################################
#' Read GEOS-CF and MERRA2 metadata from filenames
#' @description
#' Extract a collection name or time code from supported GEOS-CF, MERRA2,
#' or GlobalFWI filenames without opening the files.
#' @param path character. File path to inspect. GEOS-CF paths can also be
#'   supplied as a vector; supply one path at a time for MERRA2 and GlobalFWI.
#' @param source character(1). `"geos"` or `"merra2"`. Accepted aliases are
#'   `"GEOS"`, `"geos-cf"`, `"GEOS-CF"`, `"merra"`, `"MERRA"`, and `"MERRA2"`.
#' @param collection logical(1). Return the collection name.
#'   Defaults to `FALSE`.
#' @param date logical(1). Return the date code as `"YYYYMMDD"`.
#'   Defaults to `FALSE`.
#' @param datetime logical(1). Return the GEOS-CF time code as `"YYYYMMDDHHMM"`.
#'   Defaults to `FALSE`. For GlobalFWI files this returns only `"YYYYMMDD"`;
#'   ordinary MERRA2 filenames support `collection` or `date` instead.
#' @details
#' Select one of `collection`, `date`, or `datetime`. Selecting more than one
#' raises an error. Parsing depends on the source's filename convention;
#' the helper does not check file existence or read file metadata.
#'
#' GlobalFWI filenames beginning with `"FWI."` are handled with
#' `source = "merra2"` and return `"fwi"` as their collection.
#' @return A character vector containing the requested collection or time code.
#'   Time codes are strings, not Date or POSIXct objects.
#' @author amadeus contributors
#' @seealso [process_geos()], [process_merra2()]
#' @keywords internal auxiliary
#' @examples
#' process_collection(
#'   "GEOS-CF.v01.rpl.aqc_tavg_1hr_g1440x721_v1.20210504_0030z.nc4",
#'   source = "geos", datetime = TRUE
#' )
#' process_collection(
#'   "MERRA2_400.tavg1_2d_slv_Nx.20210504.nc4",
#'   source = "merra2", collection = TRUE
#' )
#' @export
process_collection <- function(
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

################################################################################
#' Normalize variable names for metadata inspection
#' @description
#' Remove surrounding whitespace and common time or level suffixes from
#' raster layer names to produce a sorted set of variable selectors.
#' @param layer_names character. Raster layer names, usually from
#'   [base::names()] on a `SpatRaster`.
#' @details
#' Empty strings are discarded. A trailing underscore followed by digits is
#' removed first, followed by any suffix beginning with `"_lev="`. Other name
#' components and capitalization are retained.
#' @return A sorted character vector of unique normalized names.
#' @seealso [process_collection()]
#' @keywords internal auxiliary
#' @noRd
info_normalize_layer_variables <- function(layer_names) {
  vars <- as.character(layer_names)
  vars <- trimws(vars)
  vars <- vars[nzchar(vars)]
  vars <- sub("_[0-9]+$", "", vars)
  vars <- sub("_lev=.*$", "", vars)
  sort(unique(vars))
}

################################################################################
#' Parse netCDF day codes from layer names
#' @description
#' Convert numeric day codes in layer names, such as
#' `"precipitation_amount_day=43101"`, to dates relative to an origin.
#' @param layer_names character. Names ending in an equals sign followed by
#'   a non-negative integer day code. Bare numeric strings are also accepted.
#' @param source character(1). Source label used in parsing errors.
#'   Defaults to `"gridmet"`.
#' @param origin character(1) or Date(1). Origin for numeric day codes,
#'   passed to [base::as.Date()]. Defaults to `"1900-01-01"`.
#' @return A Date vector in the same order as `layer_names`. An error is raised
#'   if any name cannot be parsed as a non-negative integer day code.
#' @author amadeus contributors
#' @seealso [process_gridmet()], [base::as.Date()]
#' @keywords internal auxiliary
#' @examples
#' process_parse_ncdf_day_codes(
#'   c("precipitation_amount_day=43101", "precipitation_amount_day=43102")
#' )
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

################################################################################
# Read, select, and stack raster layers.
################################################################################

################################################################################
#' Stack variable layers from hourly raster files
#' @description
#' Read raster files in the supplied order, select layers matching a variable,
#' and combine them into a raster stack with timestamped layer names.
#' @param paths character. Non-empty vector of raster file paths readable by
#'   [terra::rast()]. Files must have compatible spatial geometry and layers
#'   with time metadata suitable for conversion to POSIXct.
#' @param extent NULL, numeric(4), or SpatExtent. Optional read window passed
#'   as `win` to [terra::rast()]. Numeric bounds are ordered as
#'   `c(xmin, xmax, ymin, ymax)` in the raster coordinate reference system.
#' @param variable character(1). Literal substring matched against layer names,
#'   ignoring case. All matching layers in each file are retained.
#' @param short character(1). Variable identifier used as the layer-name prefix,
#'   including any level identifier needed to distinguish variables.
#' @details
#' Layers retain the input file order and their order within each file.
#' The helper does not sort by time, remove duplicate timestamps, aggregate
#' values, or reproject the data. Files should already be filtered to the
#' intended product and forecast lead, where applicable.
#'
#' Time metadata is converted with [base::as.POSIXct()]. Layer names use
#' `short` followed by `"_YYYYMMDD_HHMM"`, formatted from the resulting times.
#' No explicit time zone is supplied during conversion or formatting.
#' @return A `SpatRaster` containing the selected layers, with POSIXct time
#'   metadata and timestamped names.
#' @seealso [resolve_paths()], [terra::rast()], [terra::time()]
#' @keywords internal auxiliary
#' @noRd
stack_hourly <- function(
  paths,
  extent,
  variable,
  short
) {
  # ----------------------------------------------------------------------------
  # Variable-specific raster layers as a list.
  list_rast <- lapply(
    # --------------------------------------------------------------------------
    # Execute for each path.
    paths,
    function(x) {
      # ------------------------------------------------------------------------
      # Import full raster data from path (e.g. .nc, .nc4, .grib, etc).
      rast_x <- terra::rast(x, win = extent)
      # ------------------------------------------------------------------------
      # Identify variable-specific layers.
      idx_var <- grep(
        tolower(variable),
        tolower(names(rast_x)),
        fixed = TRUE
      )
      # ------------------------------------------------------------------------
      # Subset full raster data to variable-specific, single time. layer
      terra::subset(rast_x, idx_var)
    }
  )
  # ----------------------------------------------------------------------------
  # Merge turn multi-time, single-varaible list of raster layers.
  rast_variable <- terra::rast(list_rast)
  # ----------------------------------------------------------------------------
  # Clean `terra` time stamp.
  terra::time(rast_variable) <- as.POSIXct(terra::time(rast_variable))
  # ----------------------------------------------------------------------------
  # Apply variable short name and time stamp to layer names.
  names(rast_variable) <- paste0(
    short,
    "_",
    format(terra::time(rast_variable), "%Y%m%d_%H%M")
  )
  # ----------------------------------------------------------------------------
  rast_variable
}

################################################################################
# Prepare spatial locations and apply extent.
################################################################################

################################################################################
#' Prepare extraction locations as a SpatVector
#' @description
#' Convert supported locations to a `SpatVector`, project them to the requested
#' coordinate reference system, and apply an optional buffer.
#' @param locs SpatVector, sf, or data.frame. Extraction locations. Data frames,
#'   including data.table objects, must contain `lon` and `lat` columns in
#'   longitude and latitude degrees (EPSG:4326). Spatial inputs use their
#'   existing coordinate reference system.
#' @param crs character(1). Target coordinate reference system accepted by
#'   [terra::project()], such as an EPSG code or WKT string.
#' @param radius numeric(1). Non-negative buffer distance passed to
#'   [process_locs_radius()] after projection. Zero retains unbuffered geometry.
#'   Positive distances are in meters for longitude/latitude coordinates and
#'   in the target CRS map units for projected coordinates.
#' @return A `SpatVector` in the requested CRS, buffered when `radius > 0`.
#'   A message identifies the input type; unsupported types or data frames
#'   missing coordinate columns raise an error.
#' @author amadeus contributors
#' @seealso [process_locs_radius()], [terra::vect()], [terra::project()]
#' @importFrom terra crs
#' @importFrom terra vect
#' @importFrom terra project
#' @keywords internal auxiliary
#' @examples
#' process_locs_vector(
#'   data.frame(site_id = "A", lon = -78.8, lat = 35.9),
#'   crs = "EPSG:4326", radius = 0
#' )
#' @export
process_locs_vector <- function(
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
    sites_v <- if (nrow(locs) == 0L) {
      suppressWarnings(terra::vect(locs))
    } else {
      terra::vect(locs)
    }
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

################################################################################
#' Buffer extraction locations
#' @description
#' Return locations unchanged for a zero radius, or buffer their geometry
#' with [terra::buffer()] for a positive radius.
#' @param locs SpatVector. Extraction locations with a defined CRS.
#' @param radius numeric(1). Non-negative buffer distance. Units are meters
#'   for longitude/latitude coordinates and map units for projected coordinates.
#' @details
#' Positive buffers use `quadsegs = 180L` to approximate curved boundaries.
#' Input type and radius validation are left to the caller.
#' @return A `SpatVector`: the original locations when `radius = 0`, or their
#'   buffers when `radius > 0`.
#' @author amadeus contributors
#' @seealso [process_locs_vector()], [terra::buffer()]
#' @importFrom terra buffer
#' @keywords internal auxiliary
#' @examples
#' sites <- terra::vect(
#'   data.frame(lon = -78.8, lat = 35.9),
#'   geom = c("lon", "lat"), crs = "EPSG:4326"
#' )
#' process_locs_radius(sites, radius = 1000)
#' @export
process_locs_radius <- function(
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

################################################################################
#' Select spatial features within an extent
#' @description
#' Apply an extent through spatial subsetting, converting data-frame inputs
#' to a `SpatVector` before selection.
#' @param data sf, SpatVector, or data.frame. Spatial features to select.
#' @param extent numeric(4) or SpatExtent. Bounds ordered as
#'   `c(xmin, xmax, ymin, ymax)` in the same CRS as `data`. For data-frame
#'   inputs, bounds and coordinates are interpreted as EPSG:4326.
#' @param geom character(1) or character(2). Geometry column for WKT data, or
#'   coordinate columns such as `c("lon", "lat")`. Passed to [terra::vect()]
#'   only for data-frame inputs; it can be omitted for spatial inputs.
#' @details
#' The extent is assigned the input CRS for sf inputs. No coordinate
#' transformation is performed. Selection uses `data[extent, ]` and follows
#' the spatial subsetting behavior of the input class.
#' @return An sf object for sf inputs, or a `SpatVector` for SpatVector and
#'   data-frame inputs, containing the selected features.
#' @author amadeus contributors
#' @seealso [terra::ext()], [terra::vect()], [sf::st_filter()]
#' @importFrom sf st_as_sfc st_bbox st_crs
#' @importFrom terra ext
#' @keywords internal auxiliary
#' @examples
#' apply_extent(
#'   data.frame(lon = c(-78.8, -90), lat = c(35.9, 40)),
#'   extent = c(-80, -77, 34, 37),
#'   geom = c("lon", "lat")
#' )
#' @export
apply_extent <- function(
  data,
  extent,
  geom
) {
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

################################################################################
