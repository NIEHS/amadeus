################################################################################
# Primary {process_covariates} function.
################################################################################
# nolint start
#' Process raw data wrapper function
#' @description
#' This function processes raw data files which have
#' been downloaded by [`download_data`]. `process_covariates` and
#' the underlying source-specific processing functions have been designed to
#' operate on the raw data files. To avoid errors, \strong{do not edit the raw
#' data files before passing to `process_covariates`}.
#' @param covariate character(1). Covariate type.
#' @param path character(1). Directory or file path to raw data
#' depending on `covariate` value.
#' @param ... Arguments passed to each raw data processing function.
#' @seealso
#' * \code{\link{process_modis_swath}}: "modis_swath"
#' * \code{\link{process_modis_merge}}: "modis_merge"
#' * \code{\link{process_blackmarble}}: "blackmarble"
#' * \code{\link{process_koppen_geiger}}: "koppen-geiger", "koeppen-geiger", "koppen"
#' * \code{\link{process_ecoregion}}: "ecoregion", "ecoregions"
#' * \code{\link{process_nlcd}}: "nlcd", "NLCD"
#' * \code{\link{process_tri}}: "tri", "TRI"
#' * \code{\link{process_nei}}: "nei", "NEI"
#' * \code{\link{process_geos}}: "geos", "GEOS"
#' * \code{\link{process_goes}}: "goes", "goes_adp", "GOES"
#' * \code{\link{process_gmted}}: "gmted", "GMTED"
#' * \code{\link{process_aqs}}: "aqs", "AQS"
#' * \code{\link{process_edgar}}: "edgar"
#' * \code{\link{process_improve}}: "improve", "IMPROVE"
#' * \code{\link{process_hms}}: "hms", "smoke", "HMS"
#' * \code{\link{process_narr}}: "narr", "NARR"
#' * \code{\link{process_groads}}: "sedac_groads", "roads", "groads"
#' * \code{\link{process_population}}: "sedac_population", "population"
#' * \code{\link{process_merra2}}: "merra", "merra2", "MERRA2"
#' * \code{\link{process_gridmet}}: "gridmet", "gridMET"
#' * \code{\link{process_terraclimate}}: "terraclimate", "TerraClimate"
#' * \code{\link{process_huc}}: "huc", "HUC"
#' * \code{\link{process_cropscape}}: "cropscape", "cdl"
#' * \code{\link{process_prism}}: "prism", "PRISM"
#' * \code{\link{process_drought}}: "drought", "spei", "eddi", "usdm"
#' @return `SpatVector`, `SpatRaster`, `sf`, `data.table`, or `character` depending on
#' covariate type and selections.
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' process_covariates(
#'   covariate = "narr",
#'   date = c("2018-01-01", "2018-01-10"),
#'   variable = "weasd",
#'   path = system.file("extdata", "examples", "narr", "weasd")
#' )
#' }
#' @export
# nolint end
process_covariates <-
  function(
    covariate = c(
      "modis_swath",
      "modis_merge",
      "mcd14ml",
      "koppen-geiger",
      "blackmarble",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "goes",
      "goes_adp",
      "GOES",
      "dummies",
      "gmted",
      "aqs",
      "hms",
      "smoke",
      "sedac_population",
      "population",
      "sedac_groads",
      "groads",
      "roads",
      "nlcd",
      "tri",
      "narr",
      "nei",
      "ecoregions",
      "ecoregion",
      "merra",
      "merra2",
      "gridmet",
      "terraclimate",
      "huc",
      "cropscape",
      "cdl",
      "prism",
      "edgar",
      "improve",
      "IMPROVE",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    path = NULL,
    ...
  ) {
    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(
      covariate,
      modis_merge = process_modis_merge,
      modis_swath = process_modis_swath,
      mcd14ml = process_mcd14ml,
      blackmarble = process_blackmarble,
      ecoregion = process_ecoregion,
      ecoregions = process_ecoregion,
      koppen = process_koppen_geiger,
      narr = process_narr,
      nlcd = process_nlcd,
      smoke = process_hms,
      hms = process_hms,
      sedac_groads = process_groads,
      roads = process_groads,
      groads = process_groads,
      sedac_population = process_population,
      population = process_population,
      nei = process_nei,
      tri = process_tri,
      geos = process_geos,
      goes = process_goes,
      goes_adp = process_goes,
      gmted = process_gmted,
      aqs = process_aqs,
      merra = process_merra2,
      merra2 = process_merra2,
      gridmet = process_gridmet,
      terraclimate = process_terraclimate,
      huc = process_huc,
      cropscape = process_cropscape,
      cdl = process_cropscape,
      prism = process_prism,
      edgar = process_edgar,
      improve = process_improve,
      drought = process_drought,
      spei = function(path, ...) {
        process_drought(path = path, source = "spei", ...)
      },
      eddi = function(path, ...) {
        process_drought(path = path, source = "eddi", ...)
      },
      usdm = function(path, ...) {
        process_drought(path = path, source = "usdm", ...)
      }
    )

    res_covariate <-
      tryCatch(
        {
          what_to_run(
            path = path,
            ...
          )
        },
        error = function(e) {
          stop(
            paste0(
              e,
              "\n",
              paste0(deparse(args(what_to_run)), collapse = "\n"),
              "\n",
              "Please refer to the argument list and ",
              "the error message above to rectify the error.\n"
            )
          )
        }
      )

    return(res_covariate)
  }

################################################################################
# Process auxiliary and internal functions
################################################################################
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

################################################################################
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

################################################################################
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

################################################################################
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

################################################################################
# Internal helper: normalize variable selectors from raster layer names
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

################################################################################
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

################################################################################
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
################################################################################
