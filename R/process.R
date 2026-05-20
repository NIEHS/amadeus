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

# nolint start
#' Process MODIS sub-datasets
#' @description
#' Selected MODIS sinusoidal grid product subdataset name selector.
#' Four presets are supported. `custom_sel` supersedes
#' presets of `product` values.
#' @param product character(1). Product code.
#' @param custom_sel character(1). Custom filter.
#' If this value is not NULL, preset filter is
#' overridden.
#' @param ... Placeholders.
#' @note
#' Preset product codes and associated variables include
#' * "MOD11A1" - Land surface temperature (LST)
#' * "MOD13A2" - Normalized Difference Vegetation Index (NDVI)
#' * "MOD09GA" - Surface reflectance, and
#' * "MCD19A2" - Aerosol optical depth (AOD) and plume injection height.
#'
#' For a full list of available
#' MODIS product codes, see the "Short Name" column at
#' [NASA LP DAAC Search Data Catalog](https://www.earthdata.nasa.gov/centers/lp-daac).
#' When utilizing a product code from this "Short Name" column, \strong{do
#' not include} the version number following the period. For example, if "Short
#' Name" = MCD12C1.006, then `product = "MCD12C1"`.
# nolint end
#' @author Insang Song
#' @return A character object that conforms to the regular
#' expression. Details of regular expression in R can be found in [regexp].
#' @seealso [calculate_modis]
#' @keywords internal
#' @noRd
#' @examples
#' amadeus:::process_modis_sds(product = "MOD09GA")
# previously modis_prefilter_sds
process_modis_sds <-
  function(
    product = c(
      "MOD11A1",
      "MOD13A2",
      "MOD13Q1",
      "MYD13Q1",
      "MOD09GA",
      "MCD19A2",
      "MOD14A1",
      "MYD14A1",
      "MOD14A2",
      "MYD14A2",
      "MOD16A2",
      "MYD16A2",
      "MCD64A1",
      "MCD64CMQ",
      "MCD12Q1",
      "VNP64A1"
    ),
    custom_sel = NULL,
    ...
  ) {
    if (!is.null(custom_sel)) {
      modis_sds <- custom_sel
    } else {
      product <- match.arg(product)
      modis_sds <-
        switch(
          product,
          MOD11A1 = "(LST_)",
          MOD13A2 = "(NDVI)",
          MOD13Q1 = "250m 16 days (NDVI|EVI)",
          MYD13Q1 = "250m 16 days (NDVI|EVI)",
          MOD09GA = "(sur_refl_b0)",
          MCD19A2 = "(Optical_Depth|Injection_Height)",
          MOD14A1 = "(FireMask)",
          MYD14A1 = "(FireMask)",
          MOD14A2 = "(FireMask)",
          MYD14A2 = "(FireMask)",
          MOD16A2 = "(ET_500m|PET_500m)",
          MYD16A2 = "(ET_500m|PET_500m)",
          MCD64A1 = "(Burn Date|BurnDate)",
          MCD64CMQ = "(Burn Date|BurnDate)",
          MCD12Q1 = "(LC_Type)",
          VNP64A1 = "(BurnDate)"
        )
      if (product == "MCD19A2") {
        message(
          sprintf(
            "For MCD19A2, use %s for 5km resolution sub-datasets.\n",
            "(cos|RelAZ|Angle)"
          )
        )
      } else if (product == "MCD12Q1") {
        message(
          sprintf(
            "For MCD12Q1, use %s to select a specific land cover layer.\n",
            "(LC_Type1|LC_Type2|LC_Type3|LC_Type4|LC_Type5)"
          )
        )
      }
    }
    return(modis_sds)
  }


#' Process MODIS layers
#' @description
#' Aggregate layers in a sub-dataset in sinusoidal MODIS products.
#' @param path character(1). Full path to MODIS HDF4/HDF5 file.
#' Direct sub-dataset access is supported, for example,
#' HDF4_EOS:EOS_GRID:\{filename\}:\{base_grid_information\}:\{sub-dataset\}
#' @param subdataset character(1). Exact or regular expression filter of
#' sub-dataset.
#' @param fun_agg character(1). Function name to aggregate layers.
#' Should be acceptable to [terra::tapp].
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @keywords internal
#' @author Insang Song
#' @seealso [terra::tapp], [terra::rast], [terra::describe]
#' @description Some MODIS products consist of multi-layer subdatasets.
#' This function aggregates multiple layers into single layer `SpatRaster.`
#' `fun_agg` is applied at overlapping cells.
#' @note HDF values are read as original without scaling.
#' Users should consult MODIS product documentation to apply proper
#' scaling factor for post-hoc adjustment. If users have no preliminary
#' information about MODIS sub-datasets, consider running
#' `terra::describe(__filename__, sds = TRUE)` to navigate the full
#' list of sub-datasets in the input file then consult the documentation
#' of MODIS product.
#' @importFrom terra describe
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @importFrom terra is.rotated
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_flatten <- process_flatten_sds(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE)[1],
#'   subdataset = "(sur_refl_b0)",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# previously modis_aggregate_sds
process_flatten_sds <-
  function(
    path = NULL,
    subdataset = NULL,
    fun_agg = "mean",
    ...
  ) {
    # if curvilinear, halt
    status_curv <-
      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop(
        "The raster is curvilinear. Please rectify or warp
the input then flatten it manually."
      )
    }

    # describe provides subdataset information
    if (!any(grepl(":", path))) {
      # we use var to get detailed information in subdatasets
      sds_desc <- terra::describe(path, sds = TRUE)
      index_sds <- grep(subdataset, sds_desc$var)
      sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]
      # raw is TRUE to ignore scaling factor.
      sds_read <- terra::rast(path, subds = index_sds, raw = TRUE)
      sds_nsds <- nrow(sds_desc)
      sds_nlyr <- sds_desc$nlyr
      sds_varn <- sds_desc$var
    } else {
      sds_read <- terra::rast(path, raw = TRUE)
      sds_nsds <- 1L
      sds_nlyr <- terra::nlyr(sds_read)
      sds_varn <- names(sds_read)
    }
    if (all(sds_nlyr == 1L)) {
      sds_agg <- sds_read
    } else {
      sds_aggindex <- rep(seq_len(sds_nsds), times = sds_nlyr)
      # if there are multiple layers in a subdataset,
      # aggregate overlapping pixel values
      sds_agg <-
        terra::tapp(sds_read, index = sds_aggindex, fun = fun_agg, na.rm = TRUE)
    }
    # restore names
    names(sds_agg) <- sds_varn
    gc()
    return(sds_agg)
  }


# nolint start
#' Process MODIS .hdf files
#' @description
#' Get mosaic or merged raster from multiple MODIS hdf files.
#' @param path character. Full list of hdf file paths.
#'  preferably a recursive search result from [`base::list.files`].
#' @param date character(1). date to query. Should be in
#' `"YYYY-MM-DD"` format.
#' @param subdataset character(1). subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' Default is `NULL`, which will result in errors. Users should specify
#' which subdatasets will be imported.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (e.g., Aqua files) to fuse with `path` for improved temporal coverage.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, `"secondary_first"`.
#' @param ... For internal use.
#' @note Curvilinear products (i.e., swaths) will not be accepted.
#' MODIS products downloaded by functions in `amadeus`,
#' [MODISTools](https://cran.r-project.org/package=MODISTools),
#' and [luna](https://github.com/rspatial/luna) are accepted.
#' @seealso [`download_data`]
#' @author Insang Song
#' @return a `SpatRaster` object
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_merge <- process_modis_merge(
#'   path =
#'     list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = "2024-01-01",
#'   subdataset = "sur_refl_b01_1",
#'   fun_agg = "mean"
#' )
#' }
#' @export
# nolint end
# previously modis_get_vrt
process_modis_merge <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  ...
) {
  fusion_method <- match.arg(fusion_method)
  if (!is.character(path)) {
    stop("Argument path should be a list of hdf files (character).\n")
  }
  if (!is.null(path_secondary) && !is.character(path_secondary)) {
    stop("Argument path_secondary should be a list of hdf files (character).\n")
  }
  if (!(is.character(fun_agg) || is.function(fun_agg))) {
    stop(
      "Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n"
    )
  }
  # date format check
  amadeus::is_date_proper(instr = date)

  # interpret date
  ftarget <- modis_filter_paths_by_date(path, date = date)
  if (length(ftarget) == 0) {
    stop("No MODIS files matched the requested date.\n")
  }

  # get layer information
  layer_target <-
    lapply(ftarget, function(x) {
      process_flatten_sds(
        x,
        subdataset = subdataset,
        fun_agg = fun_agg
      )
    })
  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(ftarget) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
    gc()
  } else {
    result_merged <- layer_target[[1]]
  }

  if (!is.null(path_secondary)) {
    ftarget_secondary <- modis_filter_paths_by_date(path_secondary, date = date)
    if (length(ftarget_secondary) > 0) {
      layer_target_secondary <-
        lapply(ftarget_secondary, function(x) {
          process_flatten_sds(
            x,
            subdataset = subdataset,
            fun_agg = fun_agg
          )
        })

      if (length(ftarget_secondary) > 1) {
        result_secondary <- do.call(terra::merge, layer_target_secondary)
      } else {
        result_secondary <- layer_target_secondary[[1]]
      }

      if (
        !isTRUE(terra::compareGeom(
          result_merged,
          result_secondary,
          stopOnError = FALSE
        ))
      ) {
        stop(
          "Primary and secondary MODIS rasters have incompatible geometry.\n"
        )
      }
      if (terra::nlyr(result_merged) != terra::nlyr(result_secondary)) {
        stop(
          "Primary and secondary MODIS rasters have different layer counts.\n"
        )
      }

      if (fusion_method == "primary_first") {
        result_merged <- terra::cover(result_merged, result_secondary)
      } else if (fusion_method == "secondary_first") {
        result_merged <- terra::cover(result_secondary, result_merged)
      } else {
        idx_layers <- seq_len(terra::nlyr(result_merged))
        fused <- lapply(idx_layers, function(k) {
          terra::app(
            c(result_merged[[k]], result_secondary[[k]]),
            mean,
            na.rm = TRUE
          )
        })
        result_merged <- do.call(c, fused)
      }
    }
  }
  return(result_merged)
}


# nolint start
#' Process MODIS files as daily outputs
#' @description
#' Process MODIS HDF/H5 files into day-specific rasters over a requested
#' date range. This helper preserves daily slices instead of flattening a
#' multi-day range into one merged result.
#' @param path character. Full list of HDF/H5 file paths.
#' @param date character(1:2). Date or date range in `"YYYY-MM-DD"` format.
#' @param subdataset character(1). Subdataset names to extract.
#' Should conform to regular expression. See [`base::regex`] for details.
#' @param fun_agg Function name or custom function to aggregate overlapping
#' cell values. See `fun` description in [`terra::tapp`] for details.
#' @param path_secondary character. Optional secondary list of HDF/H5 paths
#' (for example, Aqua files) to fuse with `path` by date.
#' @param fusion_method character(1). Fusion method when `path_secondary` is
#' provided: `"mean"`, `"primary_first"`, or `"secondary_first"`.
#' @param return_type character(1). Return `"stack"` for a multi-layer
#' `SpatRaster` (default) or `"list"` for a named list of daily `SpatRaster`
#' objects.
#' @param ... Additional arguments passed to [`process_modis_merge`].
#' @return A day-preserving MODIS result as a `SpatRaster`
#' (`return_type = "stack"`) or named list (`return_type = "list"`).
#' @seealso [`process_modis_merge`], [`download_data`]
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod09ga_daily <- process_modis_daily(
#'   path = list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
#'   date = c("2024-01-01", "2024-01-07"),
#'   subdataset = "sur_refl_b01_1",
#'   return_type = "list"
#' )
#' }
#' @export
# nolint end
process_modis_daily <- function(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  return_type = c("stack", "list"),
  ...
) {
  return_type <- match.arg(return_type)
  amadeus::is_date_proper(instr = date)

  if (length(date) == 1L) {
    date <- rep(date, 2L)
  }

  date_seq <- format(
    seq(as.Date(date[1]), as.Date(date[2]), by = "day"),
    "%Y-%m-%d"
  )
  daily_rasters <- vector("list", length(date_seq))
  names(daily_rasters) <- date_seq

  for (i in seq_along(date_seq)) {
    day_i <- date_seq[[i]]
    daily_rasters[i] <- list(tryCatch(
      process_modis_merge(
        path = path,
        date = day_i,
        subdataset = subdataset,
        fun_agg = fun_agg,
        path_secondary = path_secondary,
        fusion_method = fusion_method,
        ...
      ),
      error = function(e) {
        if (
          grepl(
            "No MODIS files matched the requested date",
            e$message,
            fixed = TRUE
          )
        ) {
          return(NULL)
        }
        stop(e)
      }
    ))
  }

  daily_rasters <- daily_rasters[!vapply(daily_rasters, is.null, logical(1))]
  if (length(daily_rasters) == 0L) {
    stop("No MODIS files matched any day in the requested date range.\n")
  }

  if (return_type == "list") {
    return(daily_rasters)
  }

  rasters_named <- mapply(
    FUN = function(r_i, date_i) {
      names(r_i) <- paste0(names(r_i), "_", gsub("-", "", date_i))
      r_i
    },
    daily_rasters,
    names(daily_rasters),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  do.call(c, rasters_named)
}


process_mcd14ml <- function(
  path = NULL,
  date = NULL,
  extent = NULL,
  ...
) {
  if (is.null(path)) {
    stop("path is required.\n")
  }

  if (length(path) == 1L && dir.exists(path)) {
    path <- list.files(
      path = path,
      pattern = "\\.txt$",
      recursive = TRUE,
      full.names = TRUE
    )
  }

  path <- path[grepl("\\.txt$", path, ignore.case = TRUE)]
  if (length(path) == 0) {
    stop("No MCD14ML text files were found.\n")
  }

  txt_list <- lapply(path, data.table::fread)
  txt_data <- data.table::rbindlist(txt_list, fill = TRUE)
  names(txt_data) <- tolower(names(txt_data))

  required_cols <- c("latitude", "longitude", "acq_date")
  if (!all(required_cols %in% names(txt_data))) {
    stop("MCD14ML input is missing one or more required columns.\n")
  }

  if (!is.null(date)) {
    if (length(date) == 1L) {
      date <- c(date, date)
    }
    amadeus::is_date_proper(instr = date)
    txt_data$acq_date <- as.Date(txt_data$acq_date)
    txt_data <- txt_data[
      txt_data$acq_date >= as.Date(date[1]) &
        txt_data$acq_date <= as.Date(date[2])
    ]
  } else {
    txt_data$acq_date <- as.Date(txt_data$acq_date)
  }

  if (!"frp" %in% names(txt_data)) {
    txt_data$frp <- NA_real_
  }
  txt_data$frp <- as.numeric(txt_data$frp)
  txt_data$fire_count <- 1L
  txt_data$time <- as.integer(format(txt_data$acq_date, "%Y%m%d"))

  txt_vect <- terra::vect(
    as.data.frame(txt_data),
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326",
    keepgeom = TRUE
  )

  if (!is.null(extent)) {
    txt_vect <- apply_extent(txt_vect, extent)
  }

  return(txt_vect)
}


# nolint start
#' Process Black Marble corners
#' @description
#' Tile corner generator for Black Marble products.
#' @param hrange integer(2). Both should be in 0-35.
#' @param vrange integer(2). Both should be in 0-17.
#' @description Black Marble products are in HDF5 format and are read without
#' georeference with typical R geospatial packages.
#' This function generates a `data.frame` of corner coordinates for assignment.
#' @return `data.frame` with xmin, xmax, ymin, and ymax fields
#' @author Insang Song
#' @keywords internal
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @examples
#' process_blackmarble_corners(hrange = c(1, 2), vrange = c(1, 2))
#' @export
# nolint end
process_blackmarble_corners <-
  function(
    hrange = c(5, 11),
    vrange = c(3, 6)
  ) {
    # should be in range
    if (!all(hrange %in% seq(0, 35)) || !all(vrange %in% seq(0, 17))) {
      stop("hrange or vrange are out of range.")
    }
    # in case range is put in reverse order
    hrange <- sort(hrange)
    vrange <- sort(vrange)

    hseq <- seq(hrange[1], hrange[2])
    vseq <- seq(vrange[1], vrange[2])

    tile_df <-
      expand.grid(
        vaddr = sprintf("v%02d", vseq),
        haddr = sprintf("h%02d", hseq)
      )
    hrangec <- -180 + (hseq * 10L)
    vrangec <- 90 - (vseq * 10L)

    hlen <- abs(diff(hrange)) + 1
    vlen <- abs(diff(vrange)) + 1

    tile_df$tile <- paste0(tile_df$haddr, tile_df$vaddr)
    tile_df <- data.frame(tile = tile_df$tile)
    #return(tile_df)
    tile_df$xmin <- rep(hrangec, each = vlen)
    tile_df$xmax <- tile_df$xmin + 10L
    tile_df$ymin <- rep(vrangec, hlen) - 10L
    tile_df$ymax <- tile_df$ymin + 10L

    return(tile_df)
  }


# nolint start
#' Assign VIIRS Black Marble products corner coordinates to retrieve a merged raster
#' @description This function will return a `SpatRaster` object with
#' georeferenced h5 files of Black Marble product. Referencing corner coordinates
#' are necessary as the original h5 data do not include such information.
#' @param path character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df data.frame. Contains four corner coordinates in fields named
#' `c("xmin", "xmax", "ymin", "ymax")`.
#' See [`process_blackmarble_corners`] to generate a valid object for this argument.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param crs character(1). terra::crs compatible CRS.
#' Default is `"EPSG:4326"`
#' @param ... For internal use.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @seealso
#' * [`terra::describe`]
#' * [`terra::merge`]
#' * [`process_blackmarble_corners`]
#' @references
#' - [Wang, Z. (2022). Black Marble User Guide (Version 1.3). NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' vnp46a2 <- process_blackmarble(
#'   path =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   date = "2024-01-01",
#'   tile_df =
#'     process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
#'   subdataset = 3L,
#'   crs = "EPSG:4326"
#' )
#' }
#' @export
# previously modis_preprocess_vnp46
# nolint end
process_blackmarble <- function(
  path = NULL,
  date = NULL,
  tile_df = process_blackmarble_corners(),
  subdataset = 3L,
  crs = "EPSG:4326",
  ...
) {
  amadeus::is_date_proper(instr = date)
  # interpret date from paths
  datejul <- strftime(as.Date(date), format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(
      paste0(
        "(",
        paste(stdtile, collapse = "|"),
        ")"
      ),
      filepaths_today,
      value = TRUE
    )

  filepaths_today_tiles <-
    regmatches(
      filepaths_today,
      regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today)
    )

  vnp_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))
  # for filenames,
  # assign corner coordinates then merge
  # Subdataset 3 is BRDF-corrected nighttime light
  vnp_assigned <-
    mapply(
      function(vnp, tile_in) {
        vnp_ <- terra::rast(vnp, subds = subdataset)
        tile_ext <- tile_df[tile_df$tile == tile_in, -1]

        terra::crs(vnp_) <- terra::crs(crs)
        terra::ext(vnp_) <- unlist(tile_ext)
        vnp_
      },
      vnp_today,
      filepaths_today_tiles_list,
      SIMPLIFY = FALSE
    )
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned[[1]]
  }
  vnp_all[vnp_all == 65535L] <- NaN
  vnp_all[is.nan(vnp_all)] <- NA
  return(vnp_all)
}

#' Warp MODIS swath data into rectilinear grid raster
#' @description Swath data is a type of MODIS data,
#' where curvilinear points are stored with varying resolution depending on
#' the relative position of the sensor axis. As this type of data
#' typically does not work well with planar spatial data, users
#' should warp or rectify this data into a rectilinear raster.
#' Main procedure is done with [`stars::st_warp`], in which users are able to
#' customize the threshold to fill potential gaps that appear where
#' the target resolution is finer than the local resolution of curvilinear
#' grid points.
#' @param path File path of MODIS swath with exact sub-dataset specification.
#' @param cellsize numeric(1). Cell size (spatial resolution) of
#' output rectilinear grid raster.
#' @param threshold numeric(1). Maximum distance to fill gaps if occur.
#' @param crs integer(1)/character(1). Coordinate system definition.
#' Should be compatible with EPSG codes or WKT2.
#' See [`terra::crs`] and [`sf::st_crs`] / [EPSG](https://epsg.io/)
#' @param ... For internal use.
#' @note This function handles one file at a time.
#' @return a `stars` object
#' @author Insang Song
#' @seealso [`terra::rectify`]
#' @keywords internal
#' @importFrom stars st_warp
#' @importFrom stars read_stars
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_warp <- process_modis_warp(
#'   path = paste0(
#'     "HDF4_EOS:EOS_SWATH:",
#'     list.files(
#'       "./data/mod06l2",
#'       full.names = TRUE,
#'       pattern = ".hdf"
#'     )[1],
#'     ":mod06:Cloud_Fraction"
#'   ),
#'   cellsize = 0.1,
#'   threshold = 0.4,
#'   crs = 4326
#' )
#' }
#' @export
# previously modis_warp_stars
process_modis_warp <-
  function(
    path = NULL,
    cellsize = 0.1,
    threshold = cellsize * 4,
    crs = 4326,
    ...
  ) {
    options(sf_use_s2 = FALSE)
    ras <- stars::read_stars(path)
    rtd <-
      stars::st_warp(
        ras,
        crs = crs,
        segments = 500,
        cellsize = cellsize,
        threshold = threshold
      )
    return(rtd)
  }

# nolint start
#' Mosaic MODIS swaths
#' @description This function will return a `SpatRaster` object with
#' values of selected subdatasets. Swath data include curvilinear
#' grids, which require warping/rectifying the original curvilinear grids
#' into rectilinear grids. The function internally warps each of inputs
#' then mosaic the warped images into one large `SpatRaster` object.
#' Users need to select a subdataset to process. The full path looks like
#' `"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
#' the full path to the hdf file.
#' @param path character. Full paths of hdf files.
#' @param date character(1). Date to query.
#' @param subdataset character. Subdatasets to process.
#' __Unlike other preprocessing functions, this argument should specify
#' the exact subdataset name.__ For example, when using MOD06_L2 product,
#' one may specify `c("Cloud_Fraction", "Cloud_Optical_Thickness")`,
#' etc. The subdataset names can be found in `terra::describe()` output.
#' @param suffix character(1). Should be formatted `:{product}:`,
#' e.g., `:mod06:`
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree (decimal degree in WGS84).
#' @param ... For internal use.
#' @seealso
#' * [`process_modis_warp()`], [`stars::read_stars()`], [`stars::st_warp()`]
#' * [GDAL HDF4 driver documentation](https://gdal.org/en/latest/drivers/raster/hdf4.html)
#' * [`terra::describe()`]: to list the full subdataset list with `sds = TRUE`
#' * [`terra::sprc()`], [`terra::rast()`]
#' @return
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a single file with
#' full specification of subdataset.
#' * a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a list of files. In this case, the returned object will have the maximal extent of multiple warped layers
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mosaic
#' @importFrom stars st_mosaic
#' @importFrom terra values
#' @importFrom terra sprc
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' mod06l2_swath <- process_modis_swath(
#'   path = list.files(
#'     "./data/mod06l2",
#'     full.names = TRUE,
#'     pattern = ".hdf"
#'   ),
#'   date = "2024-01-01",
#'   subdataset = "Cloud_Fraction",
#'   suffix = ":mod06:",
#'   resolution = 0.05
#' )
#' }
#' @export
# nolint end
# previously modis_mosaic_mod06
process_modis_swath <-
  function(
    path = NULL,
    date = NULL,
    subdataset = NULL,
    suffix = ":mod06:",
    resolution = 0.05,
    ...
  ) {
    # check date format
    amadeus::is_date_proper(instr = date)
    header <- "HDF4_EOS:EOS_SWATH:"
    ras_mod06 <- vector("list", length = length(subdataset))
    datejul <- strftime(date, format = "%Y%j")
    ## FIXME: this part may result in underperformance.
    ##        Find a way to optimize this part.
    paths_today <- grep(sprintf("A%s", datejul), path, value = TRUE)

    # if two or more paths are put in,
    # these are read into a list then mosaic
    for (element in seq_along(subdataset)) {
      target_text <-
        sprintf("%s%s%s%s", header, paths_today, suffix, subdataset[element])
      # rectified stars objects to SpatRaster
      mod06_element <- split(target_text, target_text) |>
        lapply(process_modis_warp, cellsize = resolution)
      # Remove all NA layers to avoid erroneous values
      mod06_element_nas <-
        sapply(
          mod06_element,
          function(x) {
            xvals <- x[[subdataset[element]]]
            all(is.na(xvals)) || all(is.nan(xvals))
          }
        )
      mod06_element <-
        mod06_element[!mod06_element_nas & !is.null(mod06_element_nas)]

      # prepare a fail-safe alternative return
      # It will be used again later.
      alt <- terra::rast(
        xmin = -128,
        xmax = -64,
        ymin = 20,
        ymax = 52,
        resolution = resolution
      )
      # initialize values with NA
      alt[] <- NA
      alt_dim <- dim(alt)
      alt[1, 1] <- 0
      alt[1, alt_dim[2]] <- 0
      alt[alt_dim[1], 1] <- 0
      alt[alt_dim[1], alt_dim[2]] <- 0
      terra::crs(alt) <- "EPSG:4326"

      if (is.null(mod06_element) || length(mod06_element) == 0) {
        message("All layers are NA or NaN.")
        mod06_element_mosaic <- terra::deepcopy(alt)
      } else {
        # mosaick the warped SpatRasters into one
        mod06_element_mosaic <-
          do.call(stars::st_mosaic, mod06_element) |>
          terra::rast()
        # assigning variable name
        mod06_element_mosaic <-
          terra::crop(mod06_element_mosaic, terra::ext(alt))
      }
      names(mod06_element_mosaic) <- subdataset[element]
      ras_mod06[[element]] <- mod06_element_mosaic
    }
    # SpatRasterCollection can accommodate multiple SpatRasters
    # with different extents (most flexible kind)
    mod06_sprc <- terra::sprc(ras_mod06)
    # post-hoc: stack multiple layers with different extent
    # into one SpatRaster
    # 1. mosaic all layers into one
    mod06_mosaic <- terra::mosaic(mod06_sprc, fun = "median")
    # 2. Assign NAs to prepare "etching"; NA will result in NaNs
    terra::values(mod06_mosaic) <- NA
    # 3. Looping main "etching"; each element is put first
    mod06_etched <-
      sapply(mod06_sprc, terra::mosaic, y = mod06_mosaic, fun = "first")
    # 4. stack
    mod06_return <- do.call(c, mod06_etched)
    return(mod06_return)
  }


#' Process climate classification data
#' @description
#' The \code{process_koppen_geiger()} function imports and cleans raw climate
#' classification data, returning a single `SpatRaster` object.
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom terra rast
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' kg <- process_koppen_geiger(
#'   path = "./data/koppen_geiger_data.tif"
#' )
#' }
#' @export
process_koppen_geiger <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    # import data
    kg_rast <- terra::rast(path, win = extent)
    # identify time period
    period <- strsplit(
      names(kg_rast),
      "_"
    )[[1]][4]
    if (period == "present") {
      terra::metags(kg_rast) <- c(year = "1980 - 2016")
    } else {
      terra::metags(kg_rast) <- c(year = "2071 - 2100")
    }
    return(kg_rast)
  }


#' Process land cover data
#' @description
#' The \code{process_nlcd()} function imports and cleans raw land cover data,
#' returning a single `SpatRaster` object.
#' @param path character giving nlcd data path
#' @param year numeric giving the year of NLCD data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads NLCD file of selected `year`.
#' @return a `SpatRaster` object
#' @author Eva Marques, Insang Song
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' nlcd <- process_nlcd(
#'   path = "./data/",
#'   year = 2021
#' )
#' }
#' @export
process_nlcd <-
  function(
    path = NULL,
    year = 2021,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!dir.exists(path)) {
      stop("path does not exist.")
    }
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    product_codes <- c(
      "LndCov",
      "LndChg",
      "LndCnf",
      "FctImp",
      "ImpDsc",
      "SpcChg"
    )
    # open nlcd file corresponding to the year
    # Try new naming convention (with recursive search for subdirectories)
    nlcd_file <-
      list.files(
        path,
        pattern = paste0(
          "Annual_NLCD_(",
          paste(product_codes, collapse = "|"),
          ")_",
          year,
          "_.*\\.(tif|img)$"
        ),
        full.names = TRUE,
        recursive = TRUE, # ADD THIS - files may be in subdirectories
        ignore.case = TRUE # ADD THIS - for robustness
      )

    if (length(nlcd_file) == 0) {
      message("No NLCD files detected. Trying deprecated file names...")
      nlcd_file <-
        list.files(
          path,
          pattern = paste0("nlcd_", year, "_.*\\.(tif|img)$"),
          full.names = TRUE,
          recursive = TRUE, # ADD THIS
          ignore.case = TRUE # ADD THIS
        )
      if (length(nlcd_file) > 0) {
        # FIXED: was > 1, should be > 0
        message(
          paste0(
            "Deprecated file paths detected. Data still imported, but ",
            "see https://www.mrlc.gov/data/project/annual-nlcd for updated ",
            "NLCD documentation and availability."
          )
        )
      }
    }

    # check if name without extension is duplicated
    nlcd_file_base <- basename(nlcd_file)
    nlcd_file_base <- tools::file_path_sans_ext(nlcd_file_base)
    if (any(duplicated(nlcd_file_base))) {
      stop("Duplicated NLCD files are detected. Please remove duplicates.")
    }
    if (length(nlcd_file) == 0) {
      stop("NLCD data not available for this year.")
    }

    # NLCD C1V1 bug
    # `.aux.xml` metadata file was causing `NA` values to be read as `NaN`,
    # corrupting the factor/integer data values when used downstream.
    # File is hidden with preceding `._` for retention but exclusion in
    # metadata definitions.
    chr_aux_xml_path <- list.files(
      path,
      pattern = paste0(
        "Annual_NLCD_(",
        paste(product_codes, collapse = "|"),
        ")_",
        year,
        "_.*\\.aux\\.xml$" # FIXED: escaped the dot before xml
      ),
      full.names = FALSE,
      recursive = TRUE, # ADD THIS
      ignore.case = TRUE # ADD THIS
    )

    if (length(chr_aux_xml_path) > 0) {
      # FIXED: handle multiple files
      for (aux_file in chr_aux_xml_path) {
        chr_aux_xml_hide <- file.path(
          dirname(file.path(path, aux_file)),
          paste0("._", basename(aux_file))
        )
        chr_aux_xml_full <- file.path(path, aux_file)

        if (file.exists(chr_aux_xml_full) && !file.exists(chr_aux_xml_hide)) {
          message(paste0(
            "Hiding corrupt ",
            basename(aux_file),
            " metadata file."
          ))
          file.rename(chr_aux_xml_full, chr_aux_xml_hide)
        }
      }
    }

    nlcd <- terra::rast(nlcd_file, win = extent)
    terra::metags(nlcd) <- c(Year = as.character(year)) # Changed to capital Y
    return(nlcd)
  }

#' Process ecoregion data
#' @description
#' The [`process_ecoregion`] function imports and cleans raw ecoregion
#' data, returning a `SpatVector` object.
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note The function will fix Tukey's bridge in Portland, ME.
#' This fix will ensure that the EPA air quality monitoring sites
#' will be located within the ecoregion.
#' @author Insang Song
#' @return a `SpatVector` object
#' @importFrom terra vect
#' @importFrom sf st_read st_crs st_as_sfc st_transform st_intersects st_union
#' @importFrom data.table year
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' ecoregion <- process_ecoregion(
#'   path = "./data/epa_ecoregion.gpkg"
#' )
#' }
#' @export
process_ecoregion <-
  function(
    path = NULL,
    extent = NULL,
    ...
  ) {
    ecoreg <- sf::st_read(path)
    # fix Tukey's bridge in Portland, ME
    # nolint start
    poly_tukey <-
      "POLYGON ((-70.258 43.68, -70.2555 43.68, -70.255 43.6733, -70.2576 43.6732, -70.258 43.68))"
    poly_tukey <- sf::st_as_sfc(poly_tukey, crs = "EPSG:4326")
    poly_tukey <- sf::st_transform(poly_tukey, sf::st_crs(ecoreg))

    # nolint end
    ecoreg <- ecoreg[,
      grepl(
        "^(L2_KEY|L3_KEY|NA_L2NAME|US_L3NAME|NA_L3NAME)",
        names(ecoreg)
      )
    ]
    ecoreg_edit_idx <- sf::st_intersects(ecoreg, poly_tukey, sparse = FALSE)
    ecoreg_edit_idx <- vapply(ecoreg_edit_idx, function(x) any(x), logical(1))
    if (!all(ecoreg_edit_idx == 0)) {
      ecoreg_else <- ecoreg[!ecoreg_edit_idx, ]
      ecoreg_edit <- sf::st_union(ecoreg[ecoreg_edit_idx, ], poly_tukey)
      ecoreg <- rbind(ecoreg_else, ecoreg_edit)
    }
    ecoreg$time <- paste0(
      "1997 - ",
      data.table::year(Sys.time())
    )
    ecoreg <- terra::vect(ecoreg)
    if (!is.null(extent)) {
      ecoreg_crop <- terra::crop(ecoreg, extent)
      return(ecoreg_crop)
    } else {
      return(ecoreg)
    }
  }


# nolint start
#' Process toxic release data
#' @description
#' This function imports and cleans raw toxic release data,
#' returning a single `SpatVector` (points) object for the selected `year`.
#' @param path character(1). Path to the directory with TRI CSV files
#' @param year integer(1). Single year to select.
#' @param variables character. One or more regular expressions used to select
#'   TRI release variables by column name after normalization to underscore
#'   naming (for example, `STACK_AIR`, `FUGITIVE_AIR`, `WATER`). Default is
#'   `"STACK_AIR"`. Matching first uses raw TRI column names, then falls back
#'   to a normalized match where punctuation and spaces are converted to
#'   underscores (for example, `"ON-SITE RELEASE TOTAL"` matches
#'   `ON_SITE_RELEASE_TOTAL`). Recommended options include:
#'   \itemize{
#'     \item `FUGITIVE_AIR`
#'     \item `STACK_AIR`
#'     \item `WATER`
#'     \item `UNDERGROUND`
#'     \item `UNDERGROUND_CL_I`
#'     \item `UNDERGROUND_C_II_V`
#'     \item `LANDFILLS`
#'     \item `RCRA_C_LANDFILL`
#'     \item `OTHER_LANDFILLS`
#'     \item `LAND_TREATMENT`
#'     \item `SURFACE_IMPNDMNT`
#'     \item `RCRA_SURFACE_IM`
#'     \item `OTHER_SURFACE_I`
#'     \item `OTHER_DISPOSAL`
#'     \item `ON_SITE_RELEASE_TOTAL`
#'     \item `POTW_TRNS_RLSE`
#'     \item `POTW_TRNS_TRT`
#'     \item `POTW_TOTAL_TRANSFERS`
#'   }
#' @param chemical `NULL` or character. Optional one or more regular
#'   expressions used to filter chemicals. Patterns are matched against
#'   `TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, and `CAS`/`CAS.` values. If
#'   `NULL` (default), all chemicals are retained.
#' @param industry_group character(1). Optional additional grouping level.
#'   One of `"none"` (default), `"industry_sector"`,
#'   `"industry_sector_code"`, or `"both"`.
#' @param ignore_case logical(1). If `TRUE` (default), regular expression
#'   matching in `variables` and `chemical` is case-insensitive.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Kyle Messier
#' @return a `SpatVector` object (points) in `year`
#' `year` is stored in a field named `"year"`.
#' @note Use [get_tri_info()] to inspect
#' available TRI chemical IDs/names/CAS numbers and industry sector codes in
#' local TRI files. Visit [TRI Data and Tools](https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox)
#' to view the available years and variables.
#' @references
#' https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra nearby
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr ends_with
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom tidyr pivot_wider
#' @importFrom stats setNames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' tri <- process_tri(
#'   path = "./data",
#'   year = 2020,
#'   variables = c("STACK_AIR", "FUGITIVE_AIR"),
#'   chemical = "benzene",
#'   industry_group = "industry_sector"
#' )
#' }
# nolint end
#' @export
process_tri <- function(
  path = NULL,
  year = 2018,
  variables = "STACK_AIR",
  chemical = NULL,
  industry_group = c(
    "none",
    "industry_sector",
    "industry_sector_code",
    "both"
  ),
  ignore_case = TRUE,
  extent = NULL,
  ...
) {
  if (!is.character(variables) || length(variables) < 1 || anyNA(variables)) {
    stop(
      "`variables` must be a non-empty character vector of regex patterns.\n"
    )
  }
  if (length(variables) > 0 && any(!nzchar(trimws(variables)))) {
    stop("`variables` cannot include empty patterns.\n")
  }
  if (!is.null(chemical)) {
    if (!is.character(chemical) || length(chemical) < 1 || anyNA(chemical)) {
      stop("`chemical` must be NULL or a non-empty character vector.\n")
    }
    if (any(!nzchar(trimws(chemical)))) {
      stop("`chemical` cannot include empty patterns.\n")
    }
  }
  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop("`ignore_case` must be TRUE or FALSE.\n")
  }
  industry_group <- match.arg(industry_group)

  dt_tri <- tri_read_raw(path = path)

  required_cols <- c(
    "YEAR",
    "LONGITUDE",
    "LATITUDE",
    "TRI_CHEMICAL_COMPOUND_ID",
    "UNIT_OF_MEASURE"
  )
  missing_required <- setdiff(required_cols, names(dt_tri))
  if (length(missing_required) > 0) {
    stop(
      "TRI input is missing required columns: ",
      paste(missing_required, collapse = ", "),
      "\n"
    )
  }

  select_by_pattern <- function(column_names, patterns) {
    unique(unlist(
      lapply(
        patterns,
        function(pat) {
          grep(
            pat,
            column_names,
            ignore.case = ignore_case,
            value = TRUE
          )
        }
      )
    ))
  }
  normalize_name <- function(x) {
    x <- gsub("[^[:alnum:]]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }
  select_by_normalized_pattern <- function(column_names, patterns) {
    normalized_names <- normalize_name(column_names)
    if (ignore_case) {
      normalized_names <- tolower(normalized_names)
    }
    matched_idx <- unique(unlist(
      lapply(
        patterns,
        function(pat) {
          pat_norm <- normalize_name(pat)
          if (ignore_case) {
            pat_norm <- tolower(pat_norm)
          }
          grep(
            pat_norm,
            normalized_names,
            fixed = TRUE
          )
        }
      )
    ))
    column_names[matched_idx]
  }

  selected_variable_cols <- select_by_pattern(names(dt_tri), variables)
  if (length(selected_variable_cols) < 1) {
    selected_variable_cols <-
      select_by_normalized_pattern(names(dt_tri), variables)
  }
  selected_variable_cols <- setdiff(selected_variable_cols, required_cols)
  if (length(selected_variable_cols) < 1) {
    stop("`variables` did not match any TRI variable columns.\n")
  }

  industry_cols <- switch(
    industry_group,
    none = character(0),
    industry_sector = "INDUSTRY_SECTOR",
    industry_sector_code = "INDUSTRY_SECTOR_CODE",
    both = c("INDUSTRY_SECTOR_CODE", "INDUSTRY_SECTOR")
  )
  missing_industry <- setdiff(industry_cols, names(dt_tri))
  if (length(missing_industry) > 0) {
    stop(
      "TRI input is missing industry grouping columns: ",
      paste(missing_industry, collapse = ", "),
      "\n"
    )
  }

  tri_chemical_fields <- intersect(
    c("TRI_CHEMICAL_COMPOUND_ID", "CHEMICAL", "CAS", "CAS_"),
    names(dt_tri)
  )

  selected_cols <- unique(c(
    required_cols,
    selected_variable_cols,
    tri_chemical_fields,
    industry_cols
  ))
  dt_tri <- dt_tri[, selected_cols, drop = FALSE]
  dt_tri <- dt_tri[dt_tri$YEAR == year, ]
  if (nrow(dt_tri) < 1) {
    stop("No TRI rows found for requested `year`.\n")
  }

  if (!is.null(chemical)) {
    chemical_filter <- rep(FALSE, nrow(dt_tri))
    for (field in tri_chemical_fields) {
      field_vals <- as.character(dt_tri[[field]])
      field_hits <- Reduce(
        `|`,
        lapply(
          chemical,
          function(pat) grepl(pat, field_vals, ignore.case = ignore_case)
        )
      )
      chemical_filter <- chemical_filter | field_hits
    }
    dt_tri <- dt_tri[chemical_filter, , drop = FALSE]
    if (nrow(dt_tri) < 1) {
      stop("`chemical` did not match any TRI rows for requested year.\n")
    }
  }

  for (col_nm in selected_variable_cols) {
    original_col <- dt_tri[[col_nm]]
    numeric_col <- suppressWarnings(as.numeric(original_col))
    if (any(!is.na(original_col) & is.na(numeric_col))) {
      stop("Selected TRI variable column `", col_nm, "` is not numeric.\n")
    }
    dt_tri[[col_nm]] <- numeric_col
  }

  # depending on the way the chemicals are summarized
  # Unit is kilogram
  # nolint start
  unit_to_kg <- function(value, unit) {
    ifelse(
      unit == "Pounds",
      value * (453.592 / 1e3),
      ifelse(
        unit %in% c("Grams", "Gram"),
        value / 1e3,
        value
      )
    )
  }
  group_fields <- c(
    "YEAR",
    "LONGITUDE",
    "LATITUDE",
    "TRI_CHEMICAL_COMPOUND_ID",
    industry_cols
  )
  names_from_cols <- c(industry_cols, "TRI_CHEMICAL_COMPOUND_ID")
  tri_name_prefix <- if (length(selected_variable_cols) == 1) {
    paste0(selected_variable_cols, "_")
  } else {
    ""
  }
  dt_tri_x <-
    dt_tri |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_variable_cols),
        ~ unit_to_kg(., UNIT_OF_MEASURE)
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_fields))
    ) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(selected_variable_cols),
        ~ sum(., na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      values_from = dplyr::all_of(selected_variable_cols),
      names_from = dplyr::all_of(names_from_cols),
      names_prefix = tri_name_prefix,
      names_sep = "_",
      values_fill = 0
    ) |>
    dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE))
  names(dt_tri_x) <- sub(" ", "_", names(dt_tri_x))
  tri_value_cols <- setdiff(names(dt_tri_x), c("YEAR", "LONGITUDE", "LATITUDE"))
  if (length(tri_value_cols) > 0L) {
    tri_value_df <- dt_tri_x[, tri_value_cols, drop = FALSE]
    has_tri_signal <- rowSums(!is.na(tri_value_df) & tri_value_df != 0) > 0
    dt_tri_x <- dt_tri_x[has_tri_signal, , drop = FALSE]
  }
  if (nrow(dt_tri_x) < 1) {
    stop("No TRI sites found after filtering missing/zero source values.\n")
  }

  spvect_tri <-
    terra::vect(
      dt_tri_x,
      geom = c("LONGITUDE", "LATITUDE"),
      crs = "EPSG:4269", # all are NAD83
      keepgeom = TRUE
    )
  attr(spvect_tri, "tri_year") <- year
  tri_target_fields <- setdiff(
    names(spvect_tri),
    c("YEAR", "LONGITUDE", "LATITUDE")
  )
  attr(spvect_tri, "tri_target_fields") <- tri_target_fields
  attr(spvect_tri, "tri_grouping") <- names_from_cols
  attr(spvect_tri, "tri_variables") <- selected_variable_cols
  attr(spvect_tri, "tri_chemical_selector") <- chemical
  if (!is.null(extent)) {
    tri_final <- apply_extent(spvect_tri, extent)
    return(tri_final)
  } else {
    return(spvect_tri)
  }
}
# nolint end

# nolint start
#' Process road emissions data
#' @description
#' The \code{process_nei()} function imports and cleans raw road emissions data,
#' returning a single `SpatVector` object.
#' @param path character(1). Directory with NEI csv files.
#' @param county `SpatVector`/`sf`. County boundaries.
#' @param year integer(1) Year to use. Currently only 2017 or 2020
#' is accepted.
#' @param ... Placeholders.
#' @return a `SpatVector` object
#' @author Insang Song
#' @note Base files for `county` argument can be downloaded directly from
#' [U.S. Census Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' or by using `tigris` package. This function does not reproject census boundaries.
#' Users should be aware of the coordinate system of census boundary data for
#' other analyses.
#' @description NEI data comprises multiple csv files where emissions of
#' 50+ pollutants are recorded at county level. With raw data files,
#' this function will join a combined table of NEI data and county
#' boundary, then perform a spatial join to target locations.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' nei <- process_nei(
#'   path = "./data",
#'   county = system.file("gpkg/nc.gpkg", package = "sf"),
#'   year = 2017
#' )
#' }
#' @export
# nolint end
process_nei <- function(
  path = NULL,
  county = NULL,
  year = c(2017, 2020),
  ...
) {
  if (is.null(county)) {
    stop("county argument is required.")
  }
  if (!methods::is(county, "SpatVector")) {
    county <- try(terra::vect(county))
    if (inherits(county, "try-error")) {
      stop("county is unable to be converted to SpatVector.\n")
    }
  }
  if (!"GEOID" %in% names(county)) {
    stop("county should include a field named \"GEOID\"")
  }
  if (!as.character(year) %in% c("2017", "2020")) {
    stop("year should be one of 2017 or 2020.\n")
  }
  # Concatenate NEI csv files
  csvs_nei <-
    list.files(
      path = path,
      pattern = "*.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
  csvs_nei <- grep(year, csvs_nei, value = TRUE)
  if (is.null(csvs_nei) || length(csvs_nei) == 0) {
    stop("No files found for the year. The file names should include the year")
  }
  csvs_nei <- lapply(csvs_nei, data.table::fread)
  csvs_nei <- data.table::rbindlist(csvs_nei)
  # column name readjustment
  target_nm <- c("fips code", "total emissions", "emissions uom")
  # not grep-ping at once for flexibility
  target_cns <- sapply(target_nm, function(x) grep(x, colnames(csvs_nei)))
  colnames(csvs_nei)[target_cns] <-
    c("geoid", "emissions_total", "unit_measurement")

  # unify unit of measurement
  # TON here is short tonne, which is 2000 lbs.
  csvs_nei$emissions_total_ton <-
    ifelse(
      csvs_nei$unit_measurement == "TON",
      csvs_nei$emissions_total,
      csvs_nei$emissions_total / 2000
    )
  emissions_total_ton <- NULL
  geoid <- NULL
  # yearabbr <- substr(year, 3, 4)
  csvs_nei$geoid <- sprintf("%05d", as.integer(csvs_nei$geoid))
  csvs_nei <-
    csvs_nei[,
      list(
        TRF_NEINP_0_00000 = sum(emissions_total_ton, na.rm = TRUE)
      ),
      by = geoid
    ]
  csvs_nei$time <- as.integer(year)

  # read county vector
  county$GEOID <- sprintf("%05d", as.integer(county$GEOID))
  csvs_nei_df <- as.data.frame(csvs_nei)
  county_rows <- match(county$GEOID, csvs_nei_df$geoid)
  county$time <- csvs_nei_df$time[county_rows]
  county$TRF_NEINP_0_00000 <- csvs_nei_df$TRF_NEINP_0_00000[county_rows]
  county$geoid <- county$GEOID
  cnty_vect <- county[!is.na(county$TRF_NEINP_0_00000), ]
  cnty_vect <- cnty_vect[, c("geoid", "time", "TRF_NEINP_0_00000")]
  return(cnty_vect)
}

# nolint start
#' Process U.S. EPA AQS daily CSV data
#' @description
#' The \code{process_aqs()} function cleans and imports raw air quality
#' monitoring sites from pre-generated daily CSV files, returning a single
#' `SpatVector` or `sf` object.
#' `date` is used to filter the raw data read from csv files.
#' Filtered rows are then processed according to `mode` argument.
#' Some sites report multiple measurements per day with and without
#' [exceptional events](https://www.epa.gov/sites/default/files/2016-10/documents/exceptional_events.pdf)
#' the internal procedure of this function keeps "Included" if there
#' are multiple event types per site-time.
#' @param path character(1). Directory path to daily measurement data.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#'  Should be in `"YYYY-MM-DD"` format and sorted.
#' @param mode character(1). One of
#'   * "date-location" (all dates * all locations)
#'   * "available-data" (date-location pairs with available data)
#'   * "location" (unique locations).
#' @param data_field character(1). Data field to extract.
#' @param return_format character(1). `"terra"` or `"sf"` or `"data.table"`.
#' @param extent numeric(4). Spatial extent of the resulting object.
#'   The order should be `c(xmin, xmax, ymin, ymax)`.
#'   The coordinate system should be WGS84 (EPSG:4326).
#' @param ... Placeholders.
#' @seealso
#' * [`download_aqs()`]
#' * [EPA, n.d., _AQS Parameter Codes_](
#'   https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv)
#' @return a SpatVector, sf, or data.table object depending on the `return_format`
#' @importFrom data.table as.data.table
#' @importFrom utils read.csv
#' @importFrom terra vect project
#' @importFrom sf st_as_sf
#' @importFrom dplyr group_by ungroup filter mutate select distinct
#' @note Choose `date` and `mode` values with caution.
#' The function may return a massive data.table depending on the time range,
#' resulting in a long processing time or even a crash if data is too large
#' for your computing environment to process.
#' AQS data are generally intended for use as dependent variables, so
#' `process_aqs()` does not have a companion route in `calculate_covariates()`.
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' aqs <- process_aqs(
#'   path = "./data/aqs_daily_example.csv",
#'   date = c("2022-12-01", "2023-01-31"),
#'   mode = "date-location",
#'   return_format = "terra"
#' )
#' }
#' @export
process_aqs <-
  function(
    path = NULL,
    date = c("2018-01-01", "2022-12-31"),
    mode = c("date-location", "available-data", "location"),
    data_field = "Arithmetic.Mean",
    return_format = c("terra", "sf", "data.table"),
    extent = NULL,
    ...
  ) {
    mode <- match.arg(mode)
    return_format <- match.arg(return_format)
    if (!is.null(date)) {
      date <- try(as.Date(date), silent = TRUE)
      if (inherits(date, "try-error")) {
        stop("date has invalid format(s). Please check the values.")
      }
      if (length(date) != 2) {
        if (length(date) == 1) {
          date <- c(date, date)
        } else {
          stop("date should be a character vector of length 1 or 2.")
        }
      }
    } else {
      stop("date should be defined.")
    }
    if (length(path) == 1 && dir.exists(path)) {
      path <- list.files(
        path = path,
        pattern = "*.csv$",
        full.names = TRUE
      )
    }

    if (length(path) == 0) {
      stop("path does not contain csv files.")
    }
    pathfiles <- lapply(path, read.csv)

    sites <- data.table::rbindlist(pathfiles, fill = TRUE)

    ## get unique sites
    sites$site_id <-
      sprintf(
        "%02d%03d%04d%05d",
        as.integer(sites$State.Code),
        as.integer(sites$County.Code),
        as.integer(sites$Site.Num),
        as.integer(sites$Parameter.Code)
      )

    site_id <- NULL
    Datum <- NULL
    POC <- NULL
    Date.Local <- NULL
    Sample.Duration <- NULL
    Observation.Count <- NULL

    date_start <- as.Date(date[1])
    date_end <- as.Date(date[2])
    date_sequence <- seq(date_start, date_end, "day")

    parsed_dates <- as.Date(rep(NA_character_, nrow(sites)))
    raw_dates <- as.character(sites$Date.Local)
    slash_idx <- grepl("/", raw_dates, fixed = TRUE)
    dash_idx <- grepl("-", raw_dates, fixed = TRUE)
    parsed_dates[slash_idx] <- as.Date(
      raw_dates[slash_idx],
      format = "%m/%d/%Y"
    )
    parsed_dates[dash_idx] <- as.Date(raw_dates[dash_idx], format = "%Y-%m-%d")
    sites$Date.Local <- parsed_dates
    duration_keep <- startsWith(as.character(sites$Sample.Duration), "24")
    if ("Observation.Count" %in% names(sites)) {
      duration_keep <-
        duration_keep |
        (!is.na(sites$Observation.Count) & sites$Observation.Count == 24)
    }
    sites$duration_keep <- duration_keep

    # select relevant fields only
    sites <- sites |>
      dplyr::as_tibble() |>
      dplyr::filter(Date.Local %in% date_sequence) |>
      dplyr::filter(duration_keep) |>
      dplyr::group_by(site_id) |>
      dplyr::filter(POC == min(POC)) |>
      dplyr::mutate(time = as.character(Date.Local)) |>
      dplyr::ungroup()
    col_sel <- c("site_id", "Longitude", "Latitude", "Datum")
    if (mode != "available-data") {
      sites_v <- unique(sites[, col_sel])
    } else {
      col_sel <- append(col_sel, "Event.Type")
      col_sel <- append(col_sel, "time")
      col_sel <- append(col_sel, data_field)
      sites_v <- sites |>
        dplyr::select(dplyr::all_of(col_sel)) |>
        dplyr::distinct()
      # excluding site-time with multiple event types
      # sites_vdup will be "subtracted" from the original sites_v
      sites_vdup <- sites_v |>
        dplyr::group_by(site_id, time) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::filter(!!dplyr::sym("Event.Type") == "Excluded") |>
        dplyr::ungroup()
      sites_v <-
        dplyr::anti_join(
          sites_v,
          sites_vdup,
          by = c("site_id", "time", "Event.Type")
        )
    }
    names(sites_v)[2:3] <- c("lon", "lat")
    sites_v <- data.table::as.data.table(sites_v)

    # subset mainland
    sites_v <- sites_v[!grepl("^(02|15|72|78|6|80)", site_id), ]

    # NAD83 to WGS84
    sites_v_nad <-
      sites_v[sites_v$Datum == "NAD83", ]
    if (nrow(sites_v_nad) > 0) {
      sites_v_nad <- sf::st_as_sf(
        sites_v_nad,
        remove = FALSE,
        coords = c("lon", "lat"),
        crs = "EPSG:4269"
      )
      sites_v_nad <- sf::st_transform(sites_v_nad, "EPSG:4326")
      sites_v_nad$lon <- sf::st_coordinates(sites_v_nad)[, 1]
      sites_v_nad$lat <- sf::st_coordinates(sites_v_nad)[, 2]
      sites_v_nad <- sf::st_drop_geometry(sites_v_nad)
      sites_v_nad <- data.table::as.data.table(sites_v_nad)
    }
    # postprocessing: combine WGS84 and new WGS84 records
    sites_v_wgs <- sites_v[sites_v$Datum == "WGS84"]
    final_sites <- data.table::rbindlist(
      list(sites_v_wgs, sites_v_nad),
      fill = TRUE
    )
    final_sites <-
      final_sites[,
        grep("Datum", names(final_sites), invert = TRUE),
        with = FALSE
      ]

    if (mode == "date-location") {
      final_sites <-
        split(as.character(date_sequence), as.character(date_sequence)) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$time <- x
          return(fs_time)
        })
      final_sites <- data.table::rbindlist(final_sites, fill = TRUE)
    }
    if (mode == "available-data") {
      final_sites <- unique(final_sites)
    }

    final_sites <-
      switch(
        return_format,
        terra = terra::vect(
          final_sites,
          keepgeom = TRUE,
          crs = "EPSG:4326"
        ),
        sf = sf::st_as_sf(
          final_sites,
          remove = FALSE,
          dim = "XY",
          coords = c("lon", "lat"),
          crs = "EPSG:4326"
        ),
        data.table = final_sites
      )
    if (!is.null(extent)) {
      if (return_format == "data.table") {
        warning(
          "Extent is not applicable for data.table. Returning data.table...\n"
        )
        return(final_sites)
      }
      final_sites <- apply_extent(final_sites, extent)
    }

    return(final_sites)
  }


# nolint start
#' Process EDGAR emissions data
#' @description
#' The \code{process_edgar()} function imports extracted EDGAR gridded emissions
#' files and returns a single `SpatRaster` object. Raster formats supported by
#' `terra::rast()` such as NetCDF (`.nc`, `.nc4`) and GeoTIFF (`.tif`,
#' `.tiff`) are supported.
#' @param path character. Directory containing extracted EDGAR raster files or
#'   one or more file paths.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster;
#'   if `NULL` (default), the entire raster is loaded.
#' @param ... Placeholders.
#' @note
#' `process_edgar()` currently supports gridded raster outputs from
#' `download_edgar()` such as the default `format = "nc"`. Plain-text EDGAR
#' downloads should be re-downloaded as raster outputs before processing.
#' @return a `SpatRaster` object
#' @author Mariana Alifa Kassien, Insang Song
#' @seealso [`download_edgar()`], [`calculate_edgar()`]
#' @importFrom terra rast nlyr time
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
#' ##       not included in the package.
#' \dontrun{
#' edgar <- process_edgar(
#'   path = "./data/edgar",
#'   extent = c(-85, -75, 33, 37)
#' )
#' }
#' @export
# nolint end
process_edgar <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  amadeus::check_for_null_parameters(mget(ls()))

  if (length(path) == 1 && dir.exists(path)) {
    path <- list.files(
      path = amadeus::download_sanitize_path(path),
      recursive = TRUE,
      full.names = TRUE
    )
  }

  if (length(path) == 0) {
    stop("path does not contain files.")
  }

  raster_paths <- grep(
    "\\.(nc4?|tif|tiff|grd|img)$",
    path,
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(raster_paths) == 0) {
    txt_paths <- grep("\\.txt$", path, ignore.case = TRUE, value = TRUE)
    if (length(txt_paths) > 0) {
      stop(
        "process_edgar() currently supports gridded raster files only. ",
        "Re-download EDGAR with format = \"nc\" or provide extracted raster files.\n"
      )
    }
    stop("path does not contain supported EDGAR raster files.\n")
  }

  clean_name <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  edgar_rasters <- lapply(
    raster_paths,
    function(pth) {
      data <- terra::rast(pth, win = extent)
      base_name <- clean_name(tools::file_path_sans_ext(basename(pth)))
      layer_names <- clean_name(names(data))

      if (
        terra::nlyr(data) == 1 ||
          any(layer_names == "") ||
          all(grepl("^lyr_?[0-9]+$", layer_names))
      ) {
        names(data) <- sprintf(
          "edgar_%s_%03d",
          base_name,
          seq_len(terra::nlyr(data))
        )
        if (terra::nlyr(data) == 1) {
          names(data) <- paste0("edgar_", base_name)
        }
      } else {
        names(data) <- paste0("edgar_", layer_names)
      }

      if (terra::nlyr(data) == 1) {
        year_gregexpr <- gregexpr(
          "(?<!\\d)\\d{4}(?!\\d)",
          basename(pth),
          perl = TRUE
        )[[1]]
        if (year_gregexpr[1] != -1) {
          year_match <- regmatches(
            basename(pth),
            gregexpr("(?<!\\d)\\d{4}(?!\\d)", basename(pth), perl = TRUE)
          )[[1]][1]
          terra::time(data) <- as.Date(sprintf("%s-01-01", year_match))
        }
      }

      data
    }
  )

  edgar <- do.call(c, c(edgar_rasters, warn = FALSE))
  names(edgar) <- make.unique(names(edgar), sep = "_")
  return(edgar)
}


#' Process population density data
#' @description
#' The \code{process_secac_population()} function imports and cleans raw
#' population density data, returning a single `SpatRaster` object.
#' @param path character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' pop <- process_population(
#'   path = "./data/sedac_population_example.tif"
#' )
#' }
#' @export
# nolint end
process_population <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  if (substr(path, nchar(path) - 2, nchar(path)) == ".nc") {
    message(
      paste0(
        "netCDF functionality for SEDAC data is under construction.\n"
      )
    )
    return()
  }
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### import data
  data <- terra::rast(path, win = extent)
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
    message(paste0(
      "Cleaning ",
      amadeus::process_sedac_codes(
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
    #### year
    terra::metags(data) <- c(year = split2[1])
  }
  return(data)
}


# nolint start
#' Process roads data
#' @description
#' The \code{process_groads()} function imports and cleans raw road data,
#' returning a single `SpatVector` object.
#' @param path character(1). Path to geodatabase or shapefiles.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note U.S. context. The returned `SpatVector` object contains a
#' `$description` column to represent the temporal range covered by the
#' dataset. For more information, see <https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.
#' @author Insang Song
#' @return a `SpatVector` object
#' @importFrom terra vect
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' groads <- process_groads(
#'   path = "./data/groads_example.shp"
#' )
#' }
#' @export
# nolint end
process_groads <- function(
  path = NULL,
  extent = NULL,
  ...
) {
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop(
      "`path` must be a single file path to a .shp or .gdb roads file."
    )
  }
  if (!file.exists(path)) {
    stop(
      sprintf(
        "`path` does not exist: %s",
        path
      )
    )
  }
  if (!grepl("\\.(shp|gdb)$", tolower(path))) {
    stop(
      paste0(
        "`path` must point to a .shp or .gdb file. Received: ",
        path
      )
    )
  }
  #### import data
  data <- terra::vect(path, extent = extent)
  #### time period
  data$description <- "1980 - 2010"
  return(data)
}


# nolint start
#' Process wildfire smoke data
#' @description
#' The \code{process_hms()} function imports and cleans raw wildfire smoke
#' plume coverage data, returning a single `SpatVector` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param path character(1). Directory with downloaded NOAA HMS data files.
#' @param extent numeric(4) or SpatExtent giving the extent of the output
#'   if `NULL` (default), the entire data is returned
#' @param ... Placeholders.
#' @note
#' \code{process_hms()} will return a character object if there are no wildfire
#' smoke plumes present for the selected dates and density. The returned
#' character will contain the density value and the sequence of dates
#' for which no wildfire smoke plumes were detected (see "Examples").
#' If multiple density polygons overlap, the function will return the
#' highest density value.
#' @examples
#' hms <- process_hms(
#'   date = c("2018-12-30", "2019-01-01"),
#'   path = "../tests/testdata/hms/"
#' )
# nolint end
#' @author Mitchell Manware
#' @return a `SpatVector` or character object
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra subset
#' @importFrom stats na.omit
#' @export
process_hms <- function(
  date = "2018-01-01",
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### identify file paths
  paths <- list.files(
    path,
    pattern = "hms_smoke*.*.shp$",
    full.names = TRUE
  )
  paths <- paths[grep(
    ".shp",
    paths
  )]
  #### identify dates based on user input
  dates_of_interest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### dates of interest with hyphen for return in 0 polygon case
  dates_no_polygon <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = FALSE
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
    data_date <- terra::vect(data_paths[d], extent = extent)
    data_date_p <- terra::project(
      data_date,
      "EPSG:4326"
    )
    #### subset to density of interest
    data_density <- data_date_p

    #### absent polygons (ie. December 31, 2018)
    if (nrow(data_density) == 0) {
      message(paste0(
        " smoke plume polygons absent for date ",
        as.Date(
          dates_of_interest[d],
          format = "%Y%m%d"
        ),
        ". Returning empty SpatVector.\n"
      ))
      next
    } else {
      date <- as.Date(
        substr(data_density$Start[1], 1, 7),
        format = "%Y%j"
      )
      message(paste0(
        "Cleaning smoke data for date ",
        date[1],
        "..."
      ))
      #### zero buffer to avoid self intersection
      data_0_buffer <- terra::buffer(
        data_density,
        width = 0
      )
      # add dummy polygons
      poly_dummy <-
        "POLYGON((-180 -86,-179.99 -86,-179.99 -85.99,-180 -85.99,-180 -86))"
      poly_dummy <- terra::vect(poly_dummy)
      poly_dummy <- rbind(poly_dummy, poly_dummy, poly_dummy)
      poly_dummy <- terra::set.crs(poly_dummy, terra::crs(data_0_buffer))
      poly_dummy$Density <- c("Heavy", "Medium", "Light")
      poly_dummy$.hmsdummy <- 1
      data_0_buffer_a <- rbind(data_0_buffer, poly_dummy)
      data_0_buffer_a$.hmsdummy <-
        ifelse(
          is.nan(data_0_buffer_a$.hmsdummy),
          NA,
          data_0_buffer_a$.hmsdummy
        )
      #### aggregate polygons
      data_aggregate <- terra::aggregate(
        data_0_buffer_a,
        by = "Density",
        dissolve = TRUE
      )

      # Density index sorting to Heavy-Medium-Light
      sort_index <- data_aggregate$Density
      sort_index <-
        match(
          c("Heavy", "Medium", "Light"),
          sort_index
        )
      sort_index <- stats::na.omit(sort_index)
      data_aggregate <- data_aggregate[sort_index, ]

      # union polygons. Heavy-Medium-Light are 1, 2, 3, respectively.
      data_aggregate <- terra::union(data_aggregate)
      data_aggregate$indx <-
        as.numeric(
          paste0(
            data_aggregate$id_1,
            data_aggregate$id_2,
            data_aggregate$id_3
          )
        )
      # cut integers into three classes, assign labels
      data_aggregate$Density <-
        cut(
          data_aggregate$indx,
          c(0, 10, 100, 1000),
          labels = c("Light", "Medium", "Heavy"),
          right = FALSE
        )
      # reaggregate to dissolve polygons
      data_aggregate <- terra::aggregate(data_aggregate, by = "Density")
      data_aggregate$Date <- date
      #### factorize
      #### select "Density" and "Date"
      data_aggregate <- data_aggregate[, c("Density", "Date")]
      #### merge with other data
      data_return <- rbind(data_return, data_aggregate)
      reext <- terra::ext(c(-180, 180, -65, 85))
      data_return <- terra::crop(data_return, reext)
    }
  }
  #### if no polygons
  if (nrow(data_return) == 0) {
    message(paste0(
      "Smoke plume polygons absent from ",
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
    no_polygon_return <- c(as.character(dates_no_polygon))
    return(no_polygon_return)
  } else if (nrow(data_return) > 0) {
    message(paste0(
      "Returning daily smoke data from ",
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

#' Process elevation data
#' @description
#' The \code{process_gmted()} function imports and cleans raw elevation data,
#' returning a single `SpatRaster` object.
#' @param variable vector(1). Vector containing the GMTED statistic first and
#' the resolution second. (Example: variable = c("Breakline Emphasis",
#' "7.5 arc-seconds")).
#' * Statistic options: "Breakline Emphasis", "Systematic Subsample",
#'   "Median Statistic", "Minimum Statistic", "Mean Statistic",
#'   "Maximum Statistic", "Standard Deviation Statistic"
#' * Resolution options: "30 arc-seconds", "15 arc-seconds", "7.5 arc-seconds"
#' @param path character(1). Directory with downloaded GMTED  "*_grd"
#' folder containing .adf files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @note
#' `SpatRaster` layer name indicates selected variable and resolution, and year
#' of release (2010).
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra varnames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' gmted <- process_gmted(
#'   variable = c("Breakline Emphasis", "7.5 arc-seconds"),
#'   path = "./data/be75_grd"
#' )
#' }
#' @export
process_gmted <- function(
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
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
  statistic_code <- amadeus::process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  resolution <- variable[2]
  resolution_code <- amadeus::process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  message(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  #### identify file path
  paths <- list.files(
    path,
    full.names = TRUE,
    recursive = TRUE
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
  data_path <- data_paths[grep("(_grd$|w001001.adf)", data_paths)]
  #### import data
  data <- terra::rast(data_path, win = extent)
  #### layer name
  names(data) <- paste0(
    "elevation_",
    gsub(
      "_grd",
      "",
      names(data)
    ),
    "_2010"
  )
  #### varnames
  terra::varnames(data) <- paste0(
    "Elevation: ",
    statistic,
    " (",
    resolution,
    ")"
  )
  #### year
  terra::metags(data) <-
    c(year = 2010L)
  #### set coordinate reference system
  return(data)
}

# nolint start
#' Process meteorological data
#' @description
#' The \code{process_narr()} function imports and cleans raw meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name acronym. See [List of Variables in NARR Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' pressure level, and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @importFrom stringi stri_pad
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' process_narr(
#'   date = c("2018-01-01", "2018-01-10"),
#'   variable = "weasd",
#'   path = "./tests/testdata/narr/weasd"
#' )
#' }
#' @export
# nolint end
process_narr <- function(
  date = "2023-09-01",
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
    recursive = TRUE,
    full.names = TRUE
  )
  data_paths <- grep(
    sprintf("%s*.*.nc", variable),
    data_paths,
    value = TRUE
  )
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### path ncar
  path_ncar <- nchar(
    strsplit(
      data_paths[1],
      paste0(
        variable,
        "."
      )
    )[[1]][3]
  )
  if (path_ncar == 9) {
    #### define year-months of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        6
      )
    )
  } else if (path_ncar == 7) {
    #### define years of interest
    ym_of_interest <- unique(
      substr(
        date_sequence,
        1,
        4
      )
    )
  }
  #### subset file paths to only dates of interest
  data_paths_ym <- unique(
    grep(
      paste(
        ym_of_interest,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths_ym)) {
    #### import data
    data_year <- terra::rast(data_paths_ym[p], win = extent)
    #### check for mono or pressure levels
    if (grepl("level", names(data_year)[1])) {
      #### pressure levels data
      message(paste0("Detected pressure levels data...\n"))
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
        month.name[
          as.numeric(
            substr(
              gsub(
                "-",
                "",
                terra::time(data_year)[1]
              ),
              5,
              6
            )
          )
        ],
        ", ",
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
          substr(
            gsub(
              "-",
              "",
              terra::time(data_year)
            ),
            5,
            6
          ),
          days
        ),
        format = "%Y%m%d"
      )
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
      message(paste0("Detected monolevel data...\n"))
      message(paste0(
        "Cleaning ",
        variable,
        " data for ",
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
      ) %in%
        date_sequence
    )
  )
  message(paste0(
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

#' Process atmospheric composition data
#' @description
#' The \code{process_geos()} function imports and cleans raw atmospheric
#' composition data, returning a single `SpatRaster` object.
#' @details
#' GEOS-CF netCDF collections currently supported by \code{download_geos()} are:
#' \code{"aqc_tavg_1hr_g1440x721_v1"},
#' \code{"chm_tavg_1hr_g1440x721_v1"},
#' \code{"met_tavg_1hr_g1440x721_x1"},
#' \code{"xgc_tavg_1hr_g1440x721_x1"},
#' \code{"chm_inst_1hr_g1440x721_p23"}, and
#' \code{"met_inst_1hr_g1440x721_p23"}.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). GEOS-CF variable name(s). See \emph{Notes}
#'   for collection-specific variable-name guidance.
#' @param path character(1). Directory with downloaded netCDF (.nc4) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves the original hourly output.
#'   Aggregation groups layers by variable/level and date so that pressure-level
#'   structure is preserved. Not meaningful for collections that are already
#'   daily.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
#'   `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour when `daily_agg = FALSE` (default). When
#' `daily_agg = TRUE`, layer names contain the variable, pressure level, and
#' date only, and `terra::time()` is set to midnight UTC of each date.
#'
#' Collection-specific variable names accepted by \code{variable}:
#' \tabular{ll}{
#' \strong{Collection}\tab \strong{Variables}\cr
#' \code{aqc_tavg_1hr_g1440x721_v1}\tab
#' \code{no2}, \code{co}, \code{so2}, \code{pm25_rh35_gcc}, \code{o3}\cr
#' \code{chm_tavg_1hr_g1440x721_v1}\tab
#' \code{ocpi}, \code{bcpo}, \code{pm25soa_rh35_gc}, \code{dst4}, \code{prpe},
#' \code{macr}, \code{pm25ss_rh35_gcc}, \code{hno4}, \code{ch4}, \code{nh3},
#' \code{h2o2}, \code{rcho}, \code{hno3}, \code{dst1}, \code{pan},
#' \code{pm25oc_rh35_gcc}, \code{c3h8}, \code{soas}, \code{no}, \code{tolu},
#' \code{mvk}, \code{xyle}, \code{isop}, \code{noy}, \code{sala}, \code{so2},
#' \code{co}, \code{n2o5}, \code{eoh}, \code{o3}, \code{acet}, \code{c2h6},
#' \code{mek}, \code{nit}, \code{benz}, \code{soap}, \code{alk4}, \code{ocpo},
#' \code{ald2}, \code{hcho}, \code{pm25_rh35_gocar}, \code{dst3},
#' \code{pm25su_rh35_gcc}, \code{pm25_rh35_gcc}, \code{pm25ni_rh35_gcc},
#' \code{pm25bc_rh35_gcc}, \code{dst2}, \code{pm25du_rh35_gcc}, \code{bcpi},
#' \code{no2}, \code{salc}, \code{nh4}\cr
#' \code{met_tavg_1hr_g1440x721_x1}\tab
#' \code{zl}, \code{zpbl}, \code{ps}, \code{v2m}, \code{v}, \code{q2m},
#' \code{u}, \code{t2m}, \code{troppb}, \code{q}, \code{t}, \code{v10m},
#' \code{t10m}, \code{u2m}, \code{q10m}, \code{ts}, \code{slp}, \code{cldtt},
#' \code{phis}, \code{tprec}, \code{u10m}, \code{rh}\cr
#' \code{xgc_tavg_1hr_g1440x721_x1}\tab
#' \code{wetdepflx_nh4}, \code{aod550_dst6}, \code{wetdepflx_dst1},
#' \code{tropcol_io}, \code{totcol_o3}, \code{tropcol_hcho},
#' \code{drydepflx_bcpi}, \code{aod550_cloud}, \code{aod550_dst5},
#' \code{wetdepflx_hcho}, \code{aod550_salc}, \code{aod550_dust},
#' \code{wetdepflx_so2}, \code{wetdepflx_salc}, \code{wetdepflx_dst3},
#' \code{drydepflx_nit}, \code{wetdepflx_so4}, \code{aod550_sala},
#' \code{aod550_dst1}, \code{tropcol_co}, \code{wetdepflx_bcpi},
#' \code{drydepflx_sala}, \code{wetdepflx_nh3}, \code{tropcol_no2},
#' \code{wetdepflx_nit}, \code{aod550_sulfate}, \code{wetdepflx_ocpi},
#' \code{drydepflx_hcho}, \code{drydepflx_dst4}, \code{tropcol_so2},
#' \code{drydepflx_ocpi}, \code{tropcol_o3}, \code{drydepflx_nh4},
#' \code{aod550_dst7}, \code{totcol_co}, \code{totcol_so2}, \code{totcol_io},
#' \code{drydepflx_nh3}, \code{wetdepflx_sala}, \code{wetdepflx_dst4},
#' \code{drydepflx_o3}, \code{drydepflx_hno3}, \code{aod550_dst4},
#' \code{aod550_oc}, \code{totcol_no2}, \code{drydepflx_dst2},
#' \code{tropcol_bro}, \code{wetdepflx_bcpo}, \code{drydepflx_bcpo},
#' \code{wetdepflx_dst2}, \code{drydepflx_dst1}, \code{aod550_dst2},
#' \code{aod550_bc}, \code{aod550_dst3}, \code{wetdepflx_ocpo},
#' \code{drydepflx_dst3}, \code{drydepflx_salc}, \code{wetdepflx_hno3},
#' \code{drydepflx_ocpo}, \code{drydepflx_no2}, \code{totcol_hcho},
#' \code{totcol_bro}\cr
#' \code{chm_inst_1hr_g1440x721_p23}\tab
#' \code{pm25soa_rh35_gc}, \code{pm25ss_rh35_gcc}, \code{so2}, \code{co},
#' \code{o3}, \code{pm25oc_rh35_gcc}, \code{pm25du_rh35_gcc}, \code{noy},
#' \code{no2}, \code{pm25ni_rh35_gcc}, \code{pm25bc_rh35_gcc},
#' \code{pm25_rh35_gcc}, \code{pm25su_rh35_gcc}\cr
#' \code{met_inst_1hr_g1440x721_p23}\tab
#' \code{omega}, \code{t}, \code{eth}, \code{q}, \code{epv}, \code{rh},
#' \code{slp}, \code{airdens}, \code{ps}, \code{h}, \code{th}, \code{v},
#' \code{u}, \code{airvol_chem}\cr
#' }
#'
#' \code{variable} matching is case-insensitive (for example, \code{"o3"}
#' matches \code{"O3"}).
#'
#' Reference: NASA GEOS-CF OpenDAP catalog
#' <https://opendap.nccs.nasa.gov/dods/gmao/geos-cf/assim>.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra tapp
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' geos <- process_geos(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "O3",
#'   path = "./data/aqc_tavg_1hr_g1440x721_v1"
#' )
#' ## daily mean across all sub-daily layers per variable/level
#' geos_daily <- process_geos(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "O3",
#'   path = "./data/aqc_tavg_1hr_g1440x721_v1",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
process_geos <-
  function(
    date = c("2018-01-01", "2018-01-10"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    daily_agg = FALSE,
    fun = "mean",
    ...
  ) {
    #### directory setup
    path <- amadeus::download_sanitize_path(path)
    #### check for variable
    amadeus::check_for_null_parameters(mget(ls()))
    if (!is.character(variable) || length(variable) != 1 || !nzchar(variable)) {
      stop("`variable` must be a single non-empty character string.\n")
    }
    variable <- trimws(variable)
    #### check dates
    if (length(date) == 1) {
      date <- c(date, date)
    }
    stopifnot(length(date) == 2)
    date <- date[order(as.Date(date))]
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "GEOS-CF.v01.rpl",
      full.names = TRUE,
      recursive = TRUE
    )
    paths <- grep(".nc4$", paths, value = TRUE)

    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
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
    collection <- amadeus::process_collection(
      data_paths[1],
      source = "geos",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_list <- vector("list", length(data_paths))
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p])
      data_datetime <- amadeus::process_collection(
        data_paths[p],
        source = "geos",
        datetime = TRUE
      )
      message(paste0(
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
      data_variable_idx <- grep(
        tolower(variable),
        tolower(names(data_raw)),
        fixed = TRUE
      )
      if (length(data_variable_idx) == 0) {
        available_base <- sort(unique(sub("_lev=.*$", "", names(data_raw))))
        stop(
          paste0(
            "Variable '",
            variable,
            "' was not found in collection ",
            collection,
            ".\n",
            "Available case-sensitive variables include: ",
            paste(available_base, collapse = ", "),
            ".\n"
          )
        )
      }
      data_variable <- terra::subset(
        data_raw,
        subset = data_variable_idx
      )
      matched_base <- sort(unique(sub("_lev=.*$", "", names(data_variable))))
      if (!(variable %in% matched_base)) {
        message(
          "Requested variable '",
          variable,
          "' matched case-sensitive variable(s): ",
          paste(matched_base, collapse = ", "),
          ".\n"
        )
      }
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
          ":",
          "",
          gsub(
            "-",
            "",
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
      #### store for one-shot stack creation
      data_list[[p]] <- data_variable
    }
    #### combine all layers in one call
    data_return <- terra::rast(data_list)
    #### set coordinate reference system
    terra::crs(data_return) <- "EPSG:4326"
    #### optional daily aggregation
    if (isTRUE(daily_agg)) {
      t <- terra::time(data_return)
      if (!anyNA(t) && length(t) == terra::nlyr(data_return)) {
        date_str <- format(as.Date(t), "%Y%m%d")
      } else {
        # nocov start
        date_str <- regmatches(
          names(data_return),
          regexpr(
            "(?<![0-9])[0-9]{8}(?![0-9])",
            names(data_return),
            perl = TRUE
          )
        )
        if (length(date_str) != terra::nlyr(data_return)) {
          stop("daily_agg: cannot determine dates from layer times or names.\n")
        }
      } # nocov end
      var_prefix <- sub("_[0-9]{8}.*$", "", names(data_return))
      tapp_index <- paste(var_prefix, date_str, sep = "_")
      data_return <- terra::tapp(
        data_return,
        tapp_index,
        fun = fun,
        na.rm = TRUE
      )
      terra::crs(data_return) <- "EPSG:4326"
      out_dates <- regmatches(
        names(data_return),
        regexpr("[0-9]{8}$", names(data_return))
      )
      terra::time(data_return) <- as.POSIXct(
        paste0(out_dates, " 00:00:00"),
        format = "%Y%m%d %H:%M:%S",
        tz = "UTC"
      )
    }
    message(paste0(
      "Returning ",
      ifelse(isTRUE(daily_agg), "daily ", "hourly "),
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

#' Process meteorological and atmospheric data
#' @description
#' The \code{process_merra2()} function imports and cleans raw atmospheric,
#' meteorological, and MERRA2-based Fire Weather Index data, returning a single
#' `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). MERRA2 variable name(s). For daily corrected
#'   Fire Weather Index files (`collection = "fwi"` during download), use one
#'   of `"DC"`, `"DMC"`, `"FFMC"`, `"ISI"`, `"BUI"`, or `"FWI"` (or the full raw
#'   layer name).
#' @param path character(1). Directory with downloaded netCDF (`.nc4` or `.nc`)
#'   files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves the original sub-daily
#'   output. Aggregation groups layers by variable/level and date. Silently
#'   ignored for FWI collections, which are already daily.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"`, `"max"`, `"min"`, `"sum"`). Ignored when
#'   `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date, and hour for standard MERRA-2 collections when
#' `daily_agg = FALSE` (default). When `daily_agg = TRUE`, layer names contain
#' the variable, pressure level, and date only, and `terra::time()` is set to
#' midnight UTC of each date. For daily Fire Weather Index files, layer names
#' contain the variable and date only regardless of `daily_agg`.
#' Pressure level values utilized for layer names are taken directly from raw
#' data and are not edited to retain pressure level information.
#' @author Mitchell Manware
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra tapp
#' @importFrom terra time
#' @importFrom terra names
#' @importFrom terra crs
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' merra2 <- process_merra2(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "CPT",
#'   path = "./data/inst1_2d_int_Nx"
#' )
#' ## daily mean CPT
#' merra2_daily <- process_merra2(
#'   date = c("2024-01-01", "2024-01-10"),
#'   variable = "CPT",
#'   path = "./data/inst1_2d_int_Nx",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
process_merra2 <-
  function(
    date = c("2018-01-01", "2018-01-10"),
    variable = NULL,
    path = NULL,
    extent = NULL,
    daily_agg = FALSE,
    fun = "mean",
    ...
  ) {
    #### directory setup
    path <- amadeus::download_sanitize_path(path)
    #### check for variable
    amadeus::check_for_null_parameters(mget(ls()))
    #### check dates
    if (length(date) == 1) {
      date <- c(date, date)
    }
    stopifnot(length(date) == 2)
    date <- date[order(as.Date(date))]
    #### identify file paths
    paths <- list.files(
      path,
      pattern = "MERRA2_400|^FWI\\.",
      full.names = TRUE
    )
    paths <- paths[grep(
      "\\.nc4?$",
      paths
    )]
    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
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
    if (length(data_paths) == 0) {
      stop("No MERRA2 files matching the requested date were found.\n")
    }
    #### identify collection
    collection <- amadeus::process_collection(
      data_paths[1],
      source = "merra2",
      collection = TRUE
    )
    message(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    #### initiate for loop
    data_list <- vector("list", length(data_paths))
    for (p in seq_along(data_paths)) {
      #### import .nc4 data
      data_raw <- terra::rast(data_paths[p], win = extent)
      data_date <- amadeus::process_collection(
        data_paths[p],
        source = "merra2",
        date = TRUE
      )
      message(
        paste0(
          "Cleaning ",
          variable,
          " data for ",
          as.Date(
            data_date,
            format = "%Y%m%d"
          ),
          "...\n"
        )
      )
      #### subset to user-selected variable
      data_variable_index <- grep(
        variable,
        names(data_raw),
        fixed = TRUE
      )
      if (length(data_variable_index) == 0) {
        data_variable_index <- grep(
          variable,
          names(data_raw)
        )
      }
      if (length(data_variable_index) == 0) {
        stop(
          paste0(
            "Requested variable ",
            variable,
            " was not found in ",
            basename(data_paths[p]),
            ".\n"
          )
        )
      }
      data_variable <- terra::subset(
        data_raw,
        subset = data_variable_index
      )
      if (collection == "fwi") {
        #### set layer names for daily GlobalFWI files
        names(data_variable) <- paste0(
          gsub("_", ".", names(data_variable)),
          "_",
          data_date
        )
        #### set layer times
        terra::time(data_variable) <- ISOdatetime(
          year = substr(data_date, 1, 4),
          month = substr(data_date, 5, 6),
          day = substr(data_date, 7, 8),
          hour = 0,
          min = 0,
          sec = 0,
          tz = "UTC"
        )
      } else {
        #### identify time step
        times <- amadeus::process_merra2_time(
          collection = collection,
          from = data_variable
        )
        #### identify unique pressure levels
        levels <-
          unique(
            grep(
              "lev=",
              unlist(
                strsplit(names(data_variable), "_")
              ),
              value = TRUE
            )
          )
        #### empty `levels` if 2 dimensional data
        if (length(levels) == 0) {
          levels <- ""
        }
        #### merge levels and times
        leveltimes <- merge(levels, times)
        #### set layer names
        names(data_variable) <- gsub(
          "__",
          "_",
          paste0(
            variable,
            "_",
            leveltimes[, 1],
            "_",
            data_date,
            "_",
            leveltimes[, 2]
          )
        )
        #### set layer times
        terra::time(data_variable) <- ISOdatetime(
          year = substr(data_date, 1, 4),
          month = substr(data_date, 5, 6),
          day = substr(data_date, 7, 8),
          hour = substr(leveltimes[, 2], 1, 2),
          min = substr(leveltimes[, 2], 3, 4),
          sec = substr(leveltimes[, 2], 5, 6),
          tz = "UTC"
        )
      }
      #### store for one-shot stack creation
      data_list[[p]] <- data_variable
    }
    #### combine all layers in one call
    data_return <- terra::rast(data_list)
    if (collection == "fwi") {
      terra::crs(data_return) <- "EPSG:4326"
    } else {
      terra::crs(data_return) <- "EPSG:4267"
    }
    #### optional daily aggregation (not applicable to already-daily FWI)
    if (isTRUE(daily_agg) && collection != "fwi") {
      t <- terra::time(data_return)
      if (!anyNA(t) && length(t) == terra::nlyr(data_return)) {
        date_str <- format(as.Date(t), "%Y%m%d")
      } else {
        # nocov start
        date_str <- regmatches(
          names(data_return),
          regexpr(
            "(?<![0-9])[0-9]{8}(?![0-9])",
            names(data_return),
            perl = TRUE
          )
        )
        if (length(date_str) != terra::nlyr(data_return)) {
          stop("daily_agg: cannot determine dates from layer times or names.\n")
        }
      } # nocov end
      var_prefix <- sub("_[0-9]{8}.*$", "", names(data_return))
      tapp_index <- paste(var_prefix, date_str, sep = "_")
      saved_crs <- terra::crs(data_return)
      data_return <- terra::tapp(
        data_return,
        tapp_index,
        fun = fun,
        na.rm = TRUE
      )
      terra::crs(data_return) <- saved_crs
      out_dates <- regmatches(
        names(data_return),
        regexpr("[0-9]{8}$", names(data_return))
      )
      terra::time(data_return) <- as.POSIXct(
        paste0(out_dates, " 00:00:00"),
        format = "%Y%m%d %H:%M:%S",
        tz = "UTC"
      )
    }
    message(paste0(
      "Returning ",
      ifelse(
        collection == "fwi" || isTRUE(daily_agg),
        "daily ",
        "hourly "
      ),
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


# nolint start
#' Process gridMET data
#' @description
#' The \code{process_gridmet()} function imports and cleans raw gridded surface meteorological
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [gridMET Generate Wget File](https://www.climatologylab.org/wget-gridmet.html)
#' for variable names and acronym codes. (Note: variable "Burning Index" has code "bi" and variable
#' "Energy Release Component" has code "erc").
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym,
#' and date.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' gridmet <- process_gridmet(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/pr"
#' )
#' }
#' @export
# nolint end
process_gridmet <- function(
  date = c("2023-09-01", "2023-09-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  variable_checked <- amadeus::process_variable_codes(
    variables = variable,
    source = "gridmet"
  )
  variable_checked_long <- amadeus::process_gridmet_codes(
    variable_checked,
    invert = TRUE
  )
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable_checked,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### years of interest
  yoi <- unique(
    substr(
      date_sequence,
      1,
      4
    )
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        yoi,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p], win = extent)
    message(paste0(
      "Cleaning daily ",
      variable_checked,
      " data for year ",
      gsub(
        ".nc",
        "",
        strsplit(
          data_paths[p],
          paste0(
            variable_checked,
            "_"
          )
        )[[1]][2]
      ),
      "...\n"
    ))
    terra::time(data_year) <- amadeus::process_parse_ncdf_day_codes(
      layer_names = names(data_year),
      source = "gridmet",
      origin = "1900-01-01"
    )
    names(data_year) <- paste0(
      variable_checked,
      "_",
      gsub(
        "-",
        "",
        terra::time(data_year)
      )
    )
    terra::varnames(data_year) <- variable_checked
    terra::longnames(data_year) <- variable_checked_long
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
      ) %in%
        date_sequence
    )
  )
  message(paste0(
    "Returning daily ",
    variable_checked_long,
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

# nolint start
#' Process TerraClimate data
#' @description
#' The \code{process_terraclimate()} function imports and cleans climate and water balance
#' data, returning a single `SpatRaster` object.
#' @param date character(1 or 2). Date (1) or start and end dates (2).
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param variable character(1). Variable name or acronym code. See [TerraClimate Direct Downloads](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php)
#' for variable names and acronym codes.
#' @param path character(1). Directory with downloaded netCDF (.nc) files.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable acronym, year,
#' and month.
#' @note
#' TerraClimate data has monthly temporal resolution, so the first day of each month
#' is used as a placeholder temporal value.
#' @author Mitchell Manware
#' @return a `SpatRaster` object
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra subset
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' terraclimate <- process_terraclimate(
#'   date = c("2023-01-01", "2023-01-10"),
#'   variable = "Precipitation",
#'   path = "./data/ppt"
#' )
#' }
#' @export
# nolint end
process_terraclimate <- function(
  date = c("2023-09-01", "2023-09-10"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  ...
) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  variable_checked <- amadeus::process_variable_codes(
    variables = variable,
    source = "terraclimate"
  )
  variable_checked_long <- amadeus::process_terraclimate_codes(
    variable_checked,
    invert = TRUE
  )
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable_checked,
    full.names = TRUE
  )
  data_paths <- data_paths[grep(
    ".nc",
    data_paths
  )]
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### years of interest
  yoi <- unique(
    substr(
      date_sequence,
      1,
      4
    )
  )
  #### year-months of interest
  ymoi <- unique(
    substr(
      date_sequence,
      1,
      6
    )
  )
  #### subset file paths to only dates of interest
  data_paths <- unique(
    grep(
      paste(
        yoi,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )
  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths)) {
    #### import data
    data_year <- terra::rast(data_paths[p], win = extent)
    message(paste0(
      "Cleaning monthly ",
      variable_checked_long,
      " data for ",
      substr(
        terra::time(data_year)[1],
        1,
        4
      ),
      "...\n"
    ))
    names(data_year) <- paste0(
      variable_checked,
      "_",
      substr(
        gsub(
          "-",
          "",
          terra::time(data_year)
        ),
        1,
        6
      )
    )
    terra::varnames(data_year) <- variable_checked
    terra::longnames(data_year) <- variable_checked_long
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
        nchar(names(data_full)) - 5,
        nchar(names(data_full))
      ) %in%
        ymoi
    )
  )
  message(paste0(
    "Returning monthly ",
    variable_checked_long,
    " data from ",
    month.name[
      as.numeric(
        substr(
          ymoi[1],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[1],
      1,
      4
    ),
    " to ",
    month.name[
      as.numeric(
        substr(
          ymoi[length(ymoi)],
          5,
          6
        )
      )
    ],
    ", ",
    substr(
      ymoi[length(ymoi)],
      1,
      4
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}

#' Process PRISM data
#' @description
#' This function imports and cleans raw PRISM data,
#' returning a single `SpatRaster` object.
#' @param path character giving PRISM data path
#' Both file and directory path are acceptable.
#' @param element character(1). PRISM element name
#' @param time character(1). PRISM time name.
#' Should be character in length of 2, 4, 6, or 8.
#' "annual" is acceptable.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads time series or 30-year normal PRISM data.
#' @return a `SpatRaster` object with metadata of time and element.
#' @seealso [`terra::rast`], [`terra::metags`]
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' prism <- process_prism(
#'   path = "./data/PRISM_ppt_stable_4kmM3_202104_nc.nc",
#'   element = "ppt",
#'   time = "202104"
#' )
#' }
#' @export
# nolint start
process_prism <-
  function(
    path = NULL,
    element = NULL,
    time = NULL,
    extent = NULL,
    ...
  ) {
    prism_layer_name_from_file <- function(prism_file, fallback_element) {
      stem <- tools::file_path_sans_ext(basename(prism_file[1]))
      stem <- gsub("^(?i)prism_", "", stem, perl = TRUE)
      tokens <- strsplit(stem, "_", fixed = TRUE)[[1]]
      tokens <- tokens[nzchar(tokens)]

      if (length(tokens) == 0L) {
        return(tolower(fallback_element))
      }

      gsub("[^a-z0-9_]", "", tolower(tokens[1]))
    }

    # check inputs
    if (
      !element %in%
        c(
          "ppt",
          "tmin",
          "tmax",
          "tmean",
          "tdmean",
          "vpdmin",
          "vpdmax",
          "solslope",
          "soltotal",
          "solclear",
          "soltrans"
        )
    ) {
      stop("element is not a valid PRISM element.")
    }
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!nchar(time) %in% seq(2, 8, 2)) {
      stop("time does not have valid length.")
    }

    if (dir.exists(path)) {
      pattern <- "PRISM_%s*.*_%s_*.*(bil|nc|grib2|asc)$"
      pattern <- sprintf(pattern, element, time)
      prism_file <-
        list.files(
          path,
          pattern = pattern,
          full.names = TRUE
        )
    } else {
      prism_file <- path
    }
    prism <- terra::rast(prism_file, win = extent)
    if (terra::nlyr(prism) == 1L) {
      names(prism) <- prism_layer_name_from_file(prism_file, element)
    }
    terra::metags(prism) <- cbind(
      c("time", "element"),
      c(time, element)
    )
    return(prism)
  }
# nolint end

#' Process CropScape data
#' @description
#' This function imports and cleans raw CropScape data,
#' returning a single `SpatRaster` object.
#' @param path character giving CropScape data path
#' @param year numeric giving the year of CropScape data used
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Placeholders.
#' @description Reads CropScape file of selected `year`.
#' @return a `SpatRaster` object
#' @author Insang Song
#' @importFrom utils read.csv
#' @importFrom terra rast
#' @importFrom terra metags
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' cropscape <- process_cropscape(
#'   path = "./data/cropscape_example.tif",
#'   year = 2020
#' )
#' }
#' @export
process_cropscape <-
  function(
    path = NULL,
    year = 2021,
    extent = NULL,
    ...
  ) {
    # check inputs
    if (!is.character(path) || is.null(path)) {
      stop("path is not a character.")
    }
    if (!is.numeric(year)) {
      stop("year is not a numeric.")
    }
    # open cdl file corresponding to the year
    if (dir.exists(path)) {
      cdl_file <-
        list.files(
          path,
          pattern = paste0("cdl_30m_*.*", year, "_*.*.tif$"),
          full.names = TRUE
        )
    } else {
      cdl_file <- path
    }
    cdl <- terra::rast(cdl_file, win = extent)
    terra::metags(cdl) <- c(year = year)
    return(cdl)
  }


#' Retrieve Hydrologic Unit Code (HUC) data
#' @author Insang Song
#' @param path character. Path to the file or the directory containing HUC data.
#' @param layer_name character(1). Layer name in the `path`
#' @param huc_level character(1). Field name of HUC level
#' @param huc_header character(1). The upper level HUC code header to extract
#'  lower level HUCs.
#' @param extent numeric(4) or SpatExtent giving the extent of the raster
#'   if `NULL` (default), the entire raster is loaded
#' @param ... Arguments passed to `nhdplusTools::get_huc()`
#' @return a `SpatVector` object
#' @seealso [`nhdplusTools::get_huc`]
#' @importFrom terra vect
#' @importFrom terra vector_layers
#' @importFrom rlang inject
#' @examples
#' ## NOTE: Examples are wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' library(terra)
#' getf <- "WBD_National_GDB.gdb"
#' # check the layer name to read
#' terra::vector_layers(getf)
#' test1 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test2 <- process_huc(
#'   getf,
#'   layer_name = "WBDHU8",
#'   huc_level = "huc8"
#' )
#' test3 <- process_huc(
#'   "",
#'   layer_name = NULL,
#'   huc_level = NULL,
#'   huc_header = NULL,
#'   id = "030202",
#'   type = "huc06"
#' )
#' }
#' @export
process_huc <-
  function(
    path,
    layer_name = NULL,
    huc_level = NULL,
    huc_header = NULL,
    extent = NULL,
    ...
  ) {
    # exclude the coverage due to write permission related to memoization
    #nocov start
    if (missing(path) || (!file.exists(path) && !dir.exists(path))) {
      if (!requireNamespace("nhdplusTools", quietly = TRUE)) {
        stop(
          "Package 'nhdplusTools' is required when fetching HUC data ",
          "remotely. ",
          "Please install it and try again."
        )
      }
      hucpoly <- try(
        rlang::inject(nhdplusTools::get_huc(!!!list(...)))
      )
      if (inherits(hucpoly, "try-error")) {
        stop("HUC data was not found.")
      }
      hucpoly <- terra::vect(hucpoly)
    }
    #nocov end
    if (file.exists(path) || dir.exists(path)) {
      if (!is.null(huc_header)) {
        querybase <-
          sprintf(
            "SELECT * FROM %s WHERE %s LIKE '%s%%'",
            layer_name,
            huc_level,
            huc_header
          )
      } else {
        querybase <-
          sprintf("SELECT * FROM %s", layer_name)
      }
      if (!layer_name %in% terra::vector_layers(path)) {
        stop(
          paste0(
            "Layer ",
            layer_name,
            " not found in ",
            path
          )
        )
      }

      hucpoly <- try(
        terra::vect(
          path,
          query = querybase,
          extent = extent
        )
      )
    }
    return(hucpoly)
  }

################################################################################
# nolint start
#' Process NOAA GOES ADP data
#' @description
#' The \code{process_goes()} function imports and cleans NOAA GOES-16/18
#' Aerosol Detection Product (ADP) NetCDF files downloaded by
#' \code{download_goes()}, returning a single \code{SpatRaster} object with
#' CRS \code{EPSG:4326}.
#' @param date character(1 or 2). Date (YYYY-MM-DD) or start and end dates.
#' @param variable character(1). Variable name to extract: \code{"Smoke"}
#'   or \code{"Dust"}.
#' @param path character(1+). Directory with downloaded GOES ADP NetCDF files
#'   or a vector of full NetCDF file paths.
#' @param extent numeric(4) or SpatExtent. Crop extent
#'   (\code{xmin, xmax, ymin, ymax} in EPSG:4326). Default \code{NULL} loads
#'   the full raster.
#' @param daily_agg logical(1). If `TRUE`, aggregate sub-daily layers to daily
#'   values using `fun`. Default `FALSE` preserves original sub-daily layers.
#' @param fun character(1). Aggregation function passed to [terra::tapp()]
#'   (e.g. `"mean"` or `"sum"`). Ignored when `daily_agg = FALSE`.
#' @param ... Placeholders.
#' @note
#' \itemize{
#'   \item Layer names follow the convention
#'     \code{{variable}_{YYYYMMDD}_{HHMMSS}} when `daily_agg = FALSE`, e.g.
#'     \code{"Smoke_20240101_000000"}. With `daily_agg = TRUE`, layer names
#'     contain \code{{variable}_{YYYYMMDD}} and `terra::time()` is set to
#'     midnight UTC.
#'   \item \code{terra::time()} is set to POSIXct UTC for each layer.
#'   \item Files with GOES geostationary projection are reprojected to
#'     EPSG:4326.
#' }
#' @author Mitchell Manware
#' @return a \code{SpatRaster} object
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra crop
#' @importFrom terra subset
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra tapp
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
#' ##       data files.
#' \dontrun{
#' goes <- process_goes(
#'   date = c("2024-01-01", "2024-01-01"),
#'   variable = "Smoke",
#'   path = "./data/goes/"
#' )
#' goes_daily <- process_goes(
#'   date = c("2024-01-01", "2024-01-01"),
#'   variable = "Smoke",
#'   path = "./data/goes/",
#'   daily_agg = TRUE,
#'   fun = "mean"
#' )
#' }
#' @export
# nolint end
process_goes <- function(
  date = c("2024-01-01", "2024-01-01"),
  variable = NULL,
  path = NULL,
  extent = NULL,
  daily_agg = FALSE,
  fun = "mean",
  ...
) {
  #### resolve file paths from directory or explicit vector
  if (length(path) == 1 && dir.exists(path)) {
    path <- amadeus::download_sanitize_path(path)
    paths <- list.files(
      path,
      pattern = "^OR_(ADP|ABI-L2-ADP)",
      full.names = TRUE,
      recursive = TRUE
    )
  } else {
    paths <- path
  }
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### check dates
  if (length(date) == 1) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### identify file paths matching GOES ADP naming convention
  paths <- paths[grepl("^OR_(ADP|ABI-L2-ADP).*\\.nc$", basename(paths))]
  if (length(paths) == 0) {
    stop(
      paste0(
        "No GOES ADP NetCDF files found in `path`.\n",
        "When `path` is a directory, files must match ",
        "'^OR_(ADP|ABI-L2-ADP).*\\.nc$'.\n",
        "When `path` is a vector, provide full paths to matching GOES ADP ",
        "NetCDF files.\n"
      )
    )
  }
  if (!all(file.exists(paths))) {
    missing_paths <- paths[!file.exists(paths)]
    stop(
      paste0(
        "Some GOES paths do not exist:\n",
        paste0("  - ", missing_paths, collapse = "\n"),
        "\nFiles must match pattern '^OR_(ADP|ABI-L2-ADP).*\\.nc$'.\n"
      )
    )
  }
  #### parse start datetime from each filename and filter to date range
  date_from <- as.Date(date[1])
  date_to <- as.Date(date[2])
  file_dates <- vapply(
    paths,
    function(p) {
      tryCatch(
        {
          as.Date(goes_parse_start_datetime(p))
        },
        error = function(e) {
          as.Date(NA)
        }
      )
    },
    FUN.VALUE = as.Date(NA)
  )
  mask <- !is.na(file_dates) &
    file_dates >= date_from &
    file_dates <= date_to
  data_paths <- paths[mask]
  data_paths <- data_paths[order(file_dates[mask], data_paths)]
  if (length(data_paths) == 0) {
    stop(paste0(
      "No GOES ADP files matching the requested date range were found.\n",
      "Date range: ",
      date[1],
      " to ",
      date[2],
      "\n"
    ))
  }
  #### vectorized fast path for homogeneous GOES granules
  data_first <- terra::rast(data_paths[1])
  var_idx <- grep(
    paste0("^", variable, "$"),
    names(data_first)
  )
  if (length(var_idx) == 0) {
    var_idx <- grep(variable, names(data_first))
  }
  if (length(var_idx) == 0) {
    stop(paste0(
      "Requested variable '",
      variable,
      "' was not found in ",
      basename(data_paths[1]),
      ".\n"
    ))
  }
  dt_chr <- vapply(
    data_paths,
    function(p) format(goes_parse_start_datetime(p), "%Y-%m-%d %H:%M:%S"),
    character(1)
  )
  dt_vec <- as.POSIXct(dt_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  message(paste0(
    "Cleaning ",
    variable,
    " data for ",
    length(data_paths),
    " GOES files...\n"
  ))
  data_raw <- terra::rast(data_paths)
  n_files <- length(data_paths)
  n_total_layers <- terra::nlyr(data_raw)
  if (n_total_layers %% n_files != 0) {
    stop(
      paste0(
        "GOES files are not structurally homogeneous; ",
        "unable to map layers to files.\n"
      )
    )
  }
  layers_per_file <- n_total_layers / n_files
  if (max(var_idx) > layers_per_file) {
    stop(
      paste0(
        "GOES variable layer index exceeds per-file layer count; ",
        "files appear inconsistent.\n"
      )
    )
  }
  selected_idx <- unlist(lapply(
    seq_len(n_files),
    function(i) var_idx + (i - 1) * layers_per_file
  ))
  data_return <- terra::subset(data_raw, subset = selected_idx)
  #### reproject to EPSG:4326 if file uses geostationary projection
  crs_proj <- terra::crs(data_return, proj = TRUE)
  if (!is.na(crs_proj) && grepl("\\+proj=geos", crs_proj)) {
    # nocov start
    data_return <- terra::project(data_return, "EPSG:4326")
  } else if (is.na(terra::crs(data_return)) || terra::crs(data_return) == "") {
    terra::crs(data_return) <- "EPSG:4326"
  } # nocov end
  #### crop to extent (applied after reprojection)
  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }
  #### set layer name: {variable}_{YYYYMMDD}_{HHMMSS}
  dt_layer <- rep(dt_vec, each = length(var_idx))
  names(data_return) <- paste0(
    variable,
    "_",
    format(dt_layer, "%Y%m%d"),
    "_",
    format(dt_layer, "%H%M%S")
  )
  #### set time
  terra::time(data_return) <- dt_layer
  #### ensure EPSG:4326
  terra::crs(data_return) <- "EPSG:4326"
  #### optional daily aggregation
  if (isTRUE(daily_agg)) {
    t <- terra::time(data_return)
    if (!anyNA(t) && length(t) == terra::nlyr(data_return)) {
      date_str <- format(as.Date(t), "%Y%m%d")
    } else {
      # nocov start
      date_str <- regmatches(
        names(data_return),
        regexpr(
          "(?<![0-9])[0-9]{8}(?![0-9])",
          names(data_return),
          perl = TRUE
        )
      )
      if (length(date_str) != terra::nlyr(data_return)) {
        stop("daily_agg: cannot determine dates from layer times or names.\n")
      }
    } # nocov end
    var_prefix <- sub("_[0-9]{8}_[0-9]{6}$", "", names(data_return))
    tapp_index <- paste(var_prefix, date_str, sep = "_")
    saved_crs <- terra::crs(data_return)
    data_return <- terra::tapp(
      data_return,
      tapp_index,
      fun = fun,
      na.rm = TRUE
    )
    terra::crs(data_return) <- saved_crs
    out_dates <- regmatches(
      names(data_return),
      regexpr("[0-9]{8}$", names(data_return))
    )
    terra::time(data_return) <- as.POSIXct(
      paste0(out_dates, " 00:00:00"),
      format = "%Y%m%d %H:%M:%S",
      tz = "UTC"
    )
  }
  message(paste0(
    "Returning ",
    ifelse(isTRUE(daily_agg), "daily ", "sub-daily "),
    variable,
    " data from ",
    date[1],
    " to ",
    date[2],
    ".\n"
  ))
  return(data_return)
}

#' Parse GOES start datetime from filename
#' @description
#' Extracts the scan start datetime from a GOES-R series ADP filename.
#' The start timestamp field uses the format \code{sYYYYDDDHHMMSSf}
#' where \code{DDD} is the day of year (1--366).
#' @param path character(1). Full or base file path.
#' @return POSIXct scalar (UTC).
#' @keywords internal auxiliary
#' @export
goes_parse_start_datetime <- function(path) {
  fname <- basename(path)
  m <- regmatches(fname, regexpr("_s([0-9]{14})_", fname))
  if (length(m) == 0 || nchar(m) < 16) {
    stop(paste0(
      "Cannot parse GOES start datetime from filename: ",
      fname,
      "\n"
    ))
  }
  ts <- substr(m, 3, 16)
  year <- as.integer(substr(ts, 1, 4))
  doy <- as.integer(substr(ts, 5, 7))
  hour <- as.integer(substr(ts, 8, 9))
  min <- as.integer(substr(ts, 10, 11))
  sec <- as.integer(substr(ts, 12, 13))
  base_date <- as.Date(
    paste0(year, sprintf("%03d", doy)),
    format = "%Y%j"
  )
  ISOdatetime(
    year = year,
    month = as.integer(format(base_date, "%m")),
    day = as.integer(format(base_date, "%d")),
    hour = hour,
    min = min,
    sec = sec,
    tz = "UTC"
  )
}

################################################################################
# nolint start
#' Process IMPROVE aerosol monitoring data
#' @description
#' The \code{process_improve()} function reads pipe-delimited IMPROVE
#' (Interagency Monitoring of Protected Visual Environments) measurement
#' files downloaded by \code{download_improve()} and joins them with a site
#' metadata table to attach geographic coordinates and auxiliary site
#' attributes. Returns a
#' \code{SpatVector}, \code{sf}, or \code{data.table} object.
#' @details
#' Three product types are supported via \code{product}:
#' \describe{
#'   \item{\code{"raw"}}{IMPAER speciated aerosol mass concentrations.
#'     Key columns: \code{SiteCode}, \code{FactDate}, \code{ParamCode},
#'     \code{FactValue}, \code{Units}.}
#'   \item{\code{"rhr2"}}{IMPRHR2 Regional Haze Rule II light extinction
#'     (\code{bext}, \eqn{Mm^{-1}}).}
#'   \item{\code{"rhr3"}}{IMPRHR3 Regional Haze Rule III deciview index
#'     (\code{dv}).}
#' }
#' Measurement values are \strong{not} filtered by \code{Status}; callers
#' may apply their own validity flags (e.g., keep only \code{Status == "V0"}).
#' @param path character(1). Directory containing downloaded IMPROVE
#'   \code{.txt} files.
#' @param product character(1). Product type: \code{"raw"} (default),
#'   \code{"rhr2"}, or \code{"rhr3"}.
#' @param date character(1 or 2). Date (\code{"YYYY-MM-DD"}) or start/end
#'   date pair to filter measurements. Defaults to no filtering when
#'   \code{NULL}.
#' @param sites_file character(1) or \code{NULL}. Path to a site metadata
#'   file. When \code{NULL} (default), the function first looks for a file
#'   named \code{improve_sites.txt} inside \code{path}, then falls back to an
#'   embedded IMPROVE aerosol site table included in \code{amadeus}.
#' @param return_format character(1). Return object type: \code{"terra"},
#'   \code{"sf"}, or \code{"data.table"}.
#' @param extent numeric(4) or \code{NULL}. Optional crop extent
#'   \code{c(xmin, xmax, ymin, ymax)} in WGS84 / EPSG:4326. Applied only
#'   when \code{return_format} is \code{"terra"} or \code{"sf"}.
#' @param ... Placeholders.
#' @seealso \code{\link{download_improve}}
#' @return a \code{SpatVector}, \code{sf}, or \code{data.table} object
#'   depending on \code{return_format}.
#' @note IMPROVE data are measured on an every-third-day sampling schedule.
#'   Gaps between measurement dates are expected.
#' @importFrom data.table fread rbindlist as.data.table setnames
#' @importFrom terra vect crop ext project
#' @importFrom sf st_as_sf
#' @examples
#' improve <- process_improve(
#'   path = system.file("testdata/improve", package = "amadeus"),
#'   product = "raw",
#'   date = c("2022-01-01", "2022-01-31"),
#'   return_format = "data.table"
#' )
#' @export
# nolint end
process_improve <- function(
  path = NULL,
  product = c("raw", "rhr2", "rhr3"),
  date = NULL,
  sites_file = NULL,
  return_format = c("terra", "sf", "data.table"),
  extent = NULL,
  ...
) {
  product <- match.arg(product)
  return_format <- match.arg(return_format)

  #### Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("`path` must be a valid directory path.\n")
  }
  path <- amadeus::download_sanitize_path(path)

  #### Resolve file prefix from product
  prefix_map <- c(raw = "IMPAER", rhr2 = "IMPRHR2", rhr3 = "IMPRHR3")
  prefix <- prefix_map[[product]]

  #### Find measurement files
  meas_files <- list.files(
    path,
    pattern = paste0("^", prefix, "_[0-9]{4}\\.txt$"),
    full.names = TRUE
  )
  if (length(meas_files) == 0) {
    stop(sprintf(
      "No %s_YYYY.txt files found in '%s'.\n",
      prefix,
      path
    ))
  }

  #### Read and bind measurement files
  meas_list <- lapply(meas_files, function(f) {
    dt <- data.table::fread(
      f,
      sep = "|",
      header = TRUE,
      showProgress = FALSE,
      data.table = TRUE
    )
    dt
  })
  meas <- data.table::rbindlist(meas_list, fill = TRUE)

  #### Standardise date column
  data.table::set(
    meas,
    j = "FactDate",
    value = as.Date(meas[["FactDate"]])
  )

  #### Filter by date if provided
  if (!is.null(date)) {
    if (length(date) == 1) {
      date <- c(date, date)
    }
    stopifnot(length(date) == 2)
    d_start <- as.Date(date[1])
    d_end <- as.Date(date[2])
    meas <- meas[
      get("FactDate") >= d_start & get("FactDate") <= d_end,
    ]
    if (nrow(meas) == 0) {
      warning(
        "No IMPROVE measurements found for the specified date range.\n",
        call. = FALSE
      )
      return(meas)
    }
  }

  #### Resolve and read site metadata
  if (is.null(sites_file)) {
    candidate <- file.path(path, "improve_sites.txt")
    if (file.exists(candidate)) {
      sites_file <- candidate
    }
  }

  if (!is.null(sites_file) && file.exists(sites_file)) {
    sites <- data.table::fread(
      sites_file,
      sep = "|",
      header = TRUE,
      showProgress = FALSE,
      data.table = TRUE
    )
  } else {
    sites <- process_improve_sites_builtin() # nolint: object_usage_linter.
  }

  #### Deduplicate site table and merge all available metadata columns
  if ("SiteCode" %in% names(sites)) {
    sites <- data.table::as.data.table(sites)
    if (all(c("DataEndDate", "DataStartDate") %in% names(sites))) {
      data.table::set(
        sites,
        j = "DataEndDate_sort",
        value = as.Date(sites[["DataEndDate"]], format = "%m/%d/%y")
      )
      data.table::set(
        sites,
        j = "DataStartDate_sort",
        value = as.Date(sites[["DataStartDate"]], format = "%m/%d/%y")
      )
      ord <- order(
        sites[["SiteCode"]],
        sites[["DataEndDate_sort"]],
        sites[["DataStartDate_sort"]],
        decreasing = c(FALSE, TRUE, TRUE),
        method = "radix",
        na.last = TRUE
      )
      sites <- sites[ord]
      sites <- sites[!duplicated(sites[["SiteCode"]])]
      data.table::set(sites, j = "DataEndDate_sort", value = NULL)
      data.table::set(sites, j = "DataStartDate_sort", value = NULL)
    } else {
      sites <- sites[!duplicated(sites[["SiteCode"]])]
    }

    coord_cols <- c("SiteCode", "Latitude", "Longitude")
    coord_cols_present <- coord_cols[coord_cols %in% names(sites)]
    if (length(coord_cols_present) < 3) {
      warning(
        "Sites file is missing Latitude and/or Longitude columns.\n",
        call. = FALSE
      )
    } else {
      meas <- merge(meas, sites, by = "SiteCode", all.x = TRUE)
    }
  }

  #### Enforce numeric coordinate columns when present
  if ("Latitude" %in% names(meas)) {
    data.table::set(
      meas,
      j = "Latitude",
      value = suppressWarnings(as.numeric(meas[["Latitude"]]))
    )
  }
  if ("Longitude" %in% names(meas)) {
    data.table::set(
      meas,
      j = "Longitude",
      value = suppressWarnings(as.numeric(meas[["Longitude"]]))
    )
  }

  #### Return early as data.table if requested or no coordinates
  has_coords <- all(c("Latitude", "Longitude") %in% names(meas))
  if (return_format == "data.table" || !has_coords) {
    if (!has_coords && return_format != "data.table") {
      warning(
        "No site coordinates available; returning data.table.\n",
        call. = FALSE
      )
    }
    return(meas)
  }

  #### Build spatial object
  # nolint start: object_usage_linter.
  meas_complete <- meas[
    !is.na(get("Latitude")) & !is.na(get("Longitude")),
  ]
  # nolint end
  sv <- terra::vect(
    meas_complete,
    geom = c("Longitude", "Latitude"),
    crs = "EPSG:4326"
  )

  #### Apply optional extent crop
  if (!is.null(extent)) {
    sv <- terra::crop(sv, terra::ext(extent))
  }

  if (return_format == "terra") {
    return(sv)
  } else {
    return(sf::st_as_sf(sv))
  }
}

# nolint start
#' Process drought index data
#' @description
#' The \code{process_drought()} function imports and cleans raw drought index
#' files returned by \code{download_drought()}, producing a harmonized output
#' object ready for \code{calculate_drought()}:
#' \itemize{
#'   \item \strong{SPEI / EDDI} — returns a \code{SpatRaster} with one layer
#'     per time step, layer names in \code{"<source>_<timescale>_YYYY-MM-DD"}
#'     format, CRS set to \code{EPSG:4326}.
#'   \item \strong{USDM} — returns a \code{SpatVector} (polygon) with columns
#'     \code{DM} (drought-monitor class, integer 0–4), \code{date}
#'     (\code{Date}), and \code{source} (\code{"usdm"}), CRS
#'     \code{EPSG:4326}.
#' }
# nolint end
#' @param source character(1). Drought data source. One of \code{"spei"},
#'   \code{"eddi"}, or \code{"usdm"}. When called through
#'   \code{process_covariates(covariate = "spei")} the alias is forwarded
#'   automatically.
#' @param path character(1). Directory containing downloaded drought files
#'   (output of \code{download_drought()}).
#' @param date character(1 or 2). Single date or start/end dates.
#'   Format \code{"YYYY-MM-DD"}.
#' @param timescale integer(1). Accumulation timescale in months (SPEI/EDDI
#'   only; ignored for USDM). Must match the timescale used in
#'   \code{download_drought()}. Default \code{1L}.
#' @param extent numeric(4) or \code{SpatExtent}. Optional spatial crop
#'   applied before returning. \code{NULL} (default) returns full extent.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item SPEI/EDDI files are expected to follow the naming convention
#'     produced by \code{download_drought()}: \code{spei<timescale>.nc} and
#'     either legacy \code{eddi<timescale>mn<year>.nc} or current
#'     \code{EDDI_ETrs_<timescale>mn_<YYYYMMDD>.asc}.
#'   \item USDM files are expected to be weekly shapefiles named
#'     \code{USDM_<YYYYMMDD>.shp}.
#'   \item Layer/column naming is standardised so that
#'     \code{calculate_drought()} can operate identically regardless of source.
#' }
#' @author Insang Song
#' @return
#' \itemize{
#'   \item \code{SpatRaster} for SPEI or EDDI sources.
#'   \item \code{SpatVector} (polygons) for USDM source.
#' }
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom terra time
#' @importFrom terra crs
#' @importFrom terra crop
#' @importFrom terra subset
#' @importFrom terra project
#' @importFrom terra nlyr
#' @importFrom terra varnames
#' @seealso \code{\link{download_drought}}, \code{\link{calculate_drought}}
#' @examples
# nolint start
#' \dontrun{
#' ## SPEI
#' spei <- process_drought(
#'   source = "spei",
#'   path = "./data/drought",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L
#' )
#' ## USDM
#' usdm <- process_drought(
#'   source = "usdm",
#'   path = "./data/drought",
#'   date = c("2020-01-07", "2020-03-31")
#' )
#' }
#' @export
# nolint end
process_drought <- function(
  source = c("spei", "eddi", "usdm"),
  path = NULL,
  date = c("2020-01-01", "2020-12-31"),
  timescale = 1L,
  extent = NULL,
  ...
) {
  #### Validate source
  source <- match.arg(source)

  #### Sanitize path
  path <- amadeus::download_sanitize_path(path)

  #### Validate dates
  if (length(date) == 1L) {
    date <- c(date, date)
  }
  stopifnot(length(date) == 2L)
  date <- date[order(as.Date(date))]

  #### Check required parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### Dispatch to source-specific pathway
  if (source %in% c("spei", "eddi")) {
    drought_process_nc(
      source = source,
      path = path,
      date = date,
      timescale = timescale,
      extent = extent
    )
  } else {
    drought_process_usdm(
      path = path,
      date = date,
      extent = extent
    )
  }
}


#### Internal helper: SPEI / EDDI netCDF pathway
drought_process_nc <- function(source, path, date, timescale, extent) {
  ts_fmt <- sprintf("%02d", as.integer(timescale))
  date_range <- as.Date(date)

  if (source == "spei") {
    #### Single multi-year file: spei<TS>.nc
    nc_pattern <- paste0("^spei", ts_fmt, "\\.nc$")
    nc_files <- list.files(path, pattern = nc_pattern, full.names = TRUE)
    if (length(nc_files) == 0L) {
      stop(sprintf(
        "No SPEI file matching '%s' found in: %s",
        nc_pattern,
        path
      ))
    }
    data_full <- terra::rast(nc_files[1], win = extent)
    data_full <- drought_set_time_nc(data_full, source, ts_fmt, nc_files[1])
  } else {
    #### EDDI: legacy yearly netCDF (eddi01mn2020.nc) or daily/tuesday ASCII
    nc_pattern <- paste0("eddi", ts_fmt, "mn[0-9]{4}\\.nc$")
    asc_pattern <- paste0("^EDDI_ETrs_", ts_fmt, "mn_[0-9]{8}\\.asc$")
    nc_files <- list.files(path, pattern = nc_pattern, full.names = TRUE)
    asc_files <- list.files(path, pattern = asc_pattern, full.names = TRUE)

    if (length(nc_files) == 0L && length(asc_files) == 0L) {
      stop(sprintf(
        "No EDDI files matching '%s' or '%s' found in: %s",
        nc_pattern,
        asc_pattern,
        path
      ))
    }

    if (length(nc_files) > 0L) {
      data_full <- terra::rast()
      for (f in nc_files) {
        yr_rast <- terra::rast(f, win = extent)
        yr_rast <- drought_set_time_nc(yr_rast, source, ts_fmt, f)
        data_full <- c(data_full, yr_rast, warn = FALSE)
      }
    } else {
      asc_dates <- as.Date(
        sub(".*_([0-9]{8})\\.asc$", "\\1", basename(asc_files)),
        format = "%Y%m%d"
      )
      asc_files <- asc_files[order(asc_dates)]
      asc_dates <- asc_dates[order(asc_dates)]

      data_full <- terra::rast()
      for (i in seq_along(asc_files)) {
        asc_rast <- terra::rast(asc_files[i], win = extent)
        terra::time(asc_rast) <- asc_dates[i]
        names(asc_rast) <- paste0(
          source,
          "_",
          ts_fmt,
          "_",
          format(asc_dates[i], "%Y-%m-%d")
        )
        terra::varnames(asc_rast) <- source
        data_full <- c(data_full, asc_rast, warn = FALSE)
      }
    }
  }

  #### Filter layers to requested date range
  all_times <- as.Date(terra::time(data_full))
  keep_idx <- which(all_times >= date_range[1] & all_times <= date_range[2])
  if (length(keep_idx) == 0L) {
    stop(sprintf(
      "No %s data found in date range %s to %s.",
      toupper(source),
      date[1],
      date[2]
    ))
  }
  data_return <- terra::subset(data_full, keep_idx)

  #### Ensure EPSG:4326
  if (is.na(terra::crs(data_return)) || terra::crs(data_return) == "") {
    terra::crs(data_return) <- "EPSG:4326"
  }

  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }

  message(sprintf(
    "Returning %s (timescale = %d month%s) data from %s to %s.\n",
    toupper(source),
    as.integer(timescale),
    if (as.integer(timescale) == 1L) "" else "s",
    date[1],
    date[2]
  ))
  data_return
}


#### Internal helper: assign time metadata and layer names for netCDF rasters
drought_set_time_nc <- function(r, source, ts_fmt, filepath) {
  times <- terra::time(r)
  #### If terra could not read CF time, derive from filename (EDDI only)
  if (is.null(times) || all(is.na(times))) {
    yr_str <- regmatches(
      basename(filepath),
      regexpr("[0-9]{4}", basename(filepath))
    )
    if (length(yr_str) == 0L || is.na(yr_str)) {
      stop(sprintf(
        "Cannot determine time coordinates from file: %s",
        basename(filepath)
      ))
    }
    times <- seq.Date(
      as.Date(paste0(yr_str, "-01-01")),
      by = "month",
      length.out = terra::nlyr(r)
    )
    terra::time(r) <- times
  }
  names(r) <- paste0(
    source,
    "_",
    ts_fmt,
    "_",
    format(as.Date(times), "%Y-%m-%d")
  )
  terra::varnames(r) <- source
  r
}


#### Internal helper: USDM weekly polygon pathway
drought_process_usdm <- function(path, date, extent) {
  search_paths <- unique(c(path, file.path(path, "data_files")))
  shp_files <- unlist(
    lapply(
      search_paths,
      function(dir_path) {
        if (!dir.exists(dir_path)) {
          return(character(0))
        }
        list.files(
          dir_path,
          pattern = "^USDM_[0-9]{8}\\.shp$",
          full.names = TRUE
        )
      }
    ),
    use.names = FALSE
  )

  if (length(shp_files) == 0L) {
    stop(sprintf(
      "No USDM shapefiles matching 'USDM_YYYYMMDD.shp' found in: %s",
      path
    ))
  }

  #### Extract dates from filenames
  file_dates <- as.Date(
    regmatches(
      basename(shp_files),
      regexpr("[0-9]{8}", basename(shp_files))
    ),
    format = "%Y%m%d"
  )

  #### Subset to requested date range
  date_range <- as.Date(date)
  keep_idx <- which(file_dates >= date_range[1] & file_dates <= date_range[2])
  if (length(keep_idx) == 0L) {
    stop(sprintf(
      "No USDM files found in date range %s to %s.",
      date[1],
      date[2]
    ))
  }
  shp_files <- shp_files[keep_idx]
  file_dates <- file_dates[keep_idx]

  #### Read, standardise, and bind
  vlist <- vector("list", length(shp_files))
  for (i in seq_along(shp_files)) {
    v <- terra::vect(shp_files[i])
    #### Reproject to EPSG:4326
    if (!is.na(terra::crs(v)) && terra::crs(v) != "") {
      v <- terra::project(v, "EPSG:4326")
    } else {
      terra::crs(v) <- "EPSG:4326"
    }
    #### Retain only DM column plus metadata
    v <- v[, "DM"]
    v$date <- as.character(file_dates[i])
    v$source <- "usdm"
    vlist[[i]] <- v
  }
  data_return <- do.call(rbind, vlist)

  if (!is.null(extent)) {
    data_return <- terra::crop(data_return, extent)
  }

  message(sprintf(
    "Returning USDM polygon data from %s to %s (%d file%s).\n",
    date[1],
    date[2],
    length(shp_files),
    if (length(shp_files) == 1L) "" else "s"
  ))
  data_return
}
