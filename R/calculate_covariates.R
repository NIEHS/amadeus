# nolint start
#' Calculate covariates wrapper function
#' @description
#' The \code{calculate_covariates()} function extracts values at point
#' locations from a SpatRaster or SpatVector object returned from
#' \code{process_covariates()}. \code{calculate_covariates()} and the underlying
#' source-specific covariate functions have been designed to operate on the
#' processed objects. To avoid errors, \strong{do not edit the processed
#' SpatRaster or SpatVector objects before passing to
#' \code{calculate_covariates()}}.
#' @param covariate character(1). Covariate type.
#' @param from character, SpatRaster, SpatVector, or data.frame depending on
#'   the selected `covariate` route.
#' @param locs sf/SpatVector. Unique locations. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#'  Default is `"site_id"`.
#' @param .by_time NULL or character(1). Name of the time column to use
#'   temporal summarization unit token. \code{NULL} (default) disables
#'   temporal summarization.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Passed through to the underlying source-specific function for
#'   weighted extraction. If `NULL` (default), unweighted extraction is
#'   performed.
#' @param ... Arguments passed to each covariate calculation
#'  function.
#' @note `covariate` argument value is converted to lowercase.
#' @seealso
#' * \code{\link{calculate_modis}}: "modis", "MODIS"
#' * \code{\link{calculate_koppen_geiger}}: "koppen-geiger", "koeppen-geiger", "koppen"
#' * \code{\link{calculate_ecoregion}}: "ecoregion", "ecoregions"
#' * \code{\link{calculate_temporal_dummies}}: "dummies", "Dummies"
#' * \code{\link{calculate_hms}}: "hms", "smoke", "HMS"
#' * \code{\link{calculate_gmted}}: "gmted", "GMTED"
#' * \code{\link{calculate_narr}}: "narr", "NARR"
#' * \code{\link{calculate_geos}}: "geos", "geos_cf", "GEOS"
#' * \code{\link{calculate_goes}}: "goes", "goes_adp", "GOES"
#' * \code{\link{calculate_population}}: "population", "sedac_population"
#' * \code{\link{calculate_groads}}: "roads", "groads", "sedac_groads"
#' * \code{\link{calculate_nlcd}}: "nlcd", "NLCD"
#' * \code{\link{calculate_tri}}: "tri", "TRI"
#' * \code{\link{calculate_nei}}: "nei", "NEI"
#' * \code{\link{calculate_merra2}}: "merra", "MERRA", "merra2", "MERRA2"
#' * \code{\link{calculate_gridmet}}: "gridMET", "gridmet"
#' * \code{\link{calculate_terraclimate}}: "terraclimate", "TerraClimate"
#' * \code{\link{calculate_prism}}: "prism", "PRISM"
#' * \code{\link{calculate_cropscape}}: "cropscape", "cdl"
#' * \code{\link{calculate_huc}}: "huc", "HUC"
#' * \code{\link{calculate_edgar}}: "edgar"
#' * \code{\link{calculate_drought}}: "drought", "spei", "eddi", "usdm"
#' @return Calculated covariates as a data.frame or SpatVector object
#' @author Insang Song
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_covariates(
#'   covariate = "narr",
#'   from = narr, # derived from process_covariates() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# nolint end
calculate_covariates <-
  function(
    covariate = c(
      "modis",
      "koppen-geiger",
      "koeppen-geiger",
      "koppen",
      "koeppen",
      "geos",
      "dummies",
      "gmted",
      "sedac_groads",
      "groads",
      "roads",
      "ecoregions",
      "ecoregion",
      "hms",
      "smoke",
      "gmted",
      "narr",
      "geos",
      "sedac_population",
      "population",
      "nlcd",
      "merra",
      "merra2",
      "gridmet",
      "terraclimate",
      "tri",
      "nei",
      "mcd14ml",
      "prism",
      "cropscape",
      "cdl",
      "huc",
      "edgar",
      "goes",
      "goes_adp",
      "GOES",
      "drought",
      "spei",
      "eddi",
      "usdm"
    ),
    from,
    locs,
    locs_id = "site_id",
    .by_time = NULL,
    weights = NULL,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_by_time(.by_time)
    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(
      covariate,
      modis = amadeus::calculate_modis,
      ecoregion = amadeus::calculate_ecoregion,
      ecoregions = amadeus::calculate_ecoregion,
      koppen = amadeus::calculate_koppen_geiger,
      narr = amadeus::calculate_narr,
      nlcd = amadeus::calculate_nlcd,
      smoke = amadeus::calculate_hms,
      hms = amadeus::calculate_hms,
      sedac_groads = amadeus::calculate_groads,
      roads = amadeus::calculate_groads,
      groads = amadeus::calculate_groads,
      sedac_population = amadeus::calculate_population,
      population = amadeus::calculate_population,
      nei = amadeus::calculate_nei,
      mcd14ml = amadeus::calculate_modis,
      tri = amadeus::calculate_tri,
      geos = amadeus::calculate_geos,
      gmted = amadeus::calculate_gmted,
      dummies = amadeus::calculate_temporal_dummies,
      merra = amadeus::calculate_merra2,
      merra2 = amadeus::calculate_merra2,
      gridmet = amadeus::calculate_gridmet,
      terraclimate = amadeus::calculate_terraclimate,
      prism = amadeus::calculate_prism,
      cropscape = amadeus::calculate_cropscape,
      cdl = amadeus::calculate_cropscape,
      huc = amadeus::calculate_huc,
      edgar = amadeus::calculate_edgar,
      goes = amadeus::calculate_goes,
      goes_adp = amadeus::calculate_goes,
      drought = amadeus::calculate_drought,
      spei = amadeus::calculate_drought,
      eddi = amadeus::calculate_drought,
      usdm = amadeus::calculate_drought
    )

    res_covariate <-
      tryCatch(
        {
          calc_args <- c(
            list(from = from, locs = locs, locs_id = locs_id),
            list(...)
          )
          if (!is.null(weights)) {
            calc_args$weights <- weights
          }
          if (!is.null(.by_time)) {
            calc_args$.by_time <- .by_time
          }
          do.call(what_to_run, calc_args)
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


#' Calculate climate classification covariates
#' @description
#' Extract Koppen-Geiger climate classes at point or buffered locations. Returns
#' a \code{data.frame} with \code{locs_id}, a \code{description} column, and
#' either binary indicators (\code{frac = FALSE}) or fractional overlap values
#' (\code{frac = TRUE}) for climate groups A-E.
#' @param from SpatRaster(1). Output of \code{process_koppen_geiger()}.
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, return binary 0/1
#'   indicators by climate group. If `TRUE`, return fractional overlap in the
#'   extraction footprint.
#' @param radius numeric(1). Circular buffer size (meters) around point
#'   locations. Use `0` (default) for exact point extraction.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [`process_koppen_geiger`]
#' @return a data.frame or SpatVector object with climate columns named like
#'   `DUM_CLRGA_00000` (`frac = FALSE`) or `FRC_CLRGA_100000` (`frac = TRUE`)
#'   where the suffix reflects the extraction radius.
#' @note The returned object contains a
#' `$description` column to represent the temporal range covered by the
#' dataset. For more information, see
#' <https://www.nature.com/articles/sdata2018214>.
#' @author Insang Song
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra coltab
#' @importFrom terra merge
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_koppen_geiger(
#'   from = kg, # derived from process_koppen_geiger() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
# locs (locs), from (from), locs_id (id_col), variables
calculate_koppen_geiger <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = "site_id",
    weights = NULL,
    geom = FALSE,
    frac = FALSE,
    radius = 0,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop("`frac` should be a single logical value (TRUE/FALSE).")
    }
    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop("`radius` should be a single numeric value.")
    }
    if (radius < 0) {
      stop("`radius` should be greater than or equal to 0.")
    }
    radius_value <- as.integer(round(radius))
    width_radius <- max(5L, nchar(as.character(abs(radius_value))))
    radius_suffix <- sprintf(paste0("%0", width_radius, "d"), radius_value)
    value_prefix <- if (isTRUE(frac)) "FRC" else "DUM"

    # prepare locations
    locs_prepared <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    locs_kg <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]
    is_point_locs <- all(
      tolower(terra::geomtype(locs_kg)) %in% c("points", "point")
    )
    if (is_point_locs && radius_value == 0L) {
      locs_kg_extract <- terra::extract(from, locs_kg)
      locs_kg_extract[[locs_id]] <- locs_df[[locs_id]][locs_kg_extract$ID]
      locs_kg_extract$base_value <- 1
    } else {
      locs_kg_extract <- terra::extract(
        from,
        locs_kg,
        cells = TRUE,
        weights = TRUE
      )
      locs_kg_extract[[locs_id]] <- locs_df[[locs_id]][locs_kg_extract$ID]
      coverage_col <- c("weight", "weights", "fraction")
      coverage_col <- coverage_col[coverage_col %in% names(locs_kg_extract)][1]
      if (is.na(coverage_col) || length(coverage_col) == 0) {
        locs_kg_extract$base_value <- 1
      } else {
        locs_kg_extract$base_value <- as.numeric(
          locs_kg_extract[[coverage_col]]
        )
        locs_kg_extract$base_value[!is.finite(locs_kg_extract$base_value)] <- 0
      }
    }
    value_col <- setdiff(
      names(locs_kg_extract),
      c("ID", locs_id, "cell", "weight", "weights", "fraction", "base_value")
    )
    value_col <- value_col[1]
    locs_kg_extract <- locs_kg_extract[
      !is.na(locs_kg_extract[[value_col]]),
      c(locs_id, value_col, "base_value"),
      drop = FALSE
    ]
    names(locs_kg_extract)[2] <- "value"

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA,
        "Af",
        "Am",
        "Aw",
        "BWh",
        "BWk",
        "BSh",
        "BSk",
        "Csa",
        "Csb",
        "Csc",
        "Cwa",
        "Cwb",
        "Cwc",
        "Cfa",
        "Cfb",
        "Cfc",
        "Dsa",
        "Dsb",
        "Dsc",
        "Dsd",
        "Dwa",
        "Dwb",
        "Dwc",
        "Dwd",
        "Dfa",
        "Dfb",
        "Dfc",
        "Dfd",
        "ET",
        "EF"
      )
    kg_coltab <- terra::coltab(from)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )
    locs_kg_extract_e <- merge(
      locs_kg_extract,
      kg_colclass,
      by = "value",
      all.x = TRUE,
      sort = FALSE
    )

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(locs_kg_extract_e[[locs_id]])
    # errorfix: how to generalize and auto-fix it?
    locs_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    locs_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    locs_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"
    locs_kg_extract_e$class_kg <- substr(locs_kg_extract_e$class_kg, 1, 1)
    aelabels <- LETTERS[1:5]
    class_values <- data.frame(
      site_id = as.character(locs_kg_extract_e[[locs_id]]),
      class_kg = as.character(locs_kg_extract_e$class_kg),
      base_value = as.numeric(locs_kg_extract_e$base_value),
      stringsAsFactors = FALSE
    )
    class_values <- class_values[
      class_values$class_kg %in% aelabels,
      ,
      drop = FALSE
    ]
    if (nrow(class_values) > 0) {
      class_values <- stats::aggregate(
        base_value ~ site_id + class_kg,
        data = class_values,
        FUN = sum
      )
      if (!isTRUE(frac)) {
        class_values$base_value <- as.integer(class_values$base_value > 0)
      } else {
        totals <- stats::aggregate(
          base_value ~ site_id,
          data = class_values,
          FUN = sum
        )
        denom <- totals$base_value[match(class_values$site_id, totals$site_id)]
        denom[!is.finite(denom) | denom <= 0] <- NA_real_
        class_values$base_value <- class_values$base_value / denom
        class_values$base_value[!is.finite(class_values$base_value)] <- 0
        class_values$base_value <- pmin(class_values$base_value, 1)
      }
      class_matrix <- stats::xtabs(
        base_value ~ site_id + class_kg,
        data = class_values
      )
      df_ae_separated <- as.data.frame.matrix(class_matrix)
      df_ae_separated$site_id <- rownames(df_ae_separated)
      rownames(df_ae_separated) <- NULL
    } else {
      df_ae_separated <- data.frame(
        site_id = character(0),
        stringsAsFactors = FALSE
      )
    }
    for (class_name in aelabels) {
      if (!class_name %in% names(df_ae_separated)) {
        df_ae_separated[[class_name]] <- if (isTRUE(frac)) 0 else 0L
      }
    }
    df_ae_separated <- df_ae_separated[, c("site_id", aelabels), drop = FALSE]
    column_names <- sprintf(
      "%s_CLRG%s_%s",
      value_prefix,
      aelabels,
      radius_suffix
    )
    names(df_ae_separated)[
      match(aelabels, names(df_ae_separated))
    ] <- column_names
    kg_extracted <- merge(
      locs_df,
      df_ae_separated,
      by.x = locs_id,
      by.y = "site_id",
      all.x = TRUE,
      sort = FALSE
    )
    for (kg_col in column_names) {
      if (!kg_col %in% names(kg_extracted)) {
        kg_extracted[[kg_col]] <- if (isTRUE(frac)) 0 else 0L
      }
      kg_extracted[[kg_col]][is.na(kg_extracted[[kg_col]])] <-
        if (isTRUE(frac)) 0 else 0L
      if (!isTRUE(frac)) {
        kg_extracted[[kg_col]] <- as.integer(kg_extracted[[kg_col]])
      } else {
        kg_extracted[[kg_col]] <- as.numeric(kg_extracted[[kg_col]])
      }
    }
    desc_vals <- terra::metags(from)$value
    description <- if (length(desc_vals) >= 2) {
      as.character(desc_vals[2])
    } else {
      NA_character_
    }
    kg_extracted$description <- description
    if (geom %in% c("sf", "terra")) {
      kg_extracted <- kg_extracted[
        ,
        c(locs_id, "geometry", "description", column_names),
        drop = FALSE
      ]
      sites_return <- amadeus::calc_return_locs(
        covar = kg_extracted,
        POSIXt = FALSE,
        geom = geom,
        crs = terra::crs(from)
      )
      #### return data.frame
      return(sites_return)
    } else {
      kg_extracted <- kg_extracted[
        ,
        c(locs_id, "description", column_names),
        drop = FALSE
      ]
      return(kg_extracted)
    }
  }


#' Calculate land cover covariates
#' @description
#' Compute ratio of land cover class in circle buffers around points. Returns
#' a \code{data.frame} object containing \code{locs_id}, longitude, latitude,
#' time (year), and computed ratio for each land cover class.
#' @param from SpatRaster(1). Output of \code{process_nlcd()}.
#' @param locs terra::SpatVector of points geometry
#' @param locs_id character(1). Unique identifier of locations
#' @param mode character(1). One of `"exact"`
#'   (using [`exactextractr::exact_extract()`])
#'   or `"terra"` (using [`terra::freq()`]). Ignored if `locs` are points.
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points.
#' @param drop logical(1). Default `FALSE`. For buffered outputs (`radius > 0`),
#'   retain NLCD class columns even when all values are 0 (`drop = FALSE`) or
#'   remove class columns that are all 0 across all locations (`drop = TRUE`).
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values may expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`. Only valid when
#' `mode = "exact"`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @note NLCD is available in U.S. only. Users should be aware of
#' the spatial extent of the data. The results are different depending
#' on `mode` argument. The `"terra"` mode is less memory intensive
#' but less accurate because it counts the number of cells
#' intersecting with the buffer. The `"exact"` may be more accurate
#' but uses more memory as it will account for the partial overlap
#' with the buffer.
#' @seealso [`process_nlcd`]
#' @return a data.frame or SpatVector object
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom terra rast project vect crs set.crs buffer
#' @importFrom sf st_union st_geometry
#' @importFrom terra intersect metags
#' @importFrom exactextractr exact_extract
#' @importFrom collapse rowbind
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_nlcd(
#'   from = nlcd, # derived from process_nlcd() example
#'   locs = loc,
#'   locs_id = "id",
#'   mode = "exact",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_nlcd <- function(
  from,
  locs,
  locs_id = "site_id",
  mode = c("exact", "terra"),
  radius = 1000,
  drop = FALSE,
  weights = NULL,
  max_cells = 5e7,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  # check inputs
  mode <- match.arg(mode)
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius < 0) {
    stop("radius has not a likely value.")
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop("`drop` should be a single logical value (TRUE/FALSE).")
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }

  # currently only handles 1 year
  if (terra::nlyr(from) > 1) {
    stop(
      paste0(
        "`from` contains more than one data layer. Current version ",
        "only processes one year worth of NLCD data."
      )
    )
  }

  # prepare locations
  locs_prepared <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  locs_vector <- locs_prepared[[1]]
  locs_df <- locs_prepared[[2]]

  # detect new or deprecated file path stucture
  if (identical(names(from), "NLCD Land Cover Class")) {
    message(
      paste0(
        "Deprecated data format detected. Data still analyzed, but ",
        "see https://www.mrlc.gov/data/project/annual-nlcd for updated ",
        "NLCD documentation and availability."
      )
    )
  }
  year <- as.integer(terra::metags(from)$value[nrow(terra::metags(from))])
  stopifnot(year %in% 1985:2024L)

  # select points within mainland US and reproject on nlcd crs if necessary
  data_vect_b <-
    terra::project(locs_vector, y = terra::crs(from))
  is_point_locs <- all(
    tolower(terra::geomtype(locs_vector)) %in% c("points", "point")
  )

  message(
    paste0(
      "Calculating NLCD Land Cover Class covariates for ",
      year,
      "..."
    )
  )

  if (radius <= 0 && is_point_locs) {
    new_data_vect <- suppressMessages(
      amadeus::calc_worker(
        dataset = "nlcd",
        from = from,
        locs_vector = data_vect_b,
        locs_df = locs_df,
        fun = "mean",
        variable = 1,
        time = NULL,
        time_type = "timeless",
        radius = 0,
        level = NULL,
        weights = weights
      )
    )
    if ("geometry" %in% names(new_data_vect)) {
      new_data_vect <- cbind(
        new_data_vect[, c(locs_id, "geometry"), drop = FALSE],
        as.integer(year),
        new_data_vect[, setdiff(names(new_data_vect), c(locs_id, "geometry")),
          drop = FALSE
        ]
      )
    } else {
      new_data_vect <- cbind(
        new_data_vect[, locs_id, drop = FALSE],
        as.integer(year),
        new_data_vect[, setdiff(names(new_data_vect), locs_id), drop = FALSE]
      )
    }
    value_col <- setdiff(names(new_data_vect), c(locs_id, "geometry", "time"))
    if (length(value_col) == 1L) {
      names(new_data_vect)[names(new_data_vect) == value_col] <- sprintf(
        "NLCD_VALUE_%05d",
        as.integer(radius)
      )
    }
  } else {
    # create circle buffers with buf_radius
    bufs_pol <- terra::buffer(data_vect_b, width = radius)
    if (mode == "terra") {
      # terra mode
      # class_query <- "names"
      # extract land cover class in each buffer
      nlcd_at_bufs <- Map(
        function(i) {
          terra::freq(
            from,
            zones = bufs_pol[i, ],
            wide = TRUE
          )
        },
        seq_len(nrow(bufs_pol))
      )
      nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
        data = nlcd_at_bufs,
        mode = mode,
        locs_id = locs_id
      )
      if (ncol(nlcd_at_bufs_fill) >= 2) {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, -seq_len(2), drop = FALSE]
      } else {
        nlcd_at_bufs_fill <- data.frame(row.names = seq_len(nrow(locs_df)))
      }
      if (ncol(nlcd_at_bufs_fill) > 0) {
        nlcd_cellcnt <- nlcd_at_bufs_fill[
          ,
          seq_len(ncol(nlcd_at_bufs_fill)),
          drop = FALSE
        ]
        nlcd_denom <- rowSums(nlcd_cellcnt, na.rm = TRUE)
        nlcd_denom[!is.finite(nlcd_denom) | nlcd_denom == 0] <- 1
        nlcd_cellcnt <- nlcd_cellcnt / nlcd_denom
        nlcd_cellcnt[!is.finite(as.matrix(nlcd_cellcnt))] <- 0
        nlcd_at_bufs_fill[, seq_len(ncol(nlcd_at_bufs_fill))] <- nlcd_cellcnt
      }
    } else {
      # class_query <- "value"
      # ratio of each nlcd class per buffer
      bufs_polx <- bufs_pol[terra::ext(from), ] |>
        sf::st_as_sf()

      if (nrow(bufs_polx) == 0) {
        nlcd_at_bufs_fill <- data.frame(
          setNames(list(locs_df[[locs_id]]), locs_id)
        )
      } else {
        nlcd_at_bufs <- Map(
          function(i) {
            exactextractr::exact_extract(
              from,
              bufs_polx[i, ],
              fun = "frac",
              force_df = TRUE,
              progress = FALSE,
              append_cols = locs_id,
              max_cells_in_memory = max_cells
            )
          },
          seq_len(nrow(bufs_polx))
        )
        nlcd_at_bufs_fill <- amadeus::collapse_nlcd(
          data = nlcd_at_bufs,
          mode = mode,
          locs = bufs_pol,
          locs_id = locs_id
        )
      }
    }

    if (mode == "exact") {
      nlcd_at_buf_names <- names(nlcd_at_bufs_fill)
      nlcd_val_cols <- grep("^frac_", nlcd_at_buf_names, value = TRUE)
      if (length(nlcd_val_cols) == 0) {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[, locs_id, drop = FALSE]
      } else {
        nlcd_at_bufs_fill <- nlcd_at_bufs_fill[
          ,
          c(locs_id, nlcd_val_cols),
          drop = FALSE
        ]
      }
      new_data_core <- merge(
        x = locs_df,
        y = nlcd_at_bufs_fill,
        by = locs_id,
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      new_data_core <- cbind(locs_df, nlcd_at_bufs_fill)
    }

    value_cols <- setdiff(names(new_data_core), c(locs_id, "geometry"))
    if (length(value_cols) > 0) {
      new_data_core[, value_cols] <- lapply(
        new_data_core[, value_cols, drop = FALSE],
        function(x) {
          x[is.na(x)] <- 0
          x
        }
      )
      nlcd_codes <- vapply(
        value_cols,
        function(x) {
          x <- sub("^frac_", "", x)
          x <- sub("^X", "", x)
          x <- regmatches(x, regexpr("[0-9]+", x))
          ifelse(length(x) == 0 || !nzchar(x), "UNK", x)
        },
        character(1)
      )
      names(new_data_core)[match(value_cols, names(new_data_core))] <- sprintf(
        "NLCD_%s_%05d",
        nlcd_codes,
        as.integer(radius)
      )
    }

    if ("geometry" %in% names(new_data_core)) {
      new_data_vect <- cbind(
        new_data_core[, c(locs_id, "geometry"), drop = FALSE],
        as.integer(year),
        new_data_core[
          ,
          setdiff(names(new_data_core), c(locs_id, "geometry")),
          drop = FALSE
        ]
      )
    } else {
      new_data_vect <- cbind(
        new_data_core[, locs_id, drop = FALSE],
        as.integer(year),
        new_data_core[, setdiff(names(new_data_core), locs_id), drop = FALSE]
      )
    }
  }

  if (geom %in% c("sf", "terra")) {
    names(new_data_vect)[1:3] <- c(locs_id, "geometry", "time")
  } else {
    names(new_data_vect)[1:2] <- c(locs_id, "time")
  }
  if (drop && radius > 0) {
    fixed_cols <- c(locs_id, "time")
    if ("geometry" %in% names(new_data_vect)) {
      fixed_cols <- c(locs_id, "geometry", "time")
    }
    nlcd_cols <- grep(
      "^NLCD_[0-9]+_[0-9]{5}$",
      names(new_data_vect),
      value = TRUE
    )
    if (length(nlcd_cols) > 0) {
      keep_cols <- nlcd_cols[vapply(
        nlcd_cols,
        function(x) any(new_data_vect[[x]] > 0, na.rm = TRUE),
        logical(1)
      )]
      new_data_vect <- new_data_vect[, c(fixed_cols, keep_cols), drop = FALSE]
    }
  }
  new_data_vect$time <- as.integer(new_data_vect$time)
  new_data_return <- amadeus::calc_return_locs(
    covar = new_data_vect,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(new_data_return)
}


#' Calculate ecoregions covariates
#' @description
#' Extract ecoregions covariates (U.S. EPA Ecoregions Level 2/3) at point or
#' polygon locations. Returns a `data.frame` object containing `locs_id` and
#' either dummy indicators (`frac = FALSE`) or area fractions (`frac = TRUE`)
#' for each ecoregion.
#' @param from SpatVector(1). Output of [`process_ecoregion`].
#' @param locs sf/SpatVector. Unique locs. Should include
#'  a unique identifier field named `locs_id`
#' @param locs_id character(1). Name of unique identifier.
#' @param colnames character(1). Naming convention for ecoregion indicator
#'   columns. Default is `"coded"` for the existing numeric key-based names.
#'   Use `"full_ecoregion"` to emit sanitized full ecoregion names.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, returns binary dummy
#'   indicators (0/1). If `TRUE`, returns fractional overlap values.
#' @param radius numeric(1). Circular buffer size (meters) around point
#'   locations. Use `0` (default) for exact point extraction.
#' @param drop logical(1). Default `FALSE`. If `TRUE`, remove ecoregion columns
#'   that are all 0 or `NA` across returned locations.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [`process_ecoregion`]
#' @return a data.frame or SpatVector object with ecoregion indicator/fraction
#' variables and attributes of:
#'   - Indicator names are controlled by `colnames`: `"coded"` (default)
#'   creates key-based names such as `DUM_E2083_00000` and
#'   `DUM_E3064_00000` when `frac = FALSE`, or `FRC_E2083_00000` and
#'   `FRC_E3064_00000` when `frac = TRUE`; `"full_ecoregion"` creates
#'   sanitized name-based columns such as
#'   `DUM_E2_SOUTHEASTERN_USA_PLAINS_00000` /
#'   `FRC_E2_SOUTHEASTERN_USA_PLAINS_00000` and
#'   `DUM_E3_NORTHERN_PIEDMONT_00000` /
#'   `FRC_E3_NORTHERN_PIEDMONT_00000` (duplicates are suffixed, e.g. `_1`).
#'   - `attr(., "ecoregion2_code")`: Ecoregion lv.2 code and key
#'   - `attr(., "ecoregion3_code")`: Ecoregion lv.3 code and key
#' @author Insang Song
#' @importFrom terra extract
#' @importFrom data.table year
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_ecoregion(
#'   from = ecoregion, # derived from process_ecoregion() example
#'   locs = loc,
#'   locs_id = "id",
#'   colnames = "coded",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_ecoregion <-
  function(
    from = NULL,
    locs,
    locs_id = "site_id",
    colnames = c("coded", "full_ecoregion"),
    frac = FALSE,
    drop = FALSE,
    weights = NULL,
    geom = FALSE,
    radius = 0,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    sanitize_ecoregion_name <- function(x) {
      x <- iconv(x, to = "ASCII//TRANSLIT")
      x[is.na(x)] <- "UNKNOWN"
      x <- toupper(x)
      x <- gsub("[^A-Z0-9]+", "_", x)
      x <- gsub("_+", "_", x)
      x <- gsub("^_|_$", "", x)
      x[x == ""] <- "UNKNOWN"
      x
    }
    build_ecoregion_lookup <- function(
      keys,
      labels,
      prefix,
      naming_mode,
      value_prefix
    ) {
      lookup <- unique(data.frame(
        key = as.character(keys),
        label = as.character(labels),
        stringsAsFactors = FALSE
      ))
      if (naming_mode == "coded") {
        if (prefix == "E2") {
          key_num <- regmatches(
            lookup$key,
            regexpr("\\d{1,2}\\.[1-9]", lookup$key)
          )
          key_num <- sprintf("%s_%s%03d_%s", value_prefix, prefix, as.integer(
            10 * as.numeric(key_num)
          ), radius_suffix)
        } else {
          key_num <- regmatches(
            lookup$key,
            regexpr("\\d{1,3}", lookup$key)
          )
          key_num <- sprintf(
            "%s_%s%03d_%s",
            value_prefix,
            prefix,
            as.integer(as.numeric(key_num)),
            radius_suffix
          )
        }
        lookup$column_name <- key_num
      } else {
        safe_label <- sanitize_ecoregion_name(lookup$label)
        lookup$column_name <- paste0(
          value_prefix,
          "_",
          prefix,
          "_",
          safe_label,
          "_",
          radius_suffix
        )
        lookup$column_name <- make.unique(lookup$column_name, sep = "_")
      }
      lookup
    }
    get_extracted_field <- function(x, field) {
      candidate_names <- c(field, paste0(field, "_2"), paste0(field, ".1"))
      match_name <- candidate_names[candidate_names %in% names(x)][1]
      if (is.na(match_name) || length(match_name) == 0) {
        stop(
          "Required ecoregion field missing from intersection output: ",
          field
        )
      }
      values <- x[[match_name]]
      if (is.matrix(values) || is.data.frame(values)) {
        values <- values[, 1, drop = TRUE]
      }
      values
    }
    get_spatvector_field <- function(x, field) {
      vals <- terra::as.data.frame(x)[[field]]
      if (is.matrix(vals) || is.data.frame(vals)) {
        vals <- vals[, 1, drop = TRUE]
      }
      vals
    }
    colnames <- match.arg(colnames)
    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop("`frac` should be a single logical value (TRUE/FALSE).")
    }
    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop("`radius` should be a single numeric value.")
    }
    if (radius < 0) {
      stop("`radius` should be greater than or equal to 0.")
    }
    radius_value <- as.integer(round(radius))
    width_radius <- max(5L, nchar(as.character(abs(radius_value))))
    radius_suffix <- sprintf(paste0("%0", width_radius, "d"), radius_value)
    if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
      stop("`drop` should be a single logical value (TRUE/FALSE).")
    }
    value_prefix <- if (isTRUE(frac)) "FRC" else "DUM"
    # prepare locations
    locs_prepared <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    # both objects will preserve the row order
    locsp <- locs_prepared[[1]]
    locs_df <- locs_prepared[[2]]
    is_point_locs <- all(
      tolower(terra::geomtype(locsp)) %in% c("points", "point")
    )

    extracted <- terra::intersect(locsp, from)
    key2_lookup <- build_ecoregion_lookup(
      keys = from$L2_KEY,
      labels = from$NA_L2NAME,
      prefix = "E2",
      naming_mode = colnames,
      value_prefix = value_prefix
    )
    key3_labels <- if ("US_L3NAME" %in% names(from)) {
      from$US_L3NAME
    } else {
      from$NA_L3NAME
    }
    key3_lookup <- build_ecoregion_lookup(
      keys = from$L3_KEY,
      labels = key3_labels,
      prefix = "E3",
      naming_mode = colnames,
      value_prefix = value_prefix
    )
    locs_ecoreg <- data.frame(
      locs_df[0, , drop = FALSE],
      description = character(0),
      stringsAsFactors = FALSE
    )

    if (nrow(extracted) > 0) {
      extracted_df <- terra::as.data.frame(extracted)
      key2_sorted <- as.character(get_extracted_field(extracted_df, "L2_KEY"))
      key3_sorted <- as.character(get_extracted_field(extracted_df, "L3_KEY"))
      site_sorted <- as.character(extracted_df[[locs_id]])

      if (is_point_locs || !isTRUE(frac)) {
        base_value <- rep(1, length(site_sorted))
      } else {
        site_areas <- terra::expanse(locsp)
        site_ids <- as.character(get_spatvector_field(locsp, locs_id))
        site_lookup <- setNames(site_areas, site_ids)
        inter_areas <- terra::expanse(extracted)
        denom <- as.numeric(site_lookup[site_sorted])
        denom[!is.finite(denom) | denom <= 0] <- NA_real_
        base_value <- inter_areas / denom
        base_value[!is.finite(base_value)] <- 0
      }

      build_wide <- function(site_ids, keys, lookup_tbl, values) {
        vals_df <- data.frame(
          site_id = site_ids,
          key = as.character(keys),
          base_value = as.numeric(values),
          stringsAsFactors = FALSE
        )
        vals_df <- stats::aggregate(
          base_value ~ site_id + key,
          data = vals_df,
          FUN = sum
        )
        if (!isTRUE(frac)) {
          vals_df$base_value <- as.integer(vals_df$base_value > 0)
        } else {
          vals_df$base_value <- pmin(vals_df$base_value, 1)
        }
        vals_df$column_name <- lookup_tbl$column_name[
          match(vals_df$key, lookup_tbl$key)
        ]
        vals_df <- vals_df[!is.na(vals_df$column_name), , drop = FALSE]
        if (nrow(vals_df) == 0) {
          return(data.frame(
            site_id = character(),
            stringsAsFactors = FALSE
          ))
        }
        tidyr::pivot_wider(
          vals_df[, c("site_id", "column_name", "base_value"), drop = FALSE],
          names_from = "column_name",
          values_from = "base_value",
          values_fill = 0
        )
      }

      matched_ids <- unique(site_sorted)
      locs_ecoreg <- cbind(
        locs_df[locs_df[[locs_id]] %in% matched_ids, , drop = FALSE],
        description = paste0("1997 - ", data.table::year(Sys.Date()))
      )
      df_lv2 <- build_wide(site_sorted, key2_sorted, key2_lookup, base_value)
      df_lv3 <- build_wide(site_sorted, key3_sorted, key3_lookup, base_value)

      locs_ecoreg <- merge(
        locs_ecoreg,
        df_lv2,
        by.x = locs_id,
        by.y = "site_id",
        all.x = TRUE,
        sort = FALSE
      )
      locs_ecoreg <- merge(
        locs_ecoreg,
        df_lv3,
        by.x = locs_id,
        by.y = "site_id",
        all.x = TRUE,
        sort = FALSE
      )
    }

    # Catch and patch for sites with no matching ecoregions
    n_locs <- nrow(locs_df)
    n_match <- if (nrow(extracted) > 0) {
      length(unique(as.character(terra::as.data.frame(extracted)[[locs_id]])))
    } else {
      0L
    }
    if (n_match != n_locs) {
      message(
        "Warning: only ",
        n_match,
        " of the ",
        n_locs,
        " locations provided had matching ecoregions. ",
        n_locs - n_match,
        " unmatched locations will present NAs."
      )
      locs_ecoreg <- merge(locs_df, locs_ecoreg, by = locs_id, all.x = TRUE)
    }

    ecoreg_cols <- grep(
      paste0("^", value_prefix, "_E[23]_"),
      names(locs_ecoreg),
      value = TRUE
    )
    if (!isTRUE(frac) && length(ecoreg_cols) > 0) {
      locs_ecoreg[, ecoreg_cols] <- lapply(
        locs_ecoreg[, ecoreg_cols, drop = FALSE],
        function(x) {
          x[!is.na(x)] <- as.integer(x[!is.na(x)] > 0)
          x
        }
      )
    }
    if (drop && length(ecoreg_cols) > 0) {
      keep_cols <- ecoreg_cols[vapply(
        ecoreg_cols,
        function(x) any(locs_ecoreg[[x]] > 0, na.rm = TRUE),
        logical(1)
      )]
      fixed_cols <- c(
        locs_id,
        if ("geometry" %in% names(locs_ecoreg)) "geometry",
        "description"
      )
      locs_ecoreg <- locs_ecoreg[, c(fixed_cols, keep_cols), drop = FALSE]
    }

    locs_return <- amadeus::calc_return_locs(
      covar = locs_ecoreg,
      POSIXt = FALSE,
      geom = geom,
      crs = terra::crs(from)
    )
    attr(locs_return, "ecoregion2_code") <- sort(unique(from$L2_KEY))
    attr(locs_return, "ecoregion3_code") <- sort(unique(from$L3_KEY))
    return(locs_return)
  }


#' Calculate MODIS product covariates
#' @param from character, SpatRaster, or SpatVector. Either a list of
#' MODIS/VIIRS file paths (raw path mode), a preprocessed raster (direct raster
#' mode), or processed MODIS fire detections as a SpatVector with `time`,
#' `fire_count`, and `frp` fields.
#' @param from_secondary character or SpatRaster. Optional secondary input for
#' fused temporal coverage in raster/path workflows. Type must match `from`
#' (`character` with `character`, or `SpatRaster` with `SpatRaster`).
#' @param locs sf/SpatVector object. Unique locs where covariates
#' will be calculated.
#' @param locs_id character(1). Site identifier. Default is `"site_id"`
#' @param radius numeric. Radii to calculate covariates.
#' Default is `c(0, 1000, 10000, 50000)`.
#' @param preprocess function. Function to handle HDF files in raw path mode.
#' Ignored when `from` is a `SpatRaster` or `SpatVector`.
#' @param name_covariates character. Name header of covariates.
#' e.g., `"MOD_NDVIF_0_"`.
#' The calculated covariate names will have a form of
#' "\code{\{name_covariates\}\{zero-padded buffer radius in meters\}}",
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param subdataset Indices, names, or search patterns for subdatasets.
#' Find detail usage of the argument in notes.
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param package_list_add character. Reserved for backward compatibility;
#'   currently not used by `calculate_modis()`.
#' @param export_list_add character. Reserved for backward compatibility;
#'   currently not used by `calculate_modis()`.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param scale character(1). Scale expression to be applied to the raw values.
#' It is crucial that users review the technical documentation of the MODIS
#' product
#' they are using to ensure proper scale.
#' An example for the MOD11A1 product's LST_Day_1km variable (land surface
#' temperature)
#' would be `scale = "* 0.02"`.
#' Default is `NULL`, which applies no scale.
#' @param fusion_method character(1). Fusion method used only when
#' `from_secondary` is provided. Options are `"mean"` (pixel-wise mean with
#' `na.rm = TRUE`), `"primary_first"` (use `from` first), and
#' `"secondary_first"` (use `from_secondary` first).
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Arguments passed to `preprocess`.
# nolint start
#' @description `calculate_modis` orchestrates daily extraction using
#' [`calculate_modis_daily()`]. In raw-path mode, files are grouped by inferred
#' date, preprocessed for each day, and then extracted over requested radii.
#' With product-specific preprocessing, files are handled according to each
#' product's structure and naming conventions.
# nolint end
#' @note `locs` is expected to be convertible to `sf` object.
#' `sf`, `SpatVector`, and other class objects that could be converted to
#' `sf` can be used.
#' In raw path mode, `preprocess` is called once per inferred day using a
#' single-date value. Temporal aggregation across extracted rows should be done
#' with `.by_time`.
#' Common arguments in `preprocess` functions such as `date` and `path` are
#' automatically detected and passed to the function. Please note that
#' `locs` here and `path` in `preprocess` functions are assumed to have a
#' standard naming convention of raw files from NASA.
#' The argument `subdataset` should be in a proper format
#' depending on `preprocess` function:
#' * `process_modis_merge()`: Regular expression pattern.
#'   e.g., `"^LST_"`
#' * `process_modis_swath()`: Subdataset names.
#'   e.g., `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`
#' * `process_blackmarble()`: Subdataset number.
#'   e.g., for VNP46A2 product, 3L.
#'
#' For MOD13/MYD13 families, Terra and Aqua composites are 16-day phased
#' products offset by 8 days; combining both can improve effective temporal
#' coverage. This behavior aligns with NASA MOD13 product guidance:
#' <https://modis.gsfc.nasa.gov/data/dataprod/mod13.php>
#'
#' For MCD19A2 MAIAC, common sub-datasets include both optical depth and
#' plume injection height layers. Typical selectors are
#' `"(Optical_Depth|Injection_Height)"` for both families or
#' `"(Injection_Height)"` when targeting plume height only.
#'
#' For MOD14A1/MYD14A1 fire grids, the `FireMask` raw values are commonly
#' interpreted as:
#' \tabular{rll}{
#' Raw value \tab Meaning \tab Binary fire mask?\cr
#' 0 \tab not processed, missing input \tab NA / no observation\cr
#' 1 \tab obsolete, not used since Collection 1 \tab NA\cr
#' 2 \tab not processed, other reason \tab NA\cr
#' 3 \tab non-fire water pixel \tab 0\cr
#' 4 \tab cloud, land or water \tab NA or 0, depending on analysis\cr
#' 5 \tab non-fire land pixel \tab 0\cr
#' 6 \tab unknown, land or water \tab NA\cr
#' 7 \tab fire, low confidence \tab 1, or exclude for stricter mask\cr
#' 8 \tab fire, nominal confidence \tab 1\cr
#' 9 \tab fire, high confidence \tab 1\cr
#' }
#'
#' Dates with less than 80 percent of the expected number of tiles,
#' which are determined by the mode of the number of tiles, are removed.
#' Users will be informed of the dates with insufficient tiles.
#' The result data.frame will have an attribute with the dates with
#' insufficient tiles.
#' @return A data.frame or SpatVector with an attribute:
#' * `attr(., "dates_dropped")`: Dates with insufficient tiles.
#'   Note that the dates mean the dates with insufficient tiles,
#'   not the dates without available tiles. When \code{.by_time} is provided,
#'   rows are summarized with \code{calc_summarize_by()} semantics.
#' @seealso
#' This function leverages the calculation of single-day MODIS
#' covariates:
#' * [`calculate_modis_daily()`]
#'
#' Also, for preprocessing, please refer to:
#' * [`process_modis_merge()`]
#' * [`process_modis_swath()`]
#' * [`process_blackmarble()`]
#' @importFrom methods is
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows left_join
#' @importFrom rlang inject
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
#' calculate_modis(
#'   from =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = c(0L, 1000L),
#'   preprocess = process_modis_merge,
#'   name_covariates = "cloud_fraction_0",
#'   subdataset = "Cloud_Fraction",
#'   fun_summary = "mean"
#' )
#' }
#' @export
calculate_modis <-
  function(
    from = NULL,
    from_secondary = NULL,
    locs = NULL,
    locs_id = "site_id",
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    preprocess = amadeus::process_modis_merge,
    name_covariates = NULL,
    subdataset = NULL,
    fun_summary = "mean",
    .by_time = NULL,
    weights = NULL,
    package_list_add = NULL,
    export_list_add = NULL,
    max_cells = 3e7,
    geom = FALSE,
    scale = NULL,
    fusion_method = c("mean", "primary_first", "secondary_first"),
    ...
  ) {
    amadeus::check_geom(geom)
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_by_time(.by_time)
    fusion_method <- match.arg(fusion_method)
    from_is_character <- is.character(from)
    from_is_raster <- inherits(from, "SpatRaster")
    from_is_vector <- inherits(from, "SpatVector")
    if (!from_is_character && !from_is_raster && !from_is_vector) {
      stop(
        paste0(
          "from should be a character vector of paths, SpatRaster, ",
          "or SpatVector.\n"
        )
      )
    }
    if (from_is_character) {
      if (!is.null(from_secondary) && !is.character(from_secondary)) {
        stop("from_secondary should be a character vector of file paths.\n")
      }
    } else if (from_is_raster) {
      if (!is.null(from_secondary) && !inherits(from_secondary, "SpatRaster")) {
        stop("from_secondary should be SpatRaster when from is SpatRaster.\n")
      }
    } else if (!is.null(from_secondary)) {
      stop(
        paste0(
          "from_secondary is only supported for character ",
          "or SpatRaster inputs.\n"
        )
      )
    }
    if (from_is_character && !is.function(preprocess)) {
      stop(
        "preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble."
      )
    }
    # read all arguments
    # nolint start
    hdf_args <- c(as.list(environment()), list(...))
    # nolint end
    if (from_is_character) {
      dates_available_m <-
        vapply(from, modis_extract_temporal_key, character(1))
      date_scales <- vapply(from, modis_extract_temporal_scale, character(1))
      if (!is.null(from_secondary)) {
        dates_secondary_m <-
          vapply(from_secondary, modis_extract_temporal_key, character(1))
        date_scales_secondary <-
          vapply(from_secondary, modis_extract_temporal_scale, character(1))
        dates_available_m <- c(dates_available_m, dates_secondary_m)
        date_scales <- c(date_scales, date_scales_secondary)
      }
      date_scale_unique <- unique(stats::na.omit(date_scales))
      if (length(date_scale_unique) != 1L) {
        stop(
          "MODIS input files contain mixed or unsupported temporal patterns.\n"
        )
      }
      dates_available <- sort(unique(dates_available_m))

      if (is.null(from_secondary)) {
        # When multiple dates are concerned,
        # the number of tiles are expected to be the same.
        # Exceptions could exist, so here the number of tiles are checked.
        summary_available <- table(dates_available_m)
        summary_available_mode <-
          sort(table(summary_available), decreasing = TRUE)[1]
        summary_available_mode <- as.numeric(names(summary_available_mode))
        summary_available_insuf <-
          which(summary_available < floor(summary_available_mode * 0.8))

        if (length(summary_available_insuf) > 0) {
          dates_insuf <-
            modis_key_to_date(
              dates_available[summary_available_insuf],
              date_scale_unique
            )
          message(
            paste0(
              "The number of tiles on the following dates are insufficient: ",
              paste(dates_insuf, collapse = ", "),
              ".\n"
            )
          )
          # finally it removes the dates with insufficient tiles
          dates_available <- dates_available[-summary_available_insuf]
        } else {
          dates_insuf <- NA
        }
      } else {
        dates_insuf <- NA
      }
    } else {
      date_scale_unique <- NA_character_
      dates_available <- NA_character_
      dates_insuf <- NA
    }

    locs_input <- try(sf::st_as_sf(locs), silent = TRUE)
    if (inherits(locs_input, "try-error")) {
      stop(
        "locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n"
      )
    }

    if (from_is_vector) {
      calc_results_return <-
        calculate_modis_fire_vector(
          from = from,
          locs_input = locs_input,
          locs_id = locs_id,
          radius = radius,
          fun_summary = fun_summary,
          .by_time = .by_time,
          geom = geom
        )
      attr(calc_results_return, "dates_dropped") <- NA
      return(calc_results_return)
    }

    if (!is.null(scale)) {
      stopifnot(is.character(scale))
    }
    if (is.null(scale)) {
      warning(
        paste0(
          "`scale` parameter not defined. Review technical documentation ",
          "to apply proper scale. Calculation proceeding with unscaled values."
        )
      )
      scale <- "* 1"
    }

    export_list <- c()
    package_list <-
      c(
        "sf",
        "terra",
        "exactextractr",
        "data.table",
        "stars",
        "dplyr",
        "parallelly",
        "rlang",
        "amadeus"
      )
    if (!is.null(export_list_add)) {
      export_list <- append(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- append(package_list, package_list_add)
    }

    # make clusters
    if (from_is_character) {
      idx_date_available <- seq_along(dates_available)
      list_date_available <-
        split(idx_date_available, idx_date_available)
    } else {
      list_date_available <- list(1L)
    }
    calc_results <-
      lapply(
        list_date_available,
        FUN = function(datei) {
          options(sf_use_s2 = FALSE)
          radiusindex <- seq_along(radius)
          radiusindexlist <- split(radiusindex, radiusindex)
          if (from_is_character) {
            # nolint start
            day_to_pick <- dates_available[datei]
            # nolint end
            day_to_pick <- modis_key_to_date(day_to_pick, date_scale_unique)
            calc_time <- as.character(day_to_pick)
            hdf_args <- c(hdf_args, list(date = day_to_pick))
            if (is.null(from_secondary)) {
              hdf_args <- c(hdf_args, list(path = hdf_args$from))
              # unified interface with rlang::inject
              vrt_today <- rlang::inject(preprocess(!!!hdf_args))
            } else {
              day_key <- dates_available[datei]
              has_primary <- day_key %in%
                vapply(
                  hdf_args$from,
                  modis_extract_temporal_key,
                  character(1)
                )
              has_secondary <- day_key %in%
                vapply(
                  hdf_args$from_secondary,
                  modis_extract_temporal_key,
                  character(1)
                )
              if (!has_primary && !has_secondary) {
                stop("No MODIS files found for selected fusion date.\n")
              }

              raster_primary <- NULL
              raster_secondary <- NULL

              if (has_primary) {
                hdf_args_primary <- hdf_args
                hdf_args_primary$path <- hdf_args$from
                hdf_args_primary$from_secondary <- NULL
                raster_primary <- rlang::inject(preprocess(!!!hdf_args_primary))
              }
              if (has_secondary) {
                hdf_args_secondary <- hdf_args
                hdf_args_secondary$path <- hdf_args$from_secondary
                hdf_args_secondary$from_secondary <- NULL
                raster_secondary <-
                  rlang::inject(preprocess(!!!hdf_args_secondary))
              }

              if (is.null(raster_primary)) {
                vrt_today <- raster_secondary
              } else if (is.null(raster_secondary)) {
                vrt_today <- raster_primary
              } else {
                if (
                  !isTRUE(terra::compareGeom(
                    raster_primary,
                    raster_secondary,
                    stopOnError = FALSE
                  ))
                ) {
                  stop(
                    "Primary and secondary MODIS rasters have incompatible ",
                    "geometry.\n"
                  )
                }
                if (
                  terra::nlyr(raster_primary) != terra::nlyr(raster_secondary)
                ) {
                  stop(
                    "Primary and secondary MODIS rasters have different ",
                    "layer counts.\n"
                  )
                }
                if (fusion_method == "primary_first") {
                  vrt_today <- terra::cover(raster_primary, raster_secondary)
                } else if (fusion_method == "secondary_first") {
                  vrt_today <- terra::cover(raster_secondary, raster_primary)
                } else {
                  idx_layers <- seq_len(terra::nlyr(raster_primary))
                  fused <- lapply(idx_layers, function(k) {
                    terra::app(
                      c(raster_primary[[k]], raster_secondary[[k]]),
                      mean,
                      na.rm = TRUE
                    )
                  })
                  vrt_today <- do.call(c, fused)
                  names(vrt_today) <- names(raster_primary)
                }
              }
            }
          } else {
            calc_time <- NA_character_
            if (is.null(from_secondary)) {
              vrt_today <- from
            } else {
              raster_primary <- from
              raster_secondary <- from_secondary
              if (
                !isTRUE(terra::compareGeom(
                  raster_primary,
                  raster_secondary,
                  stopOnError = FALSE
                ))
              ) {
                stop(
                  "Primary and secondary MODIS rasters have incompatible ",
                  "geometry.\n"
                )
              }
              if (
                terra::nlyr(raster_primary) != terra::nlyr(raster_secondary)
              ) {
                stop(
                  "Primary and secondary MODIS rasters have different ",
                  "layer counts.\n"
                )
              }
              if (fusion_method == "primary_first") {
                vrt_today <- terra::cover(raster_primary, raster_secondary)
              } else if (fusion_method == "secondary_first") {
                vrt_today <- terra::cover(raster_secondary, raster_primary)
              } else {
                idx_layers <- seq_len(terra::nlyr(raster_primary))
                fused <- lapply(idx_layers, function(k) {
                  terra::app(
                    c(raster_primary[[k]], raster_secondary[[k]]),
                    mean,
                    na.rm = TRUE
                  )
                })
                vrt_today <- do.call(c, fused)
                names(vrt_today) <- names(raster_primary)
              }
            }
          }

          if (sum(terra::nlyr(vrt_today)) != length(name_covariates)) {
            message(
              "The number of layers in the input raster do not match
                    the length of name_covariates.\n"
            )
          }

          res0 <-
            lapply(radiusindexlist, function(k) {
              name_radius <-
                sprintf("%s%05d", name_covariates, radius[k])
              extracted <-
                try(
                  amadeus::calculate_modis_daily(
                    locs = locs_input,
                    from = vrt_today,
                    locs_id = locs_id,
                    date = calc_time,
                    fun_summary = fun_summary,
                    name_extracted = name_radius,
                    radius = radius[k],
                    weights = weights,
                    max_cells = max_cells,
                    geom = FALSE,
                    scale = scale
                  )
                )
              if (inherits(extracted, "try-error")) {
                # coerce to avoid errors
                error_df <- data.frame(
                  matrix(
                    -99999,
                    ncol = length(name_radius) + 1,
                    nrow = nrow(locs_input)
                  )
                )
                error_df <- stats::setNames(error_df, c(locs_id, name_radius))
                error_df[[locs_id]] <- unlist(locs_input[[locs_id]])
                if (is.na(calc_time)) {
                  error_df$time <- as.POSIXlt(as.Date(NA))
                } else {
                  error_df$time <- as.POSIXlt(calc_time, tz = "UTC")
                }
                extracted <- error_df
              }
              return(extracted)
            })
          res <-
            Reduce(
              function(x, y) {
                dplyr::left_join(x, y, by = c(locs_id, "time"))
              },
              res0
            )
          return(res)
        }
      )
    calc_results <- do.call(dplyr::bind_rows, calc_results)
    if (!is.null(.by_time)) {
      calc_results <- amadeus::calc_summarize_by(
        covar = calc_results,
        .by_time = .by_time,
        fun_summary = fun_summary,
        locs_id = locs_id
      )
    }
    if (geom %in% c("sf", "terra")) {
      # merge
      calc_results_return <- merge(
        locs_input[, locs_id],
        calc_results,
        by = locs_id
      )
      if (geom == "terra") {
        calc_results_return <- terra::vect(calc_results_return)
      }
    } else {
      calc_results_return <- calc_results
    }
    attr(calc_results_return, "dates_dropped") <- dates_insuf
    Sys.sleep(1L)
    return(calc_results_return)
  }


calculate_modis_fire_vector <- function(
  from,
  locs_input,
  locs_id,
  radius,
  fun_summary,
  .by_time,
  geom
) {
  if (!methods::is(from, "SpatVector")) {
    stop("from should be a SpatVector returned by process_mcd14ml.\n")
  }
  if (!all(c("time", "fire_count", "frp") %in% names(from))) {
    stop("from is missing required MCD14ML fields.\n")
  }

  locs_base <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs_input,
    locs_id = locs_id,
    radius = 0L,
    geom = geom
  )
  locs_points <- locs_base[[1]]
  locs_return <- locs_base[[2]]
  loc_index <- seq_len(nrow(locs_points))

  date_keys <- sort(unique(as.integer(from$time)))
  results_by_day <- lapply(date_keys, function(day_key) {
    from_day <- from[from$time == day_key, ]
    results_by_radius <- lapply(radius, function(radius_i) {
      col_count <- sprintf("fire_count_%05d", radius_i)
      col_frp <- sprintf("frp_%05d", radius_i)

      dist_matrix <- terra::distance(locs_points, from_day)
      dist_df <- data.frame(
        expand.grid(
          loc_index = loc_index,
          from_index = seq_len(nrow(from_day))
        ),
        distance = as.vector(dist_matrix)
      )
      if (radius_i == 0L) {
        dist_df <- dist_df[dist_df$distance == 0, ]
      } else {
        dist_df <- dist_df[dist_df$distance <= radius_i, ]
      }

      if (nrow(dist_df) == 0) {
        result_empty <- data.frame(
          loc_index = loc_index,
          fire_count = 0,
          frp = 0
        )
      } else {
        dist_df$fire_count <- from_day$fire_count[dist_df$from_index]
        dist_df$frp <- from_day$frp[dist_df$from_index]
        result_empty <-
          stats::aggregate(
            cbind(fire_count, frp) ~ loc_index,
            data = dist_df,
            FUN = sum,
            na.rm = TRUE
          )
        result_empty <-
          merge(
            data.frame(loc_index = loc_index),
            result_empty,
            by = "loc_index",
            all.x = TRUE
          )
        result_empty$fire_count[is.na(result_empty$fire_count)] <- 0
        result_empty$frp[is.na(result_empty$frp)] <- 0
      }

      names(result_empty)[names(result_empty) == "fire_count"] <- col_count
      names(result_empty)[names(result_empty) == "frp"] <- col_frp
      result_empty
    })

    result_day <- Reduce(
      function(x, y) merge(x, y, by = "loc_index", all = TRUE),
      results_by_radius
    )
    result_day[[locs_id]] <- locs_return[[locs_id]]
    result_day$time <- as.POSIXlt(
      as.character(day_key),
      format = "%Y%m%d",
      tz = "UTC"
    )
    ordered_cols <- c(
      locs_id,
      "time",
      setdiff(names(result_day), c("loc_index", locs_id, "time"))
    )
    result_day[, ordered_cols]
  })

  result_all <- do.call(rbind, results_by_day)
  if (!is.null(.by_time)) {
    result_all <- amadeus::calc_summarize_by(
      covar = result_all,
      .by_time = .by_time,
      fun_summary = fun_summary,
      locs_id = locs_id
    )
  }
  if (geom %in% c("sf", "terra")) {
    result_all <- merge(locs_return, result_all, by = locs_id)
  }

  amadeus::calc_return_locs(
    covar = result_all,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
}


#' Calculate temporal dummy covariates
#' @description
#' Calculate temporal dummy covariates at point locations. Returns a
#' \code{data.frame} object with \code{locs_id}, year binary variable for each
#' value in \code{year}, and month and day of week binary variables.
#' @param locs data.frame with a temporal field named `"time"`
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @return a data.frame or SpatVector object
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_temporal_dummies(
#'   locs = loc,
#'   locs_id = "id",
#'   year = seq(2018L, 2022L)
#' )
#' }
#' @export
calculate_temporal_dummies <-
  function(
    locs,
    locs_id = "site_id",
    year = seq(2018L, 2022L),
    weights = NULL,
    geom = FALSE,
    ...
  ) {
    amadeus::check_unsupported_by(..., .call = sys.call())
    amadeus::check_geom(geom)
    if (!methods::is(locs, "data.frame")) {
      stop("Argument locs is not a data.frame.\n")
    }
    if (!"time" %in% names(locs)) {
      stop("A mandatory field 'time' does not exist in locs.\n")
    }
    dummify <- function(vec, domain) {
      vec_unique <- domain
      vec_split <- split(vec_unique, vec_unique)
      vec_assigned <-
        lapply(vec_split, function(x) {
          as.integer(vec == x)
        })
      dt_dum <- Reduce(cbind, vec_assigned)
      dt_dum <- data.table::as.data.table(dt_dum)
      return(dt_dum)
    }

    amadeus::calc_check_time(covar = locs, POSIXt = TRUE)
    # year
    vec_year <- data.table::year(locs$time)
    dt_year_dum <- dummify(vec_year, year)
    # should the last year be the present year or 2022?
    colnames(dt_year_dum) <-
      sprintf("DUM_Y%d_0_00000", year)

    # month
    vec_month <- data.table::month(locs$time)
    dt_month_dum <- dummify(vec_month, seq(1L, 12L))
    shortmn <-
      c(
        "JANUA",
        "FEBRU",
        "MARCH",
        "APRIL",
        "MAYMA",
        "JUNEJ",
        "JULYJ",
        "AUGUS",
        "SEPTE",
        "OCTOB",
        "NOVEM",
        "DECEM"
      )
    colnames(dt_month_dum) <-
      sprintf("DUM_%s_0_00000", shortmn)

    # weekday (starts from 0 - Sunday)
    vec_wday <- as.POSIXlt(locs$time)$wday
    # subtracting 1 due to the difference in the base
    dt_wday_dum <- dummify(vec_wday, seq(1L, 7L) - 1)
    colnames(dt_wday_dum) <-
      sprintf("DUM_WKDY%d_0_00000", seq(1L, 7L))

    # column binding
    locs_dums <-
      cbind(
        locs,
        dt_year_dum,
        dt_month_dum,
        dt_wday_dum
      )

    # geom
    locs_return <- amadeus::calc_return_locs(
      covar = locs_dums,
      POSIXt = TRUE,
      geom = geom,
      crs = "EPSG:4326"
    )
    return(locs_return)
  }


# nolint start
#' Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @param from `SpatVector`(1). Point locations which contain point-source
#' covariate data.
#' @param locs sf/SpatVector(1). Locations where the sum of exponentially
#' decaying contributions are calculated.
#' @param locs_id character(1). Name of the unique id field in `point_to`.
#' @param decay_range numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param target_fields character(varying). Field names in characters.
#' @param C0 `NULL`, character(1), or numeric vector of length `nrow(from)`.
#' Optional initial source values at pollutant locations. If `NULL`
#' (default), all source values are set to 1. If character(1), the value
#' is treated as a column name in `from` and used as source values.
#' @param use_threshold logical(1). If `TRUE` (default), include only source
#' points within \code{5 * decay_range} from each target location. If `FALSE`,
#' include all source points in `from`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @return a data.frame (tibble) or SpatVector object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "decay_range")``: the range where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/ropensci/chopin)
#' Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' \insertRef{messier2012integrating}{amadeus}
#'
#' \insertRef{web_sedctutorial_package}{amadeus}
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_locs <- terra::centroids(nc, inside = TRUE)
#' pnt_locs <- pnt_locs[, "NAME"]
#' pnt_from <- terra::spatSample(nc, 10L)
#' pnt_from$pid <- seq(1, 10)
#' pnt_from <- pnt_from[, "pid"]
#' pnt_from$val1 <- rgamma(10L, 1, 0.05)
#' pnt_from$val2 <- rgamma(10L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' sum_edc(pnt_locs, pnt_from, "NAME", 1e4, vals)
#' }
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom terra nearby
#' @importFrom terra distance
#' @importFrom terra buffer
#' @importFrom rlang sym
#' @export
# nolint end
sum_edc <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    decay_range = NULL,
    target_fields = NULL,
    C0 = NULL, # nolint: object_name_linter
    use_threshold = TRUE,
    geom = FALSE
  ) {
    amadeus::check_geom(geom)
    if (!methods::is(locs, "SpatVector")) {
      locs <- try(terra::vect(locs))
    }
    if (!methods::is(from, "SpatVector")) {
      from <- try(terra::vect(from))
    }

    if (!is.numeric(decay_range) || length(decay_range) != 1L ||
          is.na(decay_range) || decay_range <= 0) {
      stop("`decay_range` must be a single positive numeric value.\n")
    }
    if (!is.character(target_fields) || length(target_fields) < 1L) {
      stop("`target_fields` must be a non-empty character vector.\n")
    }
    if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
          is.na(use_threshold)) {
      stop("`use_threshold` must be TRUE or FALSE.\n")
    }
    missing_targets <- setdiff(target_fields, names(from))
    if (length(missing_targets) > 0L) {
      stop(
        "The following `target_fields` are missing in `from`: ",
        paste(missing_targets, collapse = ", "),
        "\n"
      )
    }
    c0_values <- C0
    if (is.null(c0_values)) {
      from$C0_source <- rep(1, nrow(from))
    } else {
      if (is.character(c0_values)) {
        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
          stop("`C0` as character must be a single non-empty column name.\n")
        }
        if (!c0_values %in% names(from)) {
          stop("`C0` column `", c0_values, "` was not found in `from`.\n")
        }
        c0_values <- from[[c0_values]]
      }
      if (is.data.frame(c0_values) && ncol(c0_values) == 1L) {
        c0_values <- c0_values[[1]]
      }
      if (!is.numeric(c0_values) || length(c0_values) != nrow(from)) {
        stop(
          "`C0` must be NULL, character(1) column name in `from`, ",
          "or a numeric vector with length `nrow(from)`.\n"
        )
      }
      if (length(target_fields) != 1L) {
        stop("When `C0` is provided, `target_fields` must have length 1.\n")
      }
      from$C0_source <- c0_values
    }

    cn_overlap <- intersect(names(locs), names(from))
    if (length(cn_overlap) > 0) {
      warning(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    len_point_locs <- seq_len(nrow(locs))
    threshold_distance <- if (use_threshold) decay_range * 5 else Inf

    locs$from_id <- len_point_locs
    if (use_threshold) {
      locs_buf <-
        terra::buffer(
          locs,
          width = threshold_distance,
          quadsegs = 90
        )
      from_in <- from[locs_buf, ]
    } else {
      from_in <- from
    }
    if (nrow(from_in) < 1L) {
      res_sedc <- data.frame(locs_id = terra::as.data.frame(locs)[[locs_id]])
      names(res_sedc)[1] <- locs_id
      for (target_field in target_fields) {
        res_sedc[[target_field]] <- 0
      }
      idx_air <- grep("_AIR_", names(res_sedc))
      names(res_sedc)[idx_air] <-
        sprintf("%s_%05d", names(res_sedc)[idx_air], decay_range)

      if (geom %in% c("sf", "terra")) {
        res_sedc <- merge(
          terra::as.data.frame(locs, geom = "WKT")[, c(locs_id, "geometry")],
          res_sedc,
          locs_id
        )
      }
      res_sedc_return <- amadeus::calc_return_locs(
        covar = res_sedc,
        POSIXt = TRUE,
        geom = geom,
        crs = terra::crs(from)
      )
      attr(res_sedc_return, "decay_range") <- decay_range
      attr(res_sedc_return, "sedc_threshold") <- threshold_distance
      return(res_sedc_return)
    }
    len_point_from <- seq_len(nrow(from_in))

    # len point from? len point to?
    from_in$to_id <- len_point_from
    dist <- NULL

    # near features with distance argument: only returns integer indices
    if (use_threshold) {
      res_nearby <-
        terra::nearby(locs, from_in, distance = threshold_distance)
    } else {
      res_nearby <-
        expand.grid(
          from_id = len_point_locs,
          to_id = len_point_from
        )
    }
    # attaching actual distance
    dist_nearby <- terra::distance(locs, from_in)
    dist_nearby_df <- as.vector(dist_nearby)
    # adding integer indices
    dist_nearby_tdf <-
      expand.grid(
        from_id = len_point_locs,
        to_id = len_point_from
      )
    dist_nearby_df <- cbind(dist_nearby_tdf, dist = dist_nearby_df)

    # summary
    res_sedc <- res_nearby |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(locs)) |>
      dplyr::left_join(data.frame(from_in)) |>
      dplyr::left_join(dist_nearby_df) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05 * (value at origin)
      dplyr::mutate(w_sedc = exp((-3 * dist) / decay_range)) |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          ~ sum(w_sedc * C0_source, na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup()
    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], decay_range)

    if (geom %in% c("sf", "terra")) {
      res_sedc <- merge(
        terra::as.data.frame(locs, geom = "WKT")[, c(locs_id, "geometry")],
        res_sedc,
        locs_id
      )
    }

    res_sedc_return <- amadeus::calc_return_locs(
      covar = res_sedc,
      POSIXt = TRUE,
      geom = geom,
      crs = terra::crs(from)
    )

    attr(res_sedc_return, "decay_range") <- decay_range
    attr(res_sedc_return, "sedc_threshold") <- threshold_distance

    return(res_sedc_return)
  }


#' Calculate toxic release covariates
#' @description
#' Calculate toxic release values for polygons or isotropic buffer point
#' locations. Returns a \code{data.frame} object containing \code{locs_id}
#' and variables for each processed TRI field in \code{from}. Target fields are
#' derived from metadata attached by \code{process_tri()}, with a fallback to
#' non-coordinate columns in \code{from}.
#' @param from SpatVector(1). Output of \code{process_tri()}.
#' @param locs sf/SpatVector. Locations where TRI variables are calculated.
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param decay_range Circular buffer radius.
#' Default is \code{c(1000, 10000, 50000)} (meters)
#' @param C0 `NULL` or character vector of column names in `from`.
#' Optional source-value columns used by `sum_edc()`. If `NULL` and
#' there is one TRI target field, that field is inferred with a warning.
#' If `NULL` and there are multiple TRI target fields, each TRI target field
#' is used as its own source values (for example `STACK_AIR_*`).
#' @param use_threshold logical(1). Passed to \code{sum_edc()}. If `TRUE`
#' (default), include only source points within \code{5 * decay_range}.
#' If `FALSE`, include all source points in `from`.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @return a data.frame or SpatVector object
#' @note U.S. context.
#' @seealso [`sum_edc`], [`process_tri`]
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
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_tri(
#'   from = tri, # derived from process_tri() example
#'   locs = loc,
#'   locs_id = "id",
#'   decay_range = c(1e3L, 1e4L, 5e4L)
#' )
#' }
#' @export
calculate_tri <- function(
  from = NULL,
  locs,
  locs_id = "site_id",
  decay_range = c(1e3L, 1e4L, 5e4L),
  C0 = NULL, # nolint: object_name_linter
  use_threshold = TRUE,
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }
  }
  if (!is.numeric(decay_range)) {
    stop("`decay_range` should be numeric.\n")
  }
  if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
        is.na(use_threshold)) {
    stop("`use_threshold` must be TRUE or FALSE.\n")
  }
  c0_input <- C0
  if (!is.null(c0_input) &&
        (!is.character(c0_input) || length(c0_input) < 1L ||
           anyNA(c0_input) ||
           any(!nzchar(trimws(c0_input))))) {
    stop("`C0` must be NULL or a non-empty character vector of column names.\n")
  }
  locs_re <- terra::project(locs, terra::crs(from))

  # split by year: locs and tri locations
  tri_cols <- attr(from, "tri_target_fields")
  if (is.null(tri_cols) || length(tri_cols) < 1) {
    tri_cols <- setdiff(names(from), c("YEAR", "LONGITUDE", "LATITUDE"))
  }
  # error fix: no whitespace
  tri_cols <- sub(" ", "_", tri_cols)
  if (length(tri_cols) < 1) {
    stop(
      "No TRI target fields found in `from`. ",
      "Process TRI data using `process_tri()` before calculation.\n"
    )
  }
  if (is.null(c0_input)) {
    if (length(tri_cols) == 1L) {
      warning(
        "`C0` is NULL and only one TRI field is available; ",
        "using `", tri_cols[1], "` as source values.\n"
      )
    }
    c0_cols <- tri_cols
  } else {
    missing_c0_cols <- setdiff(c0_input, names(from))
    if (length(missing_c0_cols) > 0L) {
      stop(
        "The following `C0` columns are missing in `from`: ",
        paste(missing_c0_cols, collapse = ", "),
        "\n"
      )
    }
    if (length(c0_input) == 1L) {
      c0_cols <- rep(c0_input, length(tri_cols))
    } else if (length(c0_input) == length(tri_cols)) {
      c0_cols <- c0_input
    } else {
      stop(
        "`C0` must have length 1 or match the number of TRI target fields (",
        length(tri_cols),
        ").\n"
      )
    }
  }

  # inner lapply
  list_decay_range <- split(decay_range, decay_range)
  list_locs_tri <-
    Map(
      function(x) {
        locs_tri_s <- Reduce(
          function(df_x, df_y) dplyr::full_join(df_x, df_y, by = locs_id),
          lapply(
            seq_along(tri_cols),
            function(i) {
              tri_col <- tri_cols[i]
              tri_col_c0 <- from[[c0_cols[i]]]
              if (is.data.frame(tri_col_c0) && ncol(tri_col_c0) == 1L) {
                tri_col_c0 <- tri_col_c0[[1]]
              }
              if (!is.numeric(tri_col_c0)) {
                stop("TRI target field `", tri_col, "` is not numeric.\n")
              }
              sum_edc(
                locs = locs_re,
                from = from,
                locs_id = locs_id,
                decay_range = x,
                target_fields = tri_col,
                C0 = tri_col_c0,
                use_threshold = use_threshold,
                geom = FALSE
              )
            }
          )
        )
        return(locs_tri_s)
      },
      list_decay_range
    )

  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  locs_df <- as.data.frame(locs)
  if (!locs_id %in% names(locs_df)) {
    stop("`locs_id` was not found in `locs`.\n")
  }
  if (geom %in% c("sf", "terra")) {
    locs_geom <- terra::as.data.frame(locs_re, geom = "WKT")
    if (!locs_id %in% names(locs_geom)) {
      stop("`locs_id` was not found in `locs` after CRS transformation.\n")
    }
    df_tri <- dplyr::left_join(
      locs_geom[, c(locs_id, "geometry"), drop = FALSE],
      df_tri,
      by = locs_id
    )
  } else if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(locs_df, df_tri, by = locs_id)
  }

  df_tri_return <- amadeus::calc_return_locs(
    covar = df_tri,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )

  # read attr
  df_tri_return$time <- as.integer(attr(from, "tri_year"))

  return(df_tri_return)
}


#' Calculate road emissions covariates
#' @param from SpatVector(1). Output of \code{process_nei()}.
#' @param locs sf/SpatVector. Locations at NEI values are joined.
#' @param locs_id character(1). Unique site identifier column name.
#' Unused but kept for compatibility.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song, Ranadeep Daw
#' @seealso [`process_nei`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom methods is
#' @importFrom terra project
#' @importFrom terra intersect
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_nei(
#'   from = nei, # derived from process_nei example
#'   locs = loc,
#'   locs_id = "id"
#' )
#' }
#' @export
calculate_nei <- function(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    locs <- try(terra::vect(locs))
    if (inherits(locs, "try-error")) {
      stop("locs is unable to be converted to SpatVector.\n")
    }
  }
  # spatial join
  locs_re <- terra::project(locs, terra::crs(from))
  locs_re <- terra::intersect(locs_re, from)

  # If returning geometry, keep as SpatVector
  if (geom %in% c("terra", "sf")) {
    if (geom == "terra") {
      return(locs_re)
    } else if (geom == "sf") {
      return(sf::st_as_sf(locs_re))
    }
  } else {
    # Only convert to data.frame if geom = FALSE
    locs_re <- as.data.frame(locs_re)
    return(locs_re)
  }
}

#' Calculate wildfire smoke covariates
#' @description
#' Extract wildfire smoke plume values at point or buffered locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date, and either binary
#' indicators (`frac = FALSE`) or fractional overlap values (`frac = TRUE`) for
#' wildfire smoke plume density inherited from \code{from}.
#' @param from SpatVector(1). Output of \code{process_hms()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param .by_time NULL or character(1). Optional time grouping key used
#' when \code{.by_time} is provided. When supplied, HMS indicators are
#' summarized by \code{sum} (smoke-day counts) for `frac = FALSE`, or
#' \code{mean} for `frac = TRUE`.
#' @param frac logical(1). Default `FALSE`. If `FALSE`, return binary 0/1 smoke
#' indicators by density class. If `TRUE`, return fractional overlap by density
#' class.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso [process_hms()]
#' @author Mitchell Manware
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect as.data.frame time extract crs
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of
#' @importFrom stats setNames
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_hms(
#'   from = hms, # derived from process_hms() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_hms <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  frac = FALSE,
  geom = FALSE,
  ...
) {
  #### check for null parameters (.by_time is optional)
  params_check <- mget(ls())
  params_check[c(".by_time", "weights")] <- NULL
  amadeus::check_for_null_parameters(params_check)
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
    stop("`frac` should be a single logical value (TRUE/FALSE).")
  }
  #### from == character indicates no wildfire smoke plumes are present
  #### return 0 for all densities, locs and dates
  if (is.character(from)) {
    amadeus::check_geom(geom)
    message(paste0(
      "Inherited list of dates due to absent smoke plume polygons.\n"
    ))
    zero_value <- if (isTRUE(frac)) 0 else 0L
    skip_df <- data.frame(
      as.POSIXlt(from),
      zero_value,
      zero_value,
      zero_value
    )
    colnames(skip_df) <- c(
      "time",
      paste0("light_", sprintf("%05d", radius)),
      paste0("medium_", sprintf("%05d", radius)),
      paste0("heavy_", sprintf("%05d", radius))
    )
    # fixed: locs is replicated per the length of from
    skip_merge <-
      Reduce(
        rbind,
        Map(
          function(x) {
            cbind(locs, skip_df[rep(x, nrow(locs)), ])
          },
          seq_len(nrow(skip_df))
        )
      )

    if (!is.null(.by_time)) {
      hms_fun_summary <- if (isTRUE(frac)) "mean" else "sum"
      skip_merge <- amadeus::calc_summarize_by(
        covar = skip_merge,
        .by_time = .by_time,
        fun_summary = hms_fun_summary,
        locs_id = locs_id
      )
      did_summarize <- TRUE
    } else {
      did_summarize <- FALSE
    }
    if (did_summarize && "time" %in% names(skip_merge)) {
      skip_merge$time <- as.POSIXct(skip_merge$time, tz = "UTC")
    }
    skip_return <- amadeus::calc_return_locs(
      skip_merge,
      POSIXt = TRUE,
      geom = geom,
      crs = "EPSG:4326"
    )
    return(skip_return)
  }
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]

  #### generate date sequence for missing polygon patch
  date_sequence <- amadeus::generate_date_sequence(
    date_start = as.Date(
      from$Date[1],
      format = "%Y%m%d"
    ),
    date_end = as.Date(
      from$Date[nrow(from)],
      format = "%Y%m%d"
    ),
    sub_hyphen = FALSE
  )

  ### split date_sequence by 30 days
  date_sequence_split <-
    split(date_sequence, ceiling(seq_along(date_sequence) / 30))
  return_list <- vector("list", length(date_sequence_split))

  ### extract layer data at sites
  for (i in seq_along(date_sequence_split)) {
    message(paste0(
      "Calculating smoke intensity covariates for date range: ",
      date_sequence_split[[i]][1],
      " to ",
      date_sequence_split[[i]][length(date_sequence_split[[i]])]
    ))

    ### Full SPT
    data_template <- expand.grid(
      id = sites_id[[locs_id]],
      time = date_sequence_split[[i]]
    )
    data_template <- stats::setNames(data_template, c(locs_id, "time"))
    from_sub <- from[from$Date %in% date_sequence_split[[i]], ]

    is_point_locs <- all(
      tolower(terra::geomtype(sites_e)) %in% c("points", "point")
    )
    if (nrow(from_sub) == 0) {
      sites_extracted_layer <- data.frame(
        setNames(list(character(0)), locs_id),
        Date = character(0),
        Density = character(0),
        base_value = numeric(0)
      )
    } else if (radius == 0 && is_point_locs) {
      sites_extracted_layer <- terra::extract(from_sub, sites_e)
      sites_extracted_layer$id.y <-
        unlist(sites_e[[locs_id]])[sites_extracted_layer$id.y]
      names(sites_extracted_layer)[
        names(sites_extracted_layer) == "id.y"
      ] <- locs_id
      sites_extracted_layer$base_value <- 1
    } else {
      intersections <- terra::intersect(sites_e, from_sub)
      if (nrow(intersections) > 0) {
        inter_area <- terra::expanse(intersections)
        sites_extracted_layer <- terra::as.data.frame(intersections)
        if (isTRUE(frac)) {
          site_area <- terra::expanse(sites_e)
          site_lookup <- setNames(site_area, as.character(sites_e[[locs_id]]))
          denom <- as.numeric(
            site_lookup[as.character(sites_extracted_layer[[locs_id]])]
          )
          denom[!is.finite(denom) | denom <= 0] <- NA_real_
          sites_extracted_layer$base_value <- inter_area / denom
          sites_extracted_layer$base_value[
            !is.finite(sites_extracted_layer$base_value)
          ] <- 0
        } else {
          sites_extracted_layer$base_value <- 1
        }
      } else {
        sites_extracted_layer <- data.frame(
          setNames(list(character(0)), locs_id),
          Date = character(0),
          Density = character(0),
          base_value = numeric(0)
        )
      }
    }

    # remove duplicates and aggregate by site/date/density
    if (nrow(sites_extracted_layer) > 0) {
      sites_extracted_layer <- unique(
        sites_extracted_layer[, c(locs_id, "Date", "Density", "base_value")]
      )
      sites_extracted_layer <- stats::aggregate(
        base_value ~ .,
        data = sites_extracted_layer,
        FUN = sum
      )
      if (!isTRUE(frac)) {
        sites_extracted_layer$base_value <- as.integer(
          sites_extracted_layer$base_value > 0
        )
      } else {
        sites_extracted_layer$base_value <- pmin(
          sites_extracted_layer$base_value,
          1
        )
      }
    }

    #### merge with site_id and date
    sites_extracted_layer <-
      tidyr::pivot_wider(
        data = sites_extracted_layer,
        names_from = "Density",
        values_from = "base_value",
        id_cols = dplyr::all_of(c(locs_id, "Date")),
        values_fill = list(base_value = 0)
      )

    # Fill in missing columns
    levels_acceptable <- c("Light", "Medium", "Heavy")
    # Detect missing columns
    col_tofill <-
      setdiff(levels_acceptable, names(sites_extracted_layer))
    # Fill zeros
    if (length(col_tofill) > 0) {
      sites_extracted_layer[col_tofill] <- if (isTRUE(frac)) 0 else 0L
    }
    col_order <- c(locs_id, "Date", levels_acceptable)
    sites_extracted_layer <- sites_extracted_layer[, col_order]
    sites_extracted_layer <-
      stats::setNames(
        sites_extracted_layer,
        c(locs_id, "time", levels_acceptable)
      )

    binary_colname <-
      paste0(tolower(levels_acceptable), "_", sprintf("%05d", radius))
    # sites_extracted_layer$time <- as.Date(sites_extracted_layer$time)
    sites_extracted_layer <-
      stats::setNames(
        sites_extracted_layer,
        c(locs_id, "time", binary_colname)
      )

    # Filling NAs to 0 (explicit integer)
    # sites_extracted_layer[is.na(sites_extracted_layer)] <- 0L

    # Join full space-time pairs with extracted data
    site_extracted <- merge(
      data_template,
      sites_extracted_layer,
      by = c(locs_id, "time"),
      all.x = TRUE
    )
    # append list with the extracted data.frame
    return_list[[i]] <- site_extracted
  }

  ### Merge data.frame in list
  sites_extracted <- do.call(rbind, return_list)

  #### define column names
  colname_common <- c(locs_id, "time", binary_colname)
  if (geom %in% c("sf", "terra")) {
    sites_extracted <-
      merge(sites_extracted, sites_id, by = locs_id)
    sites_extracted <-
      stats::setNames(
        sites_extracted,
        c(colname_common, "geometry")
      )
  } else {
    sites_extracted <-
      stats::setNames(
        sites_extracted,
        colname_common
      )
  }
  # Filling NAs to 0 for smoke columns
  for (smoke_col in binary_colname) {
    sites_extracted[[smoke_col]][is.na(sites_extracted[[smoke_col]])] <-
      if (isTRUE(frac)) 0 else 0L
  }

  if (!is.null(.by_time)) {
    hms_fun_summary <- if (isTRUE(frac)) "mean" else "sum"
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = hms_fun_summary,
      locs_id = locs_id
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }

  # Messaging
  timevals <- sites_extracted[["time"]]
  intensities <- sites_extracted[, binary_colname, drop = FALSE]
  intensities <- apply(intensities, 1, sum)
  time_allzero <- unique(timevals[intensities == 0])
  time_allzero_c <- paste(time_allzero, collapse = "\n")
  message(paste0(
    "No intersecting smoke plume polygons for date(s):\n",
    time_allzero_c
  ))

  #### date to POSIXct
  if ("time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time)
  }
  #### order by date
  sites_extracted_ordered <- as.data.frame(
    sites_extracted[order(sites_extracted$time), ]
  )
  message("Returning smoke intensity covariates.")
  sites_extracted_ordered <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_extracted_ordered)
}


#' Calculate elevation covariates
#' @description
#' Extract elevation values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, year of release, and elevation variable.
#' Elevation variable column name follows the pattern
#' \code{gmted_<radius>} (for example, \code{gmted_0} or \code{gmted_100}).
#' @param from SpatRaster(1). Output from \code{process_gmted()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [`process_gmted()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_gmted(
#'   from = gmted, # derived from process_gmted() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_gmted <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "gmted",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 2,
    time = 3,
    time_type = "year",
    weights = weights
  )
  #### variable column name
  variable_name <- paste0("gmted_", as.integer(radius))
  if (geom %in% c("sf", "terra")) {
    #### convert integer to numeric
    sites_extracted[, 4] <- as.numeric(sites_extracted[, 4])
    names(sites_extracted) <- c(locs_id, "geometry", "time", variable_name)
  } else {
    #### convert integer to numeric
    sites_extracted[, 3] <- as.numeric(sites_extracted[, 3])
    names(sites_extracted) <- c(locs_id, "time", variable_name)
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}


#' Calculate meteorological covariates
#' @description
#' Extract meteorological values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id}, date, vertical pressure level, and
#' meteorological variable. Meteorological variable column name reflects
#' variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_narr()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#' when \code{.by_time} is provided.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [`process_narr`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_narr(
#'   from = narr, # derived from process_narr() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_narr <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### identify pressure level or monolevel data
  if (grepl("level", names(from)[1])) {
    narr_time <- 3
    narr_level <- 2
  } else {
    narr_time <- 2
    narr_level <- NULL
  }
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "narr",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = narr_time,
    time_type = "date",
    level = narr_level,
    weights = weights,
    ...
  )
  narr_group_extra <- if (!is.null(narr_level)) "level" else NULL
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = narr_group_extra
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
    }
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}


#' Calculate atmospheric composition covariates
#' @description
#' Extract atmospheric composition values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and atmospheric composition variable. Atmospheric
#' composition variable column name reflects variable and circular buffer
#' radius.
#' @param from SpatRaster(1). Output of \code{process_geos()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#' when \code{.by_time} is provided.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @seealso [process_geos()]
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_geos(
#'   from = geos, # derived from process_geos() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_geos <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "geos",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = c(3, 4),
    time_type = "hour",
    level = 2,
    weights = weights,
    ...
  )
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = "level"
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }
  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate population density covariates
#' @description
#' Extract population density values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, year, and population
#' density variable. Population density variable column name reflects
#' spatial resolution of \code{from} and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_population()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [process_population()]
#' @return a data.frame or SpatVector object
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_population(
#'   from = pop, # derived from process_population() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_population <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### message information
  name_split <- strsplit(
    names(from),
    "_"
  )[[1]]
  message(
    paste0(
      "Calculating population covariates for ",
      name_split[4],
      " at ",
      amadeus::process_sedac_codes(
        paste0(
          name_split[5],
          "_",
          name_split[6]
        ),
        invert = TRUE
      ),
      " resolution...\n"
    )
  )
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "skip",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 3,
    time = 4,
    time_type = "year",
    weights = weights,
    ...
  )
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}


#' Calculate roads covariates
#' @description Prepared groads data is clipped with the buffer polygons
#' of `radius`. The total length of the roads are calculated.
#' Then the density of the roads is calculated by dividing
#' the total length from the area of the buffer. `terra::linearUnits()`
#' is used to convert the unit of length to meters.
#' @param from SpatVector(1). Output of `process_groads`.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 1000).
#' @param fun function(1). Function used to summarize the length of roads
#' within sites location buffer (Default is `sum`).
#' @param drop logical(1). Should locations with zero roads in the extraction
#' buffer be dropped from results? Default is `FALSE` (retain all locations).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
# nolint start
#' @note Unit is km / sq km. The returned `data.frame` object contains a
#' `$time` column to represent the temporal range covered by the
#' dataset. For more information, see <https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.
# nolint end
#' @author Insang Song
#' @seealso [`process_groads`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom stats aggregate
#' @importFrom stats setNames
#' @importFrom terra as.data.frame
#' @importFrom terra project
#' @importFrom terra intersect
#' @importFrom terra perim
#' @importFrom terra crs
#' @importFrom terra expanse
#' @importFrom terra linearUnits
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_groads(
#'   from = groads, # derived from process_groads() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 1000,
#'   fun = "sum",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_groads <- function(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = 1000,
  fun = "sum",
  drop = FALSE,
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### check for null parameters
  if (radius <= 0) {
    stop("radius should be greater than 0.\n")
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop("`drop` should be a single logical value (TRUE/FALSE).")
  }
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]

  from_re <- terra::project(from, terra::crs(sites_e))
  from_re <- from[terra::ext(sites_e), ]
  from_clip <- terra::intersect(sites_e, from_re)
  area_buffer <- sites_e[1, ]
  area_buffer <- terra::expanse(area_buffer)

  # assign road lengths to rlength field
  from_clip$rlength <- terra::perim(from_clip)
  from_clip <-
    aggregate(
      from_clip$rlength,
      by = from_clip[[locs_id]],
      FUN = fun,
      na.rm = TRUE
    )
  # linear unit conversion
  # if no unit is detected, set to 1
  det_unit <- terra::linearUnits(from_re)
  if (det_unit == 0) {
    det_unit <- 1
  }
  total_name <- sprintf("GRD_TOTAL_%05d", radius)
  density_name <- sprintf("GRD_DENKM_%05d", radius)

  # km / sq km
  if (nrow(from_clip) > 0) {
    from_clip[["x"]] <- (from_clip[["x"]] * det_unit / 1e3)
    from_clip$density <-
      from_clip[["x"]] / (area_buffer * (det_unit^2) / 1e6)
    from_clip <-
      setNames(
        from_clip,
        c(
          locs_id,
          total_name,
          density_name
        )
      )
    from_clip <- data.frame(from_clip)
  } else {
    from_clip <- data.frame(sites_list[[2]])[0, locs_id, drop = FALSE]
    from_clip[[total_name]] <- numeric(0)
    from_clip[[density_name]] <- numeric(0)
  }
  from_clip$description <- "1980 - 2010"

  if (geom %in% c("sf", "terra")) {
    sites_geom <- data.frame(sites_list[[2]])
    from_clip <- merge(
      x = sites_geom,
      y = from_clip[, c(locs_id, "description", total_name, density_name)],
      by = locs_id,
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    sites_id <- data.frame(sites_list[[2]])[, locs_id, drop = FALSE]
    from_clip <- merge(
      x = sites_id,
      y = from_clip[, c(locs_id, "description", total_name, density_name)],
      by = locs_id,
      all.x = TRUE,
      sort = FALSE
    )
  }

  from_clip[[total_name]][is.na(from_clip[[total_name]])] <- 0
  from_clip[[density_name]][is.na(from_clip[[density_name]])] <- 0
  from_clip$description[is.na(from_clip$description)] <- "1980 - 2010"

  if (drop) {
    from_clip <- from_clip[from_clip[[total_name]] > 0, , drop = FALSE]
  }

  if (geom %in% c("sf", "terra")) {
    from_clip_reorder <- from_clip[, c(
      locs_id, "geometry", "description", total_name, density_name
    )]
  } else {
    #### reorder
    from_clip_reorder <- from_clip[, c(
      locs_id, "description", total_name, density_name
    )]
  }
  sites_return <- amadeus::calc_return_locs(
    covar = from_clip_reorder,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate meteorological and atmospheric covariates
#' @description
#' Extract meteorological and atmospheric values at point locations. Returns a
#' \code{data.frame} object containing \code{locs_id}, date and hour, vertical
#' pressure level, and meteorological or atmospheric variable. Variable column
#' name reflects variable and circular buffer radius.
#' @param from SpatRaster(1). Output of \code{process_merra2()}.
#' @param locs data.frame, characater to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders
#' @author Mitchell Manware
#' @seealso [calculate_geos()], [process_merra2()]
#' @return a data.frame or SpatVector object. When \code{.by_time} is provided,
#'   rows are aggregated using \code{calc_summarize_by()}.
#' @importFrom terra vect
#' @importFrom terra buffer
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_merra2(
#'   from = merra2, # derived from process_merra2() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_merra2 <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### identify pressure level or monolevel data
  merra2_name <- strsplit(names(from)[1], "_")[[1]]
  if (grepl("lev", names(from)[1])) {
    merra2_time <- c(3, 4)
    merra2_level <- 2
    merra2_time_type <- "hour"
  } else if (length(merra2_name) == 2) {
    merra2_time <- 2
    merra2_level <- NULL
    merra2_time_type <- "date"
  } else {
    merra2_time <- c(2, 3)
    merra2_level <- NULL
    merra2_time_type <- "hour"
  }
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "merra2",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = merra2_time,
    time_type = merra2_time_type,
    level = merra2_level,
    weights = weights,
    ...
  )
  #### optional `.by_time` summarization
  merra2_group_extra <- if (!is.null(merra2_level)) "level" else NULL
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id,
      group_cols_extra = merra2_group_extra
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }
  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate gridMET covariates
#' @description
#' Extract gridMET values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and gridMET variable. gridMET variable
#' column name reflects the gridMET variable and circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_gridmet()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Mitchell Manware
#' @seealso [`process_gridmet()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_gridmet(
#'   from = gridmet, # derived from process_gridmet() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_gridmet <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "gridmet",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = 2,
    time_type = "date",
    weights = weights,
    ...
  )
  by_time_resolved <- if (is.null(.by_time)) "day" else .by_time
  sites_extracted <- amadeus::calc_summarize_by(
    covar = sites_extracted,
    .by_time = by_time_resolved,
    fun_summary = "mean",
    locs_id = locs_id
  )
  if ("time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate TerraClimate covariates
#' @description
#' Extract TerraClimate values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and TerraClimate variable. TerraClimate
#' variable column name reflects the TerraClimate variable and
#' circular buffer radius. The `$time` column will contain the year and month
#' ("YYYYMM") as TerraClimate products have monthly temporal resolution.
#' @param from SpatRaster(1). Output from \code{process_terraclimate()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param fun character(1). Function used to summarize multiple raster cells
#' within sites location buffer (Default = `mean`).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @note
#' TerraClimate data has monthly temporal resolution, so the `$time` column
#' will contain the year and month in YYYYMM format (ie. January, 2018 =
#' 201801).
#' @author Mitchell Manware
#' @seealso [`process_terraclimate()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_terraclimate <- function(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "terraclimate",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = 2,
    time_type = "yearmonth",
    weights = weights,
    ...
  )
  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

# nolint start
#' Calculate temporally lagged covariates
#' @description
#' The \code{calculate_lagged()} function calculates daily temporal lagged covariates
#' from the output of \code{calculate_covariates()} or \code{calc_*()}.
#' @param from data.frame(1). A `data.frame` containing calculated covariates
#' returned from \code{calculate_covariates()} or \code{calc_*()}.
#' @param date character(2). Start and end dates of desired lagged covariates.
#' Length of 10 each, format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param lag integer(1). Number of lag days.
#' @param time_id character(1). Column containing time values.
#' @param locs_id character(1). Name of unique identifier.
#' @param geom logical(1). Should the function return a `SpatVector`?
#' Default is `FALSE`. The coordinate reference system of the `SpatVector` is
#' that of `from.` To return as a `SpatVector`, `from` must also be a `SpatVector`
#' @seealso [calculate_covariates()]
#' @note
#' In order to calculate temporally lagged covariates, `from` must contain at
#' least the number of lag days before the desired start date. For example, if
#' `date = c("2024-01-01", "2024-01-31)` and `lag = 1`, `from` must contain data
#' starting at 2023-12-31.
#' If `from` contains geometry features, `calculate_lagged` will return a column
#' with geometry features of the same name.
#' \code{calculate_lagged()} assumes that all columns other than `time_id`,
#' `locs_id`, and fixed columns of "lat" and "lon", follow the genre, variable,
#' lag, buffer radius format adopted in \code{calc_setcolumns()}.
#' @return a `data.frame` object
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' terracliamte_covar <- calculate_terraclimate(
#'   from = terraclimate, # derived from process_terraclimate() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean",
#'   geom = FALSE
#' )
#' calculate_lagged(
#'   from = terracliamte_covar,
#'   locs_id = "id",
#'   date = c("2023-01-02", "2023-01-10"),
#'   lag = 1,
#'   time_id = "time"
#' )
#' }
# nolint end
#' @export
calculate_lagged <- function(
  from,
  date,
  lag,
  locs_id,
  time_id = "time",
  geom = FALSE
) {
  amadeus::check_geom(geom)
  #### check years
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### geom and from
  if (geom %in% c("sf", "terra") && !("SpatVector" %in% class(from))) {
    stop(
      paste0(
        "To return with geometry, `from` must be a `SpatVector` object.\n"
      )
    )
  }
  #### check input data types
  if ("SpatVector" %in% class(from)) {
    from_full <- terra::as.data.frame(from, geom = "WKT")
    geoms <- unique(from_full[, c(locs_id, "geometry")])
    from <- from_full |> dplyr::select(-"geometry")
  }
  stopifnot(methods::is(from, "data.frame"))
  #### check if time_id is not null
  stopifnot(!is.null(time_id))
  #### return from if lag == 0
  if (lag == 0) {
    message("`lag` set to 0. Returning `from`.\n")
    return(from)
  }
  #### extract times
  time <- as.character(from[[time_id]])
  dateseq <- seq(as.Date(date[1]) - lag, as.Date(date[2]), by = 1)
  dateseq <- as.character(dateseq)
  align <- setdiff(dateseq, unique(time))
  ### check temporal alignment
  if (length(align) > 0) {
    stop(
      "Dates requested in `date` do not align with data available in `from`."
    )
  }
  unique_locs <- unique(from[[locs_id]])
  variables_merge <- NULL
  for (u in seq_along(unique_locs)) {
    from_u <- subset(
      from,
      from[[locs_id]] == unique_locs[u]
    )
    time_u <- from_u[[time_id]]
    #### extract variables
    variables <- from_u[,
      !(names(from_u) %in% c(locs_id, time_id)),
      drop = FALSE
    ]
    #### apply lag using dplyr::lag
    variables_lag <- dplyr::lag(variables, lag, default = NA)
    colnames(variables_lag) <- gsub(
      paste0("_[0-9]{1}_"),
      paste0("_", lag, "_"),
      colnames(variables_lag)
    )
    #### create the return dataframe
    variables_return <- cbind(from_u[[locs_id]], time_u, variables_lag)
    colnames(variables_return)[1:2] <- c(locs_id, time_id)
    #### identify dates of interest
    date_sequence <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = FALSE
    )
    #### filter to dates of interest
    variables_return_date <- variables_return[time_u %in% date_sequence, ]
    #### merge with other locations
    variables_merge <- rbind(variables_merge, variables_return_date)
  }
  if (geom %in% c("sf", "terra")) {
    variables_merge <- merge(variables_merge, geoms)
  }
  variables_return <- amadeus::calc_return_locs(
    covar = variables_merge,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(variables_return)
}

# prism, cropscape, huc
#' Calculate PRISM covariates
#' @description
#' Extract PRISM values at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and PRISM variable. PRISM
#' variable column name reflects the PRISM variable and
#' circular buffer radius.
#' @param from SpatRaster(1). Output from \code{process_prism()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_prism()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra time
#' @importFrom terra extract
#' @importFrom terra nlyr
#' @importFrom terra crs
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_prism(
#'   from = prism, # derived from process_prism() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_prism <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  # check input class
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
  }
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction

  message(
    sprintf(
      "Calculating PRISM covariates with %d meters radius...",
      radius
    )
  )

  # extract
  is_polygon_locs <- inherits(sites_e, "SpatVector") &&
    !all(tolower(terra::geomtype(sites_e)) %in% c("points", "point"))
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = "mean",
    weighted = !is.null(weights_prepared)
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    # use terra::extract for point locations
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
  } else {
    # use exactextractr::exact_extract for polygon locations and buffered points
    sites_e_sf <- sf::st_as_sf(sites_e)
    sites_e_buf <- if (radius > 0) {
      sf::st_buffer(sites_e_sf, dist = radius)
    } else {
      sites_e_sf
    }
    extract_args <- c(
      list(
        x = from,
        y = sites_e_buf,
        fun = fun_extract,
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)
  }

  # clean up names if they are from exact_extract (prefix "mean.")
  if (radius > 0 || is_polygon_locs) {
    exact_names <- colnames(sites_extracted)
    if (length(exact_names) == 1 && identical(exact_names, "mean")) {
      exact_names <- names(from)[1]
    } else {
      exact_names <- gsub("^mean\\.", "", exact_names)
    }
    colnames(sites_extracted) <- exact_names
  }

  # append radius
  new_names <- sprintf("%s_%d", colnames(sites_extracted), radius)
  colnames(sites_extracted) <- new_names

  # Combine with IDs
  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  # reorder to put ID first
  sites_extracted <- sites_extracted[, c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )]

  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    if (!"time" %in% names(sites_extracted)) {
      value_cols_now <- setdiff(names(sites_extracted), c(locs_id, "geometry"))
      if (length(value_cols_now) != 1L) {
        stop(
          "PRISM `.by_time` summarization requires a single covariate column ",
          "or an existing `time` column.\n"
        )
      }
      prism_time <- NA
      time_vals <- try(terra::time(from), silent = TRUE)
      if (
        !inherits(time_vals, "try-error") &&
          length(time_vals) >= 1L &&
          !is.na(time_vals[1])
      ) {
        prism_time <- time_vals[1]
      }
      if (is.na(prism_time)) {
        meta <- try(terra::metags(from), silent = TRUE)
        if (
          !inherits(meta, "try-error") &&
            is.data.frame(meta) &&
            nrow(meta) > 0
        ) {
          idx_time <- which(meta[, 1] == "time")
          if (length(idx_time) == 1L) {
            time_raw <- meta[idx_time, 2]
            if (grepl("^[0-9]{8}$", time_raw)) {
              prism_time <- as.Date(time_raw, format = "%Y%m%d")
            } else if (grepl("^[0-9]{6}$", time_raw)) {
              prism_time <- as.Date(paste0(time_raw, "01"), format = "%Y%m%d")
            } else if (grepl("^[0-9]{4}$", time_raw)) {
              prism_time <- as.Date(paste0(time_raw, "-01-01"))
            }
          }
        }
      }
      if (is.na(prism_time)) {
        stop(
          "Could not derive PRISM time for `.by_time` summarization. ",
          "Provide data with explicit time in layer metadata.\n"
        )
      }
      sites_extracted$time <- prism_time
    }
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_return)
}

#' Calculate EDGAR covariates
#' @description
#' Extract EDGAR gridded emissions values at point locations. For
#' `radius = 0`, cell values are extracted directly. For `radius > 0`,
#' means are calculated over a circular buffer around each location.
#' @param from SpatRaster(1). Output from \code{process_edgar()}.
#' @param locs data.frame, character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file containing
#'   identifier for each unique coordinate location.
#' @param radius numeric(1). Circular buffer distance around site locations.
#'   Default is `0`.
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Mariana Alifa Kassien, Insang Song
#' @seealso [`process_edgar()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra extract crs
#' @importFrom sf st_as_sf st_buffer
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
#' ##       not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_edgar(
#'   from = edgar, # derived from process_edgar() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_edgar <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
  }
  if (!is.numeric(radius) || length(radius) != 1) {
    stop("`radius` must be numeric(1).")
  }

  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]

  is_polygon_locs <- inherits(sites_e, "SpatVector") &&
    !all(tolower(terra::geomtype(sites_e)) %in% c("points", "point"))
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = "mean",
    weighted = !is.null(weights_prepared)
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
  } else {
    if (inherits(sites_e, "SpatVector")) {
      sites_e <- sf::st_as_sf(sites_e)
    }
    extract_args <- c(
      list(
        x = from,
        y = sites_e,
        fun = fun_extract,
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)
    exact_names <- names(sites_extracted)
    if (length(exact_names) == 1 && identical(exact_names, "mean")) {
      exact_names <- names(from)[1]
    } else {
      exact_names <- gsub("^mean\\.", "", exact_names)
    }
    names(sites_extracted) <- exact_names
  }

  names(sites_extracted) <- sprintf("%s_%d", names(sites_extracted), radius)
  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  ordered_cols <- c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )
  sites_extracted <- sites_extracted[, ordered_cols]

  posixt_out <- FALSE
  if (!is.null(.by_time)) {
    if (!"time" %in% names(sites_extracted)) {
      value_cols_now <- setdiff(names(sites_extracted), c(locs_id, "geometry"))
      if (length(value_cols_now) != 1L) {
        stop(
          "EDGAR `.by_time` summarization requires a single covariate column ",
          "or an existing `time` column.\n"
        )
      }
      edgar_time <- NA
      time_vals <- try(terra::time(from), silent = TRUE)
      if (!inherits(time_vals, "try-error") && length(time_vals) >= 1L) {
        edgar_time <- time_vals[1]
      }
      if (is.na(edgar_time)) {
        stop(
          "Could not derive EDGAR time for `.by_time` summarization. ",
          "Provide data with explicit time in layer metadata.\n"
        )
      }
      sites_extracted$time <- edgar_time
    }
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    if ("time" %in% names(sites_extracted)) {
      sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
      posixt_out <- TRUE
    }
  }

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = posixt_out,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}

#' Calculate Cropscape covariates
#' @description
#' Extract Cropscape (CDL) values at point locations.
#' Returns a \code{data.frame}
#' object containing \code{locs_id} and crop specific cell fractions.
#' @param from SpatRaster(1). Output from \code{process_cropscape()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param radius integer(1). Circular buffer distance around site locations.
#' (Default = 0).
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_cropscape()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra extract
#' @importFrom terra crs
#' @importFrom sf st_as_sf st_buffer
#' @importFrom exactextractr exact_extract
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_cropscape(
#'   from = cropscape, # derived from process_cropscape() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   geom = FALSE
#' )
#' }
#' @export
calculate_cropscape <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0,
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]

  if (!inherits(from, "SpatRaster")) {
    stop("`from` must be a SpatRaster object.")
  }

  message(
    sprintf(
      "Calculating Cropscape covariates with %d meters radius...",
      radius
    )
  )

  # extract
  is_polygon_locs <- inherits(sites_e, "SpatVector") &&
    !all(tolower(terra::geomtype(sites_e)) %in% c("points", "point"))
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  if (radius == 0 && !is_polygon_locs && is.null(weights_prepared)) {
    # terra::extract for point locations
    sites_extracted <- terra::extract(from, sites_e)
    sites_extracted <- sites_extracted[, -1, drop = FALSE]
    # rename
    colnames(sites_extracted) <- paste0("cropscape_", radius)
  } else {
    sites_e_sf <- sf::st_as_sf(sites_e)
    sites_e_buf <- if (radius > 0) {
      sf::st_buffer(sites_e_sf, dist = radius)
    } else {
      sites_e_sf
    }

    # fractions
    extract_args <- c(
      list(
        x = from,
        y = sites_e_buf,
        fun = "frac",
        force_df = TRUE,
        progress = FALSE
      ),
      list(...)
    )
    if (!is.null(weights_prepared)) {
      extract_args$weights <- weights_prepared
    }
    sites_extracted <- do.call(exactextractr::exact_extract, extract_args)

    colnames(sites_extracted) <- gsub(
      "frac_",
      sprintf("cropscape_%d_", radius),
      colnames(sites_extracted)
    )
  }

  sites_extracted[[locs_id]] <- sites_id[, 1]
  if (
    "geometry" %in% names(sites_id) && !"geometry" %in% names(sites_extracted)
  ) {
    sites_extracted$geometry <- sites_id$geometry
  }
  sites_extracted <- sites_extracted[, c(
    locs_id,
    if ("geometry" %in% names(sites_extracted)) "geometry",
    setdiff(names(sites_extracted), c(locs_id, "geometry"))
  )]

  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}

#' Calculate HUC covariates
#' @description
#' Extract HUC IDs at point locations. Returns a \code{data.frame}
#' object containing \code{locs_id} and HUC IDs.
#' @param from SpatVector(1). Output from \code{process_huc()}.
#' @param locs data.frame. character to file path, SpatVector, or sf object.
#' @param locs_id character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @author Insang Song
#' @seealso [`process_huc()`]
#' @return a data.frame or SpatVector object
#' @importFrom terra vect
#' @importFrom terra intersect
#' @importFrom terra crs
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
#' calculate_huc(
#'   from = huc, # derived from process_huc() example
#'   locs = loc,
#'   locs_id = "id",
#'   geom = FALSE
#' )
#' }
#' @export
calculate_huc <- function(
  from,
  locs,
  locs_id = "site_id",
  weights = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  if (!inherits(from, "SpatVector")) {
    stop("`from` must be the output of process_huc().")
  }

  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = 0, # radius irrelevant for point-in-poly usually, or treated as 0
    geom = geom
  )
  sites_e <- sites_list[[1]]

  message("Calculating HUC covariates...")

  # intersect
  sites_extracted <- terra::intersect(sites_e, from)
  sites_df <- terra::as.data.frame(sites_extracted)

  # Remove valid geometry columns if present
  cols_to_keep <- c(locs_id, setdiff(names(sites_df), locs_id))
  sites_df <- sites_df[, cols_to_keep, drop = FALSE]

  sites_return <- amadeus::calc_return_locs(
    covar = sites_df,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}

################################################################################
# nolint start
#' Calculate NOAA GOES ADP covariates
#' @description
#' Extract NOAA GOES Aerosol Detection Product (ADP) values at point
#' locations from a \code{SpatRaster} returned by \code{process_goes()}.
#' Returns a \code{data.frame} (or \code{SpatVector} / \code{sf}) containing
#' \code{locs_id}, \code{time}, and the extracted variable column
#' (\code{{variable}_{radius}}).
#' @param from SpatRaster(1). Output from \code{process_goes()}.
#' @param locs data.frame, character file path, \code{SpatVector}, or
#'   \code{sf} object with point locations.
#' @param locs_id character(1). Column name for unique location identifier.
#' @param radius integer(1). Circular buffer radius in metres around each
#'   site (default 0 = point extraction).
#' @param fun character(1). Summary function for buffered extractions
#'   (default \code{"mean"}).
#' @param .by_time NULL or character(1). Optional time grouping key used
#'   with \code{.by_time} for temporal summaries.
#' @param geom \code{FALSE}/\code{"sf"}/\code{"terra"}. Return geometry with
#'   results. Default \code{FALSE}. The CRS is inherited from \code{from}.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Placeholders.
#' @seealso \code{\link{process_goes}}
#' @author Mitchell Manware
#' @return a \code{data.frame} or \code{SpatVector} object.
#' @importFrom terra crs
#' @importFrom terra nlyr
#' @importFrom terra time
#' @importFrom terra vect
#' @importFrom terra as.data.frame
#' @importFrom terra extract
#' @importFrom methods is
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires downloaded
#' ##       and processed data.
#' \dontrun{
#' loc <- data.frame(id = "001", lon = -95.0, lat = 34.5)
#' calculate_goes(
#'   from = goes,  # derived from process_goes() example
#'   locs = loc,
#'   locs_id = "id",
#'   radius = 0,
#'   fun = "mean"
#' )
#' }
#' @export
# nolint end
calculate_goes <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "goes",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 1,
    time = c(2, 3),
    time_type = "hour",
    level = NULL,
    weights = weights,
    ...
  )
  #### optional `.by_time` summarization
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = "mean",
      locs_id = locs_id
    )
    did_summarize <- TRUE
  } else {
    did_summarize <- FALSE
  }
  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }
  sites_return <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  return(sites_return)
}

# nolint start
#' Calculate drought index covariates
#' @description
#' The \code{calculate_drought()} function extracts drought index values at
#' point locations from an object returned by \code{process_drought()}.
#' Three source datasets are supported:
#' \itemize{
#'   \item \strong{SPEI / EDDI} (\code{SpatRaster}): cell values are
#'     extracted at each location using the standard raster-extraction
#'     pipeline (\code{calc_prepare_locs()} -> \code{calc_worker()} ->
#'     \code{calc_return_locs()}).  Time column format is
#'     \code{"YYYY-MM-DD"}.
#'   \item \strong{USDM} (\code{SpatVector} polygons): the drought monitor
#'     class (\code{DM}, integer 0-4) at each location is determined via
#'     spatial overlay. A \code{time} column of class \code{Date} is
#'     populated from the \code{date} attribute of \code{from}.
#' }
#' When \code{.by_time} is supplied the extracted result is
#' passed through \code{calc_summarize_by()} using the same semantics as
#' all other \code{calculate_*()} functions in this package.
# nolint end
#' @param from SpatRaster or SpatVector. Output of \code{process_drought()}.
#'   \itemize{
#'     \item \code{SpatRaster} for SPEI or EDDI sources.
#'     \item \code{SpatVector} (polygons) for USDM source.
#'   }
#' @param locs data.frame, character (path to CSV), \code{SpatVector}, or
#'   \code{sf} object. Point locations at which to extract values.
#' @param locs_id character(1). Name of the unique location identifier column
#'   in \code{locs}. Default \code{"site_id"}.
#' @param radius integer(1). Circular buffer radius in metres around each
#'   site location used for extraction. For SPEI/EDDI this controls raster
#'   buffering; for USDM, \code{radius > 0} additionally returns class
#'   proportions within the buffer. Default \code{0L}.
#' @param fun character(1). Summary function applied to raster cells within
#'   the buffer (SPEI/EDDI only). Default \code{"mean"}.
#' @param geom \code{FALSE}, \code{"sf"}, or \code{"terra"}. Whether to
#'   attach geometry to the returned object. Default \code{FALSE}.
#' @param .by_time NULL or character(1). Name of the time column to use
#'   temporal summarization unit token. \code{NULL} disables
#'   \code{"time"}.
#' @param weights `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file
#'   path. Optional weights raster for weighted extraction. If `NULL`
#'   (default), unweighted extraction is performed.
#' @param ... Reserved for future use; currently ignored.
#' @note
#' \itemize{
#'   \item The column name for extracted drought values follows the pattern
#'     \code{"<source>_<timescale>_<radius>"} (e.g. \code{"spei_01_0"}) for
#'     SPEI/EDDI, and \code{"usdm_dm_0"} for USDM.
#'   \item For USDM with \code{radius > 0}, proportion columns are added as
#'     \code{"usdm_dm_<class>_<radius>"} for classes 0–4.
#' }
#' @author Insang Song
#' @return A \code{data.frame} (default) or \code{SpatVector}/\code{sf}
#'   object (when \code{geom} is set) with columns:
#'   \describe{
#'     \item{\code{<locs_id>}}{Location identifier.}
#'     \item{\code{time}}{Date of the observation (\code{Date} or
#'       \code{"YYYY-MM-DD"} character).}
#'     \item{\code{<value_column>}}{Extracted drought index or class value.}
#'   }
#'   When \code{.by_time} is non-\code{NULL}, rows are
#'   aggregated to the specified resolution via \code{calc_summarize_by()}.
#' @importFrom terra extract
#' @importFrom terra time
#' @importFrom terra crs
#' @seealso
#' \code{\link{process_drought}}, \code{\link{download_drought}},
#' \code{\link{calc_summarize_by}}
#' @examples
#' \dontrun{
#' locs <- data.frame(site_id = "001", lon = -97.5, lat = 35.5)
#' ## SPEI example
#' spei <- process_drought(
#'   source = "spei",
#'   path = "./data/drought",
#'   date = c("2020-01-01", "2020-12-31"),
#'   timescale = 1L
#' )
#' calculate_drought(
#'   from = spei,
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = 0L,
#'   fun = "mean"
#' )
#' ## USDM example
#' usdm <- process_drought(
#'   source = "usdm",
#'   path = "./data/drought",
#'   date = c("2020-01-07", "2020-03-31")
#' )
#' calculate_drought(
#'   from = usdm,
#'   locs = locs,
#'   locs_id = "site_id"
#' )
#' }
#' @export
calculate_drought <- function(
  from,
  locs,
  locs_id = "site_id",
  radius = 0L,
  fun = "mean",
  weights = NULL,
  geom = FALSE,
  .by_time = NULL,
  ...
) {
  #### Validate .by_time
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)

  #### Dispatch on input type
  if (inherits(from, "SpatRaster")) {
    #### SPEI / EDDI raster extraction pipeline
    sites_list <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = radius,
      geom = geom
    )
    sites_e <- sites_list[[1]]
    sites_id <- sites_list[[2]]

    #### Derive source and timescale from first layer name
    #### (e.g. "spei_01_2020-01-01")
    lyr_parts <- strsplit(names(from)[1], "_")[[1]]
    src_name <- lyr_parts[1]
    ts_fmt <- lyr_parts[2]
    col_name <- paste0(src_name, "_", ts_fmt, "_", radius)
    weighted_drought <- amadeus::calc_prepare_weights(
      from = from[[1]],
      weights = weights
    )
    drought_fun_extract <- amadeus::calc_weighted_fun(
      fun = fun,
      weighted = !is.null(weighted_drought)
    )

    sites_extracted <- NULL
    for (l in seq_len(terra::nlyr(from))) {
      data_layer <- from[[l]]
      data_time <- as.POSIXct(as.Date(terra::time(data_layer)), tz = "UTC")

      if (terra::geomtype(sites_e) == "polygons") {
        extract_args <- list(
          x = data_layer,
          y = sf::st_as_sf(sites_e),
          fun = drought_fun_extract,
          progress = FALSE,
          force_df = TRUE,
          max_cells_in_memory = 1e8
        )
        if (!is.null(weighted_drought)) {
          extract_args$weights <- weighted_drought
        }
        layer_vals <- do.call(exactextractr::exact_extract, extract_args)
      } else {
        if (is.null(weighted_drought)) {
          layer_vals <- terra::extract(
            data_layer,
            sites_e,
            method = "simple",
            ID = FALSE,
            bind = FALSE,
            na.rm = TRUE
          )
        } else {
          weighted_geoms <- amadeus::calc_prepare_exact_geoms(
            locs_vector = sites_e,
            radius = radius
          )
          layer_vals <- exactextractr::exact_extract(
            x = data_layer,
            y = weighted_geoms,
            weights = weighted_drought,
            fun = drought_fun_extract,
            progress = FALSE,
            force_df = TRUE,
            max_cells_in_memory = 1e8
          )
        }
      }

      row_df <- data.frame(
        sites_id,
        time = rep(data_time, nrow(sites_id)),
        val = layer_vals[[1]],
        stringsAsFactors = FALSE
      )
      colnames(row_df) <- c(colnames(sites_id), "time", col_name)
      sites_extracted <- rbind(sites_extracted, row_df)
    }
    crs_from <- terra::crs(from)
  } else if (inherits(from, "SpatVector")) {
    #### USDM polygon overlay and optional buffered class proportions
    use_usdm_buffer <- as.numeric(radius) > 0
    prep_radius <- if (use_usdm_buffer) radius else 0L
    sites_list <- amadeus::calc_prepare_locs(
      from = from,
      locs = locs,
      locs_id = locs_id,
      radius = prep_radius,
      geom = geom
    )
    sites_e <- sites_list[[1]]
    sites_id <- sites_list[[2]]

    col_name <- "usdm_dm_0"
    prop_col_names <- if (use_usdm_buffer) {
      paste0("usdm_dm_", 0:4, "_", radius)
    } else {
      character(0)
    }
    dates_unique <- sort(unique(terra::values(from)$date))

    result_list <- vector("list", length(dates_unique))
    for (i in seq_along(dates_unique)) {
      d <- dates_unique[i]
      from_date <- from[terra::values(from)$date == d, ]
      d_posix <- as.POSIXct(as.Date(d), tz = "UTC")
      dm_values <- rep(NA_real_, nrow(sites_id))

      if (!use_usdm_buffer) {
        extracted <- terra::extract(from_date, sites_e)
        if (!is.null(extracted) && nrow(extracted) > 0L) {
          #### keep first match per site (polygons should not overlap)
          first_per_site <- !duplicated(extracted$id.y)
          dm_values[extracted$id.y[first_per_site]] <-
            extracted$DM[first_per_site]
        }

        row_df <- data.frame(
          sites_id,
          time = rep(d_posix, nrow(sites_id)),
          dm = dm_values,
          stringsAsFactors = FALSE
        )
        colnames(row_df) <- c(colnames(sites_id), "time", col_name)
      } else {
        sites_buffer <- sites_e
        if (terra::geomtype(sites_buffer) != "polygons") {
          sites_buffer <- terra::buffer(sites_buffer, width = radius)
        }
        site_index_col <- ".__site_row__"
        sites_buffer[[site_index_col]] <- seq_len(nrow(sites_buffer))

        prop_values <- matrix(
          NA_real_,
          nrow = nrow(sites_id),
          ncol = length(prop_col_names),
          dimnames = list(NULL, prop_col_names)
        )

        intersections <- terra::intersect(sites_buffer, from_date)
        if (!is.null(intersections) && nrow(intersections) > 0L) {
          inter_df <- data.frame(
            site_row = terra::values(intersections)[[site_index_col]],
            dm = terra::values(intersections)[["DM"]],
            area = terra::expanse(intersections, unit = "m"),
            stringsAsFactors = FALSE
          )
          inter_df <- inter_df[
            !(is.na(inter_df$site_row) | is.na(inter_df$dm)),
            ,
            drop = FALSE
          ]

          if (nrow(inter_df) > 0L) {
            site_dm_area <- stats::aggregate(
              area ~ site_row + dm,
              data = inter_df,
              FUN = sum
            )
            site_area <- terra::expanse(sites_buffer, unit = "m")
            by_site <- split(site_dm_area, site_dm_area$site_row)

            for (site_row_chr in names(by_site)) {
              site_row <- as.integer(site_row_chr)
              dm_area <- by_site[[site_row_chr]]
              dm_values[site_row] <- as.numeric(
                dm_area$dm[which.max(dm_area$area)]
              )

              prop_values[site_row, ] <- 0
              denom <- site_area[site_row]
              if (is.finite(denom) && denom > 0) {
                for (j in seq_len(nrow(dm_area))) {
                  dm_class <- as.integer(dm_area$dm[j])
                  if (!is.na(dm_class) && dm_class %in% 0:4) {
                    prop_values[site_row, dm_class + 1L] <-
                      dm_area$area[j] / denom
                  }
                }
              }
            }
          }
        }

        row_df <- data.frame(
          sites_id,
          time = rep(d_posix, nrow(sites_id)),
          dm = dm_values,
          prop_values,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        colnames(row_df)[seq_len(ncol(sites_id))] <- colnames(sites_id)
        colnames(row_df)[ncol(sites_id) + 1L] <- "time"
        colnames(row_df)[ncol(sites_id) + 2L] <- col_name
      }

      result_list[[i]] <- row_df
    }
    sites_extracted <- do.call(rbind, result_list)
    crs_from <- terra::crs(from)
  } else {
    stop("`from` must be a SpatRaster (SPEI/EDDI) or SpatVector (USDM).\n")
  }

  #### Optional .by_time summarization
  did_summarize <- FALSE
  if (!is.null(.by_time)) {
    sites_extracted <- amadeus::calc_summarize_by(
      covar = sites_extracted,
      .by_time = .by_time,
      fun_summary = fun,
      locs_id = locs_id
    )
    did_summarize <- TRUE
  }

  if (did_summarize && "time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time, tz = "UTC")
  }

  amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = crs_from
  )
}
