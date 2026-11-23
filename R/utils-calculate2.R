################################################################################
# Development work for {mirai} backended functions.
# 23 November 2026
# Mitchell Manware
################################################################################

################################################################################
# Prepare locations, identifiers, and optional geometry.
################################################################################

################################################################################
#' Prepare sf locations and extraction identifiers
#' @description
#' Validate location identifiers, transform and optionally buffer locations,
#' and prepare the identifier and geometry columns used in extraction results.
#' @param from SpatRaster. Raster whose CRS is used for the prepared locations.
#' @param locs sf, SpatVector, or data.frame. Locations containing the column
#'   named by `locs_id`. Data-frame inputs must also contain `lon` and `lat`
#'   columns in longitude and latitude degrees (EPSG:4326).
#' @param locs_id character(1). Name of the identifier column to retain.
#'   Identifier values are not checked for uniqueness.
#' @param radius numeric(1). Non-negative buffer distance applied after
#'   transformation to the raster CRS. Units and buffering behavior follow
#'   `process_locs_radius_sf()`.
#' @param geom FALSE or character(1). `FALSE` retains only the identifier in
#'   the result metadata. `"sf"` and `"terra"` also retain a `geometry` column
#'   of WKT strings for later conversion by [amadeus::calc_return_locs()].
#' @details
#' `process_locs_sf()` converts, transforms, and buffers the locations. The
#' identifier is restored from the original input if conversion drops it and
#' row counts still agree; otherwise an error is raised.
#'
#' WKT describes the prepared geometry, including any projection and buffer.
#' The requested output geometry class is handled by the calling calculate
#' function; this helper always returns the same two-element list structure.
#' @return An unnamed list with two elements: the prepared sf locations and
#'   a data.frame containing `locs_id` and, when requested, WKT `geometry`.
#'   Rows in the two elements have matching order.
#' @author Mitchell Manware
#' @seealso `process_locs_sf()`, `process_locs_radius_sf()`,
#'   [amadeus::calc_return_locs()]
#' @keywords internal auxiliary
#' @noRd
calc_prepare_locs2 <- function(
  from,
  locs,
  locs_id,
  radius,
  geom = FALSE
) {
  # ----------------------------------------------------------------------------
  # Check arguments.
  amadeus::check_for_null_parameters(mget(ls()))
  checkmate::assert_subset(locs_id, names(locs))
  # ----------------------------------------------------------------------------
  df_locsid <- as.data.frame(locs)[[locs_id]]
  # ----------------------------------------------------------------------------
  # Prepare sites as `sf`.
  sf_sites <- process_locs_sf(locs, terra::crs(from), radius)
  # ----------------------------------------------------------------------------
  # Coerce geometry-enabled choices to logical.
  amadeus::check_geom(geom)
  lgl_geom <- ifelse(geom %in% c("sf", "terra"), TRUE, geom)
  # ----------------------------------------------------------------------------
  # Store sites as `data.frame`, with text geometry if geom = TRUE.
  df_sites <- if (lgl_geom) {
    df_sites_i <- sf::st_drop_geometry(sf_sites)
    df_sites_i$geometry <- sf::st_as_text(sf::st_geometry(sf_sites))
    df_sites_i
  } else {
    sf::st_drop_geometry(sf_sites)
  }
  # ----------------------------------------------------------------------------
  # Error cases.
  if (!locs_id %in% names(df_sites)) {
    if (nrow(df_sites) != length(df_locsid)) {
      stop(
        paste0(
          "`locs_id` was not retained in prepared locations and could not ",
          "be reconstructed because row counts differ."
        )
      )
    }
    df_sites[[locs_id]] <- df_locsid
  }
  # ----------------------------------------------------------------------------
  # Identify column names to retain.
  chr_retain <- if (lgl_geom) c(locs_id, "geometry") else locs_id
  # ----------------------------------------------------------------------------
  # Return `sf` sites and `df` sites in a list.
  list(sf_sites, subset(df_sites, select = chr_retain))
}

################################################################################
#' Convert, transform, and buffer locations as sf
#' @description
#' Convert supported location inputs to sf, transform them to the requested
#' CRS, and apply an optional buffer.
#' @param locs sf, SpatVector, or data.frame. Locations to prepare. Data frames,
#'   including data.table objects, must contain `lon` and `lat` columns in
#'   EPSG:4326; these coordinate columns are retained as attributes. Spatial
#'   inputs use their existing CRS.
#' @param crs character(1) or crs. Target coordinate reference system accepted
#'   by [sf::st_transform()], such as an EPSG string, WKT, or sf CRS object.
#' @param radius numeric(1). Non-negative buffer distance passed to
#'   `process_locs_radius_sf()` after transformation. Zero skips buffering.
#'   Units depend on the target CRS and the sf S2 setting.
#' @return An sf object in the requested CRS, buffered when `radius > 0`.
#'   Unsupported input classes raise an error; conversion and transformation
#'   errors are propagated from sf.
#' @author Mitchell Manware
#' @seealso `process_locs_radius_sf()`, [sf::st_as_sf()], [sf::st_transform()]
#' @keywords internal auxiliary
#' @noRd
process_locs_sf <- function(
  locs,
  crs,
  radius
) {
  # ----------------------------------------------------------------------------
  # Detect locations class.
  if (methods::is(locs, "sf")) {
    # --------------------------------------------------------------------------
    # Retain if `sf`.
    sf_sites <- locs
  } else if (methods::is(locs, "SpatVector")) {
    # --------------------------------------------------------------------------
    # Convert to `sf` if `SpatVector.`
    sf_sites <- if (nrow(locs) == 0L) {
      suppressWarnings(sf::st_as_sf(locs))
    } else {
      sf::st_as_sf(locs)
    }
  } else if (methods::is(locs, "data.frame")) {
    # --------------------------------------------------------------------------
    # Generate `sf` if `data.frame`.
    sf_sites <- sf::st_as_sf(
      data.frame(locs),
      coords = c("lon", "lat"),
      crs = sf::st_crs("EPSG:4326"),
      remove = FALSE
    )
  } else {
    # --------------------------------------------------------------------------
    # Error if not `sf`, `SpatVector`, or `data.frame.`
    stop(paste0(
      "`locs` is not a `sf`, `SpatVector`, or `data.frame` object.\n"
    ))
  }
  # ----------------------------------------------------------------------------
  # Project to passed coordinate reference system.
  sf_sitesp <- sf::st_transform(sf_sites, crs)
  # ----------------------------------------------------------------------------
  # Apply buffer radius.
  process_locs_radius_sf(sf_sitesp, radius)
}

################################################################################
#' Buffer sf extraction locations
#' @description
#' Retain locations for a zero radius, or apply [sf::st_buffer()] for a
#' positive radius.
#' @param locs sf. Locations with a defined CRS.
#' @param radius numeric(1). Non-negative buffer distance. For projected
#'   coordinates, distances use the CRS units. For longitude/latitude
#'   coordinates, numeric distances use meters when S2 is enabled and degrees
#'   when S2 is disabled.
#' @details
#' The helper passes `nQuadSegs = 180L` to [sf::st_buffer()]. This controls
#' curved boundaries for the GEOS backend; S2 uses its own buffering method.
#' The caller is responsible for validating the radius and selecting a
#' suitable CRS and distance unit. The global S2 setting is not changed.
#' @return An sf object: the input when `radius = 0`, or buffered locations
#'   when `radius > 0`.
#' @author Mitchell Manware
#' @seealso `process_locs_sf()`, [sf::st_buffer()], [sf::sf_use_s2()]
#' @keywords internal auxiliary
#' @noRd
process_locs_radius_sf <- function(
  locs,
  radius
) {
  if (radius == 0) {
    locs
  } else if (radius > 0) {
    sf::st_buffer(
      locs,
      radius,
      nQuadSegs = 180L
    )
  }
}

################################################################################
# Prepare shared inputs and dispatch extraction.
################################################################################

################################################################################
#' Dispatch raster extraction sequentially or across mirai daemons
#' @description
#' Prepare locations and optional weights, extract each raster layer, and
#' combine results into a data frame. An existing mirai daemon pool enables
#' parallel extraction; otherwise layers are processed sequentially.
#' @param dataset character(1). Source identifier passed to the time-parsing
#'   and progress-message helpers.
#' @param from SpatRaster. Processed raster with one or more layers. Layer
#'   names contain underscore-separated variable, time, and optional level
#'   fields at the positions selected by `variable`, `time`, and `level`.
#' @param locs sf, SpatVector, or data.frame. Extraction locations containing
#'   `locs_id`. Data frames also require `lon` and `lat` columns in EPSG:4326.
#'   Unbuffered point inputs should contain one point per feature so that
#'   extracted rows align with identifiers.
#' @param locs_id character(1). Name of the location identifier column.
#' @param fun character(1). Single summary operation for polygon or weighted
#'   extraction. Defaults to `"mean"`. With weights, `"mean"` and `"sum"` are
#'   mapped to their weighted equivalents by [amadeus::calc_weighted_fun()].
#'   Unweighted point extraction returns cell values directly.
#' @param variable integer(1). Position of the variable field after splitting
#'   each layer name on underscores. Defaults to `1`.
#' @param time integer vector or NULL. Positions of time fields in the split
#'   layer name. Use `NULL` with `time_type = "timeless"`.
#' @param time_type character(1). Time representation: `"date"`, `"hour"`,
#'   `"year"`, `"yearmonth"`, or `"timeless"`. Defaults to `"date"`.
#' @param radius numeric(1). Non-negative buffer distance applied to locations
#'   in the raster CRS, also used as the covariate-column suffix. Units follow
#'   `process_locs_radius_sf()`.
#' @param geom FALSE or character(1). `FALSE` omits geometry from the result;
#'   `"sf"` or `"terra"` retains prepared geometry as WKT for later conversion
#'   by the calling calculate function.
#' @param level integer(1) or NULL. Optional position of the level field in
#'   the split layer name. Defaults to `NULL`.
#' @param max_cells numeric(1). Maximum raster cells loaded at once by
#'   [exactextractr::exact_extract()]. Defaults to `1e8`; used for polygon
#'   and weighted extraction.
#' @param weights NULL, SpatRaster, polygon SpatVector or sf, or character(1).
#'   Optional weighting data or a path to them.
#'   [amadeus::calc_prepare_weights()] prepares a single weighting layer
#'   using the first layer of `from`.
#' @param ... Reserved arguments. Currently not forwarded to extraction.
#' @details
#' Locations are prepared once by `calc_prepare_locs2()`, then `calc_extract2()`
#' handles each layer. Results retain raster-layer order, with location rows
#' within each layer. Temporal aggregation and final geometry conversion are
#' performed by the calling calculate function.
#'
#' When [mirai::daemons_set()] is true, raster layers are packed with
#' `terra::wrap(..., proxy = TRUE)` and passed to [mirai::mirai_map()]. Workers
#' must have access to any underlying raster files and the required packages.
#' Prepared weights are packed separately. Locations are always shared with
#' `mori::share()` in this branch, requiring workers on the same host with
#' access to the same shared-memory environment; locality is not checked here.
#' Worker errors stop result collection. Without a daemon pool, extraction
#' uses [base::lapply()]. This helper does not create or stop daemons.
#' @return A data.frame containing identifiers, optional WKT geometry, time
#'   except for timeless data, an optional level, and one covariate column.
#'   The result remains a data.frame for every `geom` choice. Column naming
#'   follows `calc_extract2()`.
#' @author Mitchell Manware
#' @seealso `calc_prepare_locs2()`, `calc_extract2()`,
#'   [amadeus::calc_prepare_weights()], [mirai::mirai_map()]
#' @keywords internal auxiliary
#' @noRd
calc_worker_mirai <- function(
  dataset,
  from,
  locs = locs,
  locs_id,
  fun = "mean",
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  geom,
  level = NULL,
  max_cells = 1e8,
  weights = NULL,
  ...
) {
  # ----------------------------------------------------------------------------
  # Match time type argument.
  chr_time <- match.arg(time_type)
  # ----------------------------------------------------------------------------
  # Calculate custom weights.
  rast_weights <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  # ----------------------------------------------------------------------------
  # Apply custom weights to extraction function
  chr_weightedfun <- amadeus::calc_weighted_fun(
    fun = fun,
    weighted = !is.null(rast_weights)
  )
  # ----------------------------------------------------------------------------
  # Prepare extraction locations.
  list_locs <- calc_prepare_locs2(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  sf_locs <- list_locs[[1]]
  df_locs <- list_locs[[2]]
  # ----------------------------------------------------------------------------
  # Convert SpatRaster to list
  list_from <- as.list(from)
  # ----------------------------------------------------------------------------
  # Define shared arguments (includes mirai detection)
  list_args <- list(
    dataset = dataset,
    locs_vector = sf_locs,
    locs_df = df_locs,
    variable = variable,
    time = time,
    time_type = chr_time,
    radius = radius,
    level = level,
    max_cells = max_cells,
    weights_prepared = rast_weights,
    fun_extract = chr_weightedfun,
    mirai = mirai::daemons_set()
  )
  # ----------------------------------------------------------------------------
  if (list_args$mirai) {
    # --------------------------------------------------------------------------
    # Perform extraction across {mirai} daemons.
    mirai::require_daemons()
    message(sprintf(
      "Running across %02d {mirai} daemons.",
      mirai::nextget("n")
    ))
    # --------------------------------------------------------------------------
    # Wrap each layer of the listed SpatRaster objects for pass to daemons.
    list_wrapped <- lapply(
      list_from,
      function(x) terra::wrap(x, proxy = TRUE)
    )
    # --------------------------------------------------------------------------
    # Share extraction locations across all configured daemons.
    list_args$locs_vector <- mori::share(list_args$locs_vector)
    # --------------------------------------------------------------------------
    if (!is.null(list_args$weights_prepared)) {
      list_args$weights_prepared <- terra::wrap(rast_weights)
    }
    # --------------------------------------------------------------------------
    # Devel messages.
    message(paste0(
      "mori::is_shared(shared_args): ",
      mori::is_shared(list_args)
    ))
    message(paste0(
      "mori::is_shared(shared_args$locs_vector): ",
      mori::is_shared(list_args$locs_vector)
    ))
    # --------------------------------------------------------------------------
    # Dispatch `calc_extract2` across available daemons.
    mirai_jobs <- do.call(
      mirai::mirai_map,
      list(.x = list_wrapped, .f = calc_extract2, .args = list_args)
    )
    # --------------------------------------------------------------------------
    # Collect `mirai` results.
    list_results <- mirai::collect_mirai(mirai_jobs, options = ".stop")
  } else {
    # --------------------------------------------------------------------------
    # Dispatch `calc_extract2`` sequentially.
    message("Running in sequence.")
    list_results <- do.call(
      lapply,
      c(list(X = list_from, FUN = calc_extract2), list_args)
    )
  }
  # ----------------------------------------------------------------------------
  # Bind lists to a single data.frame.
  data.frame(do.call(rbind, list_results))
}

################################################################################
# Extract each layer and assemble its covariates.
################################################################################

################################################################################
#' Extract one raster layer at prepared locations
#' @description
#' Read variable, time, and optional level metadata from a layer, extract its
#' values at prepared locations, and attach location metadata to the results.
#' @param layer SpatRaster or PackedSpatRaster. One raster layer, packed with
#'   [terra::wrap()] when `mirai = TRUE`.
#' @param dataset character(1). Source identifier passed to
#'   [amadeus::calc_time()] and [amadeus::calc_message()].
#' @param locs_vector sf. Prepared locations in the layer CRS, including any
#'   requested buffer. All features must belong to the polygon family or all
#'   to the point family. Unweighted points should contain one coordinate per
#'   feature so extraction rows align with `locs_df`.
#' @param locs_df data.frame. Location identifiers and optional WKT geometry,
#'   with one row per feature in `locs_vector`, in the same order.
#' @param variable integer(1). Position of the variable field after splitting
#'   the layer name on underscores. Defaults to `1`.
#' @param time integer vector or NULL. Positions of the time fields in the
#'   split layer name. Use `NULL` only for `time_type = "timeless"`.
#' @param time_type character(1). One of `"date"`, `"hour"`, `"year"`,
#'   `"yearmonth"`, or `"timeless"`, selected by `calc_worker_mirai()`.
#'   Supply a single value when calling this helper directly.
#' @param radius numeric(1). Buffer distance already applied to locations.
#'   Used in the covariate-column name and passed to
#'   [amadeus::calc_prepare_exact_geoms()] for weighted point extraction.
#' @param level integer(1) or NULL. Optional position of the level field in
#'   the split layer name. Defaults to `NULL`.
#' @param max_cells numeric(1). Maximum cells loaded at once by
#'   [exactextractr::exact_extract()]. Defaults to `1e8`; unused for
#'   unweighted point extraction.
#' @param weights_prepared NULL, SpatRaster, or PackedSpatRaster. Prepared
#'   single-layer weights, packed when `mirai = TRUE`. No weight preparation
#'   or alignment is performed here.
#' @param fun_extract character(1). Single extraction summary operation,
#'   already resolved to a weighted operation when appropriate. Unused for
#'   unweighted point extraction.
#' @param mirai logical(1). Whether raster inputs need unpacking. `TRUE` also
#'   suppresses per-layer progress messages.
#' @param ... Reserved arguments. Currently unused.
#' @details
#' Polygons use [exactextractr::exact_extract()]. Unweighted points use
#' [terra::extract()] with `method = "simple"`. Weighted points are converted
#' to polygons by [amadeus::calc_prepare_exact_geoms()] before exact extraction.
#' Location metadata is attached by row position.
#'
#' Time parsing uses both the selected name fields and available raster time
#' metadata. Timeless results omit time and level columns. Other results
#' include `time` and, when `level` is supplied, a `level` column with
#' `"level="` and `"lev="` prefixes removed.
#' @return A data.frame containing `locs_df`, the applicable time and level
#'   fields, and one covariate column. That column is named `variable_radius`,
#'   with the variable converted to lowercase when a level column is included.
#' @author Mitchell Manware
#' @seealso `calc_worker_mirai()`, [amadeus::calc_time()],
#'   [exactextractr::exact_extract()], [terra::extract()]
#' @keywords internal auxiliary
#' @noRd
calc_extract2 <- function(
  layer,
  dataset,
  locs_vector,
  locs_df,
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  level = NULL,
  max_cells = 1e8,
  weights_prepared,
  fun_extract,
  mirai,
  ...
) {
  # ----------------------------------------------------------------------------
  # Unwrap {terra} objects if daemons detected.
  if (mirai) {
    layer <- terra::unwrap(layer)
    weights_prepared <- terra::unwrap(weights_prepared)
  }
  # ----------------------------------------------------------------------------
  # Split layer names.
  layer_time <- NULL
  data_split <- strsplit(names(layer), "_")[[1]]
  # ----------------------------------------------------------------------------
  # Extract variable name.
  data_name <- data_split[variable]
  # ----------------------------------------------------------------------------
  # Extract time information.
  if (!is.null(time)) {
    layer_time <- try(terra::time(layer), silent = TRUE)
    if (inherits(layer_time, "try-error")) {
      layer_time <- NULL
    }
    data_time <- amadeus::calc_time(
      time = data_split[time],
      format = time_type,
      dataset = dataset,
      layer_name = names(layer),
      layer_time = layer_time
    )
  }
  # ----------------------------------------------------------------------------
  # Extract data level.
  data_level <- NULL
  if (!is.null(level)) {
    data_level <- data_split[level]
  }
  # ----------------------------------------------------------------------------
  # Message on sequential run only.
  if (!mirai) {
    layer_time_msg <- if (!is.null(time)) data_split[time] else NA_character_
    amadeus::calc_message(
      dataset = dataset,
      variable = data_name,
      time = layer_time_msg,
      time_type = time_type,
      level = data_level,
      layer_time = layer_time
    )
  }
  # ----------------------------------------------------------------------------
  # Peform extraction or zonal statistic calculation at sites.
  if (all(sf::st_is(locs_vector, c("POLYGON", "MULTIPOLYGON")))) {
    # --------------------------------------------------------------------------
    # {exactextractr} for polygons and zonal statistics.
    extract_args <- list(
      x = layer,
      y = locs_vector,
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
  } else if (all(sf::st_is(locs_vector, c("POINT", "MULTIPOINT")))) {
    if (is.null(weights_prepared)) {
      # ------------------------------------------------------------------------
      # {terra::extract} for points and unweighted values.
      sites_extracted_layer <- terra::extract(
        layer,
        terra::vect(locs_vector),
        method = "simple",
        ID = FALSE,
        bind = FALSE,
        na.rm = TRUE
      )
    } else {
      # ------------------------------------------------------------------------
      # {exactextractr} for points and weighted values.
      weighted_geoms <- amadeus::calc_prepare_exact_geoms(
        locs_vector = terra::vect(locs_vector),
        radius = radius
      )
      sites_extracted_layer <- exactextractr::exact_extract(
        x = layer,
        y = weighted_geoms,
        weights = weights_prepared,
        progress = FALSE,
        force_df = TRUE,
        fun = fun_extract,
        max_cells_in_memory = max_cells
      )
    }
  }
  # ----------------------------------------------------------------------------
  # Merge with site_id, time, and pressure levels.
  if (time_type == "timeless") {
    sites_extracted_layer <- cbind(locs_df, sites_extracted_layer)
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
      sites_extracted_layer <- cbind(locs_df, data_time, sites_extracted_layer)
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
  sites_extracted_layer
}

################################################################################
