################################################################################
# Development work for {mirai} backended functions.
# 23 November 2026
# Mitchell Manware

################################################################################
# Calculate worker function which calls {calc_extract}.
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
    # Dispatch calc_extract across {mirai} daemons.
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
    # Dispatch `calc_extract`` across available daemons.
    mirai_jobs <- do.call(
      mirai::mirai_map,
      list(.x = list_wrapped, .f = calc_extract2, .args = list_args)
    )
    # --------------------------------------------------------------------------
    # Collect `mirai` results.
    list_results <- mirai::collect_mirai(mirai_jobs, options = ".stop")
  } else {
    # --------------------------------------------------------------------------
    # Dispatch calc_extract sequentially.
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
# Primary extraction function - newly independent from {calc_worker}.
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
# {sf}-based location preparation function.
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
