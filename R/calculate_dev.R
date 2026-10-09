################################################################################
# Development work for {mirai} backended functions.
# 08 October 2026
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
  # Match time type argument
  time_type <- match.arg(time_type)

  # Apply custom weights
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )

  # Apply custom weights to extraction function
  fun_extract <- amadeus::calc_weighted_fun(
    fun = fun,
    weighted = !is.null(weights_prepared)
  )

  # Prepare extraction locations.
  locs_list <- calc_prepare_locs2(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius,
    geom = geom
  )
  locs_vector <- locs_list[[1]]
  locs_df <- locs_list[[2]]

  # Convert SpatRaster to list
  list_from <- as.list(from)

  # Define shared arguments (includes mirai detection)
  shared_args <- list(
    dataset = dataset,
    locs_vector = locs_vector,
    locs_df = locs_df,
    variable = variable,
    time = time,
    time_type = time_type,
    radius = radius,
    level = level,
    max_cells = max_cells,
    weights_prepared = weights_prepared,
    fun_extract = fun_extract,
    mirai = mirai::daemons_set()
  )

  if (shared_args$mirai) {
    mirai::require_daemons()
    # Dispatch calc_extract across {mirai} daemons.
    message(sprintf(
      "Running across %02d {mirai} daemons.",
      mirai::nextget("n")
    ))
    list_wrapped <- lapply(
      list_from,
      function(x) terra::wrap(x, proxy = TRUE)
    )
    # shared_args$locs_vector <- terra::wrap(locs_vector)
    shared_args$locs_vector <- mori::share(shared_args$locs_vector)
    if (!is.null(shared_args$weights_prepared)) {
      shared_args$weights_prepared <- terra::wrap(weights_prepared)
    }
    message(paste0(
      "mori::is_shared(shared_args): ",
      mori::is_shared(shared_args)
    ))
    message(paste0(
      "mori::is_shared(shared_args$locs_vector): ",
      mori::is_shared(shared_args$locs_vector)
    ))

    jobs <- do.call(
      mirai::mirai_map,
      list(.x = list_wrapped, .f = calc_extract, .args = shared_args)
    )
    results <- mirai::collect_mirai(jobs, options = ".stop")
  } else {
    # Dispatch calc_extract sequentially
    message("Running in sequence.")
    results <- do.call(
      lapply,
      c(list(X = list_from, FUN = calc_extract), shared_args)
    )
  }
  # Bind lists to a single data.frame
  data.frame(do.call(rbind, results))
}

################################################################################
# Primary extraction function - newly independent from {calc_worker}.
calc_extract <- function(
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
  ##### Unwrap {terra} objects if using multiple daemons
  if (mirai) {
    #### Unwrap PackedSpatRaster
    layer <- terra::unwrap(layer)
    # locs_vector <- terra::unwrap(locs_vector)
    weights_prepared <- terra::unwrap(weights_prepared)
  }

  layer_time <- NULL

  #### split layer name
  data_split <- strsplit(
    names(layer),
    "_"
  )[[1]]

  #### extract variable
  data_name <- data_split[variable]
  if (!is.null(time)) {
    layer_time <- try(terra::time(layer), silent = TRUE)
    if (inherits(layer_time, "try-error")) {
      layer_time <- NULL
    }
    #### extract time
    data_time <- amadeus::calc_time(
      time = data_split[time],
      format = time_type,
      dataset = dataset,
      layer_name = names(layer),
      layer_time = layer_time
    )
  }

  #### extract level (if applicable)
  if (!is.null(level)) {
    data_level <- data_split[level]
  } else {
    data_level <- NULL
  }

  #### message
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

  #### extract layer data at sites
  if (all(sf::st_is(locs_vector, "POLYGON"))) {
    ### apply exactextractr::exact_extract for polygons
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
  } else if (all(sf::st_is(locs_vector, "POINT"))) {
    if (is.null(weights_prepared)) {
      #### apply terra::extract for points
      sites_extracted_layer <- terra::extract(
        layer,
        terra::vect(locs_vector),
        method = "simple",
        ID = FALSE,
        bind = FALSE,
        na.rm = TRUE
      )
    } else {
      weighted_geoms <- amadeus::calc_prepare_exact_geoms(
        locs_vector = locs_vector,
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

  # merge with site_id, time, and pressure levels (if applicable)
  if (time_type == "timeless") {
    sites_extracted_layer <- cbind(
      locs_df,
      sites_extracted_layer
    )
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
      sites_extracted_layer <- cbind(
        locs_df,
        data_time,
        sites_extracted_layer
      )
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
  #### check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))
  if (!locs_id %in% names(locs)) {
    stop(sprintf("locs should include columns named %s.\n", locs_id))
  }
  locs_id_values <- as.data.frame(locs)[[locs_id]]
  #### prepare sites
  sites_e <- process_locs_sf(
    locs,
    terra::crs(from),
    radius
  )
  #### site identifiers and geometry
  # check geom
  amadeus::check_geom(geom)
  if (geom %in% c("sf", "terra")) {
    geom <- TRUE
  }

  sites_df <- if (geom) {
    sites_i <- sf::st_drop_geometry(sites_e)
    sites_i$geometry <- sf::st_as_text(sf::st_geometry(sites_e))
    sites_i
  } else {
    sf::st_drop_geometry(sites_e)
  }

  if (!locs_id %in% names(sites_df)) {
    if (nrow(sites_df) != length(locs_id_values)) {
      stop(
        paste0(
          "`locs_id` was not retained in prepared locations and could not ",
          "be reconstructed because row counts differ."
        )
      )
    }
    sites_df[[locs_id]] <- locs_id_values
  }
  chr_retain <- if (geom) c(locs_id, "geometry") else locs_id
  list(sites_e, subset(sites_df, select = chr_retain))
}

################################################################################
process_locs_sf <-
  function(
    locs,
    crs,
    radius
  ) {
    #### detect sf
    if (methods::is(locs, "sf")) {
      sites_sf <- locs
    } else if (methods::is(locs, "SpatVector")) {
      #### detect terra::SpatVector
      sites_sf <- if (nrow(locs) == 0L) {
        suppressWarnings(sf::st_as_sf(locs))
      } else {
        sf::st_as_sf(locs)
      }
      ### detect data.frame object
    } else if (methods::is(locs, "data.frame")) {
      sites_sf <- sf::st_as_sf(
        data.frame(locs),
        coords = c("lon", "lat"),
        crs = sf::st_crs("EPSG:4326"),
        remove = FALSE
      )
    } else {
      stop(
        paste0(
          "`locs` is not a `SpatVector`, `sf`, or `data.frame` object.\n"
        )
      )
    }
    ##### project to desired coordinate reference system
    sites_p <- sf::st_transform(
      sites_sf,
      crs
    )
    #### buffer SpatVector
    process_locs_radius_sf(
      sites_p,
      radius
    )
  }

################################################################################
process_locs_radius_sf <-
  function(
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
# {calculate_narr} updated with the mirai optional dispatcher.
calculate_narr_mirai <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  weights = NULL,
  .by_time = NULL,
  geom = FALSE,
  max_cells = 1e8,
  ...
) {
  amadeus::check_unsupported_by(..., .call = sys.call())
  amadeus::check_by_time(.by_time)
  #### identify pressure level or monolevel data
  if (grepl("level", names(from)[1])) {
    narr_time <- 3
    narr_level <- 2
  } else {
    narr_time <- 2
    narr_level <- NULL
  }

  #### perform extraction
  sites_extracted <- calc_worker_mirai(
    from = from,
    locs = locs,
    locs_id = locs_id,
    dataset = "narr",
    variable = 1,
    time = narr_time,
    time_type = "date",
    radius = radius,
    geom = geom,
    level = narr_level,
    max_cells = max_cells,
    weights = weights,
    fun = fun
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

  amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
}

################################################################################
# {calculate_hms} updated with the mirai optional dispatcher.
calculate_hms_map <- function(
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

  # Convert {from} to a list
  list_from <- lapply(date_sequence, function(x) from[from$Date == x])

  # Define shared arguments (includes mirai detection)
  shared_args <- list(
    sites_e = sites_e,
    sites_id = sites_id,
    locs_id = locs_id,
    radius = radius,
    frac = frac,
    mirai = mirai::daemons_set()
  )

  if (shared_args$mirai) {
    mirai::require_daemons()
    # Dispatch calc_hms_extract across {mirai} daemons.
    message(sprintf(
      "Running across %02d {mirai} daemons.",
      mirai::nextget("n")
    ))
    list_wrapped <- lapply(list_from, terra::wrap)
    shared_args$sites_e <- terra::wrap(shared_args$sites_e)
    jobs <- do.call(
      mirai::mirai_map,
      list(.x = list_wrapped, .f = calc_hms_extract, .args = shared_args)
    )
    list_extracted <- mirai::collect_mirai(jobs, options = ".stop")
  } else {
    # Dispatch calc_hms_extract in sequence
    message("Running in sequence.")
    list_extracted <- do.call(
      lapply,
      c(list(X = list_from, FUN = calc_hms_extract), shared_args)
    )
  }

  ### Merge data.frame in list
  sites_extracted <- do.call(rbind, list_extracted)

  binary_colname <- paste0(
    tolower(c("Light", "Medium", "Heavy")),
    "_",
    sprintf("%05d", radius)
  )

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

  #### date to POSIXct
  if ("time" %in% names(sites_extracted)) {
    sites_extracted$time <- as.POSIXct(sites_extracted$time)
  }
  #### order by date
  sites_extracted_ordered <- as.data.frame(
    sites_extracted[order(sites_extracted$time), ]
  )
  sites_extracted_ordered <- amadeus::calc_return_locs(
    covar = sites_extracted,
    POSIXt = TRUE,
    geom = geom,
    crs = terra::crs(from)
  )
  #### return data.frame
  return(sites_extracted_ordered)
}


###############################################################################
# {calculate_hms}-specific extraction function for dispatch with {lapply}
# or {mirai::mirai_map} updated with the mirai optional dispatcher.
calc_hms_extract <- function(
  from,
  sites_e,
  sites_id,
  locs_id,
  radius,
  frac,
  mirai
) {
  from <- if (mirai) terra::unwrap(from) else from
  sites_e <- if (mirai) terra::unwrap(sites_e) else sites_e
  date <- unique(from$Date)
  ### Expand full spatiotemporal range
  data_template <- expand.grid(
    id = sites_id[[locs_id]],
    time = date
  )
  data_template <- stats::setNames(data_template, c(locs_id, "time"))
  is_point_locs <- all(
    tolower(terra::geomtype(sites_e)) %in% c("points", "point")
  )

  if (nrow(from) == 0) {
    sites_extracted_layer <- data.frame(
      setNames(list(character(0)), locs_id),
      Date = character(0),
      Density = character(0),
      base_value = numeric(0)
    )
  } else if (radius == 0 && is_point_locs) {
    sites_extracted_layer <- terra::extract(from, sites_e)
    sites_extracted_layer$id.y <- unlist(
      sites_e[[locs_id]]
    )[sites_extracted_layer$id.y]

    names(sites_extracted_layer)[
      names(sites_extracted_layer) == "id.y"
    ] <- locs_id

    sites_extracted_layer$base_value <- 1
  } else {
    intersections <- terra::intersect(sites_e, from)

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

  # remove unmatched extraction placeholders before aggregating
  if (nrow(sites_extracted_layer) > 0) {
    sites_extracted_layer <- sites_extracted_layer[
      !is.na(sites_extracted_layer$Date) &
        !is.na(sites_extracted_layer$Density),
      ,
      drop = FALSE
    ]
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
  col_tofill <- setdiff(levels_acceptable, names(sites_extracted_layer))

  # Fill zeros
  if (length(col_tofill) > 0) {
    sites_extracted_layer[col_tofill] <- if (isTRUE(frac)) 0 else 0L
  }
  col_order <- c(locs_id, "Date", levels_acceptable)
  sites_extracted_layer <- sites_extracted_layer[, col_order]
  sites_extracted_layer <- stats::setNames(
    sites_extracted_layer,
    c(locs_id, "time", levels_acceptable)
  )

  binary_colname <- paste0(
    tolower(levels_acceptable),
    "_",
    sprintf("%05d", radius)
  )
  sites_extracted_layer <- stats::setNames(
    sites_extracted_layer,
    c(locs_id, "time", binary_colname)
  )

  # Join full space-time pairs with extracted data
  site_extracted <- merge(
    data_template,
    sites_extracted_layer,
    by = c(locs_id, "time"),
    all.x = TRUE
  )
  # append list with the extracted data.frame
  site_extracted
}
