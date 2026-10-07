################################################################################
# {mirai} enabled download dispatcher.
download_run_mirai <- function(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  max_tries = 20,
  rate_limit = 2,
  daemons = NULL,
  ...
) {
  # Parallel download using available mirai daemons.
  int_daemons <- as.integer(daemons)

  message(sprintf(
    "Concurrent download using %d configrured {mirai} daemons.",
    int_daemons
  ))

  df_mirai <- data.frame(
    urls = unname(urls),
    destfiles = unname(destfiles)
  )

  jobs <- mirai::mirai_map(
    df_mirai,
    function(
      urls,
      destfiles,
      max_tries,
      rate_limit
    ) {
      amadeus::download_run_method(
        urls = urls,
        destfiles = destfiles,
        token = NULL,
        show_progress = FALSE,
        max_tries = max_tries,
        rate_limit = rate_limit,
        ...
      )
    },
    .args = list(max_tries = max_tries, rate_limit = rate_limit)
  )

  results <- mirai::collect_mirai(jobs, options = ".stop")

  list(
    success = sum(vapply(results, `[[`, numeric(1), "success")),
    failed = sum(vapply(results, `[[`, numeric(1), "failed")),
    skipped = sum(vapply(results, `[[`, numeric(1), "skipped")),
    failed_urls = unlist(
      lapply(results, `[[`, "failed_urls"),
      use.names = FALSE
    ),
    failed_files = unlist(
      lapply(results, `[[`, "failed_files"),
      use.names = FALSE
    )
  )
}

################################################################################
# {download_narr} updated with the mirai optional dispatcher.
download_narr_map <- function(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
) {
  #### 1. Check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)

  #### 2. Check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))

  #### 3. Check years
  if (length(year) == 1) {
    year <- c(year, year)
  }
  stopifnot(length(year) == 2)
  year <- year[order(year)]

  #### 4. Directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)

  #### 5. Handle deprecated parameters
  if (!isTRUE(download)) {
    warning(
      "Setting download=FALSE is deprecated.",
      " Downloads now use httr2 by default.\n",
      "To skip downloading, the function will return",
      " after discovering files.\n",
      call. = FALSE
    )
  }

  if (remove_command != FALSE) {
    warning(
      "Parameter 'remove_command' is deprecated and ignored.\n",
      call. = FALSE
    )
  }

  #### 6. Define years sequence
  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop("years should be 4-digit integers.\n")
  }
  stopifnot(
    all(
      seq(year[1], year[2], 1) %in%
        seq(1979, as.numeric(substr(Sys.Date(), 1, 4)), 1)
    )
  )
  years <- seq(year[1], year[2], 1)

  #### 7. Define variables
  variables_list <- as.list(unique(variables))

  #### 8. Collect all URLs and destination files
  list_map <- lapply(
    variables_list,
    function(x) {
      base <- amadeus::narr_variable(x)[[1]]
      month <- amadeus::narr_variable(x)[[2]]
      download_grid <- expand.grid(
        base = base,
        variable = x,
        year = years,
        month = month,
        dir = directory_to_save
      )
      download_grid$url <- with(
        download_grid,
        paste0(
          base,
          variable,
          ".",
          year,
          month,
          ".nc"
        )
      )
      download_grid$destfile <- with(
        download_grid,
        paste0(
          dir,
          variable,
          "/",
          variable,
          ".",
          year,
          month,
          ".nc"
        )
      )
      needs_download <- vapply(
        download_grid$destfile,
        amadeus::check_destfile,
        FUN.VALUE = logical(1)
      )
      var_urls <- download_grid$url[needs_download]
      var_destfiles <- download_grid$destfile[needs_download]
      stopifnot(length(var_urls) == length(var_destfiles))
      list(urls = var_urls, destfiles = var_destfiles)
    }
  )
  all_urls <- unlist(lapply(list_map, function(x) x[[1]]))
  all_destfiles <- unlist(lapply(list_map, function(x) x[[2]]))

  #### 9. Exit early if download=FALSE (deprecated behavior)
  if (!isTRUE(download)) {
    message(
      sprintf(
        "Skipping download. Found %d files available for download.\n",
        length(all_urls)
      )
    )
    return(
      invisible(
        list(
          urls = all_urls,
          destfiles = all_destfiles,
          n_files = length(all_urls)
        )
      )
    )
  }

  #### 10. Download files using httr2 (sequential or concurrent with mirai)
  if (length(all_urls) == 0L) {
    download_result <- list(success = 0L, failed = 0L, skipped = 0L)
  } else if (!mirai::daemons_set()) {
    # Sequential download if mirai daemons are not set.
    download_result <- amadeus::download_run_method(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # NARR doesn't use token authentication
      show_progress = show_progress,
      max_tries = max_tries,
      rate_limit = rate_limit
    )
  } else {
    download_result <- download_run_mirai(
      urls = all_urls,
      destfiles = all_destfiles,
      token = NULL, # NARR doesn't use token authentication
      max_tries = max_tries,
      rate_limit = rate_limit,
      daemons = mirai::nextget("n")
    )
  }

  #### 11. Return hash if requested
  if (hash) {
    return(amadeus::download_hash(hash = TRUE, directory_to_save))
  } else {
    return(invisible(download_result))
  }
}

################################################################################
# {mirai}-enabled covariate calculation function.
calc_worker_mirai <- function(
  dataset,
  from,
  locs_vector,
  locs_df,
  fun = "mean",
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  level = NULL,
  max_cells = 1e8,
  weights = NULL,
  ...
) {
  #### empty location data.frame
  sites_extracted <- NULL
  time_type <- match.arg(time_type)
  weights_prepared <- amadeus::calc_prepare_weights(
    from = from[[1]],
    weights = weights
  )
  fun_extract <- amadeus::calc_weighted_fun(
    fun = fun,
    weighted = !is.null(weights_prepared)
  )
  return(list(from, time_type, weights_prepared, fun_extract))
  for (l in seq_len(terra::nlyr(from))) {
    #### select data layer
    data_layer <- from[[l]]
    layer_time <- NULL
    #### split layer name
    data_split <- strsplit(
      names(data_layer),
      "_"
    )[[1]]
    #### extract variable
    data_name <- data_split[variable]
    if (!is.null(time)) {
      layer_time <- try(terra::time(data_layer), silent = TRUE)
      if (inherits(layer_time, "try-error")) {
        layer_time <- NULL
      }
      #### extract time
      data_time <- calc_time(
        time = data_split[time],
        format = time_type,
        dataset = dataset,
        layer_name = names(data_layer),
        layer_time = layer_time
      )
    }
    #### extract level (if applicable)
    if (!is.null(level)) {
      data_level <- data_split[level]
    } else {
      data_level <- NULL
    }
    layer_time_msg <- if (!is.null(time)) data_split[time] else NA_character_
    #### message
    calc_message(
      dataset = dataset,
      variable = data_name,
      time = layer_time_msg,
      time_type = time_type,
      level = data_level,
      layer_time = layer_time
    )
    #### extract layer data at sites
    if (terra::geomtype(locs_vector) == "polygons") {
      ### apply exactextractr::exact_extract for polygons
      extract_args <- list(
        x = data_layer,
        y = sf::st_as_sf(locs_vector),
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
    } else if (terra::geomtype(locs_vector) == "points") {
      if (is.null(weights_prepared)) {
        #### apply terra::extract for points
        sites_extracted_layer <- terra::extract(
          data_layer,
          locs_vector,
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
          x = data_layer,
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
    #### merge with empty sites_extracted
    sites_extracted <- rbind(
      sites_extracted,
      sites_extracted_layer
    )
  }
  #### finish message
  message(
    paste0(
      "Returning extracted covariates.\n"
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
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
  locs_list <- amadeus::calc_prepare_locs(
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
    # Dispatch calc_extract across {mirai} daemons.
    message(sprintf(
      "Running across %02d {mirai} daemons.",
      mirai::nextget("n")
    ))
    list_wrapped <- lapply(
      list_from,
      function(x) terra::wrap(x, proxy = TRUE)
    )
    shared_args$locs_vector <- terra::wrap(locs_vector)
    if (!is.null(shared_args$weights_prepared)) {
      shared_args$weights_prepared <- terra::wrap(weights_prepared)
    }
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
    locs_vector <- terra::unwrap(locs_vector)
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
  if (terra::geomtype(locs_vector) == "polygons") {
    ### apply exactextractr::exact_extract for polygons
    extract_args <- list(
      x = layer,
      y = sf::st_as_sf(locs_vector),
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
  } else if (terra::geomtype(locs_vector) == "points") {
    if (is.null(weights_prepared)) {
      #### apply terra::extract for points
      sites_extracted_layer <- terra::extract(
        layer,
        locs_vector,
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
