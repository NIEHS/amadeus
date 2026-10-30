# Post-science metadata hand-off. Does not call download/process/calculate.
schema_export <- function(
  root,
  run_dir,
  request,
  results,
  calls,
  source_existed_before
) {
  sha <- function(path) digest::digest(file = path, algo = 'sha256')
  encode <- function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      digits = NA,
      null = 'null'
    ))
  }
  write_json <- function(x, path) {
    jsonlite::write_json(
      x,
      path,
      auto_unbox = TRUE,
      pretty = TRUE,
      digits = NA,
      null = 'null'
    )
  }
  id <- function(kind, key) {
    paste0('gridmetteam:', basename(run_dir), '/', kind, '/', key)
  }
  registry <- read.csv(
    file.path(root, 'inputs/variables.csv'),
    stringsAsFactors = FALSE
  )
  registry <- registry[match(request$variables, registry$code), ]
  metadata <- list()
  raw <- list()
  for (code in request$variables) {
    path <- file.path(
      request$download_dir,
      code,
      paste0(code, '_', request$year, '.nc')
    )
    if (!file.exists(path)) {
      stop('Requested raw source not found: ', code)
    }
    nc <- ncdf4::nc_open(path)
    candidates <- names(nc$var)[vapply(
      nc$var,
      function(v) {
        length(v$dim) == 3 &&
          any(grepl('lon', tolower(vapply(v$dim, function(d) d$name, '')))) &&
          any(grepl('lat', tolower(vapply(v$dim, function(d) d$name, ''))))
      },
      logical(1)
    )]
    if (length(candidates) != 1) {
      ncdf4::nc_close(nc)
      stop('Ambiguous source variable')
    }
    dims <- nc$var[[candidates]]$dim
    names_dim <- tolower(vapply(dims, function(d) d$name, ''))
    lon <- dims[[which(grepl('lon', names_dim))]]$vals
    lat <- dims[[which(grepl('lat', names_dim))]]$vals
    spacing <- c(abs(mean(diff(lon))), abs(mean(diff(lat))))
    if (
      any(abs(abs(diff(lon)) - spacing[1]) > 1e-7) ||
        any(abs(abs(diff(lat)) - spacing[2]) > 1e-7)
    ) {
      stop('Nonuniform source grid not supported by this example')
    }
    attrs <- ncdf4::ncatt_get(nc, candidates)
    ncdf4::nc_close(nc)
    pol <- registry[registry$code == code, ]
    if (
      !tolower(gsub(' ', '', attrs$units, fixed = TRUE)) %in%
        strsplit(pol$accepted_source_units, '|', fixed = TRUE)[[1]]
    ) {
      stop('Unreviewed source units for ', code)
    }
    raw[[code]] <- path
    metadata[[code]] <- list(
      source_variable = candidates,
      spacing_degrees = as.list(spacing),
      attributes = attrs,
      sha256 = sha(path),
      bytes = as.numeric(file.info(path)$size),
      acquisition = if (source_existed_before[[code]]) {
        'file existed before public downloader call'
      } else {
        'file absent before public downloader call'
      },
      declared_provider_url = paste0(
        'https://www.northwestknowledge.net/metdata/data/',
        code,
        '_',
        request$year,
        '.nc'
      )
    )
  }
  dates <- seq(request$dates[1], request$dates[2], by = 'day')
  expected <- expand.grid(
    variable = request$variables,
    site_id = request$locations$site_id,
    date = as.character(dates),
    stringsAsFactors = FALSE
  )
  key <- function(x) paste(x$variable, x$site_id, x$date, sep = '|')
  if (
    anyDuplicated(key(results)) ||
      nrow(results) != nrow(expected) ||
      !setequal(key(results), key(expected)) ||
      any(!is.finite(results$value))
  ) {
    stop('Incomplete/nonfinite ordinary results; no zero filling')
  }
  for (code in intersect(
    c('pr', 'pet', 'etr', 'rmax', 'rmin', 'sph', 'vpd', 'srad', 'vs'),
    request$variables
  )) {
    if (any(results$value[results$variable == code] < 0)) {
      stop('Value outside declared nonnegative profile')
    }
  }
  for (code in intersect(c('rmax', 'rmin'), request$variables)) {
    if (any(results$value[results$variable == code] > 100)) {
      stop('Humidity above100')
    }
  }
  if (
    'th' %in%
      request$variables &&
      any(
        results$value[results$variable == 'th'] < 0 |
          results$value[results$variable == 'th'] > 360
      )
  ) {
    stop('Wind direction outside profile')
  }
  doc <- list(
    schema_version = '0.1.0',
    data_sources = list(list(
      id = id('source', 'gridmet'),
      name = 'gridmet',
      homepage = 'https://www.climatologylab.org/gridmet.html',
      access_protocol = 'https_bulk_file'
    )),
    products = list(list(
      id = id('product', 'daily'),
      name = 'gridmet_daily',
      data_source = id('source', 'gridmet'),
      spatial_support_type = 'raster_grid_cell',
      grid_definition = id('grid', 'native'),
      crs = 'EPSG:4326',
      srid = 4326L,
      temporal_resolution = 'daily',
      day_boundary_convention = 'observation_dependent',
      amadeus_dataset_name = 'gridmet',
      amadeus_process_covariate = 'gridmet',
      amadeus_calculate_covariate = 'gridmet',
      description = 'Team test: source date label encoded at nominal00:00UTC for serialization only. No verified physical sample interval, timezone or ontology equivalence asserted. No temporal aggregation.'
    ))
  )
  grid <- unlist(metadata[[1]]$spacing_degrees)
  for (m in metadata) {
    if (any(abs(unlist(m$spacing_degrees) - grid) > 1e-7)) {
      stop('Source grids disagree')
    }
  }
  doc$grid_definitions <- list(list(
    id = id('grid', 'native'),
    crs = 'EPSG:4326',
    resolution_x = grid[1],
    resolution_y = grid[2],
    resolution_unit = 'deg'
  ))
  doc$canonical_variables <- lapply(request$variables, function(code) {
    list(
      id = id('variable', code),
      name = paste0('gridmet_native_', code),
      standard_name = paste0('gridmetteam:native_', code),
      units_ucum = registry$native_ucum[match(code, registry$code)],
      value_data_type = 'continuous_numeric',
      extensivity = 'intensive',
      description = 'Local candidate for native point quantity; spatial support classification only; no temporal aggregation approval.'
    )
  })
  doc$product_variables <- lapply(request$variables, function(code) {
    list(
      id = id('product-variable', code),
      product = id('product', 'daily'),
      canonical_variable = id('variable', code),
      native_name = metadata[[code]]$source_variable,
      native_units_ucum = registry$native_ucum[match(code, registry$code)],
      amadeus_variable_code = code,
      harmonization_note = 'Identity numeric mapping; source unit spelling normalized only. Original attributes retained in source-metadata.json.'
    )
  })
  doc$location_sets <- list(list(
    id = id('location-set', 'user'),
    locs_id_field = 'site_id',
    crs = 'EPSG:4326',
    phi_status = request$phi_status,
    location_count = as.integer(nrow(request$locations))
  ))
  doc$locations <- lapply(seq_len(nrow(request$locations)), function(i) {
    s <- request$locations[i, ]
    list(
      id = id('location', s$site_id),
      location_set = id('location-set', 'user'),
      location_key = s$site_id,
      location_geom_wkt = sprintf('POINT (%.12f %.12f)', s$lon, s$lat),
      srid = 4326L
    )
  })
  req <- list(
    id = id('request', 'user'),
    location_set = id('location-set', 'user'),
    requested_canonical_variables = unname(as.list(vapply(
      request$variables,
      function(c) id('variable', c),
      ''
    ))),
    requested_product_variables = unname(as.list(vapply(
      request$variables,
      function(c) id('product-variable', c),
      ''
    ))),
    time_window_start = paste0(request$dates[1], 'T00:00:00Z'),
    time_window_end = paste0(request$dates[2] + 1, 'T00:00:00Z'),
    extraction_method = 'nearest_cell',
    buffer_radius_m = 0,
    temporal_grouping = 'day'
  )
  req$request_hash <- digest::digest(
    encode(list(
      variables = request$variables,
      dates = as.character(request$dates),
      locations = request$locations,
      radius = 0
    )),
    algo = 'sha256',
    serialize = FALSE
  )
  doc$extraction_requests <- list(req)
  doc$tool_runs <- lapply(calls, function(c) {
    r <- list(
      id = id('run', c$key),
      run_role = c$role,
      tool_name = 'amadeus',
      tool_version = as.character(packageVersion('amadeus')),
      function_name = c$function_name,
      run_arguments = encode(c$arguments),
      run_timestamp_utc = format(c$started, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
      run_duration_seconds = as.numeric(difftime(
        c$ended,
        c$started,
        units = 'secs'
      )),
      status = 'succeeded'
    )
    if (c$role %in% c('process', 'calculate')) {
      r$output_row_count <- if (c$role == 'calculate') {
        as.integer(nrow(as.data.frame(readRDS(c$output))))
      } else {
        0L
      }
      if (c$role == 'process') {
        r$run_arguments <- encode(c(
          c$arguments,
          list(
            output_row_count_convention = '0 tabular rows for raster output; raster dimensions are in the saved file'
          )
        ))
      }
    }
    if (!is.null(c$parent)) {
      r$upstream_runs <- list(id('run', c$parent))
    }
    r
  })
  asset <- function(key, path, producer, code, format = NULL) {
    a <- list(
      id = id('asset', key),
      product = id('product', 'daily'),
      local_path = normalizePath(path),
      size_bytes = as.numeric(file.info(path)$size),
      sha256 = sha(path),
      produced_by_run = id('run', producer),
      variables_present = list(id('product-variable', code)),
      materialization_mode = 'full_materialization',
      value_state = 'materialized'
    )
    if (!is.null(format)) {
      a$native_format <- format
    }
    a
  }
  doc$assets <- lapply(request$variables, function(code) {
    a <- asset(
      paste0('raw/', code),
      raw[[code]],
      'download',
      code,
      'netcdf4_cf'
    )
    a$url <- metadata[[code]]$declared_provider_url
    a
  })
  for (c in calls) {
    if (c$role != 'download') {
      code <- c$arguments$variable
      doc$assets <- c(
        doc$assets,
        list(asset(
          c$key,
          c$output,
          c$key,
          code,
          if (c$role == 'process') 'geotiff' else NULL
        ))
      )
      if (c$role == 'process' && file.exists(paste0(c$output, '.aux.xml'))) {
        doc$assets <- c(
          doc$assets,
          list(asset(
            paste0(c$key, '/metadata-companion'),
            paste0(c$output, '.aux.xml'),
            c$key,
            code
          ))
        )
      }
    }
  }
  for (i in seq_along(doc$tool_runs)) {
    c <- calls[[i]]
    if (c$role == 'process') {
      doc$tool_runs[[i]]$input_asset_ids <- list(id(
        'asset',
        paste0('raw/', c$arguments$variable)
      ))
    }
    if (c$role == 'calculate') {
      doc$tool_runs[[i]]$input_asset_ids <- list(id('asset', c$parent))
    }
  }
  doc$ambient_values_at_location <- lapply(seq_len(nrow(results)), function(i) {
    x <- results[i, ]
    u <- registry$native_ucum[match(x$variable, registry$code)]
    list(
      id = id('value', paste(x$variable, x$site_id, x$date, sep = '/')),
      location = id('location', x$site_id),
      extraction_request = req$id,
      extraction_method = 'nearest_cell',
      product_variable = id('product-variable', x$variable),
      canonical_variable = id('variable', x$variable),
      asset = id('asset', x$call_key),
      processing_run = id('run', x$call_key),
      value = as.numeric(x$value),
      native_value = as.numeric(x$value),
      value_unit_ucum = u,
      native_value_unit_ucum = u,
      valid_time_start = paste0(x$date, 'T00:00:00Z'),
      temporal_grouping = 'day',
      contributing_value_count = 1L,
      data_status = 'unknown',
      source_system = 'gridmet',
      null_semantics = if (x$value == 0) 'true_zero' else 'present'
    )
  })
  schema_validate(doc, root, results)
  path <- file.path(run_dir, 'schema-instance.json')
  if (file.exists(path)) {
    stop('Refuse overwriting accepted instance')
  }
  write_json(doc, path)
  yaml::write_yaml(
    jsonlite::read_json(path),
    file.path(run_dir, 'schema-instance.yaml'),
    precision = 17
  )
  serialized <- jsonlite::read_json(path)
  schema_validate(serialized, root, results)
  other <- yaml::read_yaml(file.path(run_dir, 'schema-instance.yaml'))
  # YAML readers simplify scalar sequences to atomic vectors; restore declared array representation.
  contract <- jsonlite::read_json(file.path(root, 'schema/contract.json'))
  for (collection in names(contract$collections)) {
    if (!is.null(other[[collection]])) {
      cls <- contract$collections[[collection]]
      other[[collection]] <- lapply(other[[collection]], function(record) {
        for (field in unlist(contract$classes[[cls]]$multiple)) {
          if (!is.null(record[[field]])) {
            record[[field]] <- unname(as.list(record[[field]]))
          }
        }
        record
      })
    }
  }

  schema_validate(other, root, results)
  if (
    !isTRUE(all.equal(
      serialized,
      other,
      tolerance = 0,
      check.attributes = FALSE
    ))
  ) {
    stop('JSON/YAML records differ')
  }
  for (a in serialized$assets) {
    if (sha(a$local_path) != a$sha256) stop('Asset changed during export')
  }
  write_json(metadata, file.path(run_dir, 'source-metadata.json'))
  receipt <- list(
    accepted = TRUE,
    records = nrow(results),
    schema_sha256 = sha(file.path(root, 'schema/amadeus.schema.json')),
    instance_sha256 = sha(path),
    ordinary_results_sha256 = sha(file.path(run_dir, 'results.csv')),
    amadeus_version = as.character(packageVersion('amadeus')),
    scientific_transformations = FALSE,
    JSON_YAML_agree = TRUE,
    date_convention = 'Nominal UTC00 date labels only; physical interval/timezone unqualified',
    production_approved = FALSE
  )
  write_json(receipt, file.path(run_dir, 'acceptance.json'))
  receipt
}
