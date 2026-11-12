.schema_source_cache <- new.env(parent = emptyenv())
schema_source_metadata <- function(root, run_dir, request, source_existed_before) {
  runtime <- schema_release(root, verify = FALSE)
  registry <- runtime$registry[match(request$variables, runtime$registry$code), ]
  key <- digest::digest(list(normalizePath(root), normalizePath(run_dir), request$download_dir,
                             request$variables, request$year, runtime$release$runtime_files[['inputs/variables.csv']]))
  if (exists(key, envir = .schema_source_cache, inherits = FALSE)) {
    return(get(key, envir = .schema_source_cache, inherits = FALSE))
  }
  sha <- function(path) digest::digest(file = path, algo = 'sha256')
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
      acquisition = if (isTRUE(source_existed_before[[code]])) {
        'file existed before public downloader call'
      } else {
        'acquisition history not observed by post-calculation writer'
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
  jsonlite::write_json(metadata, file.path(run_dir, 'source-metadata.json'),
                       auto_unbox = TRUE, pretty = TRUE, digits = NA, null = 'null')
  assign(key, metadata, envir = .schema_source_cache)
  metadata
}
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
  runtime <- schema_release(root, verify = FALSE)
  registry <- runtime$registry[match(request$variables, runtime$registry$code), ]
  catalog_variables <- setNames(runtime$dataset_metadata$variables, vapply(runtime$dataset_metadata$variables,function(x)x$code,''))
  metadata <- schema_source_metadata(root, run_dir, request, source_existed_before)
  observation <- jsonlite::read_json(file.path(run_dir, 'source-observation.json'))
  for (code in request$variables) {
    if (is.null(observation[[code]]) ||
        observation[[code]]$sha256 != metadata[[code]]$sha256 ||
        observation[[code]]$path != normalizePath(file.path(request$download_dir, code, paste0(code,'_',request$year,'.nc'))))
      stop('Storage boundary: source changed since scientific-use observation')
  }

  raw <- setNames(lapply(request$variables, function(code) {
    file.path(request$download_dir, code, paste0(code, '_', request$year, '.nc'))
  }), request$variables)
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
      label = catalog_variables[[code]]$label,
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
      label = catalog_variables[[code]]$label,
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
  asset <- function(key, path, producer, code, format = NULL, known_sha256 = NULL) {
    a <- list(
      id = id('asset', key),
      product = id('product', 'daily'),
      local_path = normalizePath(path),
      size_bytes = as.numeric(file.info(path)$size),
      sha256 = if (is.null(known_sha256)) sha(path) else known_sha256,
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
      'netcdf4_cf',
      known_sha256 = metadata[[code]]$sha256
    )
    a$url <- metadata[[code]]$declared_provider_url
    a
  })
  for (c in calls) {
    if (c$role != 'download') {
      code <- if (!is.null(c$variable)) c$variable else c$arguments$variable
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
  # Full record validation occurs once on the serialized JSON below.
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
  # Exact encoded roundtrip protects content before the one full validation.
  if (!identical(encode(doc), encode(serialized))) {
    stop('JSON serialization changed records')
  }
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
  # YAML parity above protects the alternate representation; validate actual JSON.
  storage_validate(serialized, root, results, run_dir)
  for (a in serialized$assets) {
    if (sha(a$local_path) != a$sha256) stop('Asset changed during export')
  }
  if (!identical(encode(metadata), encode(jsonlite::read_json(file.path(run_dir, 'source-metadata.json'))))) {
    stop('Observed source metadata changed after reading')
  }
  receipt <- list(
    accepted = TRUE,
    integration_revision = 'direct-output-option-A-v1',
    full_record_validations_at_export = 1L,
    source_metadata_sha256 = sha(file.path(run_dir, 'source-metadata.json')),
    dataset_catalog_revision = runtime$dataset_metadata$revision,
    dataset_catalog_sha256 = sha(file.path(root,'catalog/gridmet.json')),
    runtime_contract_sha256 = sha(file.path(root, 'release/runtime-contract.json')),
    source_hash_policy = 'initial metadata digest plus final integrity verification',
    validation_boundary = 'storage JSON structure, geometry, generated SQL and typed Parquet readback', 
    records = nrow(results),
    schema_sha256 = sha(file.path(root, 'schema/amadeus.schema.json')),
    instance_sha256 = sha(path),
    ordinary_results_sha256 = sha(file.path(run_dir, 'results.csv')),
    amadeus_version = as.character(packageVersion('amadeus')),
    scientific_transformations = FALSE,
    JSON_YAML_agree = TRUE,
    date_convention = 'Nominal UTC00 date labels only; physical interval/timezone unqualified',
    direct_output_context = .direct_output_context,
    production_approved = FALSE
  )
  write_json(receipt, file.path(run_dir, 'acceptance.json'))
  receipt
}

# External automatic boundary: users call the existing CLI, never individual validators.
# schema_export performs the single full record-validation pass before acceptance;
# selected storage enforces its own load/readback integrity. No checks are disabled.
schema_write_and_load <- function(root, run_dir, request, results, calls, before,
                                  settings, raster_checks, .perf) {
.perf$stage('storage_contract_entry')
  runtime <- schema_release(root, verify = TRUE)
  variables <- request$variables; dates <- request$dates; locations <- request$locations
if (
  anyNA(dates) ||
    dates[1] > dates[2] ||
    format(dates[1], '%Y') != format(dates[2], '%Y')
) {
  stop('Dates must be ordered and within one year for this example')
}
if (
  !all(c('site_id', 'lon', 'lat') %in% names(locations)) ||
    nrow(locations) == 0 ||
    anyNA(locations[, c('site_id', 'lon', 'lat')]) ||
    anyDuplicated(locations$site_id) ||
    any(!grepl('^[A-Za-z0-9_.-]+$', locations$site_id))
) {
  stop('sites CSV needs unique simple site_id,lon,lat columns')
}
if (
  any(!is.finite(locations$lon)) ||
    any(!is.finite(locations$lat)) ||
    any(abs(locations$lon) > 180) ||
    any(abs(locations$lat) > 90)
) {
  stop('Invalid lon/lat')
}
if (
  anyDuplicated(variables) ||
    !length(variables) ||
    any(!variables %in% runtime$registry$code)
) {
  stop('Variable outside declared16-daily-variable test profile')
}
  # Raster inspection is delayed to storage; the scientific-stage early guards are removed.
  expected_dates <- seq(dates[1], dates[2], by = 'day')
  process_calls <- Filter(function(x)x$role=='process',calls)
  for (call in process_calls) {
    raster <- terra::rast(call$output)
    if(terra::nlyr(raster)!=length(expected_dates)||!identical(as.character(as.Date(terra::time(raster))),as.character(expected_dates)))stop('Storage boundary: supplied raster dates/layers mismatch')
    counts<-as.numeric(terra::global(is.finite(raster),'sum',na.rm=TRUE)[,1])
    if(any(!is.finite(counts))||any(counts==0))stop('Storage boundary: all-missing requested raster layer')
    raster_checks[[call$key]]<-list(path=call$output,finite_cells=as.list(counts),boundary='post_calculation_serialized_output')
  }
.perf$stage('schema_export')
receipt <- schema_export(root, run_dir, request, results, calls, before)
.perf$stage('receipts_and_session')
writeLines(
  capture.output(sessionInfo()),
  file.path(run_dir, 'session-info.txt')
)
jsonlite::write_json(raster_checks, file.path(run_dir, 'raster-boundary-checks.json'), auto_unbox = TRUE, pretty = TRUE)
cat(
  'Ordinary Amadeus results and accepted schema records:',
  nrow(results),
  '\nRun:',
  run_dir,
  '\n'
)
if (settings$engine != 'none') {
  engines <- if (settings$engine == 'both') {
    c('duckdb', 'sedonadb')
  } else {
    settings$engine
  }
  rscript <- file.path(
    R.home('bin'),
    paste0('Rscript', if (.Platform$OS.type == 'windows') '.exe' else '')
  )
  for (engine in engines) {
    .perf$stage(paste0('database_', engine))
    if (engine == 'duckdb' && settings$storage != 'rowwise') {
      status <- system2(rscript, vapply(c(file.path(root, 'R/parquet_experiment.R'),
        run_dir, file.path(run_dir, 'database/duckdb'), settings$storage, 'operational'), shQuote, ''))
    } else {
      status <- system2(rscript, vapply(c(file.path(root, 'R/database.R'), run_dir, engine), shQuote, ''))
    }
    if (status != 0) {
      stop(
        'Optional database hand-off failed; scientific/schema outputs preserved'
      )
    }
  }
}

.perf$stage('workflow_completion')
if (settings$engine != 'none') for (engine in engines) {
  db_receipt <- jsonlite::read_json(file.path(run_dir, 'database', engine, 'receipt.json'))
  if (db_receipt$records != nrow(results) || !isTRUE(db_receipt$all_within_declared_tolerances) ||
      db_receipt$accepted_instance_sha256 != receipt$instance_sha256) stop('Database completion receipt mismatch')
}
publication <- storage_publish_manifest(root,run_dir,if(settings$engine=='duckdb') 'duckdb' else character())
jsonlite::write_json(list(completed = TRUE, integration_revision = 'direct-output-option-A-v1',
  schema_accepted = TRUE, selected_engine = settings$engine, selected_storage = settings$storage,
  publication_manifest_sha256 = publication$sha256, publication_policy = publication$policy,
  direct_output_context = .direct_output_context,
  dataset_catalog_sha256 = receipt$dataset_catalog_sha256,
  accepted_records = nrow(results), instance_sha256 = receipt$instance_sha256,
  runtime_contract_sha256 = receipt$runtime_contract_sha256, production_approved = FALSE),
  file.path(run_dir, 'workflow-receipt.json'), auto_unbox = TRUE, pretty = TRUE)
if(!Sys.chmod(file.path(run_dir,'workflow-receipt.json'),mode='0444'))stop('Could not mark workflow receipt read-only')
# Final pointer replacement publishes only completed runs; not a multi-file durability guarantee.
latest_tmp <- tempfile('.LATEST-',tmpdir=settings$output)
writeLines(run_dir, latest_tmp)
if(!file.rename(latest_tmp,file.path(settings$output,'LATEST.txt')))stop('Could not publish completion pointer')
  invisible(receipt)
}
