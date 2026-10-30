# Optional external record checks. Nothing is assigned into the Amadeus namespace.
schema_validate <- function(doc, root, results = NULL) {
  pinned <- jsonlite::read_json(file.path(root, 'schema/PROVENANCE.json'))
  for (name in names(pinned$files)) {
    if (
      digest::digest(file = file.path(root, 'schema', name), algo = 'sha256') !=
        pinned$files[[name]]
    ) {
      stop(
        'Pinned schema artifact changed; rebuild/review the schema release: ',
        name
      )
    }
  }

  contract <- jsonlite::read_json(file.path(root, 'schema/contract.json'))
  encode <- function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      digits = NA,
      null = 'null'
    ))
  }
  jsonvalidate::json_validator(
    file.path(root, 'schema/amadeus.schema.json'),
    engine = 'ajv'
  )(encode(doc), error = TRUE, greedy = TRUE)
  checks <- new.env(parent = globalenv())
  # Scoped compatibility for the generator's lookahead expressions; base/amadeus unchanged.
  checks$grepl <- function(pattern, x, ...) {
    base::grepl(pattern, x, ..., perl = TRUE)
  }
  sys.source(file.path(root, 'schema/amadeus_validators.R'), checks)
  ids <- list()
  for (collection in names(contract$collections)) {
    cls <- contract$collections[[collection]]
    records <- doc[[collection]]
    keys <- vapply(records, function(x) x$id, '')
    if (anyDuplicated(keys)) {
      stop('Duplicate identifier in ', collection)
    }
    ids[[cls]] <- keys
    for (record in records) {
      if (length(contract$classes[[cls]]$inline)) {
        present <- intersect(
          names(record),
          names(contract$classes[[cls]]$inline)
        )
        if (length(present)) {
          stop(
            'This gridMET team test does not support inline nested class records'
          )
        }
      }
      answer <- get(paste0('check_', cls), checks)(record)
      if (!isTRUE(answer)) stop(answer)
    }
  }
  for (collection in names(contract$collections)) {
    for (record in doc[[collection]]) {
      cls <- contract$collections[[collection]]
      refs <- contract$classes[[cls]]$references
      for (field in names(refs)) {
        if (
          !is.null(record[[field]]) &&
            any(!unlist(record[[field]]) %in% ids[[refs[[field]]]])
        ) {
          stop('Dangling reference: ', cls, '.', field)
        }
      }
      for (field in intersect(
        names(record),
        c('processing_run', 'produced_by_run', 'upstream_runs')
      )) {
        if (any(!unlist(record[[field]]) %in% ids$ToolRun)) {
          stop('Dangling run reference')
        }
      }
      if (
        !is.null(record$input_asset_ids) &&
          any(!unlist(record$input_asset_ids) %in% ids$Asset)
      ) {
        stop('Dangling input asset')
      }
    }
  }
  lookup <- function(records) {
    setNames(records, vapply(records, function(x) x$id, ''))
  }
  locations <- lookup(doc$locations)
  requests <- lookup(doc$extraction_requests)
  variables <- lookup(doc$product_variables)
  canonical <- lookup(doc$canonical_variables)
  runs <- lookup(doc$tool_runs)
  assets <- lookup(doc$assets)
  parse_time <- function(x) {
    as.POSIXct(x, format = '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
  }
  if (length(locations)) {
    geometry <- sf::st_as_sfc(
      vapply(locations, function(x) x$location_geom_wkt, ''),
      crs = 4326
    )
    if (
      any(sf::st_is_empty(geometry)) ||
        any(!sf::st_is_valid(geometry)) ||
        any(as.character(sf::st_geometry_type(geometry)) != 'POINT')
    ) {
      stop('Invalid/unsupported location geometry')
    }
  }
  for (request in requests) {
    if (
      is.na(parse_time(request$time_window_start)) ||
        is.na(parse_time(request$time_window_end)) ||
        parse_time(request$time_window_start) >=
          parse_time(request$time_window_end)
    ) {
      stop('Reversed/invalid request window')
    }
  }
  visit <- function(id, trail = character()) {
    if (id %in% trail) {
      stop('Cyclic provenance')
    }
    for (parent in unlist(runs[[id]]$upstream_runs)) {
      visit(parent, c(trail, id))
    }
  }
  for (id in names(runs)) {
    visit(id)
  }
  values <- doc$ambient_values_at_location
  for (v in values) {
    pv <- variables[[v$product_variable]]
    cv <- canonical[[v$canonical_variable]]
    request <- requests[[v$extraction_request]]
    location <- locations[[v$location]]
    if (
      pv$canonical_variable != v$canonical_variable ||
        cv$units_ucum != v$value_unit_ucum ||
        pv$native_units_ucum != v$native_value_unit_ucum
    ) {
      stop('Variable binding or unit mismatch')
    }
    if (
      location$location_set != request$location_set ||
        !v$product_variable %in% unlist(request$requested_product_variables)
    ) {
      stop('Request/location/variable membership mismatch')
    }
    if (
      parse_time(v$valid_time_start) < parse_time(request$time_window_start) ||
        parse_time(v$valid_time_start) >= parse_time(request$time_window_end)
    ) {
      stop('Result outside request window')
    }
    if (
      assets[[v$asset]]$produced_by_run != v$processing_run ||
        runs[[v$processing_run]]$run_role != 'calculate'
    ) {
      stop('Result/asset/processing-run mismatch')
    }
    if (
      !is.finite(v$value) ||
        !is.finite(v$native_value) ||
        v$value != v$native_value
    ) {
      stop('Invalid/changed native value')
    }
    if (v$null_semantics != if (v$value == 0) 'true_zero' else 'present') {
      stop('Incorrect zero/missing status')
    }
  }
  if (!is.null(results)) {
    key <- function(variable, site, date) paste(variable, site, date, sep = '|')
    actual <- vapply(
      values,
      function(v) {
        key(
          variables[[v$product_variable]]$amadeus_variable_code,
          locations[[v$location]]$location_key,
          substr(v$valid_time_start, 1, 10)
        )
      },
      ''
    )
    expected <- key(results$variable, results$site_id, results$date)
    if (
      length(actual) != length(expected) ||
        anyDuplicated(actual) ||
        anyDuplicated(expected) ||
        !setequal(actual, expected)
    ) {
      stop('Missing/duplicate result keys')
    }
    ix <- match(actual, expected)
    for (i in seq_along(values)) {
      if (
        values[[i]]$valid_time_start !=
          paste0(results$date[ix[i]], 'T00:00:00Z') ||
          abs(values[[i]]$value - results$value[ix[i]]) > 1e-10
      ) {
        stop(
          'Result date-label clock or value differs from ordinary Amadeus result'
        )
      }
    }
  }
  invisible(TRUE)
}
