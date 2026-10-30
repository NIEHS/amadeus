# Ordinary Amadeus request first; schema is an explicit post-science hand-off.
Sys.setenv(TZ = 'UTC') # Explicit representation convention in this test process.
argv <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(sub(
  '^--file=',
  '',
  grep('^--file=', commandArgs(), value = TRUE)[1]
))
root <- dirname(script)
settings <- list(
  variables = NULL,
  download_dir = '',
  start = NULL,
  end = NULL,
  sites = NULL,
  engine = NULL,
  output = file.path(root, 'outputs'),
  phi_status = 'no_phi'
)
usage <- 'Rscript example_gridmet.R --variables CODE[,CODE...]|all --start YYYY-MM-DD --end YYYY-MM-DD --sites YOUR.csv --engine none|duckdb|sedonadb|both [--output DIR] [--download-dir DIR] [--phi-status STATUS]'
if (identical(argv, '--help')) {
  cat(usage, '\n')
  quit(save = 'no', status = 0)
}
provided <- character()
if (length(argv) %% 2 != 0) {
  stop('Use --option value pairs; see README')
}
if (length(argv)) {
  for (i in seq(1, length(argv), by = 2)) {
    name <- gsub('-', '_', sub('^--', '', argv[i]), fixed = TRUE)
    if (!name %in% names(settings)) {
      stop('Unknown option: ', argv[i])
    }
    if (name %in% provided) {
      stop('Duplicate option: ', argv[i])
    }
    provided <- c(provided, name)
    settings[[name]] <- argv[i + 1]
  }
}
required_inputs <- c('variables', 'start', 'end', 'sites', 'engine')
absent <- required_inputs[
  !vapply(
    settings[required_inputs],
    function(x) is.character(x) && length(x) == 1L && nzchar(trimws(x)),
    logical(1)
  )
]
if (length(absent)) {
  stop(
    'Missing explicit request inputs: ',
    paste(paste0('--', absent), collapse = ', '),
    '\n',
    usage
  )
}
needed <- c(
  'amadeus',
  'terra',
  'sf',
  'jsonlite',
  'yaml',
  'digest',
  'checkmate',
  'jsonvalidate',
  'ncdf4'
)
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    'Missing R packages: ',
    paste(missing, collapse = ', '),
    '. See setup instructions; no automatic installation is performed.'
  )
}
if (!settings$engine %in% c('none', 'duckdb', 'sedonadb', 'both')) {
  stop('engine must be none,duckdb,sedonadb,both')
}
if (!settings$phi_status %in% c('no_phi', 'aggregated_no_phi', 'phi_present')) {
  stop('Unsupported phi-status')
}
if (
  settings$engine %in%
    c('duckdb', 'both') &&
    (!nzchar(system.file(package = 'DBI')) ||
      !nzchar(system.file(package = 'duckdb')))
) {
  stop('Optional DuckDB packages missing')
}
if (
  settings$engine %in%
    c('sedonadb', 'both') &&
    !nzchar(system.file(package = 'sedonadb'))
) {
  stop(
    'Optional native R sedonadb package missing; choose --engine none/duckdb, no Python fallback'
  )
}
variables <- if (settings$variables == 'all') {
  read.csv(file.path(root, 'inputs/variables.csv'))$code
} else {
  strsplit(settings$variables, ',', fixed = TRUE)[[1]]
}
locations <- read.csv(settings$sites, stringsAsFactors = FALSE)
dates <- as.Date(c(settings$start, settings$end))
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
    any(!variables %in% read.csv(file.path(root, 'inputs/variables.csv'))$code)
) {
  stop('Variable outside declared16-daily-variable test profile')
}
run_id <- substr(
  digest::digest(list(Sys.time(), Sys.getpid(), runif(1)), algo = 'sha256'),
  1,
  20
)
run_dir <- file.path(settings$output, paste0('run-', run_id))
if (dir.exists(run_dir)) {
  stop('Run already exists')
}
dir.create(run_dir, recursive = TRUE)
run_dir <- normalizePath(run_dir)
for (d in c('raw', 'processed', 'ordinary')) {
  dir.create(file.path(run_dir, d))
}
# These are the actual user inputs passed to Amadeus. No dataset dispatch from a schema.
request <- list(
  variables = variables,
  dates = dates,
  locations = locations,
  year = as.integer(format(dates[1], '%Y')),
  download_dir = if (nzchar(settings$download_dir)) {
    settings$download_dir
  } else {
    file.path(run_dir, 'raw')
  },
  phi_status = settings$phi_status
)
dir.create(request$download_dir, recursive = TRUE, showWarnings = FALSE)
request$download_dir <- normalizePath(request$download_dir)
saveRDS(request, file.path(run_dir, 'request.rds'))
write.csv(
  locations,
  file.path(run_dir, 'requested-sites.csv'),
  row.names = FALSE
)
calls <- list()
results <- list()
before <- setNames(
  lapply(variables, function(code) {
    file.exists(file.path(
      request$download_dir,
      code,
      paste0(code, '_', request$year, '.nc')
    ))
  }),
  variables
)
started <- Sys.time()
amadeus::download_data(
  dataset_name = 'gridmet',
  variables = request$variables,
  year = request$year,
  directory_to_save = request$download_dir,
  acknowledgement = TRUE,
  show_progress = FALSE,
  max_tries = 2,
  rate_limit = 2
)
downloaded_paths <- vapply(
  variables,
  function(code) {
    file.path(
      request$download_dir,
      code,
      paste0(code, '_', request$year, '.nc')
    )
  },
  ''
)
if (any(!file.exists(downloaded_paths))) {
  stop(
    'Download incomplete; inspect provider/network errors. No schema acceptance has been issued.'
  )
}
calls[[1]] <- list(
  key = 'download',
  role = 'download',
  function_name = 'download_data',
  started = started,
  ended = Sys.time(),
  arguments = list(
    dataset_name = 'gridmet',
    variables = as.list(variables),
    year = request$year
  )
)
for (code in variables) {
  for (i in seq_len(nrow(locations))) {
    site <- locations[i, ]
    half_width <- 0.12
    process_key <- paste('process', code, site$site_id, sep = '/')
    started <- Sys.time()
    processed <- amadeus::process_covariates(
      covariate = 'gridmet',
      variable = code,
      date = as.character(request$dates),
      path = file.path(request$download_dir, code),
      extent = terra::ext(
        site$lon - half_width,
        site$lon + half_width,
        site$lat - half_width,
        site$lat + half_width
      )
    )
    expected_dates <- seq(dates[1], dates[2], by = 'day')
    if (
      terra::nlyr(processed) != length(expected_dates) ||
        !identical(
          as.character(as.Date(terra::time(processed))),
          as.character(expected_dates)
        )
    ) {
      stop('Unexpected processed dates; do not relabel or repair')
    }
    tile <- file.path(
      run_dir,
      'processed',
      paste0(code, '-', site$site_id, '.tif')
    )
    terra::writeRaster(processed, tile, overwrite = FALSE, datatype = 'FLT8S')
    calls[[length(calls) + 1L]] <- list(
      key = process_key,
      parent = 'download',
      role = 'process',
      function_name = 'process_covariates',
      started = started,
      ended = Sys.time(),
      output = tile,
      arguments = list(
        covariate = 'gridmet',
        variable = code,
        site_id = site$site_id,
        dates = as.character(dates),
        extent = as.list(c(
          site$lon - half_width,
          site$lon + half_width,
          site$lat - half_width,
          site$lat + half_width
        ))
      )
    )
    # Standard explicit disk hand-off; preserve values and requested dates/layer names.
    from <- terra::rast(tile)
    terra::time(from) <- expected_dates
    names(from) <- paste0(code, '_', format(expected_dates, '%Y%m%d'))
    locs <- sf::st_as_sf(site, coords = c('lon', 'lat'), crs = 4326)
    calc_key <- paste('calculate', code, site$site_id, sep = '/')
    started <- Sys.time()
    ordinary <- amadeus::calculate_covariates(
      covariate = 'gridmet',
      from = from,
      locs = locs,
      locs_id = 'site_id',
      radius = 0,
      fun = 'mean',
      .by_time = NULL,
      geom = FALSE
    )
    result_file <- file.path(
      run_dir,
      'ordinary',
      paste0(code, '-', site$site_id, '.rds')
    )
    saveRDS(ordinary, result_file)
    calls[[length(calls) + 1L]] <- list(
      key = calc_key,
      parent = process_key,
      role = 'calculate',
      function_name = 'calculate_covariates',
      started = started,
      ended = Sys.time(),
      output = result_file,
      arguments = list(
        covariate = 'gridmet',
        variable = code,
        site_id = site$site_id,
        radius = 0,
        fun = 'mean',
        by_time = 'NULL; existing daily gridMET route',
        geom = FALSE
      )
    )
    tab <- as.data.frame(ordinary)
    column <- grep(paste0('^', code, '(_|$)'), names(tab), value = TRUE)
    if (
      length(column) != 1 ||
        nrow(tab) != length(expected_dates) ||
        anyDuplicated(as.Date(tab$time))
    ) {
      stop('Unexpected ordinary Amadeus result shape')
    }
    ix <- match(as.character(expected_dates), as.character(as.Date(tab$time)))
    if (anyNA(ix)) {
      stop('Missing requested day')
    }
    results[[length(results) + 1L]] <- data.frame(
      variable = code,
      site_id = site$site_id,
      date = as.character(expected_dates),
      value = as.numeric(tab[[column]][ix]),
      call_key = calc_key
    )
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(run_dir, 'results.csv'), row.names = FALSE)
saveRDS(calls, file.path(run_dir, 'calls.rds'))
saveRDS(before, file.path(run_dir, 'source-existed-before.rds'))
# Optional schema capability starts HERE, after unchanged Amadeus scientific calls.
schema <- new.env(parent = globalenv())
sys.source(file.path(root, 'R/schema_validator.R'), schema)
sys.source(file.path(root, 'R/schema_export.R'), schema)
receipt <- schema$schema_export(root, run_dir, request, results, calls, before)
writeLines(
  capture.output(sessionInfo()),
  file.path(run_dir, 'session-info.txt')
)
writeLines(run_dir, file.path(settings$output, 'LATEST.txt'))
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
    status <- system2(
      rscript,
      vapply(c(file.path(root, 'R/database.R'), run_dir, engine), shQuote, '')
    )
    if (status != 0) {
      stop(
        'Optional database hand-off failed; scientific/schema outputs preserved'
      )
    }
  }
}
