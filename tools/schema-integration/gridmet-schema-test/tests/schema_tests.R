# Invalid fixtures are in memory only. Accepted results and package are never edited.
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(sub(
  '^--file=',
  '',
  grep('^--file=', commandArgs(), value = TRUE)[1]
))
root <- dirname(dirname(script))
run_dir <- if (length(args)) {
  normalizePath(args[1])
} else {
  readLines(file.path(root, 'outputs/LATEST.txt'), n = 1)
}
schema <- new.env(parent = globalenv())
sys.source(file.path(root, 'R/schema_validator.R'), schema)
doc <- jsonlite::read_json(file.path(run_dir, 'schema-instance.json'))
results <- read.csv(file.path(run_dir, 'results.csv'))
receipt <- jsonlite::read_json(file.path(run_dir, 'acceptance.json'))
stopifnot(
  receipt$accepted,
  digest::digest(
    file = file.path(run_dir, 'schema-instance.json'),
    algo = 'sha256'
  ) ==
    receipt$instance_sha256
)
report <- list()
case <- function(name, fn, expected_error = NULL) {
  error <- tryCatch(
    {
      fn()
      NULL
    },
    error = conditionMessage
  )
  pass <- if (is.null(expected_error)) {
    is.null(error)
  } else {
    !is.null(error) && grepl(expected_error, error)
  }
  report[[length(report) + 1L]] <<- data.frame(
    test = name,
    expected = if (is.null(expected_error)) 'accept' else expected_error,
    observed = if (is.null(error)) 'accepted' else error,
    passed = pass
  )
  if (!pass) warning('Unexpected test result: ', name)
}
validate <- function(x) schema$schema_validate(x, root, results)
case('Actual ordinary results and schema records', function() validate(doc))
case(
  'Schema: missing version',
  function() {
    x <- doc
    x$schema_version <- NULL
    validate(x)
  },
  'schema_version'
)
case(
  'Schema: invalid status',
  function() {
    x <- doc
    x$ambient_values_at_location[[1]]$data_status <- 'wrong'
    validate(x)
  },
  'data_status'
)
case(
  'Duplicate identifier',
  function() {
    x <- doc
    x$locations <- c(x$locations, x$locations[1])
    validate(x)
  },
  'Duplicate identifier'
)
case(
  'Dangling location',
  function() {
    x <- doc
    x$ambient_values_at_location[[1]]$location <- 'absent'
    validate(x)
  },
  'Dangling reference'
)
case(
  'Wrong canonical binding',
  function() {
    x <- doc
    extra <- x$canonical_variables[[1]]
    extra$id <- paste0(extra$id, '-other')
    x$canonical_variables <- c(x$canonical_variables, list(extra))
    x$ambient_values_at_location[[1]]$canonical_variable <- extra$id
    validate(x)
  },
  'Variable binding'
)
case(
  'Reversed request',
  function() {
    x <- doc
    x$extraction_requests[[1]]$time_window_end <- x$extraction_requests[[
      1
    ]]$time_window_start
    validate(x)
  },
  'Reversed/invalid request'
)
case(
  'Malformed WKT',
  function() {
    x <- doc
    x$locations[[1]]$location_geom_wkt <- 'THIS IS NOT WKT'
    validate(x)
  },
  'OGR|WKT|geometry|ParseException'
)
case(
  'Wrong date-label clock',
  function() {
    x <- doc
    x$ambient_values_at_location[[1]]$valid_time_start <- sub(
      'T00:00:00Z',
      'T12:00:00Z',
      x$ambient_values_at_location[[1]]$valid_time_start,
      fixed = TRUE
    )
    validate(x)
  },
  'date-label clock'
)
case(
  'Wrong processing run',
  function() {
    x <- doc
    stopifnot(
      x$ambient_values_at_location[[1]]$processing_run != x$tool_runs[[1]]$id
    )
    x$ambient_values_at_location[[1]]$processing_run <- x$tool_runs[[1]]$id
    validate(x)
  },
  'processing-run mismatch'
)
case(
  'Wrong location-set membership',
  function() {
    x <- doc
    set <- x$location_sets[[1]]
    set$id <- paste0(set$id, '-other')
    x$location_sets <- c(x$location_sets, list(set))
    x$locations[[1]]$location_set <- set$id
    validate(x)
  },
  'membership mismatch'
)
case(
  'Cyclic provenance',
  function() {
    x <- doc
    x$tool_runs[[1]]$upstream_runs <- list(x$tool_runs[[1]]$id)
    validate(x)
  },
  'Cyclic provenance'
)
case(
  'Changed value',
  function() {
    x <- doc
    x$ambient_values_at_location[[1]]$value <- x$ambient_values_at_location[[
      1
    ]]$value +
      1
    validate(x)
  },
  'Invalid/changed native value'
)
case(
  'Incorrect present/missing semantics',
  function() {
    x <- doc
    x$ambient_values_at_location[[1]]$null_semantics <- 'structural_null'
    validate(x)
  },
  'zero/missing status'
)
case(
  'Omitted result',
  function() {
    x <- doc
    x$ambient_values_at_location <- x$ambient_values_at_location[-1]
    validate(x)
  },
  'Missing/duplicate result keys'
)
# Explicit limit: descriptions and physical source-day interpretation require domain review.
case('Descriptive text is not an automatic scientific truth check', function() {
  x <- doc
  x$products[[1]]$description <- 'Reviewer-supplied interpretation'
  validate(x)
})
table <- do.call(rbind, report)
write.csv(table, file.path(run_dir, 'schema-tests.csv'), row.names = FALSE)
jsonlite::write_json(
  list(
    cases = nrow(table),
    passed = sum(table$passed),
    failed = sum(!table$passed),
    scientific_values_changed = FALSE,
    physical_time_and_ontology_approved = FALSE,
    human_signoff = 'pending',
    package_implementation_changed = FALSE
  ),
  file.path(run_dir, 'schema-tests.json'),
  auto_unbox = TRUE,
  pretty = TRUE
)
print(table[, c('test', 'passed')], row.names = FALSE)
if (any(!table$passed)) {
  stop('One or more tests failed; share schema-tests.csv and session-info.txt')
}
