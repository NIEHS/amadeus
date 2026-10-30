# Optional native R database hand-off in a separate process; no Amadeus/sf imports.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(2L, 3L)) {
  stop(
    'Usage: Rscript R/database.R RUN_DIRECTORY duckdb|sedonadb [SQL_OR_FILE]'
  )
}
script <- normalizePath(sub(
  '^--file=',
  '',
  grep('^--file=', commandArgs(), value = TRUE)[1]
))
root <- dirname(dirname(script))
run_dir <- normalizePath(args[1])
engine <- match.arg(args[2], c('duckdb', 'sedonadb'))
needed <- c(
  'jsonlite',
  'digest',
  if (engine == 'duckdb') c('DBI', 'duckdb') else 'sedonadb'
)
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    'Optional database dependencies missing: ',
    paste(missing, collapse = ', ')
  )
}
sha <- function(p) digest::digest(file = p, algo = 'sha256')
receipt <- jsonlite::read_json(file.path(run_dir, 'acceptance.json'))
path <- file.path(run_dir, 'schema-instance.json')
if (
  !isTRUE(receipt$accepted) ||
    sha(path) != receipt$instance_sha256 ||
    sha(file.path(root, 'schema/amadeus.schema.json')) != receipt$schema_sha256
) {
  stop('Unaccepted or changed schema hand-off')
}
doc <- jsonlite::read_json(path)
for (a in doc$assets) {
  if (
    !file.exists(a$local_path) ||
      sha(a$local_path) != a$sha256 ||
      file.info(a$local_path)$size != a$size_bytes
  ) {
    stop('Changed asset: ', a$id)
  }
}
provenance <- jsonlite::read_json(file.path(root, 'schema/PROVENANCE.json'))
for (n in c(paste0('amadeus_', engine, '.sql'), 'integrity.sql')) {
  if (sha(file.path(root, 'schema', n)) != provenance$files[[n]]) {
    stop('SQL artifact differs from pinned schema release')
  }
}
contract <- jsonlite::read_json(file.path(root, 'schema/contract.json'))
statements <- function(p) {
  text <- gsub(
    '--[^\n]*',
    '',
    paste(readLines(p, warn = FALSE), collapse = '\n')
  )
  Filter(nzchar, trimws(strsplit(text, ';', fixed = TRUE)[[1]]))
}
quote_name <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
literal <- function(x) {
  if (is.null(x) || length(x) != 1 || is.list(x) || is.na(x)) {
    stop('Unexpected SQL scalar')
  }
  if (is.logical(x)) {
    return(if (x) 'TRUE' else 'FALSE')
  }
  if (is.numeric(x)) {
    if (!is.finite(x)) {
      stop('Nonfinite SQL value')
    }
    return(format(x, digits = 17, scientific = FALSE, trim = TRUE))
  }
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}
user_query <- if (length(args) == 3L) {
  if (file.exists(args[3])) {
    paste(readLines(args[3], warn = FALSE), collapse = '\n')
  } else {
    args[3]
  }
} else {
  NULL
}
if (!is.null(user_query) && engine == 'duckdb') {
  dbpath <- file.path(run_dir, 'database/duckdb/gridmet.duckdb')
  report <- jsonlite::read_json(file.path(
    run_dir,
    'database/duckdb/receipt.json'
  ))
  if (report$accepted_instance_sha256 != receipt$instance_sha256) {
    stop('Saved database belongs to another accepted instance')
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbpath, read_only = TRUE)
  print(DBI::dbGetQuery(con, user_query))
  DBI::dbDisconnect(con, shutdown = TRUE)
  quit(save = 'no', status = 0)
}
out <- if (!is.null(user_query)) {
  tempfile('gridmet-sedona-query-')
} else {
  file.path(run_dir, 'database', engine)
}
if (dir.exists(out)) {
  stop('Database output already exists; no overwrite')
}
dir.create(out, recursive = TRUE)
if (engine == 'duckdb') {
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = file.path(out, 'gridmet.duckdb')
  )
  execute <- function(sql, params = NULL) {
    if (is.null(params)) {
      DBI::dbExecute(con, sql)
    } else {
      DBI::dbExecute(con, sql, params = params)
    }
  }
  fetch <- function(sql) DBI::dbGetQuery(con, sql)
  close_db <- function() DBI::dbDisconnect(con, shutdown = TRUE)
} else {
  con <- sedonadb::sd_connect()
  fetch <- function(sql) as.data.frame(sedonadb::sd_ctx_sql(con, sql))
  execute <- function(sql, params = NULL) invisible(fetch(sql))
  close_db <- function() invisible(NULL)
}
ddl <- Filter(
  function(s) grepl('^CREATE TABLE', s, ignore.case = TRUE),
  statements(file.path(root, 'schema', paste0('amadeus_', engine, '.sql')))
)
table_name <- function(sql) strsplit(sql, '"', fixed = TRUE)[[1]][2]
tables <- vapply(ddl, table_name, '')
dependencies <- setNames(
  lapply(seq_along(ddl), function(i) {
    refs <- regmatches(
      ddl[i],
      gregexpr(
        'REFERENCES[[:space:]]+"?[A-Za-z][A-Za-z0-9_]*"?',
        ddl[i],
        perl = TRUE
      )
    )[[1]]
    setdiff(gsub('"', '', sub('^REFERENCES[[:space:]]+', '', refs)), tables[i])
  }),
  tables
)
order <- character()
left <- tables
while (length(left)) {
  ready <- left[vapply(
    left,
    function(t) all(dependencies[[t]] %in% order),
    logical(1)
  )]
  if (!length(ready)) {
    stop('Unsupported cyclic DDL dependencies')
  }
  order <- c(order, ready)
  left <- setdiff(left, ready)
}
if (engine == 'duckdb') {
  DBI::dbBegin(con)
}
tryCatch(
  {
    for (sql in ddl) {
      execute(sql)
    }
    cols <- fetch(
      "SELECT table_name,column_name,ordinal_position FROM information_schema.columns WHERE table_schema NOT IN ('information_schema','pg_catalog') ORDER BY table_name,ordinal_position"
    )
    rows <- setNames(vector('list', length(tables)), tables)
    for (collection in names(contract$collections)) {
      cls <- contract$collections[[collection]]
      for (record in doc[[collection]]) {
        scalar <- list()
        for (field in names(record)) {
          value <- record[[field]]
          if (is.null(value)) {
            next
          }
          if (is.list(value)) {
            junction <- paste(cls, field, sep = '_')
            if (!junction %in% tables) {
              stop('Unsupported inline/multivalued mapping: ', cls, '.', field)
            }
            columns <- cols$column_name[cols$table_name == junction]
            parent <- paste0(cls, '_id')
            child <- setdiff(columns, parent)
            if (length(child) != 1 || !parent %in% columns) {
              stop('Ambiguous junction mapping')
            }
            for (v in value) {
              rows[[junction]] <- c(
                rows[[junction]],
                list(setNames(list(record$id, v), c(parent, child)))
              )
            }
          } else {
            if (!field %in% cols$column_name[cols$table_name == cls]) {
              stop('Unknown scalar SQL column: ', cls, '.', field)
            }
            scalar[[field]] <- value
          }
        }
        rows[[cls]] <- c(rows[[cls]], list(scalar))
      }
    }
    inserted <- 0L
    for (table in order) {
      for (row in rows[[table]]) {
        values <- if (engine == 'duckdb') {
          rep('?', length(row))
        } else {
          vapply(row, literal, '')
        }
        sql <- paste0(
          'INSERT INTO ',
          quote_name(table),
          ' (',
          paste(quote_name(names(row)), collapse = ','),
          ') VALUES (',
          paste(values, collapse = ','),
          ')'
        )
        execute(sql, if (engine == 'duckdb') unname(row) else NULL)
        inserted <- inserted + 1L
      }
    }
    if (engine == 'duckdb') DBI::dbCommit(con)
  },
  error = function(e) {
    if (engine == 'duckdb') {
      try(DBI::dbRollback(con), silent = TRUE)
    }
    close_db()
    stop(e)
  }
)
checks <- statements(file.path(root, 'schema/integrity.sql'))
for (sql in checks) {
  if (as.numeric(fetch(sql)[1, 1]) != 0) stop('SQL integrity violation: ', sql)
}
query <- paste(
  'SELECT v.id,p.amadeus_variable_code AS variable,l.location_key AS site_id,CAST(v.valid_time_start AS VARCHAR) AS date_label,v.value,v.value_unit_ucum,v.native_value,v.asset,v.processing_run FROM "AmbientValueAtLocation" v JOIN "ProductVariable" p ON v.product_variable=p.id JOIN "Location" l ON v.location=l.id ORDER BY v.id'
)
actual <- fetch(query)
expected <- doc$ambient_values_at_location
ix <- match(actual$id, vapply(expected, function(x) x$id, ''))
if (nrow(actual) != length(expected) || anyNA(ix) || anyDuplicated(actual$id)) {
  stop('Database record count or identity mismatch')
}
registry <- read.csv(file.path(root, 'inputs/variables.csv'))
tol <- registry$database_tolerance[match(actual$variable, registry$code)]
error <- abs(actual$value - vapply(expected[ix], function(v) v$value, 0))
if (
  any(error > tol) ||
    any(
      actual$value_unit_ucum !=
        vapply(expected[ix], function(v) v$value_unit_ucum, '')
    )
) {
  stop('Database numeric/unit mismatch')
}
write.csv(actual, file.path(out, 'results.csv'), row.names = FALSE)
write.csv(cols, file.path(out, 'columns.csv'), row.names = FALSE)
writeLines(query, file.path(out, 'results.sql'))
if (!is.null(user_query)) {
  print(fetch(user_query))
}
version <- as.character(fetch('SELECT version()')[[1]])
close_db()
reopened <- FALSE
if (engine == 'duckdb') {
  con2 <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = file.path(out, 'gridmet.duckdb'),
    read_only = TRUE
  )
  stopifnot(
    DBI::dbGetQuery(
      con2,
      'SELECT COUNT(*) n FROM "AmbientValueAtLocation"'
    )$n ==
      receipt$records
  )
  DBI::dbDisconnect(con2, shutdown = TRUE)
  reopened <- TRUE
}
jsonlite::write_json(
  list(
    engine = engine,
    engine_version = version,
    records = nrow(actual),
    inserted_rows = inserted,
    integrity_queries = length(checks),
    all_within_declared_tolerances = TRUE,
    maximum_numeric_storage_difference = max(error),
    accepted_instance_sha256 = receipt$instance_sha256,
    persistent_reopen_passed = reopened,
    profile = 'Core SQL/WKT; spatial SQL not tested',
    production_approved = FALSE
  ),
  file.path(out, 'receipt.json'),
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA
)
cat(
  engine,
  ':',
  nrow(actual),
  'results;',
  length(checks),
  'SQL checks passed\n'
)
