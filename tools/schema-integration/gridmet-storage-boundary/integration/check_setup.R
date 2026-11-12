# Read-only dependency/capability check; never installs anything or alters Amadeus.
required <- c(
  'amadeus',
  'terra',
  'sf',
  'jsonlite',
  'yaml',
  'digest',
  'jsonvalidate',
  'ncdf4', 'DBI', 'duckdb'
)
optional <- c('sedonadb')
for (p in c(required, optional)) {
  cat(
    p,
    ':',
    if (nzchar(system.file(package = p))) {
      as.character(packageVersion(p))
    } else if (p %in% optional) {
      'optional; not installed'
    } else {
      'MISSING REQUIRED'
    },
    '\n'
  )
}
missing <- required[
  !vapply(required, function(p) nzchar(system.file(package = p)), logical(1))
]
if (length(missing)) {
  stop('Install missing required packages; see README')
}
for (fn in c('download_data', 'process_covariates', 'calculate_covariates')) {
  if (!fn %in% getNamespaceExports('amadeus')) {
    stop('Installed Amadeus lacks ', fn)
  }
}
cat(
  'Public Amadeus routes are available. DuckDB is required for the storage boundary; Sedona remains optional.\n'
)
# Optional publication filesystem probe, separate from scientific execution.
# Supply the intended output parent to test that filesystem; otherwise use this R session's temp directory.
setup_args <- commandArgs(TRUE)
publication_parent <- if(length(setup_args))normalizePath(setup_args[1],mustWork=TRUE)else tempdir()
probe <- tempfile('.publication-probe-',tmpdir=publication_parent)
dir.create(probe)
first <- file.path(probe,'snapshot');writeLines('probe',first)
mode_ok <- isTRUE(Sys.chmod(first,'0444'))
from <- file.path(probe,'new-pointer');to <- file.path(probe,'pointer')
writeLines('old',to);writeLines('new',from)
rename_ok <- isTRUE(file.rename(from,to))&&identical(readLines(to),'new')
Sys.chmod(first,'0644');unlink(probe,recursive=TRUE)
if(!mode_ok||!rename_ok)stop('Publication filesystem needs supported chmod and same-directory pointer replacement; ordinary Amadeus remains independent')
cat('Publication filesystem probe passed (mode change and pointer replacement); privileged/ACL tamper protection is not established.\n')
