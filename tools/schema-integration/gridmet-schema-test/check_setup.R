# Read-only dependency/capability check; never installs anything or alters Amadeus.
required <- c(
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
optional <- c('DBI', 'duckdb', 'sedonadb')
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
  'Public Amadeus routes are available. Optional engines checked separately when requested.\n'
)
