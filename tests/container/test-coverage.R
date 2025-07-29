# Set and check library paths.
.libPaths(grep("ddn", .libPaths(), value = TRUE, invert = TRUE))
.libPaths()

# Define temporary working directory.
runnertemp <- file.path("/opt/tmp")
dir.exists(runnertemp)

# Create temporary working direectory.
dir.create(
  file.path(runnertemp, "package"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Open connection.
sink(paste0(runnertemp, "/package/testthat.Rout.res"))

# Calculate package coverage.
cov <- covr::package_coverage(quiet = FALSE)

# Close connection.
sink()

# Coveragte as list.
covd <- covr::coverage_to_list(cov)$totalcoverage

# Save coverage table.
write.table(
  covd[length(covd)],
  file = file.path(".", "local_cov.Rout"),
  row.names = FALSE,
  col.names = FALSE
)
