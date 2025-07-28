args <- commandArgs(trailingOnly = TRUE)
runnertemp <- args[[1]]
ghworkspace <- args[[2]]

dir.create(
  file.path(runnertemp, "package"),
  showWarnings = FALSE,
  recursive = TRUE
)
sink(paste0(runnertemp, "/package/testthat.Rout.res"))
cov <- covr::package_coverage(quiet = FALSE)
sink()
covd <- covr::coverage_to_list(cov)$totalcoverage
write.table(
  covd[length(covd)],
  file = file.path(ghworkspace, "local_cov.Rout"),
  row.names = FALSE,
  col.names = FALSE
)
