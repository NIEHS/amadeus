args <- commandArgs(trailingOnly = TRUE)
runnertemp <- args[[1]]
ghworkspace <- args[[2]]

sink(paste0(runnertemp, '/package/testthat.Rout.res'))
cov <- covr::package_coverage()
sink()
covd <- covr::coverage_to_list(cov)$totalcoverage
write.table(
  covd[length(covd)],
  file = file.path(ghworkspace, 'local_cov.Rout'),
  row.names = FALSE,
  col.names = FALSE
)
