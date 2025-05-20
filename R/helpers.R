# nocov start
#' Run all tests within a single file from `tests/testthat/` directory
#' with the `container.sif` container.
#' @param pattern A regular expression to match the test file name.
#' @return NULL; Prints the output of the `testthat` tests.
#' @seealso [testthat::test_file()]
#' @keywords Miscellaneous
test <- function(pattern = NULL) {
  if (is.null(pattern)) stop()
  system(
    paste(
      c(
        "apptainer exec --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
        "container.sif Rscript --no-init-file -e \"",
        ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
        "value = TRUE, invert = TRUE)); devtools::load_all('/mnt'); ",
        "library(dplyr); library(testthat); ",
        "test_file <- list.files('/mnt/tests/testthat', full.names = TRUE, ",
        "pattern = '", pattern, "'); source_files <- list.files('/mnt/R', ",
        "full.names = TRUE); covr::file_coverage(source_files, test_file)\""
      ),
      collapse = ""
    )
  )
}

#' Calculate code coverage of the `beethoven` package with the
#' `container.sif` container.
#' @return NULL; Prints the output of the code coverage.
#' @seealso [covr::package_coverage()]; [covr::coverage_to_list()]
#' @keywords Miscellaneous
cov <- function() {
  system(
    paste(
      c(
        "apptainer exec --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
        "container.sif Rscript --no-init-file -e \"",
        ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
        "value = TRUE, invert = TRUE)); devtools::load_all('/mnt'); ",
        "library(dplyr); library(testthat); ",
        "cov <- covr::package_coverage(install_path = '/tmp/cov', ",
        "clean = FALSE); ",
        "saveRDS(cov, '/mnt/cov/cov_", format(Sys.time(), "%m%d_%H%M"),
        ".rds'); covr::coverage_to_list(cov)\""
      ),
      collapse = ""
    )
  )
}

#' Open interactive session with `container.sif` container.
#' @return NULL
#' @keywords Miscellaneous
interactive <- function(dir = "./tests/container") {
  file <- file.path(dir, "interactive.sh")
  system(paste0(". ", file))
}
# nocov end