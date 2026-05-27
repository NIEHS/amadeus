args <- commandArgs(trailingOnly = TRUE)
runnertemp <- args[[1]]
ghworkspace <- args[[2]]

message("=== Starting Coverage Script ===")
message("Runner temp: ", runnertemp)
message("Workspace: ", ghworkspace)
message("Working directory: ", getwd())

# Ensure we're in the package root
setwd(ghworkspace)

# Coverage CI should exercise deterministic tests only. GitHub Actions sets
# NOT_CRAN=true by default, which enables live/network integration tests marked
# with skip_on_cran(); those are appropriate for dedicated integration jobs but
# make coverage runs flaky.
Sys.setenv(NOT_CRAN = "false")
Sys.setenv(AMADEUS_COVERAGE_CI = "true")

ns <- asNamespace("testthat")
unlockBinding("skip_if_offline", ns)
assign(
  "skip_if_offline",
  function(...) {
    testthat::skip("Skipping live/offline-guarded tests in coverage CI")
  },
  envir = ns
)
lockBinding("skip_if_offline", ns)

# Create output directory
pkg_dir <- file.path(runnertemp, "package")
dir.create(pkg_dir, showWarnings = FALSE, recursive = TRUE)

# Now try coverage
message("\n=== Starting Coverage Calculation ===")
tryCatch(
  {
    # Coverage from tests only. `type = "all"` additionally rebuilds
    # vignettes and runs \donttest{} examples, both of which trigger
    # heavy network downloads on CI and have previously hung the
    # workflow at the 6-hour job timeout. The tests/ suite already
    # exercises every public function (see test_report) and is the
    # canonical signal for patch coverage.
    cov <- covr::package_coverage(
      type = "tests",
      quiet = FALSE,
      clean = FALSE,
      install_path = pkg_dir,
      pre_clean = FALSE,
      code = c(
        'library(testthat)',
        'library(amadeus)'
      )
    )

    message("Coverage calculation successful")

    # Extract coverage
    covd <- covr::coverage_to_list(cov)$totalcoverage

    if (is.null(covd) || length(covd) == 0) {
      covd <- 0
    } else {
      covd <- covd[length(covd)]
    }

    message("Coverage: ", covd, "%")

    # Write coverage
    output_file <- file.path(ghworkspace, "local_cov.Rout")
    write.table(covd, file = output_file, row.names = FALSE, col.names = FALSE)

    message("Coverage written to: ", output_file)
  },
  error = function(e) {
    message("\n!!! ERROR during coverage calculation !!!")
    message("Error message: ", conditionMessage(e))

    # Try to find and display the test failure file
    fail_files <- list.files(
      runnertemp,
      pattern = "testthat.Rout.fail",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(fail_files) > 0) {
      message("\n=== Content of ", fail_files[1], " ===")
      cat(readLines(fail_files[1]), sep = "\n")
    }

    # Look for any .Rout files
    rout_files <- list.files(
      runnertemp,
      pattern = "\\.Rout",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(rout_files) > 0) {
      message("\n=== Found .Rout files ===")
      for (rf in rout_files) {
        message("\nFile: ", rf)
        message("Content:")
        tryCatch(
          {
            cat(readLines(rf, n = 100), sep = "\n") # First 100 lines
          },
          error = function(e2) {
            message("Could not read file: ", conditionMessage(e2))
          }
        )
      }
    }

    # Write 0 coverage
    output_file <- file.path(ghworkspace, "local_cov.Rout")
    write.table(0, file = output_file, row.names = FALSE, col.names = FALSE)

    quit(save = "no", status = 1)
  }
)

message("\n=== Coverage completed successfully ===")
