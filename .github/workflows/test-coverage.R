args <- commandArgs(trailingOnly = TRUE)
runnertemp <- args[[1]]
ghworkspace <- args[[2]]

message("=== Starting Coverage Script ===")
message("Runner temp: ", runnertemp)
message("Workspace: ", ghworkspace)
message("Working directory: ", getwd())

# Ensure we're in the package root
setwd(ghworkspace)

# Create output directory
pkg_dir <- file.path(runnertemp, "package")
dir.create(pkg_dir, showWarnings = FALSE, recursive = TRUE)

# First, try running tests directly to see what fails
message("\n=== Running Tests Directly First ===")
test_results <- tryCatch(
  {
    testthat::test_local(
      path = ghworkspace,
      reporter = testthat::ProgressReporter$new(max_failures = Inf),
      stop_on_failure = FALSE,
      load_package = "source"
    )
  },
  error = function(e) {
    message("!!! Test execution failed !!!")
    message("Error: ", conditionMessage(e))
    print(e)
    return(NULL)
  }
)

# Show test results
if (!is.null(test_results)) {
  message("\n=== Test Summary ===")
  print(test_results)

  if (any(test_results$failed > 0)) {
    message("\n!!! TESTS FAILED !!!")
    message("Failed: ", sum(test_results$failed))
    message("Warnings: ", sum(test_results$warning))
    message("Skipped: ", sum(test_results$skipped))
  }
}

# Now try coverage
message("\n=== Starting Coverage Calculation ===")
tryCatch(
  {
    library(amadeus)

    # Try coverage with more verbose output
    cov <- covr::package_coverage(
      type = "all",
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
