# test/testthat/test_coverage.R
args <- commandArgs(trailingOnly = TRUE)
runnertemp <- args[[1]]
ghworkspace <- args[[2]]

# Validate arguments
if (is.na(runnertemp) || is.na(ghworkspace)) {
  stop("Missing required arguments: runnertemp and/or ghworkspace")
}

message("Runner temp: ", runnertemp)
message("Workspace: ", ghworkspace)
message("Current directory: ", getwd())

# Ensure we're in the right directory
setwd(ghworkspace)

# Create output directory
pkg_dir <- file.path(runnertemp, "package")
dir.create(
  pkg_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

message("Created package directory: ", pkg_dir)

# Run coverage with error handling
tryCatch(
  {
    message("Starting coverage calculation...")
    message("R version: ", R.version.string)
    message("covr version: ", packageVersion("covr"))

    # Check if package can be loaded
    message("Checking if package loads...")
    library(amadeus)
    message("Package loaded successfully")

    # Create sink file for detailed output
    sink_file <- file.path(pkg_dir, "testthat.Rout.res")
    message("Sink file: ", sink_file)

    # Run coverage (DON'T sink stderr, only stdout)
    sink(sink_file, split = TRUE) # split=TRUE shows output in console too

    cov <- covr::package_coverage(
      type = "all",
      quiet = FALSE,
      clean = FALSE,
      install_path = pkg_dir,
      pre_clean = FALSE
    )

    sink()

    message("Coverage calculation complete")

    # Extract coverage percentage
    covd <- covr::coverage_to_list(cov)$totalcoverage

    if (is.null(covd) || length(covd) == 0) {
      warning("No coverage data generated")
      covd <- 0
    } else {
      covd <- covd[length(covd)]
      message("Coverage: ", covd, "%")
    }

    # Write coverage to file
    output_file <- file.path(ghworkspace, "local_cov.Rout")
    write.table(
      covd,
      file = output_file,
      row.names = FALSE,
      col.names = FALSE
    )

    message("Coverage written to: ", output_file)
    message("Contents: ", readLines(output_file))

    # Also print coverage summary to console
    message("\n=== Coverage Summary ===")
    print(cov)

    # Show files with low coverage
    zero_cov <- covr::zero_coverage(cov)
    if (length(zero_cov) > 0) {
      message("\n=== Files with Zero Coverage ===")
      print(names(zero_cov))
    }
  },
  error = function(e) {
    # Make sure sink is closed
    while (sink.number() > 0) {
      sink()
    }

    message("\n!!! ERROR during coverage calculation !!!")
    message("Error message: ", conditionMessage(e))
    message("\nTraceback:")
    traceback()

    # Write 0 coverage so workflow doesn't fail on missing file
    output_file <- file.path(ghworkspace, "local_cov.Rout")
    write.table(
      0,
      file = output_file,
      row.names = FALSE,
      col.names = FALSE
    )

    message("Wrote 0 to coverage file due to error")

    # Exit with error code
    quit(save = "no", status = 1)
  }
)

message("\n=== Coverage calculation completed successfully ===")
