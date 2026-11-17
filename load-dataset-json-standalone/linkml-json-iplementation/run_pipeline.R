#!/usr/bin/env Rscript
# R orchestrates the complete build; LinkML's Python toolchain compiles/validates.
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
here <- dirname(normalizePath(script, mustWork = TRUE))
repo <- dirname(here)
usage <- paste(
  "Rscript linkml-json-iplementation/run_pipeline.R",
  "  [--python PATH] [--input-dir PATH] [--output-dir PATH]",
  sep = "\n"
)
if ("--help" %in% args) {
  cat(usage, "\n")
  quit(status = 0L)
}
options <- list(
  "--python" = Sys.getenv("AMADEUS_CATALOG_PYTHON", unset = Sys.which("python3")),
  "--input-dir" = file.path(repo, "global_dataset_catalogs"),
  "--output-dir" = file.path(here, "output")
)
if (length(args) %% 2L != 0L) stop(usage)
if (length(args)) for (i in seq.int(1L, length(args), by = 2L)) {
  if (!args[i] %in% names(options)) stop("Unknown option: ", args[i], "\n", usage)
  options[[args[i]]] <- args[i + 1L]
}
for (package in c("yaml", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install R package: ", package)
}
python <- options[["--python"]]
if (!nzchar(python)) stop("Provide --python pointing to the LinkML environment")
if (!file.exists(python)) python <- Sys.which(python)
if (!nzchar(python)) stop("Python interpreter not found")
# Preserve the virtual-environment executable symlink; resolving it would
# launch the base interpreter and silently lose installed LinkML dependencies.
python <- file.path(normalizePath(dirname(python), mustWork = TRUE), basename(python))
input <- normalizePath(options[["--input-dir"]], mustWork = TRUE)
output <- normalizePath(options[["--output-dir"]], mustWork = FALSE)
bridge <- file.path(here, "python", "linkml_bridge.py")
source(file.path(here, "R", "convert_catalog.R"), local = TRUE)
run_python <- function(command) {
  arguments <- c(bridge, command, "--input-dir", input, "--output-dir", output)
  status <- system2(python, args = vapply(arguments, shQuote, character(1)))
  if (status != 0L) stop("Python ", command, " failed; see validation_report.json")
}
main <- function() {
  run_python("prepare")
  plan <- jsonlite::read_json(file.path(output, "conversion_plan.json"), simplifyVector = FALSE)
  schema <- jsonlite::read_json(plan$schema_file, simplifyVector = FALSE)
  reader <- file.path(repo, "dataset_spec_objects", "catalog_updates_20261109", "read_catalog.R")
  for (document in plan$documents) {
    cat("R conversion:", document$name, "\n")
    convert_catalog(document$input, plan$schema_file, document$output, reader, schema)
  }
  jsonlite::write_json(list(
    documents_converted = length(plan$documents),
    R_version = as.character(getRversion()),
    packages = list(yaml = as.character(utils::packageVersion("yaml")),
                    jsonlite = as.character(utils::packageVersion("jsonlite")))
  ), file.path(output, "r_conversion_report.json"), auto_unbox = TRUE, pretty = TRUE)
  run_python("validate")
  cat("Completed:", file.path(output, "validation_report.json"), "\n")
}
tryCatch(main(), error = function(e) {
  # The bridge may already have supplied detailed per-document diagnostics.
  report <- file.path(output, "validation_report.json")
  prior <- if (file.exists(report)) jsonlite::read_json(report) else NULL
  if (is.null(prior) || !identical(prior$status, "FAIL")) {
    # Protect source/schema directories even if invalid output arguments were used.
    protected <- normalizePath(c(file.path(repo, "global_dataset_catalogs"),
                                file.path(repo, "validated_catalog_handles"),
                                file.path(repo, "amadeus_schema")), mustWork = TRUE)
    safe <- output != repo && !any(vapply(protected, function(p) {
      output == p || startsWith(output, paste0(p, .Platform$file.sep))
    }, logical(1)))
    if (safe) {
      dir.create(output, recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(list(status = "FAIL", phase = "R_orchestration_or_conversion",
                               error = conditionMessage(e)), report, auto_unbox = TRUE, pretty = TRUE)
    }
  }
  message("FAIL: ", conditionMessage(e))
  quit(status = 1L)
})
