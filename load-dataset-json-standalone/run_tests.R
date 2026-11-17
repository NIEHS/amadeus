#!/usr/bin/env Rscript
# Run the unchanged regression suites without depending on the caller's cwd.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed = TRUE)
root <- dirname(normalizePath(script, mustWork = TRUE))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && (length(args) != 2L || args[1L] != "--python")) {
  stop("Usage: Rscript --vanilla run_tests.R [--python PATH]")
}
python <- if (length(args)) args[2L] else {
  configured <- Sys.getenv("AMADEUS_CATALOG_PYTHON", unset = "")
  candidates <- c(configured, file.path(root, ".venv-catalog/bin/python"),
                  file.path(root, ".venv-catalog/Scripts/python.exe"))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) candidates[1L] else Sys.which("python3")
}
if (!file.exists(python)) python <- Sys.which(python)
if (!nzchar(python) || !file.exists(python)) stop("Provide --python PATH; see README.md")
# Preserve executable symlinks so a virtual environment remains active.
python <- file.path(normalizePath(dirname(python), mustWork = TRUE), basename(python))
Sys.setenv(AMADEUS_CATALOG_PYTHON = python)
for (package in c("yaml", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Run Rscript --vanilla setup.R")
}
old <- getwd()
setwd(root)
results_dir <- file.path(root, "test-results")
dir.create(results_dir, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
# Relative script arguments avoid Rscript's --file encoding of spaces; cwd is root.
impl <- "linkml-json-iplementation"
jobs <- list(
  dependencies = list(python, c("-c", paste(
    "import importlib.metadata as m, sys;",
    "print(sys.version);",
    "print({p:m.version(p) for p in ('linkml','linkml-runtime','jsonschema','PyYAML')})"
  ))),
  serialization = list(rscript, c("--vanilla", file.path(impl, "tests/test_conversion.R"))),
  bridge = list(python, c("-m", "unittest", "discover", "-s", file.path(impl, "tests"),
                        "-p", "test_bridge.py", "-v")),
  loaders = list(rscript, c("--vanilla", file.path(impl, "tests/test_dataset_loaders.R")))
)
results <- list()
for (name in names(jobs)) {
  job <- jobs[[name]]
  log <- file.path(results_dir, paste0(name, ".log"))
  cat("Running", name, "\n")
  status <- system2(job[[1L]], vapply(job[[2L]], shQuote, character(1)),
                    stdout = log, stderr = log)
  cat(paste(readLines(log, warn = FALSE), collapse = "\n"), "\n")
  results[[name]] <- list(status = if (status == 0L) "PASS" else "FAIL",
                          exit_code = status, log = basename(log))
  if (status != 0L) break
}
passed <- length(results) == length(jobs) &&
  all(vapply(results, function(x) x$exit_code == 0L, logical(1)))
jsonlite::write_json(list(
  status = if (passed) "PASS" else "FAIL",
  reviewed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  R_version = R.version.string,
  R_packages = lapply(setNames(c("yaml", "jsonlite"), c("yaml", "jsonlite")),
                      function(p) as.character(utils::packageVersion(p))),
  suites = results
), file.path(results_dir, "test_report.json"), pretty = TRUE, auto_unbox = TRUE)
setwd(old)
if (!passed) stop("Tests failed; see ", file.path(results_dir, "test_report.json"))
cat("PASS: all standalone test suites; report:", file.path(results_dir, "test_report.json"), "\n")
