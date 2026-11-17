# Install runtime R dependencies only; Python setup is documented in README.md.
packages <- c("yaml", "jsonlite")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package installation failed: ", package)
  }
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
cat(R.version.string, "\nR dependencies ready. See README.md for Python setup.\n")
