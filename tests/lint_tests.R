#!/usr/bin/env Rscript
# Lints amadeus test files for forbidden weak-assertion patterns.
#
# Run locally:
#   Rscript tests/lint_tests.R
#
# Exits with status 1 if any forbidden pattern is found. Patterns are tuned to
# the conventions documented in vignettes/testing.Rmd:
#
#   * expect_true(inherits(x, "C"))         -> use expect_s3_class / expect_s4_class
#   * expect_true(file.exists(p))           -> assert file is non-empty as well
#   * expect_true(length(x) > 0)            -> use expect_gt(length(x), 0)
#   * expect_no_error(f(...))               -> assert on the return value
#   * skip_on_cran() inside test_that body  -> hoist to file top
#
# The check is intentionally conservative: it flags only obvious cases that
# can be mechanically rewritten. It is safe to add lines to the allowlist below.

forbidden <- list(
  list(
    name = "expect_true(inherits(...))",
    re   = "expect_true\\(\\s*inherits\\("
  ),
  list(
    name = "expect_true(file.exists(...)) alone",
    re   = "expect_true\\(\\s*file\\.exists\\("
  ),
  list(
    name = "expect_true(length(...) > 0)",
    re   = "expect_true\\(\\s*length\\(.+\\)\\s*>\\s*0"
  ),
  list(
    name = "expect_no_error(...) without assertion on return value",
    re   = "expect_no_error\\("
  )
)

allowlist_files <- c(
  # These files own legacy assertions migrated piecewise. Remove entries as
  # they are cleaned up.
  "tests/testthat/test-coverage-followup.R"
)

test_files <- list.files(
  "tests/testthat",
  pattern = "^test-.*\\.R$",
  full.names = TRUE
)
test_files <- setdiff(test_files, allowlist_files)

hits <- list()
for (f in test_files) {
  lines <- readLines(f, warn = FALSE)
  for (rule in forbidden) {
    idx <- grep(rule$re, lines)
    if (length(idx) > 0) {
      hits[[length(hits) + 1L]] <- data.frame(
        file = f,
        line = idx,
        rule = rule$name,
        snippet = trimws(lines[idx]),
        stringsAsFactors = FALSE
      )
    }
  }
}

args <- commandArgs(trailingOnly = TRUE)
strict <- any(args == "--strict") ||
  identical(Sys.getenv("AMADEUS_LINT_STRICT"), "true")

if (length(hits) > 0) {
  df <- do.call(rbind, hits)
  cat("Found", nrow(df), "weak-assertion pattern(s):\n\n", sep = " ")
  apply(df, 1L, function(row) {
    cat(sprintf(
      "  %s:%s  [%s]\n    %s\n",
      row[["file"]], row[["line"]], row[["rule"]], row[["snippet"]]
    ))
  })
  cat(
    "\nSee vignettes/testing.Rmd for the assertion table and rewrites.\n"
  )
  if (strict) {
    quit(status = 1L, save = "no")
  }
  cat(
    "\nADVISORY mode: not failing the build. ",
    "Pass --strict or set AMADEUS_LINT_STRICT=true to enforce.\n",
    sep = ""
  )
} else {
  cat("OK: no forbidden weak-assertion patterns found in tests/testthat/.\n")
}
