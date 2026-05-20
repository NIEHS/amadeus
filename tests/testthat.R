# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(amadeus)

if (identical(Sys.getenv("AMADEUS_COVERAGE_CI"), "true")) {
  ns <- asNamespace("testthat")
  unlockBinding("skip_if_offline", ns)
  assign(
    "skip_if_offline",
    function(...) {
      skip("Skipping live/offline-guarded tests in coverage CI")
    },
    envir = ns
  )
  lockBinding("skip_if_offline", ns)
}

test_check("amadeus")
