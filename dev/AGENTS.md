# Copilot Instructions for amadeus

## Package Overview

`amadeus` is an R package for accessing and analyzing large-scale,
publicly available environmental data (atmospheric, meteorological,
climate, emissions, pollution). It wraps 20+ data sources behind three
primary functions:
[`download_data()`](https://niehs.github.io/amadeus/dev/reference/download_data.md),
[`process_covariates()`](https://niehs.github.io/amadeus/dev/reference/process_covariates.md),
and
[`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md).

## Architecture

- **Dispatch pattern**: Wrapper functions (`download_data`,
  `process_covariates`, `calculate_covariates`) route to source-specific
  implementations via string matching on `dataset_name`.
- **File layout**:
  - `R/download.R` + `R/download_auxiliary.R` — download wrappers and
    source-specific download functions
  - `R/process.R` + `R/process_auxiliary.R` — process wrappers and
    source-specific process functions
  - `R/calculate_covariates.R` + `R/calculate_covariates_auxiliary.R` —
    extraction wrappers and source-specific calc functions
  - `R/manipulate_spacetime_data.R` — spatiotemporal utilities
  - `R/ignore.R` — miscellaneous helpers
- **Spatial stack**: `terra` (primary raster/vector), `sf`, `stars`,
  `exactextractr`

## Code Conventions

- Full Roxygen2 documentation on all exported functions with `@param`,
  `@return`, `@author`, `@seealso`, `@examples`
- `character(1)`, `logical(1)`, `numeric(1)` scalar type annotations in
  `@param` docs
- Use `data.table`, `dplyr`, `tidyr` for tabular data; avoid base R
  loops where vectorized alternatives exist
- `httr2` for HTTP requests
- Linting via `lintr`; `nolint start/end` blocks used sparingly for long
  URLs
- Tests use `testthat` (edition 3) with
  [`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
  wrappers

## Testing

- Test files live in `tests/testthat/` named `test-<dataset>.R`. Live
  API tests live alongside as `test-<dataset>-live.R` and are gated by
  `skip_if_no_live_tests()`.
- Shared mock/fixture helpers live in `tests/testthat/helper-*.R`
  (auto-loaded by testthat): `helper-mocks-download.R`,
  `helper-mocks-process.R`, `helper-fixtures.R`, `helper-skips.R`.
- Run mocked tests with
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html);
  run live tests with
  `AMADEUS_LIVE_TESTS=true devtools::test(filter = "-live$")`. The
  scheduled workflow `.github/workflows/test-live.yaml` runs live tests
  weekly.
- Test descriptions must use the form
  `"<fn>(<arg=value>, ...): <expected behavior>"` so failures identify
  the input combination under test.
- Prefer typed expectations (`expect_s4_class`, `expect_gt`,
  `expect_length`) over `expect_true(inherits(...))`,
  `expect_true(length(x) > 0)`, or `expect_no_error()` wrappers.
- See `vignettes/testing.Rmd` for full conventions and the
  `tests/test_report/test_report.html` quality scorecard.

## Adding a New Dataset

1.  Add a `download_<name>()` function in `download_auxiliary.R`
2.  Add the dataset name string(s) to the dispatch block in
    [`download_data()`](https://niehs.github.io/amadeus/dev/reference/download_data.md)
    in `download.R`
3.  Repeat for `process_<name>()` /
    [`process_covariates()`](https://niehs.github.io/amadeus/dev/reference/process_covariates.md)
    and `calc_<name>()` /
    [`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md)
    as needed
4.  Export new functions in `NAMESPACE` (via `@export` roxygen tag +
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html))
5.  Add `test-<name>.R` (mocked, CRAN-safe) using the `helper-mocks-*`
    factories, and `test-<name>-live.R` (gated by
    `skip_if_no_live_tests()`) for real-API verification

## Common Pitfalls

- `terra` objects are not serializable across parallel workers — use
  file paths, not in-memory objects, across workers
- Always validate `acknowledgement = TRUE` at the top of download
  functions before any side effects
- CRS must be standardized to `EPSG:4326` (or documented otherwise) in
  process functions
