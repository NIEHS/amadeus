# Copilot Instructions for amadeus

## Package Overview
`amadeus` is an R package for accessing and analyzing large-scale, publicly available environmental data (atmospheric, meteorological, climate, emissions, pollution). It wraps 20+ data sources behind three primary functions: `download_data()`, `process_covariates()`, and `calculate_covariates()`.

## Architecture
- **Dispatch pattern**: Wrapper functions (`download_data`, `process_covariates`, `calculate_covariates`) route to source-specific implementations via string matching on `dataset_name`.
- **File layout**:
  - `R/download.R` + `R/download_auxiliary.R` — download wrappers and source-specific download functions
  - `R/process.R` + `R/process_auxiliary.R` — process wrappers and source-specific process functions
  - `R/calculate_covariates.R` + `R/calculate_covariates_auxiliary.R` — extraction wrappers and source-specific calc functions
  - `R/manipulate_spacetime_data.R` — spatiotemporal utilities
  - `R/ignore.R` — miscellaneous helpers
- **Spatial stack**: `terra` (primary raster/vector), `sf`, `stars`, `exactextractr`

## Code Conventions
- Full Roxygen2 documentation on all exported functions with `@param`, `@return`, `@author`, `@seealso`, `@examples`
- `character(1)`, `logical(1)`, `numeric(1)` scalar type annotations in `@param` docs
- Use `data.table`, `dplyr`, `tidyr` for tabular data; avoid base R loops where vectorized alternatives exist
- `httr2` for HTTP requests
- Linting via `lintr`; `nolint start/end` blocks used sparingly for long URLs
- Tests use `testthat` (edition 3) with `testthat::test_that()` wrappers

## Testing
- Test files live in `tests/testthat/` named `test-<dataset>.R`
- Each dataset has dedicated tests; `test-download.R`, `test-process.R`, `test-calc.R` cover wrapper functions
- Run tests with `devtools::test()` or `Rscript -e "testthat::test_dir('tests/testthat/')"`
- Many download tests are skipped in CI (require credentials/network); see `tests/testskip/`

## Adding a New Dataset
1. Add a `download_<name>()` function in `download_auxiliary.R`
2. Add the dataset name string(s) to the dispatch block in `download_data()` in `download.R`
3. Repeat for `process_<name>()` / `process_covariates()` and `calc_<name>()` / `calculate_covariates()` as needed
4. Export new functions in `NAMESPACE` (via `@export` roxygen tag + `devtools::document()`)
5. Add a `test-<name>.R` test file

## Common Pitfalls
- `terra` objects are not serializable across parallel workers — use file paths, not in-memory objects, across workers
- Always validate `acknowledgement = TRUE` at the top of download functions before any side effects
- CRS must be standardized to `EPSG:4326` (or documented otherwise) in process functions
