# Agent Guide: amadeus

**amadeus** (**a** **m**echanism for **d**ata, **e**nvironments, and **u**ser **s**etup) is an R package for downloading, processing, and extracting large-scale spatiotemporal environmental data from 20+ public sources. Published in *Environmental Modelling & Software* (2025), maintained at NIEHS by the Spatiotemporal Exposures and Toxicology Group.

---

## Package Architecture

### Three-tier user API
| Function | Purpose |
|---|---|
| `download_data()` | Download raw files from public URLs |
| `process_covariates()` | Import/clean into `SpatRaster`/`SpatVector`/`sf` |
| `calculate_covariates()` | Extract values at point/polygon locations |

Each wrapper delegates to source-specific functions (e.g., `download_modis()`, `process_merra2()`, `calculate_narr()`).

### R source files
| File | Responsibility |
|---|---|
| `R/download.R` | `download_data()` + all source-specific download functions |
| `R/download_auxiliary.R` | Download helpers: URL building, auth, hashing, dir setup |
| `R/process.R` | `process_covariates()` + source-specific process functions |
| `R/process_auxiliary.R` | Spatial/temporal processing helpers |
| `R/calculate_covariates.R` | `calculate_covariates()` + source-specific calc functions |
| `R/calculate_covariates_auxiliary.R` | Covariate extraction helpers |
| `R/manipulate_spacetime_data.R` | Type conversions: `sf` ↔ `sftime` ↔ `SpatRaster`/`SpatVector` |
| `R/helpers.R` | Date/time validation, generic checks |
| `R/ignore.R` | Package-level documentation metadata |

### Supported data sources
- **Climate/Weather**: TerraClimate, GridMET, MERRA-2, NARR, GEOS-CF, PRISM
- **Land use**: NLCD, MODIS, Cropscape (CDL), Ecoregions
- **Emissions/Air quality**: EPA AQS, NEI, EDGAR, HMS smoke, Open Landmap
- **Hydrology**: HUC (via nhdplusTools), GEO-roads
- **Elevation**: GMTED2010
- **Population**: NASA SEDAC
- **Climate zones**: Köppen-Geiger
- **Nighttime lights**: Black Marble (VIIRS)

---

## Development Workflow

### Setup
```r
# Install dependencies
devtools::install_deps()

# Load package locally
devtools::load_all()
```

### Documentation
Documentation is in **Roxygen2** (markdown enabled). Regenerate after editing `#'` comments:
```r
devtools::document()
```
The pkgdown site is built automatically on push via `.github/workflows/pkgdown.yaml`.

### Running tests
```r
# Full test suite
devtools::test()

# Single test file
testthat::test_file("tests/testthat/test-narr.R")

# With coverage
covr::package_coverage()
```

Test files live in `tests/testthat/`. Resource-intensive tests are in `tests/testskip/` and are excluded from routine runs. Test data (~1 GB sample files) lives in `tests/testdata/`.

### Linting
```r
lintr::lint_package()
```
Rules (`.lintr`): max line length 80; `commented_code_linter`, `return_linter`, and `indentation_linter` are disabled. `tests/`, `inst/migration-to-httr-guide.R`, and vignettes are excluded.
Temporary ratchet policy: keep `indentation_linter` disabled until `air` formatting and lint style are reconciled; re-enable after the active modules are cleaned to pass without indentation exceptions.

### R CMD CHECK
```r
devtools::check()
```
CI runs this on macOS (release, xl), Windows, and Ubuntu (devel, release, oldrel-1) via `.github/workflows/check-standard.yaml`. Requires `EARTHDATA_TOKEN` env var for NASA data tests.

---

## Coding Conventions

- **Spatial inputs**: `sf` or `SpatVector` objects; must include a `locs_id` column.
- **Spatial outputs**: `data.frame` or `SpatVector` depending on the `geom` argument.
- **Time handling**: Custom `mysftime` class; use converters like `sftime_as_spatraster()`, `sf_as_mysftime()`.
- **HTTP requests**: Use `httr2` (not `httr`); apply retry/throttle for rate-limited APIs.
- **Line length**: ≤ 100 characters.
- **No explicit `return()`**: consistent with disabled `return_linter`.
- **No commented-out code** in committed files.

### Adding a new data source
1. Add a `download_<source>()` function in `R/download.R`; register it in the `switch` inside `download_data()`.
2. Add a `process_<source>()` function in `R/process.R`; register it in `process_covariates()`.
3. Add a `calculate_<source>()` function in `R/calculate_covariates.R`; register it in `calculate_covariates()`.
4. Add corresponding tests in `tests/testthat/test-<source>.R`.
5. Add sample test data to `tests/testdata/` if needed.
6. Document with Roxygen2; run `devtools::document()`.
7. Update `NEWS.md` under the current version.

---

## Key Dependencies

| Package | Role |
|---|---|
| `terra` (≥ 1.8-50) | Raster/vector spatial ops |
| `sf` | Vector spatial data |
| `sftime`, `stars` | Spatiotemporal data types |
| `exactextractr` | Precise raster extraction |
| `data.table` | Fast tabular operations |
| `httr2` | HTTP downloads with retry/throttle |
| `rvest` | Web scraping for URL discovery |
| `nhdplusTools` | HUC delineation |
| `dplyr`, `tidyr`, `collapse` | Data manipulation |
| `Rdpack` | Documentation macros |

R ≥ 4.2.0 required.

---

## CI/CD Workflows (`.github/workflows/`)

| Workflow | Trigger | Purpose |
|---|---|---|
| `check-standard.yaml` | push/PR | R CMD CHECK on 5 platforms |
| `lint.yaml` | push/PR | `lintr` style check |
| `test-coverage.yaml` | push/PR | `covr` coverage report |
| `test-coverage-local.yaml` | manual | Singularity container coverage |
| `pkgdown.yaml` | push to main | Build & deploy docs site |

Required secrets: `EARTHDATA_TOKEN`, `GITHUB_PAT`.

---

## Versioning & Release

Versions follow `MAJOR.MINOR.PATCH` (CRAN) with a dev suffix (e.g., `1.3.2.2003`).
- Update `Version:` in `DESCRIPTION`
- Add an entry to `NEWS.md`
- Tag the release commit after CRAN submission; record submission in `CRAN-SUBMISSION`

---

## Useful References

- Package repo: `github.com/NIEHS/amadeus`
- Published paper: *Environmental Modelling & Software* (2025)
- Maintainer: Kyle Messier <kyle.messier@nih.gov>
- Vignettes: `vignettes/` (TerraClimate, GridMET, MODIS, NARR, AQS workflows)
- pkgdown site: auto-deployed to GitHub Pages
