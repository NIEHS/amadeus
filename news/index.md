# Changelog

## amadeus 2.1.0

### Download tier Refactor

- Standardized source downloaders and
  [`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md)
  around shared argument checks, URL and destination planning, download
  execution, and return handling. Existing non-empty files are skipped
  by the shared download runner, which also creates missing destination
  directories.
- Standardized `download = FALSE` previews as invisible lists containing
  `urls`, `destfiles`, and `n_files`. Previews include all planned
  files, including existing destinations. MODIS, MERRA-2, and GOES
  discovery can still make network requests. The `download` and
  `remove_command` arguments remain deprecated with warnings.
- Standardized download summaries as invisible lists containing
  `success`, `failed`, `skipped`, `failed_urls`, and `failed_files`.
  Both failure vectors are `character(0)` when no downloads fail,
  including when all files are skipped. Counts describe file downloads,
  not extracted archive members.
- Aligned `hash = TRUE` returns with each source’s output directory,
  using `data_files/` for extracted archives. Hashes are visible
  character scalars and include existing files recursively. Preview
  returns take precedence over hashing.
- Consolidated date and year validation. Downloaders using
  `sequence_years()` accept one year, two endpoints, or a longer vector
  and request every year between the smallest and largest values. Date
  ranges accept endpoints in either order. Added configurable bounds and
  documented the implemented source-specific limits for HMS, NARR, NLCD,
  and CropScape; requests outside those limits fail rather than being
  truncated.
- Removed first-URL preflight probes and their warnings. File-transfer
  failures are handled by the shared download runner;
  [`check_url_status()`](https://niehs.github.io/amadeus/reference/check_url_status.md)
  remains available for other uses.
- Updated
  [`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md)
  to accept multiple archives with either one shared extraction
  directory or one directory per archive. Source downloaders use the
  common helper for batch extraction, including multi-year NLCD
  requests. Requested archive cleanup runs after the extraction loop.
- Changed the default CropScape source to GMU and updated the USDA
  download URL. Archives are stored in `zip_files/`, with separate
  extraction folders for each archive under `data_files/`. Added
  `remove_zip` support.
- Added
  [`download_eddi()`](https://niehs.github.io/amadeus/reference/download_eddi.md),
  [`download_spei()`](https://niehs.github.io/amadeus/reference/download_spei.md),
  and
  [`download_usdm()`](https://niehs.github.io/amadeus/reference/download_usdm.md).
  [`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md)
  delegates to these functions, and
  [`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md)
  routes their dataset aliases directly. SPEI now uses an explicit
  `version` selection (`"2.11"`, `"2.10"`, or `"legacy"`, defaulting to
  `"2.11"`) instead of automatically falling back between versions.
- Added CMR pagination to MODIS discovery so requests can retrieve more
  than 2000 granules. Removed the 31-day warning; existing
  product-specific cross-year restrictions remain. Incomplete or
  inconsistent paginated responses raise discovery errors.
- Moved MERRA-2 product mappings to a package data file and separated
  listing discovery from downloading. MERRA-2 and GOES listing helpers
  use `xml2`; GOES listing failures now stop discovery instead of
  warning and continuing.
- Expanded NEI inventory selections to 2008, 2011, 2014, 2017, 2020,
  and 2023. Mixed supported and unsupported selections warn and retain
  supported years; selections with none supported stop. Named year
  lookups keep URLs and filenames aligned for reversed or repeated
  selections. NEI years remain discrete selections, and
  [`process_nei()`](https://niehs.github.io/amadeus/reference/process_nei.md)
  still supports only 2017 and 2020.
- Reorganized R files into `api-`, `source-`, and `utils-` groups,
  consolidated download helpers, and made internal helper calls
  explicitly qualified. Existing helper exports are retained. Updated
  download documentation and test mocks for the shared workflow and
  return values.

## amadeus 2.0.3

- Fixed
  [`download_narr()`](https://niehs.github.io/amadeus/reference/download_narr.md)
  base URL to drop the “Datasets” directory.

## amadeus 2.0.2

- Added GRIDMET EDDI integration for all eight accumulation periods,
  with `eddi` defaulting to `eddi30d`.

- Added native 4-km GRIDMET SPEI downloads and extraction for eight
  accumulation periods, retaining five-day observation dates; `spei`
  defaults to `spei30d`.

- Fixed process_gridmet() to use valid native NetCDF time metadata from
  terra::time() when available and fall back to layer-name parsing when
  needed

- Fixed download_gridmet() to handle cases where all requested files
  already exist locally without calling download_run_method() with an
  empty URL list

## amadeus 2.0.0

CRAN release: 2026-05-21

### Major updates to code base - breaking changes have been minmized but please report if 1.3.x versions are not working as expected

- Refactored code base to improve maintainability and utilize modern R
  API designs and best practices
- The only breaking change that should affect previous version users is
  the
  [`process_tri()`](https://niehs.github.io/amadeus/reference/process_tri.md)
  and
  [`calculate_tri()`](https://niehs.github.io/amadeus/reference/calculate_tri.md)
  series.

### Spatial and Temporal summarization with `.by` and `.by_time` parameters

- Added `.by` and `.by_time` parameters to all `calculate_*()` functions
  for consistent spatial and temporal summarization options across all
  datasets
- The default behavior of `calculate_*()` functions remains unchanged -
  i.e., no summarization, returning extracted or summarized values at
  the temporal resolution of the data - but users can now specify `.by`
  for spatial grouping (e.g., by HUC, county, state) and `.by_time` for
  temporal grouping (e.g., by year, month) to obtain summarized
  covariate values directly from the calculation step

### ‘frac’ option for categorical variable covariate calculation

- `hms`, `koppen`, and `ecoregion` datasets now have optional fraction
  covariate calculation that returns the fraction of each category in
  the radius buffer.

### `drop` options for categorical variable covariate calculation

- `hms`, `koppen`, and `ecoregion` datasets now have optional `drop`
  parameter that will drop or exclude categories that don’t have any
  coverage in the radius buffer for a given point, which can help reduce
  the number of columns returned when many categories are possible but
  only a few are present in the area around the point.

### Detailed vignettes for each dataset including available variables, spatial and temporal resolution, and example use cases

- workflow vignette for each dataset with detailed information on
  available variables, spatial and temporal resolution, and example use
  cases for each dataset

### nhdplusTools use

- Moved nhdplusTools from Imports to Suggests and added
  [`requireNamespace("nhdplusTools")`](https://doi-usgs.github.io/nhdplusTools/)
  checks in all functions that use it

### Additional MODIS products

- Added support for additional MODIS products with a focus on the burned
  area and active fire products

### Finalize data API that were previously in development

- Added or completed the functonality for PRISM, EDGAR, CropScape, and
  HUC datasets

### Use of GitHub Copilot for code generation and refactoring

- Used GitHub Copilot for help with code generation and refactoring
  across the code base, including function development, documentation,
  and unit tests; all generated code was reviewed and edited by the
  development team to ensure accuracy and consistency with package
  standards

### Drought Index Support

- Added
  [`download_drought()`](https://niehs.github.io/amadeus/reference/download_drought.md),
  [`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md),
  and
  [`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)
  supporting three drought datasets:
  - **SPEI** (Standardized Precipitation-Evapotranspiration Index):
    multi-year netCDF files by accumulation timescale from
    <https://spei.csic.es>
  - **EDDI** (Evaporative Demand Drought Index): weekly files by
    accumulation timescale from NOAA PSL
    (<https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/>);
    legacy netCDF files from the `CONUS_Archive` path are also supported
  - **USDM** (U.S. Drought Monitor): weekly polygon shapefiles from
    <https://droughtmonitor.unl.edu>
- All three datasets are accessible via the top-level wrappers:
  [`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md),
  [`process_covariates()`](https://niehs.github.io/amadeus/reference/process_covariates.md),
  and
  [`calculate_covariates()`](https://niehs.github.io/amadeus/reference/calculate_covariates.md)
  using aliases `"drought"`, `"spei"`, `"eddi"`, or `"usdm"` for
  `dataset_name` / `covariate`.
- [`process_drought()`](https://niehs.github.io/amadeus/reference/process_drought.md)
  returns a `SpatRaster` (SPEI/EDDI) or `SpatVector` polygons (USDM),
  both with CRS `EPSG:4326`.
- [`calculate_drought()`](https://niehs.github.io/amadeus/reference/calculate_drought.md)
  supports `.by` / `.by_time` post-extraction summarization consistent
  with all other `calculate_*()` functions.

### Migrate from wget/curl to httr2

- Completed migration of all `download_*` functions from
  `httr`/`wget`/`curl` command-line calls to `httr2` for all network
  requests
- Deprecated `download` parameter (use default `download = TRUE`) and
  `remove_command` parameter across all download functions; both now
  emit informative warnings and are ignored
- Added `hash` parameter to all `download_*` functions for optional MD5
  file integrity verification via
  [`download_hash()`](https://niehs.github.io/amadeus/reference/download_hash.md)
  (using system `md5sum`)
- Added `unzip` and `remove_zip` parameters to
  [`download_prism()`](https://niehs.github.io/amadeus/reference/download_prism.md)
  for post-download archive handling
- Added
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  internal helper for unified httr2-based file retrieval with progress
  reporting, retry logic, and rate limiting
- Improved
  [`check_url_status()`](https://niehs.github.io/amadeus/reference/check_url_status.md)
  for general-purpose URL validation
- Expanded unit test coverage with mock-based tests for all download
  functions covering deprecation warnings, hash paths, and file-exists
  branches
- Added full variable reference to
  [`download_narr()`](https://niehs.github.io/amadeus/reference/download_narr.md)
  documentation, listing all 88 available NARR variable abbreviations
  with descriptions grouped by category (monolevel, pressure level,
  subsurface); resolves
  [\#194](https://github.com/NIEHS/amadeus/issues/194)
- Added
  [`download_narr()`](https://niehs.github.io/amadeus/reference/download_narr.md)
  tests covering every variable abbreviation across all three variable
  categories
- Fixed
  [`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md)
  dispatch to include `"edgar"` so
  [`download_edgar()`](https://niehs.github.io/amadeus/reference/download_edgar.md)
  is reachable via the wrapper function

## amadeus 1.3.2

- Fixed deprecated file paths for NLCD, MODIS, and Ecoregions datasets
- Removed the certificate verification from ecoregion download which is
  not needed anymore
- For NASA datasets MODIS, VIIRS, and Geos-CF, added additional tags in
  the wget scripts to prevent API throttling
- httr upgraded to httr2
- Modified internal URL status check function for general use
- Added the news function to display the NEWS.md content within R
  pkgdown site
- Improved the reporting in the GitHub Actions workflows

## amadeus 1.2

- `future` and `future.apply` dependencies were removed
  - `nthreads` argument is removed from `calculate_modis_par()` and
    [`calculate_nlcd()`](https://niehs.github.io/amadeus/reference/calculate_nlcd.md)
- `calculate_modis_par()` is renamed to
  [`calculate_modis()`](https://niehs.github.io/amadeus/reference/calculate_modis.md)

## amadeus 1.1

- `calc_*()` functions are renamed to `calculate_*()` per naming
  convention of other function family in the package

## amadeus 1.0

- First CRAN release (v.1.0.0)
