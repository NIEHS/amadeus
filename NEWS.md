# amadeus 1.3.4 (dev)

## Drought Index Support

- Added `download_drought()`, `process_drought()`, and `calculate_drought()`
  supporting three drought datasets:
  - **SPEI** (Standardized Precipitation-Evapotranspiration Index): multi-year
    netCDF files by accumulation timescale from
    <https://spei.csic.es>
  - **EDDI** (Evaporative Demand Drought Index): annual netCDF files from
    NOAA PSL (<https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_Archive/data>)
  - **USDM** (U.S. Drought Monitor): weekly polygon shapefiles from
    <https://droughtmonitor.unl.edu>
- All three datasets are accessible via the top-level wrappers:
  `download_data()`, `process_covariates()`, and `calculate_covariates()` using
  aliases `"drought"`, `"spei"`, `"eddi"`, or `"usdm"` for `dataset_name` /
  `covariate`.
- `process_drought()` returns a `SpatRaster` (SPEI/EDDI) or `SpatVector`
  polygons (USDM), both with CRS `EPSG:4326`.
- `calculate_drought()` supports `.by` / `.by_time` post-extraction
  summarization consistent with all other `calculate_*()` functions.

## OPeNDAP Support for NASA Datasets

- Added `use_opendap = FALSE`, `extent = NULL`, and `variables = NULL` parameters
  to `download_merra2()`, `download_geos()`, and `download_modis()`. All defaults
  preserve existing behavior (fully backward compatible).
- When `use_opendap = TRUE`, requests are routed through NASA OPeNDAP servers
  enabling server-side spatial and variable subsetting — only the requested
  region/variables travel over the wire, substantially reducing download sizes
  for large global datasets (MERRA-2: 576×361, GEOS-CF: 1440×721 global grids).
- New exported auxiliary functions:
  - `extent_to_merra2_indices(extent)` — maps a lat/lon bounding box to
    0-based MERRA-2 grid indices (0.5° grid, 361 lat × 576 lon points)
  - `extent_to_geos_indices(extent)` — same for GEOS-CF 0.25° grid
    (721 lat × 1440 lon points)
  - `extent_to_modis_tiles(extent)` — returns MODIS sinusoidal tile codes
    (`"hXXvYY"`) overlapping a bounding box using the official NASA MODLAND
    tile boundary lookup table (`sn_bound_10deg.txt`); correct at all latitudes
    including high-latitude where the sinusoidal projection makes geographic
    lon bounds non-uniform
  - `build_opendap_constraint(variables, time_idx, lat_idx, lon_idx)` — builds
    a DAP2 constraint expression string for GES DISC / NCCS OPeNDAP servers
  - `build_opendap_url(base, filename, constraint)` — assembles the final
    OPeNDAP URL
- Added `inst/extdata/sn_bound_10deg.txt` — the official NASA MODLAND sinusoidal
  tile bounding coordinates table (460 non-fill tiles, 18×36 grid)
- Authentication reuses existing `NASA_EARTHDATA_TOKEN` / `get_token()` mechanism
  unchanged
- 100 new unit tests in `tests/testthat/test-opendap.R` covering all five
  auxiliary functions (pure unit tests), all three download functions
  (mocked — no network required, runs on CI), and live integration tests
  (skipped on CI, require `NASA_EARTHDATA_TOKEN`)

# amadeus 1.3.3
- Completed migration of all `download_*` functions from `httr`/`wget`/`curl`
  command-line calls to `httr2` for all network requests
- Deprecated `download` parameter (use default `download = TRUE`) and
  `remove_command` parameter across all download functions; both now emit
  informative warnings and are ignored
- Added `hash` parameter to all `download_*` functions for optional MD5
  file integrity verification via `download_hash()` (using system `md5sum`)
- Added `unzip` and `remove_zip` parameters to `download_prism()` for
  post-download archive handling
- Added `download_run_method()` internal helper for unified httr2-based
  file retrieval with progress reporting, retry logic, and rate limiting
- Improved `check_url_status()` for general-purpose URL validation
- Expanded unit test coverage with mock-based tests for all download
  functions covering deprecation warnings, hash paths, and file-exists branches
- Added full variable reference to `download_narr()` documentation, listing
  all 88 available NARR variable abbreviations with descriptions grouped by
  category (monolevel, pressure level, subsurface); resolves
  [#194](https://github.com/NIEHS/amadeus/issues/194)
- Added `download_narr()` tests covering every variable abbreviation across
  all three variable categories
- Fixed `download_data()` dispatch to include `"edgar"` so
  `download_edgar()` is reachable via the wrapper function

# amadeus 1.3.2
- Fixed deprecated file paths for NLCD, MODIS, and Ecoregions datasets
- Removed the certificate verification from ecoregion download which is not needed anymore
- For NASA datasets MODIS, VIIRS, and Geos-CF, added additional tags in the wget scripts to prevent API throttling 
- httr upgraded to httr2
- Modified internal URL status check function for general use
- Added the news function to display the NEWS.md content within R pkgdown site
- Improved the reporting in the GitHub Actions workflows


# amadeus 1.2
- `future` and `future.apply` dependencies were removed
    - `nthreads` argument is removed from `calculate_modis_par()` and `calculate_nlcd()`
- `calculate_modis_par()` is renamed to `calculate_modis()`

# amadeus 1.1
- `calc_*()` functions are renamed to `calculate_*()` per naming convention of other function family in the package

# amadeus 1.0
- First CRAN release (v.1.0.0)