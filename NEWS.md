# amadeus 2.0.0
## Major updates to code base - breaking changes have been minmized but please report if 1.3.x versions are not working as expected

- Refactored code base to improve maintainability and utilize modern R API designs and best practices
- The only breaking change that should affect previous version users is the `process_tri()` and `calculate_tri()` series. 

## Spatial and Temporal summarization with `.by` and `.by_time` parameters

- Added `.by` and `.by_time` parameters to all `calculate_*()` functions for consistent spatial and temporal summarization options across all datasets
- The default behavior of `calculate_*()` functions remains unchanged - i.e., no summarization, returning extracted or summarized values at the temporal resolution of the data -  but users can now specify `.by` for spatial grouping (e.g., by HUC, county, state) and `.by_time` for temporal grouping (e.g., by year, month) to obtain summarized covariate values directly from the calculation step

## 'frac' option for categorical variable covariate calculation 

- `hms`, `koppen`, and `ecoregion` datasets now have optional fraction covariate calculation 
that returns the fraction of each category in the radius buffer. 

## `drop` options for categorical variable covariate calculation 

- `hms`, `koppen`, and `ecoregion` datasets now have optional `drop` parameter that will drop or exclude categories that don't have any coverage in the radius buffer for a given point, which can help reduce the number of columns returned when many categories are possible but only a few are present in the area around the point.

## Detailed vignettes for each dataset including available variables, spatial and temporal resolution, and example use cases

- workflow vignette for each dataset with detailed information on available variables, spatial and temporal resolution, and example use cases for each dataset

## nhdplusTools use

- Moved nhdplusTools from Imports to Suggests and added `requireNamespace("nhdplusTools")` checks in all functions that use it

## Additional MODIS products

- Added support for additional MODIS products with a focus on the burned area and active fire products

## Finalize data API that were previously in development

- Added or completed the functonality for PRISM, EDGAR, CropScape, and HUC datasets 

## Use of GitHub Copilot for code generation and refactoring

- Used GitHub Copilot for help with code generation and refactoring across the code base, including function development, documentation, and unit tests; all generated code was reviewed and edited by the development team to ensure accuracy and consistency with package standards

## Drought Index Support

- Added `download_drought()`, `process_drought()`, and `calculate_drought()`
  supporting three drought datasets:
  - **SPEI** (Standardized Precipitation-Evapotranspiration Index): multi-year
    netCDF files by accumulation timescale from
    <https://spei.csic.es>
  - **EDDI** (Evaporative Demand Drought Index): weekly files by accumulation
    timescale from NOAA PSL
    (<https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/>);
    legacy netCDF files from the `CONUS_Archive` path are also supported
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

## Migrate from wget/curl to httr2 
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
