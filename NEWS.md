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