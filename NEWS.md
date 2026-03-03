# amadeus 1.3.3
- Completed migration of all `download_*` functions from `httr`/`wget`/`curl`
  command-line calls to `httr2` for all network requests
- Deprecated `download` parameter (use default `download = TRUE`) and
  `remove_command` parameter across all download functions; both now emit
  informative warnings and are ignored
- Added `hash` parameter to all `download_*` functions for optional SHA-256
  file integrity verification via `download_hash()`
- Added `unzip` and `remove_zip` parameters to `download_prism()` for
  post-download archive handling
- Added `download_run_method()` internal helper for unified httr2-based
  file retrieval with progress reporting, retry logic, and rate limiting
- Improved `check_url_status()` for general-purpose URL validation
- Expanded unit test coverage with mock-based tests for all download
  functions covering deprecation warnings, hash paths, and file-exists branches

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