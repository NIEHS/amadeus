# Download meteorological and atmospheric data

The `download_merra2()` function accesses and downloads various
meteorological and atmospheric collections from [NASA's Modern-Era
Retrospective analysis for Research and Applications, Version 2
(MERRA-2) model](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), and
the daily corrected Global Fire Weather Index (FWI) product derived from
MERRA-2 weather inputs.

## Usage

``` r
download_merra2(
  collection = c("inst1_2d_asm_Nx", "inst1_2d_int_Nx", "inst1_2d_lfo_Nx",
    "inst3_3d_asm_Np", "inst3_3d_aer_Nv", "inst3_3d_asm_Nv", "inst3_3d_chm_Nv",
    "inst3_3d_gas_Nv", "inst3_2d_gas_Nx", "inst6_3d_ana_Np", "inst6_3d_ana_Nv",
    "statD_2d_slv_Nx", "tavg1_2d_adg_Nx", "tavg1_2d_aer_Nx", "tavg1_2d_chm_Nx",
    "tavg1_2d_csp_Nx", "tavg1_2d_flx_Nx", "tavg1_2d_int_Nx", "tavg1_2d_lfo_Nx",
    "tavg1_2d_lnd_Nx", "tavg1_2d_ocn_Nx", "tavg1_2d_rad_Nx", "tavg1_2d_slv_Nx",
    "tavg3_3d_mst_Ne", "tavg3_3d_trb_Ne", "tavg3_3d_nav_Ne", "tavg3_3d_cld_Np", 
    
    "tavg3_3d_mst_Np", "tavg3_3d_rad_Np", "tavg3_3d_tdt_Np", "tavg3_3d_trb_Np",
    "tavg3_3d_udt_Np", "tavg3_3d_odt_Np", "tavg3_3d_qdt_Np", "tavg3_3d_asm_Nv",
    "tavg3_3d_cld_Nv", "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx", "fwi"),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- collection:

  character vector. One or more supported MERRA-2 collection
  identifiers, or `"fwi"` for the daily corrected Global Fire Weather
  Index product (`MERRA2.CORRECTED`). Repeated identifiers are removed.
  The default selects all collections listed in the function signature,
  including FWI. Supported identifiers and product metadata are stored
  in the installed `extdata/df_merra2.csv` lookup table.

- nasa_earth_data_token:

  character(1) or NULL. NASA Earthdata token, path to a file containing
  a token, or `NULL` to use the `NASA_EARTHDATA_TOKEN` environment
  variable.
  [`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md)
  checks the environment variable first, then a supplied file path or
  token string. Token lookup is required when any standard MERRA-2
  collection is requested.

- date:

  character(1 or 2). A single day or two range endpoints in
  `"YYYY-MM-DD"` format. Endpoints may be supplied in either order; the
  resulting daily sequence includes both endpoints.

- directory_to_save:

  character(1). Non-empty directory path for downloads. A path must be
  supplied despite the `NULL` default. Data files are saved under a
  subdirectory named for each collection; available XML companions are
  saved in that collection's `metadata` subdirectory.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed, acknowledging that the
  downloads may require substantial storage and memory.

- download:

  logical(1). Deprecated. `FALSE` emits a warning and returns a download
  preview. Required token lookup, directory creation, and file discovery
  still run before returning. Standard collections require remote
  listing requests in preview mode; FWI URLs are constructed locally.

- remove_command:

  logical(1). Deprecated and ignored. `TRUE` emits a warning.

- hash:

  logical(1). If `TRUE`, return the combined MD5 hash calculated by
  [`download_hash()`](https://niehs.github.io/amadeus/reference/download_hash.md)
  for all files under `directory_to_save`, including metadata and files
  from earlier downloads. Default is `FALSE`. Ignored when
  `download = FALSE`.

- show_progress:

  logical(1). Show per-file progress messages and download progress
  bars. Default is `TRUE`; summary and status messages may still be
  printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per listing or file request, including
  the initial attempt. Must be at least 1. Default is 20.

- rate_limit:

  numeric(1). Non-negative request-throttling interval in seconds.
  Default is 2. Passed to both discovery and file downloads; their
  throttling behavior is described below.

## Value

With `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts, plus `failed_urls` and
`failed_files` character vectors. All five fields are always present;
the two failure vectors are `character(0)` when no downloads failed,
including when every file was skipped. Counts include XML metadata
files.

With `download = TRUE` and `hash = TRUE`, a character scalar containing
the combined directory MD5 hash, returned visibly.

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`, regardless of `hash`.

## Details

Standard MERRA-2 file URLs are discovered from monthly HTML directory
listings. Published stream and revision prefixes are retained, and all
matching files are included when more than one revision is listed for a
date. Every requested date must have at least one matching `.nc4` file
in its directory. Companion `.nc4.xml` metadata is optional and is
included only when the corresponding data file is also listed. FWI `.nc`
URLs are constructed directly from the requested dates.

Discovery stops before file downloads if a monthly listing cannot be
read, a requested data date is missing, or different URLs in a listing
would use the same local filename. Once discovery succeeds,
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty files and records individual download failures,
warning after the remaining files have been attempted.

Listing requests to the same host are spaced by at least `rate_limit`
seconds; 0 disables listing throttling. File downloads use the
host-based token bucket described in
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
Discovery and file downloads use their default per-attempt timeout of
3600 seconds; this function does not expose a `timeout` argument.

## Note

Due to NASA data access policies, standard MERRA-2 GES DISC downloads
require a valid NASA Earthdata token for authentication. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/reference/setup_nasa_token.md)
for setup. The `"fwi"` collection is hosted on the public GlobalFWI
portal and does not require Earthdata authentication.

## See also

[`setup_nasa_token()`](https://niehs.github.io/amadeus/reference/setup_nasa_token.md),
[`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`download_hash()`](https://niehs.github.io/amadeus/reference/download_hash.md),
[`process_merra2()`](https://niehs.github.io/amadeus/reference/process_merra2.md),
[`get_merra2_info()`](https://niehs.github.io/amadeus/reference/get_merra2_info.md)

## Author

Mitchell Manware, Insang Song, Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure NASA_EARTHDATA_TOKEN with setup_nasa_token() before downloading.
download_merra2(
  collection = "inst1_2d_int_Nx",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# Download two collections for an inclusive date range.
download_merra2(
  collection = c("inst1_2d_int_Nx", "tavg1_2d_slv_Nx"),
  date = c("2024-01-01", "2024-01-02"),
  directory_to_save = file.path(tempdir(), "merra2"),
  acknowledgement = TRUE,
  max_tries = 3
)
} # }
```
