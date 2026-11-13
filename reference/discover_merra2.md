# Discover MERRA-2 download files

Read monthly HTML directory listings for one standard MERRA-2 collection
and build a download table for the requested dates. Uses the filenames
published by the server, including their stream and revision prefixes.

## Usage

``` r
discover_merra2(df, collection, max_tries = 20, rate_limit = 2, timeout = 3600)
```

## Arguments

- df:

  data.frame. Non-empty table with the following character columns:

  base

  :   Monthly HTTP(S) directory URL ending in a forward slash.

  date

  :   Requested date in `"YYYYMMDD"` format.

  vdir

  :   Local directory in which to save the collection's data files.

  Each `base`/`date` pair must identify one input row after identical
  rows are removed. If a `collection` column is present, it is checked
  for a single unique character value. The caller is responsible for
  ensuring this value matches the `collection` argument, which controls
  filename matching. Other columns are carried into the returned table.

- collection:

  character(1). Standard MERRA-2 collection identifier, such as
  `"inst1_2d_asm_Nx"`. FWI URLs are constructed separately by
  [`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md).

- max_tries:

  integer(1). Maximum number of attempts for each listing request,
  including the initial attempt. Must be at least 1. Default is 20.
  Validated by the caller's `download_check()` call.

- rate_limit:

  numeric(1). Minimum seconds between listing requests to the same host.
  Must be non-negative. Default is 2; 0 disables throttling. Validated
  by the caller's `download_check()` call.

- timeout:

  numeric(1). Timeout in seconds for each request attempt. Must be at
  least 0.001. Default is 3600. This is not a time limit for discovery
  across all directories and retries.

## Value

A `data.frame` retaining the input columns, with one row per discovered
data file or associated XML metadata file. The following columns are
added or updated:

- file:

  Decoded filename, without URL directories or query parameters.

- url:

  Absolute download URL resolved from the directory listing.

- vdir:

  Destination directory; XML rows use its `metadata` subdirectory.

- destfile:

  Local file path formed from `vdir` and `file`.

## Details

Each distinct monthly directory is read once, subject to retries for
transient HTTP errors and transport failures. HTML listings are parsed
with xml2 to extract anchor `href` attributes and decode HTML entities.
This reads links present in the HTML response; JavaScript is not
executed. Relative links are resolved against the final response URL
after redirects. Download URLs retain their directory paths and query
parameters. All matching stream and revision prefixes are retained,
including multiple revisions for one date; the helper does not select a
preferred revision.

Every requested date within each directory must have at least one
matching `.nc4` data file. XML metadata is optional and is included only
when its corresponding data file is also listed. Duplicate URLs are
removed. Discovery stops if data dates are missing, a listing request
fails after retries, or different URLs in one listing share the same
filename.

Directory creation and data-file downloads are handled by
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md)
and
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).

## See also

[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  base = paste0(
    "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/",
    "M2I1NXASM.5.12.4/2024/01/"
  ),
  date = "20240101",
  vdir = file.path(tempdir(), "inst1_2d_asm_Nx")
)
discover_merra2(
  df = df,
  collection = "inst1_2d_asm_Nx",
  max_tries = 3,
  timeout = 60
)
} # }
```
