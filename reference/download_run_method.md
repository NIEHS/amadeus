# Download files using httr2

Download files sequentially with httr2, streaming each response to disk.
Handles bearer-token authentication, retries, request throttling,
progress reporting, and a summary of successful, failed, and skipped
files.

## Usage

``` r
download_run_method(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2,
  timeout = 3600,
  http_version = NULL
)
```

## Arguments

- urls:

  character vector. Non-empty vector of URLs to download.

- destfiles:

  character vector. Destination file paths, in the same order and of the
  same length as `urls`.

- token:

  character(1) or NULL. Bearer token added to every request when
  supplied. Default is `NULL`. Resolve token files or environment
  variables with
  [`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md)
  before calling this function.

- show_progress:

  logical(1). Show per-file progress messages and download progress
  bars. Default is `TRUE`; status and summary messages may still be
  printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed to
  [`httr2::req_throttle()`](https://httr2.r-lib.org/reference/req_throttle.html)
  as `rate = 1 / rate_limit`; the host-based token bucket controls the
  average rate and can allow bursts.

- timeout:

  numeric(1). Timeout in seconds for each request attempt. Default is
  3600 (one hour). This does not limit the total time across multiple
  files or retries. Connection establishment also has a 30-second
  timeout, subject to the overall per-attempt timeout.

- http_version:

  integer(1) or NULL. Force HTTP version via curl's
  CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2. NULL
  (default) lets curl negotiate automatically. Pass 2L for servers that
  drop HTTP/2 connections (e.g., www.mrlc.gov for NLCD).

## Value

An invisible list containing:

- `success`: number of files downloaded and found to be non-empty.

- `failed`: number of files whose download or non-empty-file check
  failed.

- `skipped`: number of existing non-empty destination files.

- `failed_urls`: character vector of URLs whose downloads failed.

- `failed_files`: character vector of the corresponding destination
  basenames.

All five fields are always present. When no downloads fail,
`failed_urls` and `failed_files` are `character(0)`, including when
every file is skipped.

## Details

Existing non-empty destination files are skipped. Missing or empty files
are downloaded, with missing parent directories created as needed.

HTTP 429, 502, 503, and 504 responses and transport failures are
eligible for retries. When a server retry delay is unavailable, the
configured backoff is a random delay between 0 and `min(i^2, 30)`
seconds for retry index `i`. Server-provided retry delays may exceed 30
seconds. HTTP 500 is explicitly allowed by the error policy and is not
included in the retry status set. Download validation checks that the
saved file is non-empty; it does not validate its contents or format.

Individual request errors are recorded and remaining files are
attempted. Files left by a failed request or empty download are removed.
A warning reports failed filenames after the download loop finishes.

## See also

[`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md),
[`check_destfile()`](https://niehs.github.io/amadeus/reference/check_destfile.md),
[`download_preview()`](https://niehs.github.io/amadeus/reference/download_preview.md)

## Examples

``` r
if (FALSE) { # \dontrun{
download_run_method(
  urls = "https://example.com/data.nc4",
  destfiles = file.path(tempdir(), "data.nc4"),
  max_tries = 3
)
} # }
```
