# Download files sequentially or using configured mirai daemons

Download files using
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
When
[`mirai::daemons_set()`](https://mirai.r-lib.org/reference/daemons_set.html)
detects configured daemons in the `"default"` compute profile,
distribute URL/destination pairs across those workers and combine their
results. Otherwise, download sequentially in the calling process.

## Usage

``` r
download_run(
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
  same length as `urls`. Use distinct absolute paths accessible to both
  the calling process and all workers for concurrent downloads.

- token:

  character(1) or NULL. Bearer token added to every request when
  supplied. Default is `NULL`. Resolve token files or environment
  variables with
  [`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md)
  before calling this function.

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

- timeout:

  numeric(1). Timeout in seconds for each request attempt. Default is
  3600 (one hour). This does not limit the total time across multiple
  files or retries.

- http_version:

  integer(1) or NULL. Force HTTP version via curl's
  CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
  `NULL` (default) lets curl negotiate automatically. Passed unchanged
  to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  in both execution modes.

## Value

An invisible list containing:

- `success`: number of files downloaded and found to be non-empty.

- `failed`: number of files whose download or non-empty-file check
  failed.

- `skipped`: number of existing non-empty destination files.

- `failed_urls`: character vector of URLs whose downloads failed.

- `failed_files`: character vector of the corresponding destination
  basenames.

For non-empty requests that return normally, all five fields are
present; `failed_urls` and `failed_files` are `character(0)` when no
downloads fail. A warning lists failed filenames when `failed > 0`.

## Details

Workers must have access to compatible `amadeus` code and its
dependencies, including internal helpers such as
[`download_report()`](https://niehs.github.io/amadeus/reference/download_report.md).
Loading development code with
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
in the calling process does not load it on workers. During development,
use
[`mirai::everywhere()`](https://mirai.r-lib.org/reference/everywhere.html)
to load the same package source on workers before submitting downloads.
Remote workers need access to the same destination storage as the
calling process.

Existing non-empty destination files are skipped. Missing or empty files
are downloaded, with missing parent directories created as needed.
Request handling, retries, and file validation are delegated to
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
Throttling is maintained separately in each process. Scaling the
interval by a fixed daemon count targets the combined average request
rate; token buckets can still allow bursts.

Handled request failures are included in the returned summary.
Concurrent results are collected without early stopping. After
collection, mirai error values representing unexpected worker errors,
worker interrupts, or connection failures are checked before
aggregation. If any are present, the function stops with
`"Download worker errors:"`, listing each affected destination path and
its error message or code. No combined summary is printed or returned in
this case. Otherwise, the results are combined and reported by
[`download_report()`](https://niehs.github.io/amadeus/reference/download_report.md)
in the calling process, then returned invisibly.

## Concurrent downloads

File downloads use configured daemons in the `"default"` compute
profile, or run sequentially when that profile has no configured
daemons. Daemon creation and shutdown are managed by the caller using
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
For concurrent downloads, configure a fixed local pool with
`mirai::daemons(n, .compute = "default")` before calling this function.
Daemon detection, counting, and task dispatch always use the `"default"`
compute profile, regardless of the active profile. Daemon counting and
rate scaling assume that the pool still contains the `n` workers
configured by that call, with no workers added separately or pool
resizing during downloads. Each worker receives one URL/destination pair
per task; the remaining request arguments are constant across tasks.

Source-specific validation, URL discovery, archive extraction, and
directory hashing run in the calling process. Only file downloads are
dispatched to workers.

## See also

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`download_report()`](https://niehs.github.io/amadeus/reference/download_report.md),
[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html),
[`mirai::mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html)

## Author

Mitchell Manware

## Examples

``` r
if (FALSE) { # \dontrun{
download_run(
  urls = "https://example.com/data.nc4",
  destfiles = file.path(tempdir(), "data.nc4"),
  max_tries = 3
)
} # }
```
