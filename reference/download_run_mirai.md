# Download files using configured mirai daemons

Internal concurrent runner used by
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md).
Distribute URL/destination pairs across the configured `"default"`
compute profile, calling
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
once per pair, then collect and combine the results.

## Usage

``` r
download_run_mirai(
  urls = NULL,
  destfiles = NULL,
  token = NULL,
  max_tries = 20,
  rate_limit = 2,
  timeout,
  http_version,
  daemons = NULL
)
```

## Arguments

- urls:

  character vector. Non-empty vector of URLs to download.

- destfiles:

  character vector. Distinct absolute destination paths, in the same
  order and of the same length as `urls`. Paths must be accessible to
  both the calling process and all workers.

- token:

  character(1) or NULL. Resolved bearer token added to every request
  when supplied. Default is `NULL`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Multiplied by `as.integer(daemons)` before being passed to each
  worker.

- timeout:

  numeric(1). Timeout in seconds for each request attempt. Required;
  [`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
  supplies 3600 by default.

- http_version:

  integer(1) or NULL. HTTP version passed to curl via
  CURLOPT_HTTP_VERSION: 1L = HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2.
  Required; supply `NULL` to allow automatic negotiation, as
  [`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
  does by default.

- daemons:

  integer(1). Positive number of workers in the fixed `"default"` pool.
  Although the default is `NULL`, a count must be supplied.
  [`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
  obtains it with `mirai::nextget("n", .compute = "default")`.

## Value

An invisible list containing numeric counts `success`, `failed`, and
`skipped`, plus character vectors `failed_urls` and `failed_files`. The
latter contains destination basenames. For valid non-empty inputs, both
failure vectors are `character(0)` when no downloads fail.

## Details

The caller manages daemon creation and shutdown. Configure a fixed local
pool with `mirai::daemons(n, .compute = "default")` before use and
supply the same count as `daemons`. Detection and dispatch always use
`"default"`, regardless of the active compute profile. Rate scaling
assumes that the pool size remains fixed and matches the supplied count;
each process has its own throttle state, and token buckets can still
allow bursts.

[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
validates URLs and destinations before calling this helper. Direct
callers must supply valid inputs. Each task receives one URL and
destination, with the remaining request arguments held constant and
`show_progress = FALSE`. Workers need compatible `amadeus` code and
access to the destination storage; see
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
for development setup.

Results are collected without early stopping. Handled request failures
remain part of the combined summary. After collection, unexpected worker
errors, worker interrupts, or connection failures returned as mirai
error values cause an error headed `"Download worker errors:"`, listing
each affected destination and its error message or code. This happens
before aggregation, so no combined summary is printed or returned in
that case. Otherwise,
[`download_report()`](https://niehs.github.io/amadeus/reference/download_report.md)
prints the combined totals and warns about failed files in the calling
process.

## See also

[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`download_report()`](https://niehs.github.io/amadeus/reference/download_report.md),
[`mirai::mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html),
[`mirai::collect_mirai()`](https://mirai.r-lib.org/reference/collect_mirai.html)

## Author

Mitchell Manware
