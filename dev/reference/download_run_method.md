# Download files using httr2

Execute downloads using httr2 with robust retry logic and rate limiting.
This function handles authentication, retries, progress tracking, and
streams files directly to disk. HTTP-status retries use exponential
backoff capped at 30 s to avoid long hangs from DNS timeouts (each
attempt takes ~10 s). Transport-level failures (SSL drops, connection
resets) are also retried up to `max_tries` times.

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

  character vector. URLs to download

- destfiles:

  character vector. Destination file paths (same length as urls)

- token:

  character(1). Authentication token (optional, e.g., for NASA
  EarthData)

- show_progress:

  logical(1). Show download progress bars (default TRUE)

- max_tries:

  integer(1). Maximum number of retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

- timeout:

  numeric(1). Timeout in seconds for each request (default 3600 = 1
  hour)

- http_version:

  integer(1). Force HTTP version via curl's CURLOPT_HTTP_VERSION: 1L =
  HTTP/1.0, 2L = HTTP/1.1, 3L = HTTP/2. NULL (default) lets curl
  negotiate automatically. Pass 2L for servers that drop HTTP/2
  connections (e.g., www.mrlc.gov for NLCD).

## Value

invisible list with success and failure counts
