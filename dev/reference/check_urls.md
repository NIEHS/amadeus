# Implement `check_url_status`

Apply
[`check_url_status()`](https://niehs.github.io/amadeus/dev/reference/check_url_status.md)
function to a sample of download URLs.

## Usage

``` r
check_urls(urls = urls, size = NULL, method = NULL)
```

## Arguments

- urls:

  character vector of URLs

- size:

  number of observations to be sampled from `urls`

- method:

  If set to `"SKIP"`, the HTTP status will not be checked and returned.

## Value

logical vector for URL status = 200
