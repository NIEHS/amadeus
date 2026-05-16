# Parse GOES start datetime from filename

Extracts the scan start datetime from a GOES-R series ADP filename. The
start timestamp field uses the format `sYYYYDDDHHMMSSf` where `DDD` is
the day of year (1–366).

## Usage

``` r
goes_parse_start_datetime(path)
```

## Arguments

- path:

  character(1). Full or base file path.

## Value

POSIXct scalar (UTC).
