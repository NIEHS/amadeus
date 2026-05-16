# Bucket a time column to a `.by_time` unit

Buckets time values to one of the supported `.by_time` units.

## Usage

``` r
bucket_time_by_unit(time_vals, unit)
```

## Arguments

- time_vals:

  vector. Time values to bucket.

- unit:

  character(1). A valid `.by_time` time-unit token.

## Value

vector. Bucketed values as POSIXct (minute/hour) or Date.

## Author

Insang Song
