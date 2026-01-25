# Generate time sequence

Generate a sequence of time values based on the GEOS-CF collection.

## Usage

``` r
generate_time_sequence(collection)
```

## Arguments

- collection:

  character(1). GEOS-CF data collection

## Value

vector

## Note

GEOS-CF hourly values are observed on the hour (ie. 0000 = 12:00:00 AM,
0100 = 01:00:00 AM) or the half hour (ie. 0030 = 12:30:00 AM, 0130 =
01:30:00 AM). Typically, 2-dimensional collections (latitude and
longitude only) utilize half hour, and 3-dimensional collections
(latitude, longitude, and time) utilize hour.
