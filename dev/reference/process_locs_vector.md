# Process locations as `SpatVector`

Detect `SpatVector` object, or convert locations from class `sf`,
`data.frame` or `data.table` to `SpatVector` object, project to
coordinate reference system, and apply circular buffer.

## Usage

``` r
process_locs_vector(locs, crs, radius)
```

## Arguments

- locs:

  data.frame(1). Data frame containing columns for unique identifier,
  latitude, and longitude. Latitude and longitude columns **must** be
  named "lat" and "lon", respectively.

- crs:

  Coordinate reference system (CRS) description utilizing
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

- radius:

  integer(1). Circular buffer size (meters).

## Value

a `SpatVector` object
