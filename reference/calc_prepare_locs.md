# Prepare extraction locations

Prepare the point locations for extracting data by transforming `locs`
to a `SpatVector`, projecting to the coordinate reference system of
`from`, and creating a `data.frame` containing `locs_id` for retaining
extracted values.

## Usage

``` r
calc_prepare_locs(from, locs, locs_id, radius, geom = FALSE)
```

## Arguments

- from:

  SpatRaster(1) or SpatVector(1). Output from `process_\*()`. Passed
  from `calc_\*()`.

- locs:

  data.frame. character to file path, SpatVector, or sf object. Passed
  from `calc_\*()`.

- locs_id:

  character(1). Column within `locations` CSV file containing identifier
  for each unique coordinate location. Passed from `calc_\*()`.

- radius:

  integer(1). Circular buffer distance around site locations. (Default =
  0). Passed from `calc_\*()`.

- geom:

  logical(1). Should the geometry of `locs` be returned in the
  `data.frame`? Default is `FALSE`, options "sf" or "terra" will
  preserve geometry, but will use `terra` for extraction.

## Value

A `list` containing `SpatVector` and `data.frame` objects

## See also

[`process_locs_vector()`](https://niehs.github.io/amadeus/reference/process_locs_vector.md),
[`check_for_null_parameters()`](https://niehs.github.io/amadeus/reference/check_for_null_parameters.md)
