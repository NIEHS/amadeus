# Prepare covariates for return

Check the time column for proper class and, if `geom = TRUE`, transform
`data.frame` into a `SpatVector` object.

## Usage

``` r
calc_return_locs(covar, POSIXt = TRUE, geom, crs)
```

## Arguments

- covar:

  data.frame(1). Calculated covariates `data.frame`.

- POSIXt:

  logical(1). Should the time values in `covar` be of class `POSIXt`? If
  `FALSE`, the time values will be checked for integer class (year and
  year-month).

- geom:

  FALSE/"sf"/"terra". Should `covar` be returned as a `data.frame`?
  Default is `FALSE`, options with geometry are "sf" or "terra".

- crs:

  terra::crs(1). Coordinate reference system (inherited from `from`).

## Value

a data.frame or SpatVector object (depending on `geom` paramter)

## Author

Mitchell Manware
