# Convert a `SpatRaster` to an `sftime`

Convert a `SpatRaster` object to an `sftime` object. `x` must contain a
time-defining column, identified in `timename`.

## Usage

``` r
spatraster_as_sftime(x, varname, timename = "time")
```

## Arguments

- x:

  a `SpatRaster` object

- varname:

  character for variable column name in the sftime

- timename:

  character for time column name in the sftime (default: "time")

## Value

a `sftime` object

## See also

[terra::rast](https://rspatial.github.io/terra/reference/rast.html)

## Author

Eva Marques
