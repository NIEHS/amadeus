# Convert a `SpatVector` to an `sftime`

Convert a `SpatVector` object to an `sftime` object. `x` must contain a
time-defining column, identified in `timename`.

## Usage

``` r
spatvector_as_sftime(x, timename = "time")
```

## Arguments

- x:

  a `SpatVector` object

- timename:

  character for time column name in x (default: "time")

## Value

an `sftime` object

## See also

[terra::vect](https://rspatial.github.io/terra/reference/vect.html)

## Author

Eva Marques
