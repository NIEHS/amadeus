# Convert an `sftime` to a `SpatRaster`

Convert an `sftime` object to a `SpatRaster` object. Returns a
`SpatRatser` with one layer for each time step in `x`.

## Usage

``` r
sftime_as_spatraster(x, varname)
```

## Arguments

- x:

  an `sftime` object

- varname:

  variable to rasterize

## Value

a `SpatRaster` object

## Note

Running `sftime_as_spatraster` can take a long time if `x` is not
spatially structured.

## See also

[terra::rast](https://rspatial.github.io/terra/reference/rast.html)

## Author

Eva Marques
