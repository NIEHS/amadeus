# Convert a `SpatRasterDataset` to an `sftime`

Convert a `SpatRasterDataset` object to an `sftime` object. `x` must
contain a time-defining column, identified in `timename`.

## Usage

``` r
spatrds_as_sftime(x, timename = "time")
```

## Arguments

- x:

  a `SpatRasterDataset` object (~ list of named SpatRasters)

- timename:

  character for time column name in the sftime (default: "time")

## Value

an `sftime` object

## See also

[terra::sds](https://rspatial.github.io/terra/reference/sds.html)

## Author

Eva Marques
