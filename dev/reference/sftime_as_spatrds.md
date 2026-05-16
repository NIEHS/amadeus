# Convert an `sftime` to a `SpatRasterDataset`

Convert an `sftime` object to a `SpatRasterDataset` object.

## Usage

``` r
sftime_as_spatrds(x)
```

## Arguments

- x:

  an `sftime` object

## Value

an `SpatRasterDataset` object

## Note

Running `sftime_as_spatrds` can take a long time if `x` is not spatially
and temporally structured.

## See also

[terra::sds](https://rspatial.github.io/terra/reference/sds.html)

## Author

Eva Marques
