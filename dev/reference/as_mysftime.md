# Create an `sftime` object

Create a `sftime` object from one of `data.frame`, `data.table`, `sf`,
`sftime`, `SpatRaster`, `SpatRasterDataset`, `SpatVector`

## Usage

``` r
as_mysftime(x, ...)
```

## Arguments

- x:

  an object of class `data.frame`, `data.table`, `sf`, `sftime`,
  `SpatRaster`, `SpatRasterDataset` or `SpatVector`

- ...:

  if x is a data.frame or data.table: lonname, latname, timename and crs
  arguments are required. If x is a sf or sftime, timename argument is
  required. If x is a terra::SpatRaster, varname argument is required.

## Value

an `sftime` object with constrained time column name

## See also

[check_mysftime](https://niehs.github.io/amadeus/dev/reference/check_mysftime.md),
[sf_as_mysftime](https://niehs.github.io/amadeus/dev/reference/sf_as_mysftime.md),
[data.frame](https://rdrr.io/r/base/data.frame.html),
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html),
[terra::rast](https://rspatial.github.io/terra/reference/rast.html),
[terra::sds](https://rspatial.github.io/terra/reference/sds.html),
[terra::vect](https://rspatial.github.io/terra/reference/vect.html)

## Author

Eva Marques
