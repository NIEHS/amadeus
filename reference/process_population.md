# Process population density data

The `process_secac_population()` function imports and cleans raw
population density data, returning a single `SpatRaster` object.

## Usage

``` r
process_population(path = NULL, extent = NULL, ...)
```

## Arguments

- path:

  character(1). Path to GeoTIFF (.tif) or netCDF (.nc) file.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Author

Mitchell Manware

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
pop <- process_population(
  path = "./data/sedac_population_example.tif"
)
} # }
```
