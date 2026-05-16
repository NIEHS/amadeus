# Process land cover data

The `process_nlcd()` function imports and cleans raw land cover data,
returning a single `SpatRaster` object.

Reads NLCD file of selected `year`.

## Usage

``` r
process_nlcd(path = NULL, year = 2021, extent = NULL, ...)
```

## Arguments

- path:

  character giving nlcd data path

- year:

  numeric giving the year of NLCD data used

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Author

Eva Marques, Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
nlcd <- process_nlcd(
  path = "./data/",
  year = 2021
)
} # }
```
