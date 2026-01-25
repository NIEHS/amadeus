# Process CropScape data

This function imports and cleans raw CropScape data, returning a single
`SpatRaster` object.

Reads CropScape file of selected `year`.

## Usage

``` r
process_cropscape(path = NULL, year = 2021, extent = NULL, ...)
```

## Arguments

- path:

  character giving CropScape data path

- year:

  numeric giving the year of CropScape data used

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
cropscape <- process_cropscape(
  path = "./data/cropscape_example.tif",
  year = 2020
)
} # }
```
