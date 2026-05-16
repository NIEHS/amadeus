# Process EDGAR emissions data

The `process_edgar()` function imports extracted EDGAR gridded emissions
files and returns a single `SpatRaster` object. Raster formats supported
by
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
such as NetCDF (`.nc`, `.nc4`) and GeoTIFF (`.tif`, `.tiff`) are
supported.

## Usage

``` r
process_edgar(path = NULL, extent = NULL, ...)
```

## Arguments

- path:

  character. Directory containing extracted EDGAR raster files or one or
  more file paths.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster; if `NULL`
  (default), the entire raster is loaded.

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Note

`process_edgar()` currently supports gridded raster outputs from
[`download_edgar()`](https://niehs.github.io/amadeus/dev/reference/download_edgar.md)
such as the default `format = "nc"`. Plain-text EDGAR downloads should
be re-downloaded as raster outputs before processing.

## See also

[`download_edgar()`](https://niehs.github.io/amadeus/dev/reference/download_edgar.md),
[`calculate_edgar()`](https://niehs.github.io/amadeus/dev/reference/calculate_edgar.md)

## Author

Mariana Alifa Kassien, Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires data that is
##       not included in the package.
if (FALSE) { # \dontrun{
edgar <- process_edgar(
  path = "./data/edgar",
  extent = c(-85, -75, 33, 37)
)
} # }
```
