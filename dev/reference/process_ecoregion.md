# Process ecoregion data

The `process_ecoregion` function imports and cleans raw ecoregion data,
returning a `SpatVector` object.

## Usage

``` r
process_ecoregion(path = NULL, extent = NULL, ...)
```

## Arguments

- path:

  character(1). Path to Ecoregion Shapefiles

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatVector` object

## Note

The function will fix Tukey's bridge in Portland, ME. This fix will
ensure that the EPA air quality monitoring sites will be located within
the ecoregion.

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
ecoregion <- process_ecoregion(
  path = "./data/epa_ecoregion.gpkg"
)
} # }
```
