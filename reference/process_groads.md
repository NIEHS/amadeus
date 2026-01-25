# Process roads data

The `process_groads()` function imports and cleans raw road data,
returning a single `SpatVector` object.

## Usage

``` r
process_groads(path = NULL, extent = NULL, ...)
```

## Arguments

- path:

  character(1). Path to geodatabase or shapefiles.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatVector` object

## Note

U.S. context. The returned `SpatVector` object contains a `$description`
column to represent the temporal range covered by the dataset. For more
information, see
<https://data.nasa.gov/dataset/global-roads-open-access-data-set-version-1-groadsv1>.

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
groads <- process_groads(
  path = "./data/groads_example.shp"
)
} # }
```
