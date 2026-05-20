# Process MODIS layers

Aggregate layers in a sub-dataset in sinusoidal MODIS products.

Some MODIS products consist of multi-layer subdatasets. This function
aggregates multiple layers into single layer `SpatRaster.` `fun_agg` is
applied at overlapping cells.

## Usage

``` r
process_flatten_sds(path = NULL, subdataset = NULL, fun_agg = "mean", ...)
```

## Arguments

- path:

  character(1). Full path to MODIS HDF4/HDF5 file. Direct sub-dataset
  access is supported, for example,
  HDF4_EOS:EOS_GRID:{filename}:{base_grid_information}:{sub-dataset}

- subdataset:

  character(1). Exact or regular expression filter of sub-dataset.

- fun_agg:

  character(1). Function name to aggregate layers. Should be acceptable
  to
  [terra::tapp](https://rspatial.github.io/terra/reference/tapp.html).

- ...:

  Placeholders.

## Value

a `SpatRaster` object

## Note

HDF values are read as original without scaling. Users should consult
MODIS product documentation to apply proper scaling factor for post-hoc
adjustment. If users have no preliminary information about MODIS
sub-datasets, consider running
`terra::describe(__filename__, sds = TRUE)` to navigate the full list of
sub-datasets in the input file then consult the documentation of MODIS
product.

## See also

[terra::tapp](https://rspatial.github.io/terra/reference/tapp.html),
[terra::rast](https://rspatial.github.io/terra/reference/rast.html),
[terra::describe](https://rspatial.github.io/terra/reference/describe.html)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
mod09ga_flatten <- process_flatten_sds(
  path =
    list.files("./data", pattern = "MOD09GA.", full.names = TRUE)[1],
  subdataset = "(sur_refl_b0)",
  fun_agg = "mean"
)
} # }
```
