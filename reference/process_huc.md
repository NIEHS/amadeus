# Retrieve Hydrologic Unit Code (HUC) data

Retrieve Hydrologic Unit Code (HUC) data

## Usage

``` r
process_huc(
  path,
  layer_name = NULL,
  huc_level = NULL,
  huc_header = NULL,
  extent = NULL,
  ...
)
```

## Arguments

- path:

  character. Path to the file or the directory containing HUC data.

- layer_name:

  character(1). Layer name in the `path`

- huc_level:

  character(1). Field name of HUC level

- huc_header:

  character(1). The upper level HUC code header to extract lower level
  HUCs.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Arguments passed to
  [`nhdplusTools::get_huc()`](https://doi-usgs.github.io/nhdplusTools/reference/get_huc.html)

## Value

a `SpatVector` object

## See also

[`nhdplusTools::get_huc`](https://doi-usgs.github.io/nhdplusTools/reference/get_huc.html)

## Author

Insang Song

## Examples

``` r
## NOTE: Examples are wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
library(terra)
getf <- "WBD_National_GDB.gdb"
# check the layer name to read
terra::vector_layers(getf)
test1 <- process_huc(
  getf,
  layer_name = "WBDHU8",
  huc_level = "huc8"
)
test2 <- process_huc(
  getf,
  layer_name = "WBDHU8",
  huc_level = "huc8"
)
test3 <- process_huc(
  "",
  layer_name = NULL,
  huc_level = NULL,
  huc_header = NULL,
  id = "030202",
  type = "huc06"
)
} # }
```
