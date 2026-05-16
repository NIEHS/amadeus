# Process PRISM data

This function imports and cleans raw PRISM data, returning a single
`SpatRaster` object.

Reads time series or 30-year normal PRISM data.

## Usage

``` r
process_prism(path = NULL, element = NULL, time = NULL, extent = NULL, ...)
```

## Arguments

- path:

  character giving PRISM data path Both file and directory path are
  acceptable.

- element:

  character(1). PRISM element name

- time:

  character(1). PRISM time name. Should be character in length of 2, 4,
  6, or 8. "annual" is acceptable.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatRaster` object with metadata of time and element.

## See also

[`terra::rast`](https://rspatial.github.io/terra/reference/rast.html),
[`terra::metags`](https://rspatial.github.io/terra/reference/metags.html)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
prism <- process_prism(
  path = "./data/PRISM_ppt_stable_4kmM3_202104_nc.nc",
  element = "ppt",
  time = "202104"
)
} # }
```
