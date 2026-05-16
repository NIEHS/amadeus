# Mosaic MODIS swaths

This function will return a `SpatRaster` object with values of selected
subdatasets. Swath data include curvilinear grids, which require
warping/rectifying the original curvilinear grids into rectilinear
grids. The function internally warps each of inputs then mosaic the
warped images into one large `SpatRaster` object. Users need to select a
subdataset to process. The full path looks like
`"HDF4_EOS:EOS_SWATH:{file_path}:mod06:subdataset"`, where file_path is
the full path to the hdf file.

## Usage

``` r
process_modis_swath(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  suffix = ":mod06:",
  resolution = 0.05,
  ...
)
```

## Arguments

- path:

  character. Full paths of hdf files.

- date:

  character(1). Date to query.

- subdataset:

  character. Subdatasets to process. **Unlike other preprocessing
  functions, this argument should specify the exact subdataset name.**
  For example, when using MOD06_L2 product, one may specify
  `c("Cloud_Fraction", "Cloud_Optical_Thickness")`, etc. The subdataset
  names can be found in
  [`terra::describe()`](https://rspatial.github.io/terra/reference/describe.html)
  output.

- suffix:

  character(1). Should be formatted `:{product}:`, e.g., `:mod06:`

- resolution:

  numeric(1). Resolution of output raster. Unit is degree (decimal
  degree in WGS84).

- ...:

  For internal use.

## Value

- a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a single
  file with full specification of subdataset.

- a `SpatRaster` object (crs = `"EPSG:4326"`): if `path` is a list of
  files. In this case, the returned object will have the maximal extent
  of multiple warped layers

## See also

- [`process_modis_warp()`](https://niehs.github.io/amadeus/dev/reference/process_modis_warp.md),
  [`stars::read_stars()`](https://r-spatial.github.io/stars/reference/read_stars.html),
  [`stars::st_warp()`](https://r-spatial.github.io/stars/reference/st_warp.html)

- [GDAL HDF4 driver
  documentation](https://gdal.org/en/latest/drivers/raster/hdf4.html)

- [`terra::describe()`](https://rspatial.github.io/terra/reference/describe.html):
  to list the full subdataset list with `sds = TRUE`

- [`terra::sprc()`](https://rspatial.github.io/terra/reference/sprc.html),
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
mod06l2_swath <- process_modis_swath(
  path = list.files(
    "./data/mod06l2",
    full.names = TRUE,
    pattern = ".hdf"
  ),
  date = "2024-01-01",
  subdataset = "Cloud_Fraction",
  suffix = ":mod06:",
  resolution = 0.05
)
} # }
```
