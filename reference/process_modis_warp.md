# Warp MODIS swath data into rectilinear grid raster

Swath data is a type of MODIS data, where curvilinear points are stored
with varying resolution depending on the relative position of the sensor
axis. As this type of data typically does not work well with planar
spatial data, users should warp or rectify this data into a rectilinear
raster. Main procedure is done with
[`stars::st_warp`](https://r-spatial.github.io/stars/reference/st_warp.html),
in which users are able to customize the threshold to fill potential
gaps that appear where the target resolution is finer than the local
resolution of curvilinear grid points.

## Usage

``` r
process_modis_warp(
  path = NULL,
  cellsize = 0.1,
  threshold = cellsize * 4,
  crs = 4326,
  ...
)
```

## Arguments

- path:

  File path of MODIS swath with exact sub-dataset specification.

- cellsize:

  numeric(1). Cell size (spatial resolution) of output rectilinear grid
  raster.

- threshold:

  numeric(1). Maximum distance to fill gaps if occur.

- crs:

  integer(1)/character(1). Coordinate system definition. Should be
  compatible with EPSG codes or WKT2. See
  [`terra::crs`](https://rspatial.github.io/terra/reference/crs.html)
  and
  [`sf::st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html) /
  [EPSG](https://epsg.io/)

- ...:

  For internal use.

## Value

a `stars` object

## Note

This function handles one file at a time.

## See also

[`terra::rectify`](https://rspatial.github.io/terra/reference/rectify.html)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
mod06l2_warp <- process_modis_warp(
  path = paste0(
    "HDF4_EOS:EOS_SWATH:",
    list.files(
      "./data/mod06l2",
      full.names = TRUE,
      pattern = ".hdf"
    )[1],
    ":mod06:Cloud_Fraction"
  ),
  cellsize = 0.1,
  threshold = 0.4,
  crs = 4326
)
} # }
```
