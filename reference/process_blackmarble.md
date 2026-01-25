# Assign VIIRS Black Marble products corner coordinates to retrieve a merged raster

This function will return a `SpatRaster` object with georeferenced h5
files of Black Marble product. Referencing corner coordinates are
necessary as the original h5 data do not include such information.

## Usage

``` r
process_blackmarble(
  path = NULL,
  date = NULL,
  tile_df = process_blackmarble_corners(),
  subdataset = 3L,
  crs = "EPSG:4326",
  ...
)
```

## Arguments

- path:

  character. Full paths of h5 files.

- date:

  character(1). Date to query.

- tile_df:

  data.frame. Contains four corner coordinates in fields named
  `c("xmin", "xmax", "ymin", "ymax")`. See
  [`process_blackmarble_corners`](https://niehs.github.io/amadeus/reference/process_blackmarble_corners.md)
  to generate a valid object for this argument.

- subdataset:

  integer(1). Subdataset number to process. Default is 3L.

- crs:

  character(1). terra::crs compatible CRS. Default is `"EPSG:4326"`

- ...:

  For internal use.

## Value

a `SpatRaster` object

## References

- [Wang, Z. (2022). Black Marble User Guide (Version 1.3).
  NASA.](https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.3_Sep_2022.pdf)

## See also

- [`terra::describe`](https://rspatial.github.io/terra/reference/describe.html)

- [`terra::merge`](https://rspatial.github.io/terra/reference/merge.html)

- [`process_blackmarble_corners`](https://niehs.github.io/amadeus/reference/process_blackmarble_corners.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
vnp46a2 <- process_blackmarble(
  path =
    list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
  date = "2024-01-01",
  tile_df =
    process_blackmarble_corners(hrange = c(8, 10), vrange = c(4, 5)),
  subdataset = 3L,
  crs = "EPSG:4326"
)
} # }
```
