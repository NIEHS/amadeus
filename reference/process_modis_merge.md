# Process MODIS .hdf files

Get mosaic or merged raster from multiple MODIS hdf files.

## Usage

``` r
process_modis_merge(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  ...
)
```

## Arguments

- path:

  character. Full list of hdf file paths. preferably a recursive search
  result from
  [`base::list.files`](https://rdrr.io/r/base/list.files.html).

- date:

  character(1). date to query. Should be in `"YYYY-MM-DD"` format.

- subdataset:

  character(1). subdataset names to extract. Should conform to regular
  expression. See [`base::regex`](https://rdrr.io/r/base/regex.html) for
  details. Default is `NULL`, which will result in errors. Users should
  specify which subdatasets will be imported.

- fun_agg:

  Function name or custom function to aggregate overlapping cell values.
  See `fun` description in
  [`terra::tapp`](https://rspatial.github.io/terra/reference/tapp.html)
  for details.

- path_secondary:

  character. Optional secondary list of HDF/H5 paths (e.g., Aqua files)
  to fuse with `path` for improved temporal coverage.

- fusion_method:

  character(1). Fusion method when `path_secondary` is provided:
  `"mean"`, `"primary_first"`, `"secondary_first"`.

- ...:

  For internal use.

## Value

a `SpatRaster` object

## Note

Curvilinear products (i.e., swaths) will not be accepted. MODIS products
downloaded by functions in `amadeus`,
[MODISTools](https://cran.r-project.org/package=MODISTools), and
[luna](https://github.com/rspatial/luna) are accepted.

## See also

[`download_data`](https://niehs.github.io/amadeus/reference/download_data.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
mod09ga_merge <- process_modis_merge(
  path =
    list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
  date = "2024-01-01",
  subdataset = "sur_refl_b01_1",
  fun_agg = "mean"
)
} # }
```
