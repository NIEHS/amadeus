# Process MODIS files as daily outputs

Process MODIS HDF/H5 files into day-specific rasters over a requested
date range. This helper preserves daily slices instead of flattening a
multi-day range into one merged result.

## Usage

``` r
process_modis_daily(
  path = NULL,
  date = NULL,
  subdataset = NULL,
  fun_agg = "mean",
  path_secondary = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  return_type = c("stack", "list"),
  ...
)
```

## Arguments

- path:

  character. Full list of HDF/H5 file paths.

- date:

  character(1:2). Date or date range in `"YYYY-MM-DD"` format.

- subdataset:

  character(1). Subdataset names to extract. Should conform to regular
  expression. See [`base::regex`](https://rdrr.io/r/base/regex.html) for
  details.

- fun_agg:

  Function name or custom function to aggregate overlapping cell values.
  See `fun` description in
  [`terra::tapp`](https://rspatial.github.io/terra/reference/tapp.html)
  for details.

- path_secondary:

  character. Optional secondary list of HDF/H5 paths (for example, Aqua
  files) to fuse with `path` by date.

- fusion_method:

  character(1). Fusion method when `path_secondary` is provided:
  `"mean"`, `"primary_first"`, or `"secondary_first"`.

- return_type:

  character(1). Return `"stack"` for a multi-layer `SpatRaster`
  (default) or `"list"` for a named list of daily `SpatRaster` objects.

- ...:

  Additional arguments passed to
  [`process_modis_merge`](https://niehs.github.io/amadeus/dev/reference/process_modis_merge.md).

## Value

A day-preserving MODIS result as a `SpatRaster`
(`return_type = "stack"`) or named list (`return_type = "list"`).

## See also

[`process_modis_merge`](https://niehs.github.io/amadeus/dev/reference/process_modis_merge.md),
[`download_data`](https://niehs.github.io/amadeus/dev/reference/download_data.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
mod09ga_daily <- process_modis_daily(
  path = list.files("./data", pattern = "MOD09GA.", full.names = TRUE),
  date = c("2024-01-01", "2024-01-07"),
  subdataset = "sur_refl_b01_1",
  return_type = "list"
)
} # }
```
