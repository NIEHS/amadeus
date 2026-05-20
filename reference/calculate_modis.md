# Calculate MODIS product covariates

`calculate_modis` orchestrates daily extraction using
[`calculate_modis_daily()`](https://niehs.github.io/amadeus/reference/calculate_modis_daily.md).
In raw-path mode, files are grouped by inferred date, preprocessed for
each day, and then extracted over requested radii. With product-specific
preprocessing, files are handled according to each product's structure
and naming conventions.

## Usage

``` r
calculate_modis(
  from = NULL,
  from_secondary = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = c(0L, 1000L, 10000L, 50000L),
  preprocess = amadeus::process_modis_merge,
  name_covariates = NULL,
  subdataset = NULL,
  fun_summary = "mean",
  .by_time = NULL,
  weights = NULL,
  package_list_add = NULL,
  export_list_add = NULL,
  max_cells = 3e+07,
  geom = FALSE,
  scale = NULL,
  fusion_method = c("mean", "primary_first", "secondary_first"),
  ...
)
```

## Arguments

- from:

  character, SpatRaster, or SpatVector. Either a list of MODIS/VIIRS
  file paths (raw path mode), a preprocessed raster (direct raster
  mode), or processed MODIS fire detections as a SpatVector with `time`,
  `fire_count`, and `frp` fields.

- from_secondary:

  character or SpatRaster. Optional secondary input for fused temporal
  coverage in raster/path workflows. Type must match `from` (`character`
  with `character`, or `SpatRaster` with `SpatRaster`).

- locs:

  sf/SpatVector object. Unique locs where covariates will be calculated.

- locs_id:

  character(1). Site identifier. Default is `"site_id"`

- radius:

  numeric. Radii to calculate covariates. Default is
  `c(0, 1000, 10000, 50000)`.

- preprocess:

  function. Function to handle HDF files in raw path mode. Ignored when
  `from` is a `SpatRaster` or `SpatVector`.

- name_covariates:

  character. Name header of covariates. e.g., `"MOD_NDVIF_0_"`. The
  calculated covariate names will have a form of
  "`{name_covariates}{zero-padded buffer radius in meters}`", e.g.,
  'MOD_NDVIF_0_50000' where 50 km radius circular buffer was used to
  calculate mean NDVI value.

- subdataset:

  Indices, names, or search patterns for subdatasets. Find detail usage
  of the argument in notes.

- fun_summary:

  character or function. Function to summarize extracted raster values.

- .by_time:

  NULL or character(1). Optional time grouping key used with `.by_time`
  for temporal summaries.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- package_list_add:

  character. Reserved for backward compatibility; currently not used by
  `calculate_modis()`.

- export_list_add:

  character. Reserved for backward compatibility; currently not used by
  `calculate_modis()`.

- max_cells:

  integer(1). Maximum number of cells to be read at once. Higher values
  will expedite processing, but will increase memory usage. Maximum
  possible value is `2^31 - 1`. See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- scale:

  character(1). Scale expression to be applied to the raw values. It is
  crucial that users review the technical documentation of the MODIS
  product they are using to ensure proper scale. An example for the
  MOD11A1 product's LST_Day_1km variable (land surface temperature)
  would be `scale = "* 0.02"`. Default is `NULL`, which applies no
  scale.

- fusion_method:

  character(1). Fusion method used only when `from_secondary` is
  provided. Options are `"mean"` (pixel-wise mean with `na.rm = TRUE`),
  `"primary_first"` (use `from` first), and `"secondary_first"` (use
  `from_secondary` first).

- ...:

  Arguments passed to `preprocess`.

## Value

A data.frame or SpatVector with an attribute:

- `attr(., "dates_dropped")`: Dates with insufficient tiles. Note that
  the dates mean the dates with insufficient tiles, not the dates
  without available tiles. When `.by_time` is provided, rows are
  summarized with
  [`calc_summarize_by()`](https://niehs.github.io/amadeus/reference/calc_summarize_by.md)
  semantics.

## Note

`locs` is expected to be convertible to `sf` object. `sf`, `SpatVector`,
and other class objects that could be converted to `sf` can be used. In
raw path mode, `preprocess` is called once per inferred day using a
single-date value. Temporal aggregation across extracted rows should be
done with `.by_time`. Common arguments in `preprocess` functions such as
`date` and `path` are automatically detected and passed to the function.
Please note that `locs` here and `path` in `preprocess` functions are
assumed to have a standard naming convention of raw files from NASA. The
argument `subdataset` should be in a proper format depending on
`preprocess` function:

- [`process_modis_merge()`](https://niehs.github.io/amadeus/reference/process_modis_merge.md):
  Regular expression pattern. e.g., `"^LST_"`

- [`process_modis_swath()`](https://niehs.github.io/amadeus/reference/process_modis_swath.md):
  Subdataset names. e.g.,
  `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`

- [`process_blackmarble()`](https://niehs.github.io/amadeus/reference/process_blackmarble.md):
  Subdataset number. e.g., for VNP46A2 product, 3L.

For MOD13/MYD13 families, Terra and Aqua composites are 16-day phased
products offset by 8 days; combining both can improve effective temporal
coverage. This behavior aligns with NASA MOD13 product guidance:
<https://modis.gsfc.nasa.gov/data/dataprod/mod13.php>

For MCD19A2 MAIAC, common sub-datasets include both optical depth and
plume injection height layers. Typical selectors are
`"(Optical_Depth|Injection_Height)"` for both families or
`"(Injection_Height)"` when targeting plume height only.

For MOD14A1/MYD14A1 fire grids, the `FireMask` raw values are commonly
interpreted as:

|  |  |  |
|----|----|----|
| Raw value | Meaning | Binary fire mask? |
| 0 | not processed, missing input | NA / no observation |
| 1 | obsolete, not used since Collection 1 | NA |
| 2 | not processed, other reason | NA |
| 3 | non-fire water pixel | 0 |
| 4 | cloud, land or water | NA or 0, depending on analysis |
| 5 | non-fire land pixel | 0 |
| 6 | unknown, land or water | NA |
| 7 | fire, low confidence | 1, or exclude for stricter mask |
| 8 | fire, nominal confidence | 1 |
| 9 | fire, high confidence | 1 |

Dates with less than 80 percent of the expected number of tiles, which
are determined by the mode of the number of tiles, are removed. Users
will be informed of the dates with insufficient tiles. The result
data.frame will have an attribute with the dates with insufficient
tiles.

## See also

This function leverages the calculation of single-day MODIS covariates:

- [`calculate_modis_daily()`](https://niehs.github.io/amadeus/reference/calculate_modis_daily.md)

Also, for preprocessing, please refer to:

- [`process_modis_merge()`](https://niehs.github.io/amadeus/reference/process_modis_merge.md)

- [`process_modis_swath()`](https://niehs.github.io/amadeus/reference/process_modis_swath.md)

- [`process_blackmarble()`](https://niehs.github.io/amadeus/reference/process_blackmarble.md)

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
calculate_modis(
  from =
    list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
  locs = locs,
  locs_id = "site_id",
  radius = c(0L, 1000L),
  preprocess = process_modis_merge,
  name_covariates = "cloud_fraction_0",
  subdataset = "Cloud_Fraction",
  fun_summary = "mean"
)
} # }
```
