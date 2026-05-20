# A single-date MODIS worker

The function operates at MODIS/VIIRS products on a daily basis. Given
that the raw hdf files are downloaded from NASA, standard file names
include a data retrieval date flag starting with letter "A". Leveraging
that piece of information, the function will select files of scope on
the date of interest. Please note that this function does not provide a
function to filter swaths or tiles, so it is strongly recommended to
check and pre-filter the file names at users' discretion.

## Usage

``` r
calculate_modis_daily(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = 0L,
  date = NULL,
  name_extracted = NULL,
  fun_summary = "mean",
  weights = NULL,
  max_cells = 3e+07,
  geom = FALSE,
  scale = NULL,
  ...
)
```

## Arguments

- from:

  SpatRaster. Preprocessed objects.

- locs:

  SpatVector/sf/sftime object. Locations where MODIS values are
  summarized.

- locs_id:

  character(1). Field name where unique site identifiers are stored.
  Default is `"site_id"`

- radius:

  numeric. Radius to generate circular buffers.

- date:

  Date(1). date to query.

- name_extracted:

  character. Names of calculated covariates.

- fun_summary:

  function. Summary function for multilayer rasters. Passed to `foo`.
  See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- weights:

  `NULL`, `SpatRaster`, polygon `SpatVector`/`sf`, or file path.
  Optional weights raster for weighted extraction. If `NULL` (default),
  unweighted extraction is performed.

- max_cells:

  integer(1). Maximum number of cells to be read at once. Higher values
  will expedite processing, but will increase memory usage. Maximum
  possible value is `2^31 - 1`.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.` See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- scale:

  character(1). Scale expression to be applied to the raw values. It is
  crucial that users review the technical documentation of the MODIS
  product they are using to ensure proper scale. An example for the
  MOD11A1 product's LST_Day_1km variable (land surface temperature)
  would be `scale = "* 0.02"`. Default is `NULL`, which applies no
  scale.

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object.

## See also

- Preprocessing:
  [`process_modis_merge()`](https://niehs.github.io/amadeus/reference/process_modis_merge.md),
  [`process_modis_swath()`](https://niehs.github.io/amadeus/reference/process_modis_swath.md),
  [`process_blackmarble()`](https://niehs.github.io/amadeus/reference/process_blackmarble.md)

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
calculate_modis_daily(
  from = mod06l2_warp, # dervied from process_modis() example
  locs = locs,
  locs_id = "id",
  radius = 0,
  date = "2024-01-01",
  name_extracted = "cloud_fraction_0",
  fun_summary = "mean",
  max_cells = 3e7
)
} # }
```
