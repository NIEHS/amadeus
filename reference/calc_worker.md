# Perform covariate extraction

Extract covariate values from `SpatRaster` object passed from
`process_*()`.

## Usage

``` r
calc_worker(
  dataset,
  from,
  locs_vector,
  locs_df,
  fun,
  variable = 1,
  time,
  time_type = c("date", "hour", "year", "yearmonth", "timeless"),
  radius,
  level = NULL,
  max_cells = 1e+08,
  ...
)
```

## Arguments

- dataset:

  character(1). Dataset name.

- from:

  SpatRaster(1). Cleaned `SpatRaster` object.

- locs_vector:

  SpatVector(1). Cleaned `SpatVector` object passed from
  [`calc_prepare_locs()`](https://niehs.github.io/amadeus/reference/calc_prepare_locs.md).
  Contains location point/polygon values.

- locs_df:

  data.frame(1). Cleaned `data.frame` object passed from
  [`calc_prepare_locs()`](https://niehs.github.io/amadeus/reference/calc_prepare_locs.md).
  Contains location identifiers.

- fun:

  character(1). Summary function. Passed to
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html).

- variable:

  integer. Position within the layer name containing the variable
  name/code.

- time:

  integer. Position within the layer name containing the time value(s).

- time_type:

  character(1). Type of time observation. One of "date", "hour", "year",
  "yearmonth", "timeless".

- radius:

  integer(1). Buffer distance (m). Passed from
  [`calc_prepare_locs()`](https://niehs.github.io/amadeus/reference/calc_prepare_locs.md).
  Used in column naming.

- level:

  integer. Position within the layer name containing the vertical
  pressure level value (if applicable). Default = `NULL`.

- max_cells:

  integer(1). Maximum number of cells to be read at once. Higher values
  will expedite processing, but will increase memory usage. Maximum
  possible value is `2^31 - 1`. See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- ...:

  Placeholders.

## Value

a `data.frame` object
