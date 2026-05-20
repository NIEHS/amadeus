# Summarize extracted covariates by temporal bucket

Applies a named summary function across covariate columns after
bucketing the `time` column to a coarser temporal resolution (daily by
default). When `fun_temporal` is `NULL`, the input is returned unchanged
(backward-compatible default). A WKT `"geometry"` column produced by
[`calc_prepare_locs()`](https://niehs.github.io/amadeus/reference/calc_prepare_locs.md)
is preserved by carrying forward the first observed geometry per group.

## Usage

``` r
calc_summarize_temporal(
  covar,
  fun_temporal,
  locs_id = "site_id",
  time_col = "time",
  time_bucket = "day",
  group_cols_extra = NULL
)
```

## Arguments

- covar:

  data.frame. Extracted covariate table, typically the output of
  [`calc_worker()`](https://niehs.github.io/amadeus/reference/calc_worker.md)
  or a `calculate_*()` function before the
  [`calc_return_locs()`](https://niehs.github.io/amadeus/reference/calc_return_locs.md)
  call. Must contain the columns named by `locs_id` and `time_col`.

- fun_temporal:

  NULL or character(1). Name of the summary function. One of `"mean"`,
  `"median"`, `"sum"`, `"max"`, `"min"`, or `NULL` (no aggregation;
  backward-compatible default).

- locs_id:

  character(1). Name of the location-identifier column in `covar`.
  Default `"site_id"`.

- time_col:

  character(1). Name of the time column in `covar`. Default `"time"`.

- time_bucket:

  character(1). Temporal resolution to summarise to. One of `"day"`
  (default), `"week"`, `"month"`, or `"year"`.

- group_cols_extra:

  character or NULL. Additional column names to include in the grouping
  key (e.g. `"level"` for pressure-level data). Default `NULL`.

## Value

a data.frame. When `fun_temporal` is `NULL`, `covar` is returned as-is.
Otherwise each row represents one unique group / time-bucket combination
with covariate columns aggregated by `fun_temporal`.

## Author

Insang Song
