# Summarize extracted covariates at native temporal grain

Internal helper that summarizes numeric covariates by
`locs_id + time + group_cols_extra` while preserving the original time
representation.

## Usage

``` r
calc_summarize_native_time(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
)
```

## Arguments

- covar:

  data.frame. Extracted covariates.

- fun_summary:

  character(1) or function. Summary function.

- locs_id:

  character(1). Location-id column.

- time_col:

  character(1). Time column in `covar`.

- group_cols_extra:

  character or NULL. Extra grouping columns.

## Value

a data.frame.
