# Summarize extracted covariates by `.by_time` temporal unit

Generic temporal summarizer for covariate tables. When `.by_time` is
`NULL`, the input is returned unchanged. Otherwise, numeric covariates
are summarized by `locs_id + bucketed time + group_cols_extra`.

## Usage

``` r
calc_summarize_by(
  covar,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  .by_time = NULL,
  group_cols_extra = NULL,
  ...
)
```

## Arguments

- covar:

  data.frame. Extracted covariates.

- fun_summary:

  character(1) or function. Summary function (e.g., `"mean"`, `"sum"`).

- locs_id:

  character(1). Location-id column.

- time_col:

  character(1). Time column in `covar`.

- .by_time:

  NULL or character(1). Temporal unit token.

- group_cols_extra:

  character or NULL. Extra grouping columns.

- ...:

  Placeholders.

## Value

a data.frame.

## Author

Insang Song
