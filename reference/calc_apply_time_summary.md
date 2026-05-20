# Apply default/native or explicit temporal summarization

Apply default/native or explicit temporal summarization

## Usage

``` r
calc_apply_time_summary(
  covar,
  .by_time = NULL,
  fun_summary = "mean",
  locs_id = "site_id",
  time_col = "time",
  group_cols_extra = NULL
)
```

## Arguments

- covar:

  data.frame.

- .by_time:

  NULL or character(1).

- fun_summary:

  character(1) or function.

- locs_id:

  character(1).

- time_col:

  character(1).

- group_cols_extra:

  character or NULL.

## Value

data.frame
