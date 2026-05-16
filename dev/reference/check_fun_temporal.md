# Validate the `fun_temporal` parameter

Validates the `fun_temporal` argument used by covariate extraction
functions. When `NULL` (the default), no temporal aggregation is applied
and existing per-layer extraction behavior is preserved. When
non-`NULL`, the value must be one of `"mean"`, `"median"`, `"sum"`,
`"max"`, or `"min"`.

## Usage

``` r
check_fun_temporal(fun_temporal)
```

## Arguments

- fun_temporal:

  NULL or character(1). Name of the temporal summary function. `NULL`
  means no temporal aggregation (default / backward-compatible
  behavior).

## Value

`NULL` invisibly; stops with an informative error if the value is
invalid.

## Author

Insang Song
