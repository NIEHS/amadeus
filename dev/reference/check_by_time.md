# Validate the `.by_time` temporal summarization argument

Validates the `.by_time` argument used by covariate extraction functions
for temporal summarization. When non-`NULL`, `.by_time` must be a single
character string naming a supported temporal unit token (singular or
plural): `"minute"`, `"hour"`, `"day"`, `"week"`, `"month"`,
`"quarter"`, or `"year"`.

## Usage

``` r
check_by_time(.by_time)
```

## Arguments

- .by_time:

  NULL or character(1). Temporal summarization unit. `NULL` means no
  temporal summarization.

## Value

`NULL` invisibly; stops with an informative error if the value is
invalid.

## Author

Insang Song
