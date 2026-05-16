# Reject deprecated legacy grouping argument in dots

Internal helper for calculate APIs that now support temporal
summarization via `.by_time` only. Stops immediately when a deprecated
legacy grouping argument is supplied through `...`.

## Usage

``` r
check_unsupported_by(..., .call = NULL)
```

## Arguments

- ...:

  Placeholders.

## Value

`NULL` invisibly; stops on deprecated legacy grouping input.

## Author

Insang Song
