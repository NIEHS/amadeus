# Generate date sequence

Generate a sequence of dates from `date_start` to `date_end`.

## Usage

``` r
generate_date_sequence(date_start, date_end, sub_hyphen = TRUE)
```

## Arguments

- date_start:

  character(1). Beginning of date sequence.

- date_end:

  character(1). End of date sequence.

- sub_hyphen:

  logical(1). Substitute hyphen in dates. If `TRUE`, returns date
  sequence as "YYYYMMDD". If `FALSE`, returns date sequence as
  "YYYY-MM-DD".

## Value

vector
