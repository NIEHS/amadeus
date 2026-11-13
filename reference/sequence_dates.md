# Generate an inclusive sequence of dates

Generate consecutive daily dates from a single date or two range
endpoints. Range endpoints are ordered, and requests outside the
permitted range raise an error.

## Usage

``` r
sequence_dates(date, min = "1950-01-01", max = Sys.Date(), hyphen = FALSE)
```

## Arguments

- date:

  character(1 or 2). A single date or two range endpoints in
  `"YYYY-MM-DD"` format. Endpoints may be supplied in either order.
  Dates must be between `min` and `max`, inclusive.

- min:

  Date(1) or character(1). Inclusive lower bound for date validation.
  Character values use `"YYYY-MM-DD"` format.

- max:

  Date(1) or character(1). Inclusive upper bound for date validation.
  Character values use `"YYYY-MM-DD"` format.

- hyphen:

  logical(1). Include hyphens in the returned date strings. If `FALSE`
  (the default), return `"YYYYMMDD"`; if `TRUE`, return `"YYYY-MM-DD"`.

## Value

A character vector of daily dates in ascending order, including both
endpoints. A single input date returns one date string. The output
format is controlled by `hyphen`.

## Details

Bounds are checked before generating the sequence. Any requested date
outside the permitted range causes the entire request to raise an error;
dates are not filtered or truncated. Remote file availability is not
checked.

## See also

[`base::seq.Date()`](https://rdrr.io/r/base/seq.Date.html),
[`base::as.Date()`](https://rdrr.io/r/base/as.Date.html)

## Author

Mitchell Manware

## Examples

``` r
sequence_dates("2020-05-05")
#> [1] "20200505"
sequence_dates(c("2020-05-07", "2020-05-05"))
#> [1] "20200505" "20200506" "20200507"
sequence_dates(c("2020-05-05", "2020-05-07"), hyphen = TRUE)
#> [1] "2020-05-05" "2020-05-06" "2020-05-07"
```
