# Check date format

Check date input strings conform to the required format.

## Usage

``` r
is_date_proper(instr = NULL, format = "%Y-%m-%d")
```

## Arguments

- instr:

  character(1). String to check.

- format:

  character(1). Matching format to be checked. Default is `"%Y-%m-%d"`,
  which can detect `"%Y/%m/%d`. See
  [`strftime`](https://rdrr.io/r/base/strptime.html) for details of
  formatting this string.

## Value

No returning value. It stops the function if `instr` doesn't conform to
the `format`.

## Author

Insang Song
