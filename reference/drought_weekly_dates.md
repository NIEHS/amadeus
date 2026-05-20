# Generate weekly Tuesday dates for drought products

Return a character vector of YYYYMMDD strings for each Tuesday falling
within `[date_start, date_end]`. EDDI and USDM are both released on
Tuesdays; this helper centralises the logic.

## Usage

``` r
drought_weekly_dates(date_start, date_end)
```

## Arguments

- date_start:

  character(1). Start date, `"YYYY-MM-DD"`.

- date_end:

  character(1). End date, `"YYYY-MM-DD"`.

## Value

character vector of `"YYYYMMDD"` strings (may be length 0).
