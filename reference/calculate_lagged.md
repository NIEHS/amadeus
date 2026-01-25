# Calculate temporally lagged covariates

The `calculate_lagged()` function calculates daily temporal lagged
covariates from the output of
[`calculate_covariates()`](https://niehs.github.io/amadeus/reference/calculate_covariates.md)
or `calc_*()`.

## Usage

``` r
calculate_lagged(from, date, lag, locs_id, time_id = "time", geom = FALSE)
```

## Arguments

- from:

  data.frame(1). A `data.frame` containing calculated covariates
  returned from
  [`calculate_covariates()`](https://niehs.github.io/amadeus/reference/calculate_covariates.md)
  or `calc_*()`.

- date:

  character(2). Start and end dates of desired lagged covariates. Length
  of 10 each, format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").

- lag:

  integer(1). Number of lag days.

- locs_id:

  character(1). Name of unique identifier.

- time_id:

  character(1). Column containing time values.

- geom:

  logical(1). Should the function return a `SpatVector`? Default is
  `FALSE`. The coordinate reference system of the `SpatVector` is that
  of `from.` To return as a `SpatVector`, `from` must also be a
  `SpatVector`

## Value

a `data.frame` object

## Note

In order to calculate temporally lagged covariates, `from` must contain
at least the number of lag days before the desired start date. For
example, if `date = c("2024-01-01", "2024-01-31)` and `lag = 1`, `from`
must contain data starting at 2023-12-31. If `from` contains geometry
features, `calculate_lagged` will return a column with geometry features
of the same name. `calculate_lagged()` assumes that all columns other
than `time_id`, `locs_id`, and fixed columns of "lat" and "lon", follow
the genre, variable, lag, buffer radius format adopted in
[`calc_setcolumns()`](https://niehs.github.io/amadeus/reference/calc_setcolumns.md).

## See also

[`calculate_covariates()`](https://niehs.github.io/amadeus/reference/calculate_covariates.md)

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
loc <- data.frame(id = "001", lon = -78.90, lat = 35.97)
terracliamte_covar <- calculate_terraclimate(
  from = terraclimate, # derived from process_terraclimate() example
  locs = loc,
  locs_id = "id",
  radius = 0,
  fun = "mean",
  geom = FALSE
)
calculate_lagged(
  from = terracliamte_covar,
  locs_id = "id",
  date = c("2023-01-02", "2023-01-10"),
  lag = 1,
  time_id = "time"
)
} # }
```
