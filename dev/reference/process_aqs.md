# Process U.S. EPA AQS daily CSV data

The `process_aqs()` function cleans and imports raw air quality
monitoring sites from pre-generated daily CSV files, returning a single
`SpatVector` or `sf` object. `date` is used to filter the raw data read
from csv files. Filtered rows are then processed according to `mode`
argument. Some sites report multiple measurements per day with and
without [exceptional
events](https://www.epa.gov/sites/default/files/2016-10/documents/exceptional_events.pdf)
the internal procedure of this function keeps "Included" if there are
multiple event types per site-time.

## Usage

``` r
process_aqs(
  path = NULL,
  date = c("2018-01-01", "2022-12-31"),
  mode = c("date-location", "available-data", "location"),
  data_field = "Arithmetic.Mean",
  return_format = c("terra", "sf", "data.table"),
  extent = NULL,
  ...
)
```

## Arguments

- path:

  character(1). Directory path to daily measurement data.

- date:

  character(1 or 2). Date (1) or start and end dates (2). Should be in
  `"YYYY-MM-DD"` format and sorted.

- mode:

  character(1). One of

  - "date-location" (all dates \* all locations)

  - "available-data" (date-location pairs with available data)

  - "location" (unique locations).

- data_field:

  character(1). Data field to extract.

- return_format:

  character(1). `"terra"` or `"sf"` or `"data.table"`.

- extent:

  numeric(4). Spatial extent of the resulting object. The order should
  be `c(xmin, xmax, ymin, ymax)`. The coordinate system should be WGS84
  (EPSG:4326).

- ...:

  Placeholders.

## Value

a SpatVector, sf, or data.table object depending on the `return_format`

## Note

Choose `date` and `mode` values with caution. The function may return a
massive data.table depending on the time range, resulting in a long
processing time or even a crash if data is too large for your computing
environment to process. AQS data are generally intended for use as
dependent variables, so `process_aqs()` does not have a companion route
in
[`calculate_covariates()`](https://niehs.github.io/amadeus/dev/reference/calculate_covariates.md).

## See also

- [`download_aqs()`](https://niehs.github.io/amadeus/dev/reference/download_aqs.md)

- [EPA, n.d., *AQS Parameter
  Codes*](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv)

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
aqs <- process_aqs(
  path = "./data/aqs_daily_example.csv",
  date = c("2022-12-01", "2023-01-31"),
  mode = "date-location",
  return_format = "terra"
)
} # }
```
