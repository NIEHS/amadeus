# Process IMPROVE aerosol monitoring data

The `process_improve()` function reads pipe-delimited IMPROVE
(Interagency Monitoring of Protected Visual Environments) measurement
files downloaded by
[`download_improve()`](https://niehs.github.io/amadeus/reference/download_improve.md)
and joins them with a site metadata table to attach geographic
coordinates and auxiliary site attributes. Returns a `SpatVector`, `sf`,
or `data.table` object.

## Usage

``` r
process_improve(
  path = NULL,
  product = c("raw", "rhr2", "rhr3"),
  date = NULL,
  sites_file = NULL,
  return_format = c("terra", "sf", "data.table"),
  extent = NULL,
  ...
)
```

## Arguments

- path:

  character(1). Directory containing downloaded IMPROVE `.txt` files.

- product:

  character(1). Product type: `"raw"` (default), `"rhr2"`, or `"rhr3"`.

- date:

  character(1 or 2). Date (`"YYYY-MM-DD"`) or start/end date pair to
  filter measurements. Defaults to no filtering when `NULL`.

- sites_file:

  character(1) or `NULL`. Path to a site metadata file. When `NULL`
  (default), the function first looks for a file named
  `improve_sites.txt` inside `path`, then falls back to an embedded
  IMPROVE aerosol site table included in `amadeus`.

- return_format:

  character(1). Return object type: `"terra"`, `"sf"`, or
  `"data.table"`.

- extent:

  numeric(4) or `NULL`. Optional crop extent `c(xmin, xmax, ymin, ymax)`
  in WGS84 / EPSG:4326. Applied only when `return_format` is `"terra"`
  or `"sf"`.

- ...:

  Placeholders.

## Value

a `SpatVector`, `sf`, or `data.table` object depending on
`return_format`.

## Details

Three product types are supported via `product`:

- `"raw"`:

  IMPAER speciated aerosol mass concentrations. Key columns: `SiteCode`,
  `FactDate`, `ParamCode`, `FactValue`, `Units`.

- `"rhr2"`:

  IMPRHR2 Regional Haze Rule II light extinction (`bext`, \\Mm^{-1}\\).

- `"rhr3"`:

  IMPRHR3 Regional Haze Rule III deciview index (`dv`).

Measurement values are **not** filtered by `Status`; callers may apply
their own validity flags (e.g., keep only `Status == "V0"`).

## Note

IMPROVE data are measured on an every-third-day sampling schedule. Gaps
between measurement dates are expected.

## See also

[`download_improve`](https://niehs.github.io/amadeus/reference/download_improve.md)

## Examples

``` r
improve <- process_improve(
  path = system.file("testdata/improve", package = "amadeus"),
  product = "raw",
  date = c("2022-01-01", "2022-01-31"),
  return_format = "data.table"
)
```
