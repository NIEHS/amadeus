# Download IMPROVE aerosol monitoring data

The `download_improve()` function accesses and downloads IMPROVE
(Interagency Monitoring of Protected Visual Environments) data files
from the VIEWS/VIBE data export service hosted at CIRA/CSU. Annual files
are downloaded as `.txt.zip` archives and extracted to pipe-delimited
`.txt` files containing aerosol measurements at federal-land monitoring
stations.

## Usage

``` r
download_improve(
  year = c(2018, 2022),
  product = c("raw", "rhr2", "rhr3"),
  url_improve = "https://vibe.cira.colostate.edu/data/export/",
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- year:

  integer(1 or 2). Year or start/end years.

- product:

  character(1). Product selector: `"raw"` (aerosol, default), `"rhr2"`
  (Regional Haze Rule II), or `"rhr3"` (Regional Haze Rule III).

- url_improve:

  character(1). Base URL to the IMPROVE data export service.

- directory_to_save:

  character(1). Directory to save downloaded files.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- show_progress:

  logical(1). Show download progress (default `TRUE`).

- hash:

  logical(1). Return hash of downloaded files (default `FALSE`).

- max_tries:

  integer(1). Maximum retry attempts (default 20).

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2).

## Value

invisible list with download results; or hash character if
`hash = TRUE`.

## Note

- IMPROVE data does not require authentication.

- Three product types are available: `"raw"` (IMPAER — speciated aerosol
  mass concentrations), `"rhr2"` (IMPRHR2 — Regional Haze Rule II light
  extinction), `"rhr3"` (IMPRHR3 — Regional Haze Rule III deciview
  index).

- Site metadata is handled by
  [`process_improve`](https://niehs.github.io/amadeus/reference/process_improve.md)
  using an embedded table; annual downloads include measurement files
  only.

- IMPROVE monitors ~\\1 \mu g / m^3\\ precision instruments deployed at
  Class I and other federal land areas.

## See also

[`process_improve`](https://niehs.github.io/amadeus/reference/process_improve.md)

## Author

Insang Song, Mitchell Manware

## Examples

``` r
if (FALSE) { # \dontrun{
download_improve(
  year = 2022,
  product = "raw",
  directory_to_save = "./data/improve/",
  acknowledgement = TRUE
)
} # }
```
