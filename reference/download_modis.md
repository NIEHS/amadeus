# Download MODIS product files using the shared download workflow

Query NASA CMR for file URLs, prepare a download table, and download
MODIS or VIIRS files using the shared validation, preview, download, and
return helpers.

## Usage

``` r
download_modis(
  product = c("MOD09GA", "MYD09GA", "MOD09GQ", "MYD09GQ", "MOD09A1", "MYD09A1",
    "MOD09Q1", "MYD09Q1", "MOD11A1", "MYD11A1", "MOD11A2", "MYD11A2", "MOD11B1",
    "MYD11B1", "MOD13A1", "MYD13A1", "MOD13A2", "MYD13A2", "MOD13Q1", "MYD13Q1",
    "MOD13A3", "MYD13A3", "MCD12Q1", "MOD14A1", "MYD14A1", "MOD14A2", "MYD14A2",
    "MOD14CM1", "MYD14CM1", "MOD16A2", "MYD16A2", "MCD64A1", "MCD64CMQ", "MOD06_L2",
    "MCD14ML", "MCD19A2", "VNP46A2", "VNP64A1"),
  version = "061",
  nasa_earth_data_token = NULL,
  date = c("2023-09-01", "2023-09-01"),
  extent = c(-125, 22, -64, 50),
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

- product:

  character(1). Supported MODIS or VIIRS product code. The default
  selects the first listed product.

- version:

  character(1). Requested CMR version, default `"061"`. Existing
  product-specific overrides are retained; see Details.

- nasa_earth_data_token:

  character(1) or NULL. NASA Earthdata token, token-file path, or `NULL`
  to use `NASA_EARTHDATA_TOKEN`. Resolved by
  [`get_token()`](https://niehs.github.io/amadeus/reference/get_token.md),
  which checks the environment variable first.

- date:

  character(1 or 2). Single date or range endpoints in `"YYYY-MM-DD"`
  format. Endpoints may be supplied in either order.

- extent:

  numeric(4). Bounding box in decimal degrees, ordered
  `c(west, south, east, north)`. Default covers the continental US.

- directory_to_save:

  character(1). Non-empty output directory. Files are saved directly in
  this directory.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  token lookup, directory creation, and discovery. Discovery still
  queries CMR in preview mode; data files are not downloaded.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns.

- show_progress:

  logical(1). Show per-file download progress. Discovery and summary
  messages may still be printed when `FALSE`.

- hash:

  logical(1). Return the combined directory MD5 hash when `TRUE`.
  Ignored when `download = FALSE`.

- max_tries:

  integer(1). Maximum attempts per CMR or file request, including the
  first attempt. Default is 20.

- rate_limit:

  numeric(1). Non-negative request-throttling interval in seconds,
  passed to discovery and downloads. Default is 2. Discovery uses a
  capacity-one bucket; 0 disables discovery throttling. File downloads
  follow the throttling policy in
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. Otherwise, with `hash = FALSE`, the
invisible download summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
With `hash = TRUE`, a visible combined MD5 hash for all files under
`directory_to_save`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

Product-specific version overrides and a CM1-to-A2 fallback are applied
before filename date filtering. Files use a flat destination directory.
MOD06_L2 uses `"6.1"`; MOD14CM1/MYD14CM1 use `"005"`; MCD64CMQ uses
`"006"`; MCD14ML uses `"6.1NRT"`; VNP46A2/VNP64A1 omit the version
filter. An empty CM1 response triggers the existing A2 version `"006"`
fallback.

Cross-year ranges remain restricted to MOD06_L2, MOD14CM1, MYD14CM1,
MCD14ML, MCD64A1, MCD64CMQ, and VNP64A1. Discovery retrieves successive
CMR pages of up to 2000 granules using the `CMR-Search-After` response
header. There is no 31-day warning or restriction. The query parameters,
retry settings, timeout, and throttle are retained across pages.

When supplied, `CMR-Hits` is checked against the number of retrieved
granule records before link or date filtering. Discovery stops if a page
request fails, the reported count changes, a required continuation value
is missing or repeated, or the final record count does not match
`CMR-Hits`.

CMR supplies actual file URLs, so a product/date Cartesian grid is not
needed. The inclusive date sequence is used to establish ordered query
endpoints. A file count is reported without claiming daily or monthly
completeness for tiled, swath, or composite products.

Discovery keeps the existing 120-second per-attempt timeout; file
downloads use
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)'s
default timeout.

## References

Lyapustin A, Wang Y (2022). “MODIS/Terra+Aqua Land Aerosol Optical Depth
Daily L2G Global 1km SIN Grid V061.”
[doi:10.5067/MODIS/MCD19A2.061](https://doi.org/10.5067/MODIS/MCD19A2.061)
. <https://www.earthdata.nasa.gov/data/catalog/lpcloud-mcd19a2-061>.
MODIS Atmosphere Science Team (2017). “MODIS/Terra Clouds 5-Min L2 Swath
1km and 5km.”
[doi:10.5067/MODIS/MOD06_L2.061](https://doi.org/10.5067/MODIS/MOD06_L2.061)
.
<https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MOD06_L2>.
Vermote E, Wolfe R (2021). “MODIS/Terra Surface Reflectance Daily L2G
Global 1km and 500m SIN Grid V061.”
[doi:10.5067/MODIS/MOD09GA.061](https://doi.org/10.5067/MODIS/MOD09GA.061)
. <https://www.earthdata.nasa.gov/data/catalog/lpcloud-mod09ga-061>. Wan
Z, Hook S, Hulley G (2021). “MODIS/Terra Land Surface
Temperature/Emissivity Daily L3 Global 1km SIN Grid V061.”
[doi:10.5067/MODIS/MOD11A1.061](https://doi.org/10.5067/MODIS/MOD11A1.061)
. <https://www.earthdata.nasa.gov/data/catalog/lpcloud-mod11a1-061>.
Didan K (2021). “MODIS/Terra Vegetation Indices 16-Day L3 Global 1km SIN
Grid V061.”
[doi:10.5067/MODIS/MOD13A2.061](https://doi.org/10.5067/MODIS/MOD13A2.061)
. <https://www.earthdata.nasa.gov/data/catalog/lpcloud-mod13a2-061>.
Román MO, Wang Z, Sun Q, Kalb V, Miller SD, Molthan A, Schultz L, Bell
J, Stokes EC, Pandey B, Seto KC, Hall D, Oda T, Wolfe RE, Lin G,
Golpayegani N, Devadiga S, Davidson C, Sarkar S, Praderas C, Schmaltz J,
Boller R, Stevens J, Ramos González OM, Padilla E, Alonso J, Detrés Y,
Armstrong R, Miranda I, Conte Y, Marrero N, MacManus K, Esch T, Masuoka
EJ (2018). “NASA's Black Marble nighttime lights product suite.” *Remote
Sensing of Environment*, **210**, 113–143. ISSN 00344257.
[doi:10.1016/j.rse.2018.03.017](https://doi.org/10.1016/j.rse.2018.03.017)
. <https://linkinghub.elsevier.com/retrieve/pii/S003442571830110X>.

## See also

[`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md),
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_modis(
  product = "MOD09GA",
  date = c("2024-01-01", "2024-01-03"),
  extent = c(-80, 35, -75, 40),
  directory_to_save = file.path(tempdir(), "modis"),
  acknowledgement = TRUE,
  download = FALSE
)
} # }
```
