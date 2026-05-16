# Download MODIS product files

Downloads MODIS data using httr2 with robust retry logic and rate
limiting. This function queries NASA's CMR API for available granules
and downloads relevant tiles based on the specified extent.

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

  character(1). MODIS product code

- version:

  character(1). Default is `"061"`, meaning v061.

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token. For
  security, recommended options (in priority order):

  - NULL (default): Reads from NASA_EARTHDATA_TOKEN environment variable

  - File path: e.g., "~/.nasa_earthdata_token"

  - Token string: Direct token (not recommended for scripts)

  Use
  [`setup_nasa_token()`](https://niehs.github.io/amadeus/dev/reference/setup_nasa_token.md)
  for interactive setup.

- date:

  character(1 or 2). Date range "YYYY-MM-DD" format

- extent:

  numeric(4). Bounding box `c(min_lon, max_lon, min_lat, max_lat)`.
  Default covers continental US: `c(-125, 22, -64, 50)`.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed with download

- download:

  logical(1). DEPRECATED. Downloads now happen automatically. Set to
  FALSE to skip downloading (generates file list only).

- remove_command:

  logical(1). Deprecated, ignored.

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum download retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

invisible list with download results; or hash character if hash=TRUE

## Note

Due to NASA data access policies, downloads require a valid NASA
Earthdata token for authentication. For security, it's recommended to
store your token in an environment variable or file rather than in your
code. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/dev/reference/setup_nasa_token.md)
for easy, secure token setup.

Both dates in `date` should be in the same year. Directory structure:
input/modis/raw/{version}/{product}/{year}/{day_of_year}.

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

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
# RECOMMENDED: Set up token once (persists across sessions)
setup_nasa_token()

# Then download without specifying token
download_modis(
  product = "MOD09GA",
  version = "061",
  date = "2024-01-01",
  extent = c(-80, 35, -75, 40),
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# ALTERNATIVE: Token from file
download_modis(
  product = "MOD09GA",
  version = "061",
  date = "2024-01-01",
  extent = c(-80, 35, -75, 40),
  nasa_earth_data_token = "~/.nasa_earthdata_token",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)

# ALTERNATIVE: Set token for current session
Sys.setenv(NASA_EARTHDATA_TOKEN = "your_token_here")
download_modis(
  product = "MOD09GA",
  date = "2024-01-01",
  acknowledgement = TRUE
)

# Date range
download_modis(
  product = "MOD09GA",
  version = "061",
  date = c("2024-01-01", "2024-01-07"),
  extent = c(-80, 35, -75, 40),
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
