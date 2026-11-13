# Download atmospheric composition data

The `download_geos()` function accesses and downloads various
atmospheric composition collections from NASA's Global Earth Observing
System (GEOS) compositional forecast model.

## Usage

``` r
download_geos(
  collection = c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1",
    "met_tavg_1hr_g1440x721_x1", "xgc_tavg_1hr_g1440x721_x1",
    "chm_inst_1hr_g1440x721_p23", "met_inst_1hr_g1440x721_p23"),
  nasa_earth_data_token = NULL,
  date = c("2018-01-01", "2018-01-01"),
  hours = 0:23,
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

- collection:

  character(1). GEOS-CF data collection file name.

- nasa_earth_data_token:

  character(1) or NULL. NASA EarthData authentication token.

- date:

  character(1 or 2). A single date or inclusive range endpoints in
  `"YYYY-MM-DD"` format, in either order.

- hours:

  integer(1 to 24). Hours of day to download for each date, in UTC.
  Supply values from 0 through 23. Default is `0:23`, which selects all
  hours. The collection determines the minute within each selected hour.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- show_progress:

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. With `download = TRUE` and `hash = FALSE`,
an invisible summary from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
containing `success`, `failed`, `skipped`, `failed_urls`, and
`failed_files`. The two failure vectors are `character(0)` when no
downloads failed, including when every file was skipped. With
`download = TRUE` and `hash = TRUE`, a visible character scalar
containing the combined MD5 hash of the output directory.

## Details

Hour selection uses the timestamps of the requested collection. Hourly
time-averaged collections (`_tavg_1hr_`) use timestamps at 30 minutes
past the hour, while hourly instantaneous collections (`_inst_1hr_`) use
timestamps on the hour. For example, `hours = c(0, 6)` selects `00:30`
and `06:30` UTC for time-averaged collections, or `00:00` and `06:00`
UTC for instantaneous collections. The same hour selection applies to
every date in the requested range.

## Note

Due to NASA data access policies, downloads require a valid NASA
Earthdata token for authentication. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/reference/setup_nasa_token.md)
for setup.

## References

Keller CA, Knowland KE, Duncan BN, Liu J, Anderson DC, Das S, Lucchesi
RA, Lundgren EW, Nicely JM, Nielsen E, Ott LE, Saunders E, Strode SA,
Wales PA, Jacob DJ, Pawson S (2021). “Description of the NASA GEOS
Composition Forecast Modeling System GEOS‐CF v1.0.” *Journal of Advances
in Modeling Earth Systems*, **13**(4), e2020MS002413. ISSN 1942-2466,
1942-2466.
[doi:10.1029/2020MS002413](https://doi.org/10.1029/2020MS002413) .

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_geos(
  collection = "aqc_tavg_1hr_g1440x721_v1",
  date = "2024-01-01",
  hours = c(0, 6, 12, 18),
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
