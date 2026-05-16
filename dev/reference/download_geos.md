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

  character(1 or 2). Date range "YYYY-MM-DD" format

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

invisible list with download results; or hash character if hash=TRUE

## Note

Due to NASA data access policies, downloads require a valid NASA
Earthdata token for authentication. Use
[`setup_nasa_token()`](https://niehs.github.io/amadeus/dev/reference/setup_nasa_token.md)
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
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
