# Download climate classification data

The `download_koppen_geiger()` function accesses and downloads climate
classification data.

## Usage

``` r
download_koppen_geiger(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- data_resolution:

  character(1). Available resolutions.

- time_period:

  character(1). "Present" (1980-2016) or "Future" (2071-2100).

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip zip files (default TRUE).

- remove_zip:

  logical(1). Remove zip files after unzipping (default FALSE).

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

Köppen-Geiger data does not require authentication.

## References

Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z,
Jiang X, Van Dijk AIJM, Miralles DG (2023). “High-resolution (1 km)
Köppen-Geiger maps for 1901–2099 based on constrained CMIP6
projections.” *Scientific Data*, **10**(1), 724. ISSN 2052-4463.
[doi:10.1038/s41597-023-02549-6](https://doi.org/10.1038/s41597-023-02549-6)
. <https://www.nature.com/articles/s41597-023-02549-6>. Beck HE,
Zimmermann NE, McVicar TR, Vergopolan N, Berg A, Wood EF (2018).
“Present and future Köppen-Geiger climate classification maps at 1-km
resolution.” *Scientific data*, **5**(1), 1–12.
[doi:10.1038/sdata.2018.214](https://doi.org/10.1038/sdata.2018.214) .

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_koppen_geiger(
  data_resolution = "0.0083",
  time_period = "Present",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
