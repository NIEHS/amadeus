# Download climate classification data

The `download_koppen_geiger()` function accesses and downloads climate
classification data from the *Present and future Köppen-Geiger climate
classification maps at 1-km resolution*([link for
article](https://www.nature.com/articles/sdata2018214); [link for
data](https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2)).

## Usage

``` r
download_koppen_geiger(
  data_resolution = c("0.0083", "0.083", "0.5"),
  time_period = c("Present", "Future"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE
)
```

## Arguments

- data_resolution:

  character(1). Available resolutions are `"0.0083"` degrees (approx. 1
  km), `"0.083"` degrees (approx. 10 km), and `"0.5"` degrees (approx.
  50 km).

- time_period:

  character(1). Available times are `"Present"` (1980-2016) and
  `"Future"` (2071-2100). ("Future" classifications are based on
  scenario RCP8.5).

- directory_to_save:

  character(1). Directory to save data. Two sub-directories will be
  created for the downloaded zip files ("/zip_files") and the unzipped
  shapefiles ("/data_files").

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- download:

  logical(1). `FALSE` will generate a \*.txt file containing all
  download commands. By setting `TRUE` the function will download all of
  the requested data files.

- remove_command:

  logical(1). Remove (`TRUE`) or keep (`FALSE`) the text file containing
  download commands.

- unzip:

  logical(1). Unzip zip files. Default is `TRUE`.

- remove_zip:

  logical(1). Remove zip files from directory_to_download. Default is
  `FALSE`.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- Zip and/or data files will be downloaded and stored in respective
  sub-directories within `directory_to_save`.

## References

Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z,
Jiang X, Van Dijk AIJM, Miralles DG (2023). “High-resolution (1 km)
Köppen-Geiger maps for 1901–2099 based on constrained CMIP6
projections.” *Scientific Data*, **10**(1), 724. ISSN 2052-4463,
[doi:10.1038/s41597-023-02549-6](https://doi.org/10.1038/s41597-023-02549-6)
, <https://www.nature.com/articles/s41597-023-02549-6>.

Beck HE, Zimmermann NE, McVicar TR, Vergopolan N, Berg A, Wood EF
(2018). “Present and future Köppen-Geiger climate classification maps at
1-km resolution.” *Scientific data*, **5**(1), 1–12.
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
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
