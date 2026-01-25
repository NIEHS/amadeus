# Download elevation data

The `download_gmted()` function accesses and downloads Global
Multi-resolution Terrain Elevation Data (GMTED2010) from [U.S.
Geological Survey and National Geospatial-Intelligence
Agency](https://www.usgs.gov/coastal-changes-and-impacts/gmted2010).

## Usage

``` r
download_gmted(
  statistic = c("Breakline Emphasis", "Systematic Subsample", "Median Statistic",
    "Minimum Statistic", "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
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

- statistic:

  character(1). Available statistics include `"Breakline Emphasis"`,
  `"Systematic Subsample"`, `"Median Statistic"`, `"Minimum Statistic"`,
  `"Mean Statistic"`, `"Maximum Statistic"`, and
  `"Standard Deviation Statistic"`.

- resolution:

  character(1). Available resolutions include `"7.5 arc-seconds"`,
  `"15 arc-seconds"`, and `"30 arc-seconds"`.

- directory_to_save:

  character(1). Directory to save data. Two sub-directories will be
  created for the downloaded zip files ("/zip_files") and the unzipped
  data files ("/data_files").

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
  download commands. Default is FALSE.

- unzip:

  logical(1). Unzip zip files. Default is `TRUE`.

- remove_zip:

  logical(1). Remove zip file from directory_to_download. Default is
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

- Zip and/or data files will be downloaded and stored in
  `directory_to_save`.

## References

Danielson JJ, Gesch DB (2011). “Global multi-resolution terrain
elevation data 2010 (GMTED2010).” Open-File Report 2011-1073, U.S.
Geological Survey. Series: Open-File Report,
<https://doi.org/10.3133/ofr20111073>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_gmted(
  statistic = "Breakline Emphasis",
  resolution = "7.5 arc-seconds",
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = FALSE
)
} # }
```
