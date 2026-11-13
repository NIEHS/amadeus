# Download National Hydrography Dataset Plus (NHDPlus) data

Download a national NHDPlus Version 2.1 `.7z` archive from Amazon S3.
The selected region and data type determine the archive to download.

## Usage

``` r
download_huc(
  region = c("Lower48", "Islands"),
  type = c("Seamless", "OceanCatchment"),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = FALSE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- region:

  character(1). Archive region: `"Lower48"` (default) or `"Islands"`.

- type:

  character(1). Data type: `"Seamless"` (default) or `"OceanCatchment"`.

- directory_to_save:

  character(1). Non-empty parent directory. The `.7z` archive is saved
  in `zip_files/`; when `unzip = TRUE`, its contents are extracted into
  `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. Downloaded archives and
  extracted data can require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` emits a warning and returns a preview
  of the planned archive without downloading or extracting it. Default
  is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` emits a warning. No command
  file is generated. Default is `FALSE`.

- unzip:

  logical(1). Extract the downloaded `.7z` archive into `data_files/`
  using the archive package. Default is `FALSE`, which retains the
  archive without extracting its contents.

- remove_zip:

  logical(1). Remove the downloaded `.7z` archive and its `zip_files/`
  directory, including remaining contents, after extraction. Default is
  `FALSE`. May be `TRUE` only when `unzip = TRUE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `data_files/` instead of the download summary when `download = TRUE`.
  Ignored when `download = FALSE`. Default is `FALSE`.

- show_progress:

  logical(1). Show file download progress. Default is `TRUE`. Other
  status messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Request-throttling interval in seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Controls the average request rate, rather than guaranteeing a minimum
  gap between requests. Default is 2.

## Value

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files = 1` is returned regardless of `hash`.

When `download = TRUE` and `hash = FALSE`, returns the download summary
from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
invisibly, including `success`, `failed`, and `skipped` file counts. The
summary always contains `failed_urls` and `failed_files` character
vectors, both `character(0)` when no downloads failed, including when
every file was skipped. Counts describe archive downloads, not extracted
files.

When `download = TRUE` and `hash = TRUE`, returns a visible character
scalar containing the combined MD5 hash of the files under
`data_files/`.

## Details

`region` and `type` each select one value. Matching is case-sensitive,
and unambiguous abbreviations are accepted. If omitted, each argument
uses its first listed choice.

`zip_files/` and `data_files/` are sibling directories created beneath
`directory_to_save`, including when `unzip = FALSE` or
`download = FALSE`. Preview mode returns the planned archive even if it
already exists locally and does not check remote availability.

During downloading,
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips an existing non-empty archive in `zip_files/`. That archive is
still passed to extraction when `unzip = TRUE`. Existing extracted data
alone do not cause a download to be skipped. Archive member names and
paths are preserved beneath `data_files/`; pass the extracted
geodatabase path to
[`process_huc()`](https://niehs.github.io/amadeus/reference/process_huc.md).

Hashing includes all files recursively under `data_files/`, including
existing files, and excludes archives in `zip_files/`. With
`unzip = FALSE`, hashing still uses `data_files/`, which may be empty or
contain previously extracted data.

## Note

For HUC, set `type = "Seamless"`. HUC12 layer presents in the seamless
geodatabase. Users can aggregate HUC12 layer to make HUC6, HUC8, HUC10,
etc. For whom wants to download a specific region, please visit [Get
NHDPlus
Data](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#ListofAreas)

## References

U.S. Geological Survey (2023). “National Hydrography Dataset (NHD) –
USGS National Map Downloadable Data Collection.”
<https://www.usgs.gov/national-hydrography>.

## See also

[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
[`process_huc()`](https://niehs.github.io/amadeus/reference/process_huc.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_huc(
  region = "Lower48",
  type = "Seamless",
  directory_to_save = file.path(tempdir(), "nhdplus"),
  acknowledgement = TRUE,
  unzip = TRUE
)
} # }
```
