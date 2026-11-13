# Download EDGAR emissions data

Construct EDGAR v8.1 air-pollutant or VOC-speciation archive URLs and
download them using the shared validation, preview, and download
helpers.

## Usage

``` r
download_edgar(
  species = c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM2.5", "SO2"),
  version = "8.1",
  temp_res = NULL,
  sector_yearly = NULL,
  sector_monthly = NULL,
  sector_voc = NULL,
  format = "nc",
  output = "emi",
  year_range = NULL,
  voc = NULL,
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE,
  show_progress = TRUE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- species:

  character vector. Air pollutants to download for version `"8.1"`:
  `"BC"`, `"CO"`, `"NH3"`, `"NMVOC"`, `"NOx"`, `"OC"`, `"PM10"`,
  `"PM2.5"`, or `"SO2"`. Defaults to all listed species. Input is
  case-insensitive; `"PM25"` is also accepted. Ignored for VOC
  speciation, which uses `voc` instead.

- version:

  character(1). `"8.1"` (default) for air pollutants or `"8.1_voc"` for
  VOC speciation.

- temp_res:

  character(1) or NULL. Required for version `"8.1"`: `"yearly"`,
  `"monthly"`, or `"timeseries"`. Timeseries archives can contain
  tabular data; see Details for processing limitations. For version
  `"8.1_voc"`, this does not affect URL selection; if supplied, it is
  retained in the local archive-name prefix instead of `"voc"`.

- sector_yearly:

  character vector or NULL. Yearly sectors: `"AGS"`, `"AWB"`, `"CHE"`,
  `"ENE"`, `"IND"`, `"MNM"`, `"NMM"`, `"PRU_SOL"`, `"RCO"`, `"REF_TRF"`,
  `"SWD_INC"`, `"SWD_LDF"`, `"TNR_Aviation_CDS"`, `"TNR_Aviation_CRS"`,
  `"TNR_Aviation_LTO"`, `"TNR_Aviation_SPS"`, `"TNR_Other"`,
  `"TNR_Ship"`, `"TRO"`, `"WWT"`, or `"TOTALS"`. `NULL` selects totals.
  Used only for yearly version `"8.1"` data.

- sector_monthly:

  character vector or NULL. Monthly sectors: `"AGRICULTURE"`,
  `"BUILDINGS"`, `"FUEL_EXPLOITATION"`, `"IND_COMBUSTION"`,
  `"IND_PROCESSES"`, `"POWER_INDUSTRY"`, `"TRANSPORT"`, or `"WASTE"`.
  `NULL` selects full-species archives. Used only for monthly version
  `"8.1"` data.

- sector_voc:

  character vector or NULL. VOC sectors, using the same choices as
  `sector_monthly`. `NULL` selects full VOC-group archives. Used only
  for version `"8.1_voc"`.

- format:

  character(1). `"nc"` (default) or `"txt"`. Flux output and monthly
  version `"8.1"` data require `"nc"`. Selects an archive template where
  supported; it does not convert the downloaded files.

- output:

  character(1). `"emi"` (default) for emissions or `"flx"` for fluxes.
  Full-species monthly, timeseries, and full VOC-group archives use
  fixed provider filenames that do not vary with `output` or `format`.

- year_range:

  integer(1 or more) or NULL. One year, two endpoints, or a non-empty
  vector of years. Every year from the minimum through the maximum is
  selected, including gaps: `c(2018, 2023)` and `c(2018, 2020, 2023)`
  both select `2018:2023`. Years must be non-missing integer values. For
  yearly version `"8.1"` data or sector-specific VOC data, `NULL`
  (default) selects bundled archives instead of individual yearly
  archives. Monthly, timeseries, and full VOC-group archives always
  contain the provider's bundled years; `year_range` is validated when
  supplied but does not change their URLs or subset their contents.

- voc:

  integer vector, character vector, or NULL. VOC group numbers from 1 to
  25, required for version `"8.1_voc"`. Numeric strings such as `"1"`
  are accepted. Ignored for version `"8.1"`.

- directory_to_save:

  character(1). Non-empty parent directory. Archives are saved in
  `zip_files/` and extracted into `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. Archives and extracted data can
  require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without making network requests. Default is
  `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. Default is `FALSE`.

- unzip:

  logical(1). Extract downloaded or existing archives into
  `data_files/`. Default is `TRUE`. When `FALSE`, archives remain in
  `zip_files/` without extraction.

- remove_zip:

  logical(1). Remove archives and their parent `zip_files/` directory
  recursively after the extraction loop finishes, including any other
  files in that directory. Requires `unzip = TRUE`. Default is `FALSE`.

- hash:

  logical(1). Return the combined MD5 hash of all files recursively
  under `data_files/`, including previously extracted files. With
  `unzip = FALSE`, this still hashes `data_files/`, not the archives.
  Default is `FALSE`; ignored when `download = FALSE`.

- show_progress:

  logical(1). Show per-file download progress. Default is `TRUE`;
  summary messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file request, including the initial
  attempt. Must be at least 1. Default is 20.

- rate_limit:

  numeric(1). Non-negative request-throttling interval in seconds,
  passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Default is 2.

## Value

- With `download = FALSE`, an invisible list containing `urls`,
  `destfiles`, and `n_files`, the number of planned archive downloads.

- With `download = TRUE` and `hash = FALSE`, the invisible summary from
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  with `success`, `failed`, and `skipped` counts. It always contains
  `failed_urls` and `failed_files` character vectors, both
  `character(0)` when no downloads failed, including when every file was
  skipped. These counts describe archives, not extracted files.

- With `download = TRUE` and `hash = TRUE`, a visible character(1)
  combined MD5 hash of all files under `data_files/`, including existing
  files.

## Details

Scalar selectors `version`, `format`, and `output` use
[`base::match.arg()`](https://rdrr.io/r/base/match.arg.html), allowing
unambiguous partial matches. This also applies to `temp_res` for version
`"8.1"`; for VOC speciation, a supplied `temp_res` is used as-is in the
local archive-name prefix. Sector names require exact, case-sensitive
matches. Only the sector argument for the selected version and temporal
resolution is used; timeseries downloads use no sector argument.

URL construction retains the v8.1 FT2022 archive templates, including
the PM2.5 folder/filename conventions and the SO2 `_v2` suffix on full
monthly and timeseries archives. Monthly full-species filenames cover
2000–2022; timeseries and full VOC-group filenames cover 1970–2022.
Available remote files are not discovered by querying directory
listings. Year validation checks the input range, not whether EDGAR
provides an archive for each requested year or combination of arguments.

Timeseries archives are not necessarily raster products. For example,
the NOx timeseries archive contains an Excel workbook (`.xlsx`) and a
readme, even when `format = "nc"`.
[`process_edgar()`](https://niehs.github.io/amadeus/reference/process_edgar.md)
currently accepts raster files only, so this workbook cannot be passed
through the
[`process_edgar()`](https://niehs.github.io/amadeus/reference/process_edgar.md)
and
[`calculate_edgar()`](https://niehs.github.io/amadeus/reference/calculate_edgar.md)
workflow.

Local archive names retain the `edgar_<temp_res>_` prefix, or
`edgar_voc_` when VOC speciation is requested with `temp_res = NULL`.
Preview results include all constructed URLs and destination paths,
including existing files. A preview creates the output directories but
makes no network requests and does not verify remote availability. No
shell-command file is created.

[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
skips existing non-empty destination files and records individual
download failures in its summary. File downloads use the shared default
timeout. Extraction and cleanup occur before returning, and extraction
errors can prevent a normal return. Summary counts describe downloads,
not extraction.

## See also

[`download_data()`](https://niehs.github.io/amadeus/reference/download_data.md),
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md),
[`process_edgar()`](https://niehs.github.io/amadeus/reference/process_edgar.md)

## Author

Mariana Alifa Kassien

## Examples

``` r
if (FALSE) { # \dontrun{
download_edgar(
  species = "CO",
  temp_res = "yearly",
  sector_yearly = "ENE",
  year_range = c(2021, 2022),
  directory_to_save = file.path(tempdir(), "edgar"),
  acknowledgement = TRUE,
  download = FALSE
)
download_edgar(
  version = "8.1_voc",
  voc = 1:3,
  sector_voc = "AGRICULTURE",
  year_range = 2021,
  directory_to_save = file.path(tempdir(), "edgar_voc"),
  acknowledgement = TRUE,
  download = FALSE
)
} # }
```
