# Download PRISM data

Download one time period and climate element from the PRISM Climate
Group Web Service. ZIP archives and extracted data are stored in
separate subdirectories using the shared download workflow.

## Usage

``` r
download_prism(
  time,
  element = c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax", "solslope",
    "soltotal", "solclear", "soltrans"),
  data_type = c("ts", "normals_800", "normals"),
  format = c("nc", "asc", "grib2"),
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

- time:

  character(1). Single time-period code, retaining leading zeros. For
  `data_type = "ts"`, use `YYYYMMDD` for a daily grid (from 1981),
  `YYYYMM` for a monthly grid (from 1895), or `YYYY` for an annual grid
  (from 1895). Each time-series request selects one grid. The legacy
  normals options use `MM` for a monthly normal, `"14"` for an annual
  normal, or `MMDD` for a daily normal; see Details for service
  availability. Local validation requires a character scalar with 2, 4,
  6, or 8 characters; it does not validate calendar dates or
  availability.

- element:

  character(1). Climate element: `"ppt"` (default), `"tmin"`, `"tmax"`,
  `"tmean"`, `"tdmean"`, `"vpdmin"`, or `"vpdmax"`. The legacy normals
  options also accept `"solslope"`, `"soltotal"`, `"solclear"`, and
  `"soltrans"`; these are rejected for `"ts"`.

- data_type:

  character(1). Selected data type: `"ts"` (default) for 4 km time
  series, `"normals_800"` for the legacy 800 m normals URL template, or
  `"normals"` for the legacy 4 km normals URL template. See Details for
  service availability.

- format:

  character(1). Requested time-series raster format: `"nc"` (default),
  `"asc"`, or `"grib2"`. The argument is validated but omitted from
  normals requests, with a message. See Details for provider support.

- directory_to_save:

  character(1). Non-empty parent directory. Archives are downloaded into
  `zip_files/` and, when `unzip = TRUE`, extracted into `data_files/`.

- acknowledgement:

  logical(1). Must be `TRUE` to proceed. Downloaded archives and
  extracted rasters can require substantial storage.

- download:

  logical(1). Deprecated. `FALSE` warns and returns a preview after
  directory creation, without contacting the service, downloading, or
  validating archives. Default is `TRUE`.

- remove_command:

  logical(1). Deprecated and ignored; `TRUE` warns. No command file is
  generated. Default is `FALSE`.

- unzip:

  logical(1). Extract the validated ZIP archive into `data_files/`.
  Default is `TRUE`. `FALSE` retains the archive without extracting it;
  ZIP validation still runs when `download = TRUE`.

- remove_zip:

  logical(1). Remove the downloaded archive and its `zip_files/`
  directory, including remaining contents, after extraction. Default is
  `FALSE`. May be `TRUE` only when `unzip = TRUE`. No removal occurs
  when `download = FALSE`.

- hash:

  logical(1). Return a combined MD5 hash of the files under
  `data_files/` instead of the download summary. Default is `FALSE`.
  Ignored when `download = FALSE`.

- show_progress:

  logical(1). Show file download progress. Default is `TRUE`. Other
  status messages may still be printed when `FALSE`.

- max_tries:

  integer(1). Maximum attempts per file download request, including the
  initial attempt. Default is 20.

- rate_limit:

  numeric(1). Non-negative throttling interval in seconds, passed to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
  Default is 2.

## Value

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. Otherwise, after downloading, validation,
and optional extraction, `hash = FALSE` returns the download summary
from
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
invisibly. With `hash = TRUE`, returns a visible character scalar
containing the combined MD5 hash of `data_files/`.

The download summary always contains `success`, `failed`, `skipped`,
`failed_urls`, and `failed_files`. The two failure vectors are
`character(0)` when no files failed, including when every file was
skipped.

## Details

`zip_files/` and `data_files/` are sibling directories created beneath
`directory_to_save`, including for previews and when `unzip = FALSE`.
The local archive name is constructed from the requested element, data
type, time, and, for time series, format. Names and paths inside the
archive are preserved during extraction.

After downloading, the destination archive is checked by listing its
contents with [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html).
This check also applies to existing non-empty archives skipped by
[`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md).
An unreadable ZIP raises an error, with a preview of the response text
when available. Listing an archive does not fully verify its members or
raster contents.

Hashing covers all files under `data_files/`, including existing files,
and excludes archives in `zip_files/`. With `unzip = FALSE`, hashing
still uses `data_files/`, which may be empty or contain previously
extracted data.

The PRISM Web Service Guide dated 26 March 2025 documents time-series
ZIP downloads, with daily data from 1981 and monthly and annual data
from 1895. It states that normals are unavailable through this service
and lists `nc`, `asc`, and `bil` format conversions, rather than
`grib2`. The normals and `grib2` options remain accepted by this
function but are not documented as supported by the current service. See
the guide in References.

## References

Daly C, Taylor GH, Gibson WP, Parzybok TW, Johnson GL, Pasteris PA
(2000). “HIGH-QUALITY SPATIAL CLIMATE DATA SETS FOR THE UNITED STATES
AND BEYOND.” *Transactions of the ASAE*, **43**(6), 1957–1962. ISSN
2151-0059. [doi:10.13031/2013.3101](https://doi.org/10.13031/2013.3101)
.
<http://elibrary.asabe.org/abstract.asp??JID=3&AID=3101&CID=t2000&v=43&i=6&T=1>.

- [PRISM Climate Group](https://prism.oregonstate.edu/)

- [PRISM Web Service
  Guide](https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf)

## See also

[`download_setup_dir()`](https://niehs.github.io/amadeus/reference/download_setup_dir.md),
[`download_unzip()`](https://niehs.github.io/amadeus/reference/download_unzip.md),
[`process_prism()`](https://niehs.github.io/amadeus/reference/process_prism.md)

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_prism(
  time = "202104",
  element = "ppt",
  data_type = "ts",
  format = "nc",
  directory_to_save = file.path(tempdir(), "prism"),
  acknowledgement = TRUE,
  download = FALSE # Preview the URL and zip_files/ destination.
)
} # }
```
