# Download elevation data

The `download_gmted()` function accesses and downloads Global
Multi-resolution Terrain Elevation Data (GMTED2010) from U.S. Geological
Survey.

## Usage

``` r
download_gmted(
  statistic = c("Breakline Emphasis", "Systematic Subsample", "Median Statistic",
    "Minimum Statistic", "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"),
  resolution = c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
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

- statistic:

  character(1). One of `"Breakline Emphasis"` (default),
  `"Systematic Subsample"`, `"Median Statistic"`, `"Minimum Statistic"`,
  `"Mean Statistic"`, `"Maximum Statistic"`, or
  `"Standard Deviation Statistic"`.

- resolution:

  character(1). One of `"7.5 arc-seconds"` (default),
  `"15 arc-seconds"`, or `"30 arc-seconds"`.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically. Set to FALSE
  to skip downloading and return the planned file list.

- remove_command:

  logical(1). Deprecated, ignored.

- unzip:

  logical(1). Unzip zip files (default TRUE).

- remove_zip:

  logical(1). Remove zip files after unzipping (default FALSE).

- show_progress:

  logical(1). Show per-file messages and download progress bars during
  sequential downloads. Default is `TRUE`. Ignored during concurrent
  downloads, where per-file progress is disabled. Status and summary
  messages may still be printed when `FALSE`.

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Request-throttling interval in seconds. Default is 2.
  Passed unchanged to
  [`download_run_method()`](https://niehs.github.io/amadeus/reference/download_run_method.md)
  for sequential downloads. For concurrent downloads, multiplied by the
  configured daemon count for the `"default"` compute profile before
  being passed to each worker.

## Value

When `download = TRUE` and `hash = FALSE`, an invisible list containing
`success`, `failed`, and `skipped` file counts. If all requested
archives already exist and are nonempty, no files are downloaded; the
returned list contains `success = 0`, `failed = 0`, and `skipped` equal
to the number of requested files. The list always contains `failed_urls`
and `failed_files` character vectors, both `character(0)` when no
downloads failed, including when every file was skipped.

When `download = FALSE`, an invisible list containing `urls`,
`destfiles`, and `n_files` is returned without downloading, regardless
of `hash`.

When `download = TRUE` and `hash = TRUE`, a character(1) combined MD5
hash of the files in the `data_files` directory is returned instead of a
list.

## Details

`statistic` and `resolution` each select one value. Matching is
case-sensitive, and unambiguous abbreviations are accepted. If omitted,
each argument uses its first listed choice.

## Note

GMTED data does not require authentication.

## Concurrent downloads

File downloads use configured daemons in the `"default"` compute
profile, or run sequentially when that profile has no configured
daemons. Daemon creation and shutdown are managed by the caller using
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
For concurrent downloads, configure a fixed local pool with
`mirai::daemons(n, .compute = "default")` before calling this function.
Daemon detection, counting, and task dispatch always use the `"default"`
compute profile, regardless of the active profile. Daemon counting and
rate scaling assume that the pool still contains the `n` workers
configured by that call, with no workers added separately or pool
resizing during downloads. Each worker receives one URL/destination pair
per task; the remaining request arguments are constant across tasks.

Source-specific validation, URL discovery, archive extraction, and
directory hashing run in the calling process. Only file downloads are
dispatched to workers.

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
  acknowledgement = TRUE
)
} # }
```
