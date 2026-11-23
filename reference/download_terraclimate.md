# Download TerraClimate data

The `download_terraclimate` function accesses and downloads climate and
water balance data from the University of California Merced Climatology
Lab's TerraClimate dataset.

## Usage

``` r
download_terraclimate(
  variables = NULL,
  year = c(2018, 2022),
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

- variables:

  character. Variable(s) name(s).

- year:

  integer(1 or more). One year, two endpoints, or a non-empty vector of
  years. Every year from the minimum through the maximum is requested,
  including gaps: `c(2018, 2023)` requests `2018:2023`. Default is
  `c(2018, 2022)`. Missing and fractional years are rejected.

- directory_to_save:

  character(1). Directory to save data.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- download:

  logical(1). DEPRECATED. Downloads happen automatically.

- remove_command:

  logical(1). Deprecated, ignored.

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

With `download = FALSE`, an invisible preview list containing `urls`,
`destfiles`, and `n_files`. With `download = TRUE` and `hash = FALSE`,
an invisible summary from
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
containing `success`, `failed`, `skipped`, `failed_urls`, and
`failed_files`. The two failure vectors are `character(0)` when no
downloads failed, including when every file was skipped. With
`download = TRUE` and `hash = TRUE`, a visible character scalar
containing the combined MD5 hash of the output directory.

## Note

TerraClimate data does not require authentication.

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

Abatzoglou JT, Dobrowski SZ, Parks SA, Hegewisch KC (2018).
“TerraClimate, a high-resolution global dataset of monthly climate and
climatic water balance from 1958–2015.” *Scientific data*, **5**(1),
1–12.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_terraclimate(
  variables = "ppt",
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
