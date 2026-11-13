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

## Note

TerraClimate data does not require authentication.

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
