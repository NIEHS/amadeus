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

  logical(1). Show download progress (default TRUE)

- hash:

  logical(1). Return hash of downloaded files (default FALSE)

- max_tries:

  integer(1). Maximum retry attempts (default 20)

- rate_limit:

  numeric(1). Minimum seconds between requests (default 2)

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
