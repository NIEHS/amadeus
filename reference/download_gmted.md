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

  character(1). Available statistics.

- resolution:

  character(1). Available resolutions.

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
