# Download toxic release data

The `download_tri()` function accesses and downloads toxic release data
from the U.S. Environmental Protection Agency's (EPA) Toxic Release
Inventory (TRI) Program. The EPA TRI basic data files contain annual,
facility-reported toxic chemical release and waste management
information. EPA publishes TRI basic files in multiple annual variants
under the same service endpoint: a nationwide file (`"US"`),
state-specific files identified by two-letter postal abbreviations (for
example `"AZ"` or `"NC"`), and a tribal file (`"tbl"`).

## Usage

``` r
download_tri(
  year = c(2018L, 2022L),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  jurisdiction = "US",
  download = TRUE,
  remove_command = FALSE,
  show_progress = TRUE,
  hash = FALSE,
  max_tries = 20,
  rate_limit = 2
)
```

## Arguments

- year:

  integer(1 or 2). Year or start/end years for downloading data.

- directory_to_save:

  character(1). Directory to download files.

- acknowledgement:

  logical(1). Must be TRUE to proceed.

- jurisdiction:

  character(1). TRI file variant to download. Use `"US"` for the
  nationwide file, a two-letter state or territory code such as `"AZ"`
  or `"NC"` for a jurisdiction-specific file, or `"tbl"` for the tribal
  file. Default is `"US"`.

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

invisible list with download results; or hash character if hash=TRUE

## Note

TRI data does not require authentication. State and tribal downloads are
saved with jurisdiction-specific file names, while the U.S.-wide
download keeps the historical `tri_raw_<year>.csv` naming pattern.

## References

United States Environmental Protection Agency (2024). “TRI Basic Data
Files: Calendar Years 1987 – Present.”
<https://www.epa.gov/toxics-release-inventory-tri-program/tri-data-action-0>.

## Author

Mariana Kassien, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_tri(
  year = 2021L,
  directory_to_save = tempdir(),
  jurisdiction = "NC",
  acknowledgement = TRUE
)
} # }
```
