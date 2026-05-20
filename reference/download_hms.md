# Download wildfire smoke data

The `download_hms()` function accesses and downloads wildfire smoke
plume coverage data from NOAA's Hazard Mapping System Fire and Smoke
Product.

## Usage

``` r
download_hms(
  data_format = "Shapefile",
  date = c("2018-01-01", "2018-01-01"),
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

- data_format:

  character(1). "Shapefile" or "KML".

- date:

  character(1 or 2). Date range "YYYY-MM-DD" format

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

HMS data does not require authentication.

## References

(????). “Hazard Mapping System Fire and Smoke Product: Hazard Mapping
System.” <https://www.ospo.noaa.gov/products/land/hms.html#about>.
<https://www.ospo.noaa.gov/products/land/hms.html#about>.

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_hms(
  data_format = "Shapefile",
  date = "2024-01-01",
  directory_to_save = tempdir(),
  acknowledgement = TRUE
)
} # }
```
