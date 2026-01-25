# Download meteorological data

The `download_narr` function accesses and downloads daily meteorological
data from [NOAA's North American Regional Reanalysis (NARR)
model](https://psl.noaa.gov/data/gridded/data.narr.html).

## Usage

``` r
download_narr(
  variables = NULL,
  year = c(2018, 2022),
  directory_to_save = NULL,
  acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE,
  hash = FALSE
)
```

## Arguments

- variables:

  character. Variable(s) name acronym. See [List of Variables in NARR
  Files](https://ftp.cpc.ncep.noaa.gov/NARR/fixed/merged_land_AWIP32corrected.pdf)
  for variable names and acronym codes.

- year:

  integer(1 or 2). length of 4. Year or start/end years for downloading
  data.

- directory_to_save:

  character(1). Directory(s) to save downloaded data files.

- acknowledgement:

  logical(1). By setting `TRUE` the user acknowledges that the data
  downloaded using this function may be very large and use lots of
  machine storage and memory.

- download:

  logical(1). `FALSE` will generate a \*.txt file containing all
  download commands. By setting `TRUE` the function will download all of
  the requested data files.

- remove_command:

  logical(1). Remove (`TRUE`) or keep (`FALSE`) the text file containing
  download commands.

- hash:

  logical(1). By setting `TRUE` the function will return an
  [`rlang::hash_file()`](https://rlang.r-lib.org/reference/hash.html)
  hash character corresponding to the downloaded files. Default is
  `FALSE`.

## Value

- For `hash = FALSE`, NULL

- For `hash = TRUE`, an
  [`rlang::hash_file`](https://rlang.r-lib.org/reference/hash.html)
  character.

- netCDF (.nc) files will be stored in `directory_to_save`.

## Note

"Pressure levels" variables contain variable values at 29 atmospheric
levels, ranging from 1000 hPa to 100 hPa. All pressure levels data will
be downloaded for each variable.

## References

Mesinger F, DiMego G, Kalnay E, Mitchell K, Shafran PC, Ebisuzaki W,
Jović D, Woollen J, Rogers E, Berbery EH, Ek MB, Fan Y, Grumbine R,
Higgins W, Li H, Lin Y, Manikin G, Parrish D, Shi W (2006). “North
American Regional Reanalysis.” *Bulletin of the American Meteorological
Society*, **87**(3), 343–360. ISSN 0003-0007, 1520-0477,
[doi:10.1175/BAMS-87-3-343](https://doi.org/10.1175/BAMS-87-3-343) .

## Author

Mitchell Manware, Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
download_narr(
  variables = c("weasd", "omega"),
  year = 2023,
  directory_to_save = tempdir(),
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = TRUE
)
} # }
```
