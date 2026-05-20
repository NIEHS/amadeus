# Get TRI lookup information for chemicals or industries

Returns a lookup table from local TRI files. By default it returns
chemical information (`TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, `CASN`).
Set `type = "industries"` to return industry sector information
(`INDUSTRY_SECTOR_CODE`, `INDUSTRY_SECTOR`).

## Usage

``` r
get_tri_info(
  path = NULL,
  type = c("chemicals", "industries"),
  year = NULL,
  include_na = FALSE,
  ...
)
```

## Arguments

- path:

  character(1). Path to the directory with TRI CSV files (from
  `download_tri`).

- type:

  character(1). Lookup table to return. One of `"chemicals"` (default)
  or `"industries"`.

- year:

  `NULL` or integer(1). Optional single year filter. If `NULL`
  (default), all years in `path` are included.

- include_na:

  logical(1). If `FALSE` (default), rows where lookup fields are all
  missing are removed.

- ...:

  Placeholders.

## Value

a `data.frame` containing the requested TRI lookup table.

## Author

Kyle Messier

## Examples

``` r
if (FALSE) { # \dontrun{
get_tri_info(path = "./data")
get_tri_info(path = "./data", type = "industries")
get_tri_info(path = "./data", year = 2020)
} # }
```
