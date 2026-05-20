# Process toxic release data

This function imports and cleans raw toxic release data, returning a
single `SpatVector` (points) object for the selected `year`.

## Usage

``` r
process_tri(
  path = NULL,
  year = 2018,
  variables = "STACK_AIR",
  chemical = NULL,
  industry_group = c("none", "industry_sector", "industry_sector_code", "both"),
  ignore_case = TRUE,
  extent = NULL,
  ...
)
```

## Arguments

- path:

  character(1). Path to the directory with TRI CSV files

- year:

  integer(1). Single year to select.

- variables:

  character. One or more regular expressions used to select TRI release
  variables by column name after normalization to underscore naming (for
  example, `STACK_AIR`, `FUGITIVE_AIR`, `WATER`). Default is
  `"STACK_AIR"`. Matching first uses raw TRI column names, then falls
  back to a normalized match where punctuation and spaces are converted
  to underscores (for example, `"ON-SITE RELEASE TOTAL"` matches
  `ON_SITE_RELEASE_TOTAL`). Recommended options include:

  - `FUGITIVE_AIR`

  - `STACK_AIR`

  - `WATER`

  - `UNDERGROUND`

  - `UNDERGROUND_CL_I`

  - `UNDERGROUND_C_II_V`

  - `LANDFILLS`

  - `RCRA_C_LANDFILL`

  - `OTHER_LANDFILLS`

  - `LAND_TREATMENT`

  - `SURFACE_IMPNDMNT`

  - `RCRA_SURFACE_IM`

  - `OTHER_SURFACE_I`

  - `OTHER_DISPOSAL`

  - `ON_SITE_RELEASE_TOTAL`

  - `POTW_TRNS_RLSE`

  - `POTW_TRNS_TRT`

  - `POTW_TOTAL_TRANSFERS`

- chemical:

  `NULL` or character. Optional one or more regular expressions used to
  filter chemicals. Patterns are matched against
  `TRI_CHEMICAL_COMPOUND_ID`, `CHEMICAL`, and `CAS`/`CAS.` values. If
  `NULL` (default), all chemicals are retained.

- industry_group:

  character(1). Optional additional grouping level. One of `"none"`
  (default), `"industry_sector"`, `"industry_sector_code"`, or `"both"`.

- ignore_case:

  logical(1). If `TRUE` (default), regular expression matching in
  `variables` and `chemical` is case-insensitive.

- extent:

  numeric(4) or SpatExtent giving the extent of the raster if `NULL`
  (default), the entire raster is loaded

- ...:

  Placeholders.

## Value

a `SpatVector` object (points) in `year` `year` is stored in a field
named `"year"`.

## Note

Use
[`get_tri_info()`](https://niehs.github.io/amadeus/reference/get_tri_info.md)
to inspect available TRI chemical IDs/names/CAS numbers and industry
sector codes in local TRI files. Visit [TRI Data and
Tools](https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox)
to view the available years and variables.

## References

https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox

## Author

Kyle Messier

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
tri <- process_tri(
  path = "./data",
  year = 2020,
  variables = c("STACK_AIR", "FUGITIVE_AIR"),
  chemical = "benzene",
  industry_group = "industry_sector"
)
} # }
```
