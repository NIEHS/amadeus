# Load internal package lookup tables

Initialize `.amadeus$df_merra2` from the CSV file installed with the
package. R calls this hook automatically when the amadeus namespace is
loaded, including through
[`library(amadeus)`](https://niehs.github.io/amadeus/) or access with
`amadeus::`.

## Usage

``` r
.onLoad(libname, pkgname)
```

## Arguments

- libname:

  character(1). Library directory containing the installed package.
  Supplied by R when the namespace is loaded.

- pkgname:

  character(1). Name of the package being loaded. Supplied by R when the
  namespace is loaded.

## Value

Invisibly, the data frame assigned to `.amadeus$df_merra2`. The hook is
called for its initialization side effect.

## Details

Locate `extdata/df_merra2.csv` in the specified package installation
with [`base::system.file()`](https://rdrr.io/r/base/system.file.html)
and read it with
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html). The
resulting data frame is stored in the private
[.amadeus](https://niehs.github.io/amadeus/reference/dot-amadeus.md)
environment. Its columns describe product identifiers, download
locations, versions, authentication requirements, and file extensions.
Namespace loading fails if the file cannot be located or read. The hook
reads the installed CSV without querying remote catalogs or validating
the table's schema or contents; maintainers should check these when
editing the source CSV. See
[.amadeus](https://niehs.github.io/amadeus/reference/dot-amadeus.md) for
the schema and maintenance notes.

## See also

[.amadeus](https://niehs.github.io/amadeus/reference/dot-amadeus.md),
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md)
