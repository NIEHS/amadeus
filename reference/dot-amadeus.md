# Store internal package data

Private environment containing data initialized when the amadeus
namespace is loaded. Stores lookup tables for use by package functions,
including metadata for standard MERRA-2 collections and the derived
Global Fire Weather Index (FWI) product.

## Usage

``` r
.amadeus
```

## Format

An environment with an empty parent environment. After
[`.onLoad()`](https://niehs.github.io/amadeus/reference/dot-onLoad.md)
runs, `df_merra2` contains a data frame with one row per supported
product and the following columns:

- collection_id:

  Collection identifier accepted by
  [`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md),
  including `"fwi"`.

- esdt_name:

  Earth Science Data Type (ESDT) short name.

- DOI:

  Digital Object Identifier for the product.

- base_url:

  Base URL used to construct download URLs.

- version:

  Product version used by the download source.

- requires_token:

  Logical flag indicating whether a NASA Earthdata token is required by
  the source. Descriptive metadata;
  [`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md)
  currently determines token lookup from the requested collection
  identifiers.

- file_extension:

  Data file extension, including the leading period.

The ESDT name and DOI are `NA` for FWI in the supplied lookup table.

## Details

The lookup table is maintained in `inst/extdata/df_merra2.csv` in the
package source. Its initial collection identifiers, ESDT short names,
DOIs, and download locations were extracted from the mappings previously
embedded in
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md).
Standard products use NASA GES DISC metadata; the derived FWI entry uses
the GlobalFWI download location.

When updating the table, check the provider's product metadata for the
identifier, DOI, version, and download location. Keep one row per unique
`collection_id`, preserve the column names, use `TRUE`/`FALSE` for
`requires_token`, and use `NA` for unavailable identifiers. Versions are
source labels and may use different formats across products. Update the
downloader's default collection vector if the default selection should
include a newly supported product. The in-memory table is populated when
the namespace loads; it is not refreshed automatically after CSV edits.

## See also

[`.onLoad()`](https://niehs.github.io/amadeus/reference/dot-onLoad.md),
[`download_merra2()`](https://niehs.github.io/amadeus/reference/download_merra2.md)
