################################################################################
#' Store internal package data
#'
#' @description
#' Private environment containing data initialized when the amadeus namespace
#' is loaded. Stores lookup tables for use by package functions, including
#' metadata for standard MERRA-2 collections and the derived Global Fire
#' Weather Index (FWI) product.
#'
#' @format
#' An environment with an empty parent environment. After [.onLoad()] runs,
#' `df_merra2` contains a data frame with one row per supported product and
#' the following columns:
#' \describe{
#'   \item{collection_id}{Collection identifier accepted by
#'     [download_merra2()], including `"fwi"`.}
#'   \item{esdt_name}{Earth Science Data Type (ESDT) short name.}
#'   \item{DOI}{Digital Object Identifier for the product.}
#'   \item{base_url}{Base URL used to construct download URLs.}
#'   \item{version}{Product version used by the download source.}
#'   \item{requires_token}{Logical flag indicating whether a NASA Earthdata
#'     token is required by the source. Descriptive metadata;
#'     [download_merra2()] currently determines token lookup from the
#'     requested collection identifiers.}
#'   \item{file_extension}{Data file extension, including the leading period.}
#' }
#' The ESDT name and DOI are `NA` for FWI in the supplied lookup table.
#'
#' @details
#' The lookup table is maintained in `inst/extdata/df_merra2.csv` in the package
#' source. Its initial collection identifiers, ESDT short names, DOIs, and
#' download locations were extracted from the mappings previously embedded
#' in [download_merra2()]. Standard products use NASA GES DISC metadata;
#' the derived FWI entry uses the GlobalFWI download location.
#'
#' When updating the table, check the provider's product metadata for the
#' identifier, DOI, version, and download location. Keep one row per unique
#' `collection_id`, preserve the column names, use `TRUE`/`FALSE` for
#' `requires_token`, and use `NA` for unavailable identifiers. Versions are
#' source labels and may use different formats across products. Update the
#' downloader's default collection vector if the default selection should
#' include a newly supported product. The in-memory table is populated when
#' the namespace loads; it is not refreshed automatically after CSV edits.
#'
#' @seealso [.onLoad()], [download_merra2()]
#' @keywords internal
.amadeus <- new.env(parent = emptyenv())

################################################################################
#' Load internal package lookup tables
#'
#' @description
#' Initialize `.amadeus$df_merra2` from the CSV file installed with the
#' package. R calls this hook automatically when the amadeus namespace is
#' loaded, including through `library(amadeus)` or access with `amadeus::`.
#'
#' @param libname character(1). Library directory containing the installed
#'   package. Supplied by R when the namespace is loaded.
#' @param pkgname character(1). Name of the package being loaded. Supplied by R
#'   when the namespace is loaded.
#'
#' @details
#' Locate `extdata/df_merra2.csv` in the specified package installation with
#' [base::system.file()] and read it with [utils::read.csv()]. The resulting
#' data frame is stored in the private [.amadeus] environment. Its columns
#' describe product identifiers, download locations, versions, authentication
#' requirements, and file extensions. Namespace loading fails if the file
#' cannot be located or read. The hook reads the installed CSV without
#' querying remote catalogs or validating the table's schema or contents;
#' maintainers should check these when editing the source CSV. See [.amadeus]
#' for the schema and maintenance notes.
#'
#' @return
#' Invisibly, the data frame assigned to `.amadeus$df_merra2`. The hook is
#' called for its initialization side effect.
#'
#' @seealso [.amadeus], [download_merra2()]
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .amadeus$df_merra2 <- read.csv(
    system.file(
      "extdata",
      "df_merra2.csv",
      package = pkgname,
      lib.loc = libname,
      mustWork = TRUE
    )
  )
}
