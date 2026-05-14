# Reusable mock factories for amadeus process_* and calculate_covariates_* tests
#
# Process/calc functions read files from disk and operate on terra/sf objects.
# These helpers return either canned spatial objects (see helper-fixtures.R)
# or factories that intercept file IO.

#' Bindings that intercept `list.files()` for a process function.
#'
#' @param files character vector of fake file paths to return.
#' @keywords internal
mocks_list_files <- function(files) {
  force(files)
  list(
    list.files = function(...) files
  )
}

#' Bindings that return a canned `SpatRaster` from `terra::rast()`.
#'
#' Use to avoid touching disk when testing process logic that wraps `rast()`.
#'
#' @param raster a `SpatRaster` (default: small synthetic raster from
#'   `fixture_spatraster()`).
#' @keywords internal
mocks_terra_rast <- function(raster = fixture_spatraster()) {
  force(raster)
  list(
    rast = function(...) raster
  )
}

#' Bindings that short-circuit `exactextractr::exact_extract()`.
#'
#' @param value scalar or vector returned for each polygon.
#' @keywords internal
mocks_exact_extract <- function(value = 1) {
  force(value)
  list(
    exact_extract = function(x, y, ...) {
      data.frame(value = rep(value, length.out = NROW(y)))
    }
  )
}
