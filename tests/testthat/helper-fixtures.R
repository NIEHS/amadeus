# Canonical fixtures (small spatial objects, sample dates) for amadeus tests.
#
# Returning fresh objects from helper functions (rather than top-level
# bindings) avoids accidental cross-test mutation and keeps `terra::SpatRaster`
# pointers fresh in each `test_that` block (terra objects don't survive
# serialization across workers).

#' A small synthetic SpatRaster over the contiguous US bounding box.
#'
#' @param nrow,ncol integer(1) raster dimensions.
#' @param value numeric fill value.
#' @keywords internal
fixture_spatraster <- function(nrow = 10L, ncol = 10L, value = 1) {
  testthat::skip_if_not_installed("terra")
  r <- terra::rast(
    nrows = nrow, ncols = ncol,
    xmin = -125, xmax = -65, ymin = 24, ymax = 50,
    crs = "EPSG:4326"
  )
  terra::values(r) <- rep(value, terra::ncell(r))
  r
}

#' A small SpatVector of point locations across the contiguous US.
#'
#' @param n integer(1) number of points.
#' @keywords internal
fixture_points <- function(n = 5L) {
  testthat::skip_if_not_installed("terra")
  set.seed(1)
  df <- data.frame(
    site_id = sprintf("s%02d", seq_len(n)),
    lon = stats::runif(n, -120, -75),
    lat = stats::runif(n, 28, 48)
  )
  terra::vect(df, geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom = TRUE)
}

#' A canonical AOI bounding box as a SpatVector polygon (EPSG:4326).
#' @keywords internal
fixture_aoi <- function() {
  testthat::skip_if_not_installed("terra")
  e <- terra::ext(-125, -65, 24, 50)
  terra::vect(e, crs = "EPSG:4326")
}

#' A short range of sample dates for date-based downloads.
#' @keywords internal
fixture_dates <- function() {
  c("2024-01-01", "2024-01-02")
}
