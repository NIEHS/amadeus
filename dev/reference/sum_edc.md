# Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates

Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC)
covariates

## Usage

``` r
sum_edc(
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  decay_range = NULL,
  target_fields = NULL,
  C0 = NULL,
  use_threshold = TRUE,
  geom = FALSE
)
```

## Arguments

- from:

  `SpatVector`(1). Point locations which contain point-source covariate
  data.

- locs:

  sf/SpatVector(1). Locations where the sum of exponentially decaying
  contributions are calculated.

- locs_id:

  character(1). Name of the unique id field in `point_to`.

- decay_range:

  numeric(1). Distance at which the source concentration is reduced to
  `exp(-3)` (approximately -95 %)

- target_fields:

  character(varying). Field names in characters.

- C0:

  `NULL`, character(1), or numeric vector of length `nrow(from)`.
  Optional initial source values at pollutant locations. If `NULL`
  (default), all source values are set to 1. If character(1), the value
  is treated as a column name in `from` and used as source values.

- use_threshold:

  logical(1). If `TRUE` (default), include only source points within
  `5 * decay_range` from each target location. If `FALSE`, include all
  source points in `from`.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

## Value

a data.frame (tibble) or SpatVector object with input field names with a
suffix `"_sedc"` where the sums of EDC are stored. Additional attributes
are attached for the EDC information.

- \`attr(result, "decay_range")“: the range where concentration reduces
  to approximately five percent

- \`attr(result, "sedc_threshold")“: the threshold distance at which
  emission source points are excluded beyond that

## Note

The function is originally from
[chopin](https://github.com/ropensci/chopin) Distance calculation is
done with terra functions internally. Thus, the function internally
converts sf objects in `point_*` arguments to terra. The threshold
should be carefully chosen by users.

## References

Messier KP, Akita Y, Serre ML (2012). “Integrating Address Geocoding,
Land Use Regression, and Spatiotemporal Geostatistical Estimation for
Groundwater Tetrachloroethylene.” *Environmental Science & Technology*,
**46**(5), 2772–2780. ISSN 0013-936X.
[doi:10.1021/es203152a](https://doi.org/10.1021/es203152a) .

Wiesner C (????). “Euclidean Sum of Exponentially Decaying Contributions
Tutorial.”

## Author

Insang Song

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
set.seed(101)
ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
nc <- terra::vect(ncpath)
nc <- terra::project(nc, "EPSG:5070")
pnt_locs <- terra::centroids(nc, inside = TRUE)
pnt_locs <- pnt_locs[, "NAME"]
pnt_from <- terra::spatSample(nc, 10L)
pnt_from$pid <- seq(1, 10)
pnt_from <- pnt_from[, "pid"]
pnt_from$val1 <- rgamma(10L, 1, 0.05)
pnt_from$val2 <- rgamma(10L, 2, 1)

vals <- c("val1", "val2")
sum_edc(pnt_locs, pnt_from, "NAME", 1e4, vals)
} # }
```
