# Time grouping in calculate\_\* functions

## How `.by_time` works

`calculate_*()` functions use `.by_time` for optional temporal
summarization:

- If `.by_time = NULL`, results keep the native processed time rows.
- If `.by_time` is a time unit (`"day"`, `"week"`, `"month"`,
  `"quarter"`, `"year"`), rows are grouped by site and that temporal
  bucket.
- Supported sub-daily tokens are also available (`"minute"`, `"hour"`),
  depending on the underlying dataset time resolution.

## Minimal reproducible example using `calculate_geos()`

``` r

# Build a toy 4-layer raster with GEOS-style layer names
r <- terra::rast(
  ncols = 2, nrows = 1, xmin = -81, xmax = -79, ymin = 35, ymax = 36,
  crs = "EPSG:4326", nlyrs = 4
)
terra::values(r[[1]]) <- c(1, 2)
terra::values(r[[2]]) <- c(3, 4)
terra::values(r[[3]]) <- c(5, 6)
terra::values(r[[4]]) <- c(7, 8)
names(r) <- c(
  "no2_lev=850_20200101_010000",
  "no2_lev=850_20200101_130000",
  "no2_lev=850_20200201_010000",
  "no2_lev=850_20200201_130000"
)

locs <- data.frame(
  site_id = c("A", "B"),
  lon = c(-80.5, -79.5),
  lat = c(35.5, 35.5)
)
```

### 1) Default behavior: native temporal resolution

``` r

native_rows <- calculate_geos(
  from = r,
  locs = locs,
  locs_id = "site_id",
  radius = 0,
  geom = FALSE
)
#> Detected `data.frame` extraction locations...
#> Calculating no2 covariates at lev=850 for 2020-01-01 01:00:00...
#> Calculating no2 covariates at lev=850 for 2020-01-01 13:00:00...
#> Calculating no2 covariates at lev=850 for 2020-02-01 01:00:00...
#> Calculating no2 covariates at lev=850 for 2020-02-01 13:00:00...
#> Returning extracted covariates.
head(native_rows)
#>   site_id                time level no2_0
#> 1       A 2020-01-01 01:00:00   850     1
#> 2       B 2020-01-01 01:00:00   850     2
#> 3       A 2020-01-01 13:00:00   850     3
#> 4       B 2020-01-01 13:00:00   850     4
#> 5       A 2020-02-01 01:00:00   850     5
#> 6       B 2020-02-01 01:00:00   850     6
```

### 2) Time bucketing with `.by_time`

``` r

monthly <- calculate_geos(
  from = r,
  locs = locs,
  locs_id = "site_id",
  radius = 0,
  .by_time = "month",
  geom = FALSE
)
#> Detected `data.frame` extraction locations...
#> Calculating no2 covariates at lev=850 for 2020-01-01 01:00:00...
#> Calculating no2 covariates at lev=850 for 2020-01-01 13:00:00...
#> Calculating no2 covariates at lev=850 for 2020-02-01 01:00:00...
#> Calculating no2 covariates at lev=850 for 2020-02-01 13:00:00...
#> Returning extracted covariates.
monthly
#>   site_id       time level no2_0
#> 1       A 2020-01-01   850     2
#> 2       A 2020-02-01   850     6
#> 3       B 2020-01-01   850     3
#> 4       B 2020-02-01   850     7
```

## Practical guidance

1.  Use default `.by_time = NULL` when you want native temporal output.
2.  Use `.by_time` for coarser temporal summaries at the scale needed
    for analysis.
