# USDA CropScape

This article demonstrates a compact workflow for USDA CropScape
(Cropland Data Layer) rasters.

This vignette runs its live workflow when rendered locally. The heavy
download, processing, extraction, and plotting chunks are skipped
automatically on CI, CRAN checks, and pkgdown builds; set
`AMADEUS_RUN_VIGNETTES=true` to force live execution in those
environments.

## Available inputs and data availability

- `download_data(dataset_name = "cropscape", ...)` accepts a `year` and
  `source = "USDA"` or `"GMU"`.
- CropScape is an annual 30 m Cropland Data Layer product: USDA
  downloads are available from 2008 through the latest release, while
  GMU extends back to 1997.
- File formats depend on source: USDA serves yearly ZIP archives and GMU
  serves yearly `.tar.gz` archives.
- CropScape values are categorical crop/land-use classes. Covariate
  calculation are a fraction of each land cover class within the
  specified radius or areal unit.

The table below reproduces the crop and related land-use classes listed
in the official USDA CDL metadata used by CropScape; see the [2024 CDL
metadata](https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/metadata_CDL24_FGDC-STD-001-1998.htm)
for the complete legend and additional class details.

| CDL code | cropland class              |
|---------:|:----------------------------|
|        1 | Corn                        |
|        2 | Cotton                      |
|        3 | Rice                        |
|        4 | Sorghum                     |
|        5 | Soybeans                    |
|        6 | Sunflower                   |
|       10 | Peanuts                     |
|       11 | Tobacco                     |
|       12 | Sweet Corn                  |
|       13 | Pop or Orn Corn             |
|       14 | Mint                        |
|       21 | Barley                      |
|       22 | Durum Wheat                 |
|       23 | Spring Wheat                |
|       24 | Winter Wheat                |
|       25 | Other Small Grains          |
|       26 | Dbl Crop WinWht/Soybeans    |
|       27 | Rye                         |
|       28 | Oats                        |
|       29 | Millet                      |
|       30 | Speltz                      |
|       31 | Canola                      |
|       32 | Flaxseed                    |
|       33 | Safflower                   |
|       34 | Rape Seed                   |
|       35 | Mustard                     |
|       36 | Alfalfa                     |
|       37 | Other Hay/Non Alfalfa       |
|       38 | Camelina                    |
|       39 | Buckwheat                   |
|       41 | Sugarbeets                  |
|       42 | Dry Beans                   |
|       43 | Potatoes                    |
|       44 | Other Crops                 |
|       45 | Sugarcane                   |
|       46 | Sweet Potatoes              |
|       47 | Misc Vegs & Fruits          |
|       48 | Watermelons                 |
|       49 | Onions                      |
|       50 | Cucumbers                   |
|       51 | Chick Peas                  |
|       52 | Lentils                     |
|       53 | Peas                        |
|       54 | Tomatoes                    |
|       55 | Caneberries                 |
|       56 | Hops                        |
|       57 | Herbs                       |
|       58 | Clover/Wildflowers          |
|       59 | Sod/Grass Seed              |
|       60 | Switchgrass                 |
|       61 | Fallow/Idle Cropland        |
|       64 | Shrubland                   |
|       66 | Cherries                    |
|       67 | Peaches                     |
|       68 | Apples                      |
|       69 | Grapes                      |
|       70 | Christmas Trees             |
|       71 | Other Tree Crops            |
|       72 | Citrus                      |
|       74 | Pecans                      |
|       75 | Almonds                     |
|       76 | Walnuts                     |
|       77 | Pears                       |
|       92 | Aquaculture                 |
|      204 | Pistachios                  |
|      205 | Triticale                   |
|      206 | Carrots                     |
|      207 | Asparagus                   |
|      208 | Garlic                      |
|      209 | Cantaloupes                 |
|      210 | Prunes                      |
|      211 | Olives                      |
|      212 | Oranges                     |
|      213 | Honeydew Melons             |
|      214 | Broccoli                    |
|      215 | Avocados                    |
|      216 | Peppers                     |
|      217 | Pomegranates                |
|      218 | Nectarines                  |
|      219 | Greens                      |
|      220 | Plums                       |
|      221 | Strawberries                |
|      222 | Squash                      |
|      223 | Apricots                    |
|      224 | Vetch                       |
|      225 | Dbl Crop WinWht/Corn        |
|      226 | Dbl Crop Oats/Corn          |
|      227 | Lettuce                     |
|      228 | Dbl Crop Triticale/Corn     |
|      229 | Pumpkins                    |
|      230 | Dbl Crop Lettuce/Durum Wht  |
|      231 | Dbl Crop Lettuce/Cantaloupe |
|      232 | Dbl Crop Lettuce/Cotton     |
|      233 | Dbl Crop Lettuce/Barley     |
|      236 | Dbl Crop WinWht/Sorghum     |
|      237 | Dbl Crop Barley/Corn        |
|      238 | Dbl Crop WinWht/Cotton      |
|      239 | Dbl Crop Soybeans/Cotton    |
|      240 | Dbl Crop Soybeans/Oats      |
|      241 | Dbl Crop Corn/Soybeans      |
|      242 | Blueberries                 |
|      243 | Cabbage                     |
|      244 | Cauliflower                 |
|      245 | Celery                      |
|      246 | Radishes                    |
|      247 | Turnips                     |
|      248 | Eggplants                   |
|      249 | Gourds                      |
|      250 | Cranberries                 |
|      254 | Dbl Crop Barley/Soybeans    |

## Download representative requests

``` r

directory_to_save <- file.path(tempdir(), "cropscape_workflow")

download_data(
  dataset_name = "cropscape",
  year = 2020,
  source = "USDA",
  directory_to_save = file.path(directory_to_save, "usda_2020"),
  acknowledgement = TRUE,
  unzip = TRUE
)
```

## Process one workflow-ready data product

``` r

cropscape_path <- list.files(
  file.path(directory_to_save, "usda_2020"),
  pattern = "2020.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)[1]

cropscape_reference <- terra::rast(cropscape_path)

print(cropscape_reference)

processed_data <- process_covariates(
  covariate = "cropscape",
  path = cropscape_path,
  year = 2020,
  extent = {
    ref_ext <- terra::ext(cropscape_reference)
    dx <- terra::xmax(ref_ext) - terra::xmin(ref_ext)
    dy <- terra::ymax(ref_ext) - terra::ymin(ref_ext)
    terra::ext(
      terra::xmin(ref_ext) + 0.35 * dx,
      terra::xmin(ref_ext) + 0.65 * dx,
      terra::ymin(ref_ext) + 0.35 * dy,
      terra::ymin(ref_ext) + 0.65 * dy
    )
  }
)
plot(processed_data, main = "Processed CropScape raster")
```

## Calculate covariates at points

Note that if no radius is specified, the point extraction will return
the value of the raster cell containing each point. In this case, the
output columns are binary indicators of whether each CropScape class is
present at that location. With a radius specified, the output columns
are a fraction of each class within the circular buffer around each
point, so values can range from 0 to 1 and multiple classes can be
present at the same location with varying proportions.

``` r


domain_x <- c(terra::xmin(processed_data), terra::xmax(processed_data))
domain_y <- c(terra::ymin(processed_data), terra::ymax(processed_data))
domain_dx <- diff(domain_x)
domain_dy <- diff(domain_y)

candidate_xy <- expand.grid(
  lon = seq(domain_x[1] + 0.12 * domain_dx, domain_x[2] - 0.12 * domain_dx, length.out = 5),
  lat = seq(domain_y[1] + 0.12 * domain_dy, domain_y[2] - 0.12 * domain_dy, length.out = 5)
)
raster_crs <- sf::st_crs(terra::crs(processed_data))
if (is.na(raster_crs)) {
  stop("`processed_data` is missing CRS; cannot build extraction points.")
}
example_points_sf <- sf::st_as_sf(
  candidate_xy,
  coords = c("lon", "lat"),
  crs = raster_crs
)
example_points_sf$site_id <- paste0("site_", seq_len(nrow(example_points_sf)))


point_values <- calculate_covariates(
  covariate = "cropscape",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 0,
  geom = "sf"
)

print(point_values)

point_proportion_100m <- calculate_covariates(
  covariate = "cropscape",
  from = processed_data,
  locs = example_points_sf,
  locs_id = "site_id",
  radius = 100,
  geom = "sf"
)

print(point_proportion_100m)
```
