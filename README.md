[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus/graph/badge.svg)](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/amadeus)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/check-standard.yaml)
[![lint](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/amadeus/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# amadeus

**A** **M**echanism/**M**achine for **D**ata, **E**nvironments, and **U**ser **S**etup

Description of package

## Data Sources

## `download.R`

`download()` is a wrapper function which calls source-specific data download functions.

| Source-Specfiic Function     | Data Source  | Data Type  | Data Genre  |
| ---------------------------- | ------------ | ---------- | ----------- |
| `download_aqs_data()`    | Environmental Protection Agency (EPA) Air Data | csv | Air Pollution |
| `download_ecoregion_data()` | Environmental Protection Agency (EPA) Ecoregions | **data type** | Climate Regions |
| `download_geos_cf_data()` | NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF) | netCDF | Atmosphere, Meteorology |

## `import.R` or `process.R`

## `calc.R` or `covar.R`

## References
