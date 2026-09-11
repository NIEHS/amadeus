# Amadeus unit and mocked-test improvement report

**Prepared:** September 9, 2026  
**Scope:** Ecoregion, drought (SPEI, EDDI, and USDM), and CropScape  
**Functions:** `download_data()`, `process_covariates()`, and
`calculate_covariates()` plus their source-specific implementations

## Executive summary

The main recommendation is to retain small, focused unit tests while adding a
network-free linked mock test for every dataset:

```text
simulated provider response
        -> real download dispatcher and downloader
        -> process-ready local fixture
        -> real process dispatcher and processor
        -> real calculate dispatcher and calculator
        -> explicit structural and scientific contracts
```

This is primarily an improvement in test reproducibility, coverage, and failure
diagnosis. It is not evidence that production downloads or scientific
processing have become faster. The linked tests replace unpredictable network
and large-file operations with small committed fixtures while leaving package
dispatch, file discovery, CRS handling, temporal selection, spatial
transformation, and covariate calculation real.

The implemented two-iteration comparison produced 20 measured runs: five
dataset specifications, two strategies, and two iterations. Every run passed.
For every recommended strategy, a simulated download artifact was created and
the resulting local output was passed directly to processing.

| Dataset | Process contracts | Calculate contracts | Linked workflow |
|---|---:|---:|---|
| Ecoregion | 6/6 | 5/5 | Passed |
| SPEI | 9/9 | 5/5 | Passed |
| EDDI | 9/9 | 5/5 | Passed |
| USDM | 6/6 | 9/9 | Passed, including 1,000 m buffering |
| CropScape | 8/8 | 6/6 | Passed |

The USDM comparison also exposed a real package defect. Locations derived from
USDM polygons retained `DM`, `date`, and `source`; these collided with provider
fields during `terra::intersect()`. The package now retains only a collision-safe
site-row key on the location side of the intersection. The complete drought
test file passes 174 assertions after this change.

## Core differences from the earlier test approach

### 1. Successful execution is replaced by observable contracts

A test such as this proves only that an error was not raised:

```r
testthat::expect_no_error(
  result <- process_cropscape(fixture, year = 2019)
)
```

The recommended form verifies the output needed by the next package stage:

```r
result <- process_cropscape(fixture, year = 2019)

testthat::expect_s4_class(result, "SpatRaster")
testthat::expect_equal(terra::nlyr(result), 1L)
testthat::expect_gt(terra::ncell(result), 0L)
testthat::expect_true(terra::hasValues(result))
testthat::expect_false(is.na(terra::crs(result)))
```

### 2. No-op download mocks are supplemented with process-ready artifacts

The existing focused mock pattern validates URL construction but does not
create the file that processing expects:

```r
local_download_mocks(
  download_run_method = function(urls, destfiles, ...) {
    list(success = length(destfiles), failed = 0L, skipped = 0L)
  }
)
```

The linked pattern preserves the local side effect:

```r
local_download_mocks(
  download_run_method = function(urls, destfiles, ...) {
    file.copy(fixture, destfiles, overwrite = TRUE)
    list(success = length(destfiles), failed = 0L, skipped = 0L)
  }
)
```

Both tests are useful. The first isolates URL and argument behavior; the second
detects an incompatible download-to-process boundary.

### 3. Public dispatchers are tested alongside direct functions

Direct tests such as `process_drought(source = "spei", ...)` should remain.
At least one test per alias should also call:

```r
download_data(dataset_name = "spei", ...)
process_covariates(covariate = "spei", ...)
calculate_covariates(covariate = "spei", ...)
```

This detects missing aliases, incorrect source routing, and argument-forwarding
problems that direct-function tests cannot detect.

### 4. Scientific invariants are tested, not just classes and column presence

Examples include:

- raster layer names agree with dates and timescales;
- output rows preserve location identifiers;
- drought classes are in `0:4`;
- buffered class proportions are bounded by zero and one;
- complete class proportions sum to one;
- binary ecoregion indicators contain only zero and one;
- CropScape class fractions are bounded and sum to one.

## Shared test organization

Use four complementary layers:

| Test layer | Purpose | Network |
|---|---|---|
| Input-validation unit test | Invalid arguments fail before side effects | No |
| Focused download mock | URL, destination, extraction call, cleanup, hash | No |
| Process/calculate unit test | Structural and scientific contracts on fixtures | No |
| Linked mocked workflow | Download output is accepted by process and calculate | No |
| Separately gated live test | Provider availability and real archive changes | Yes |

The shared download boundary should continue to use
`helper-mocks-download.R`. Dataset-specific artifact installation belongs in a
specification because a GPKG, NetCDF, ASCII raster, shapefile bundle, and TIFF
cannot be installed in the same way.

## Ecoregion findings and recommendations

### Findings

- Existing tests validate URL discovery, directory creation, reading, and basic
  object class.
- Some older blocks use `expect_no_error()`, which does not establish schema,
  CRS, feature count, valid geometry, or calculation semantics.
- A no-op extraction mock cannot prove that the downloaded GPKG is discoverable
  by `process_ecoregion()`.
- The linked comparison successfully creates the ZIP destination, copies the
  committed GPKG into `data_files`, processes it, and calculates deterministic
  indicators.

### Recommended download mock

```r
testthat::test_that(
  "download_data(dataset_name=ecoregion): creates process-ready GPKG",
  {
    fixture <- testthat::test_path(
      "..", "testdata", "ecoregions", "eco_l3_clip.gpkg"
    )
    extraction_directory <- NULL

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        testthat::expect_match(urls, "us_eco_l3_state_boundaries\\.zip$")
        dir.create(dirname(destfiles), recursive = TRUE, showWarnings = FALSE)
        writeBin(charToRaw("mock archive"), destfiles)
        list(success = 1L, failed = 0L, skipped = 0L)
      },
      download_unzip = function(file_name, directory_to_unzip, ...) {
        extraction_directory <<- directory_to_unzip
        dir.create(directory_to_unzip, recursive = TRUE, showWarnings = FALSE)
        file.copy(
          fixture,
          file.path(directory_to_unzip, basename(fixture)),
          overwrite = TRUE
        )
      }
    )

    download_data(
      dataset_name = "ecoregion",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_true(file.exists(file.path(
      extraction_directory,
      basename(fixture)
    )))
  }
)
```

### Recommended process contracts

```r
ecoregion <- process_covariates(
  covariate = "ecoregion",
  path = fixture
)

required_fields <- c(
  "L2_KEY", "L3_KEY", "NA_L2NAME", "US_L3NAME", "NA_L3NAME", "time"
)

testthat::expect_s4_class(ecoregion, "SpatVector")
testthat::expect_gt(terra::nrow(ecoregion), 0L)
testthat::expect_true(all(terra::is.valid(ecoregion)))
testthat::expect_true(all(required_fields %in% names(ecoregion)))
testthat::expect_false(is.na(terra::crs(ecoregion)))
testthat::expect_false(anyNA(ecoregion$L3_KEY))
```

### Recommended calculate contracts

```r
result <- calculate_covariates(
  covariate = "ecoregion",
  from = ecoregion,
  locs = locations,
  locs_id = "site_id",
  frac = FALSE,
  drop = TRUE
)

indicator_columns <- grep("^DUM_E[23]", names(result), value = TRUE)
testthat::expect_s3_class(result, "data.frame")
testthat::expect_equal(nrow(result), terra::nrow(locations))
testthat::expect_identical(result$site_id, locations$site_id)
testthat::expect_gt(length(indicator_columns), 0L)
testthat::expect_true(all(vapply(
  result[indicator_columns],
  function(x) all(x %in% c(0L, 1L), na.rm = TRUE),
  logical(1)
)))
```

## SPEI findings and recommendations

### Findings

- SPEI uses one multi-year NetCDF per timescale; archive extraction is not part
  of this pathway.
- Existing drought process tests already provide strong coverage for class,
  layers, time, CRS, and values.
- The principal improvement is connecting the destination requested by
  `download_drought()` to the real processing and calculation stages.
- Exact layer names should be checked, because they encode source, timescale,
  and date.

### Recommended download mock

```r
testthat::test_that(
  "download_data(dataset_name=spei, timescale=1): installs requested NetCDF",
  {
    fixture <- testthat::test_path(
      "..", "testdata", "drought", "spei", "spei01.nc"
    )
    destination <- NULL

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        destination <<- destfiles
        testthat::expect_match(urls, "/spei01\\.nc$")
        file.copy(fixture, destfiles, overwrite = TRUE)
        list(success = 1L, failed = 0L, skipped = 0L)
      }
    )

    download_data(
      dataset_name = "spei",
      date = c("2020-01-01", "2020-03-31"),
      timescale = 1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_true(file.exists(destination))
    testthat::expect_identical(basename(destination), "spei01.nc")
  }
)
```

### Recommended process contracts

```r
spei <- process_covariates(
  covariate = "spei",
  path = download_directory,
  date = c("2020-01-01", "2020-03-31"),
  timescale = 1L
)

testthat::expect_s4_class(spei, "SpatRaster")
testthat::expect_equal(terra::nlyr(spei), 3L)
testthat::expect_identical(
  names(spei),
  c(
    "spei_01_2020-01-01",
    "spei_01_2020-02-01",
    "spei_01_2020-03-01"
  )
)
testthat::expect_equal(as.Date(terra::time(spei)), as.Date(c(
  "2020-01-01", "2020-02-01", "2020-03-01"
)))
testthat::expect_true(terra::same.crs(spei, "EPSG:4326"))
testthat::expect_true(terra::hasValues(spei))
```

### Recommended calculate contracts

```r
result <- calculate_covariates(
  covariate = "spei",
  from = spei,
  locs = locations,
  locs_id = "site_id",
  radius = 0
)

testthat::expect_s3_class(result, "data.frame")
testthat::expect_equal(
  nrow(result),
  terra::nrow(locations) * terra::nlyr(spei)
)
testthat::expect_setequal(unique(result$site_id), locations$site_id)
testthat::expect_true("spei_01_0" %in% names(result))
testthat::expect_s3_class(result$time, "POSIXct")
testthat::expect_type(result$spei_01_0, "double")
```

## EDDI findings and recommendations

### Findings

- EDDI download output consists of dated weekly ASCII rasters, while the
  committed legacy fixture is a yearly NetCDF.
- A linked mock must create the exact downloaded ASCII filename; copying the
  NetCDF under its original name does not test the current download-to-process
  contract.
- Filename ordering and filename-derived dates are central reproducibility
  contracts.
- The comparison converts one deterministic NetCDF fixture layer into the
  expected ASCII artifact without making a network request.

### Recommended download mock

```r
testthat::test_that(
  "download_data(dataset_name=eddi, date=2020-01-07): creates dated ASCII raster",
  {
    fixture <- testthat::test_path(
      "..", "testdata", "drought", "eddi", "eddi01mn2020.nc"
    )
    destination <- NULL

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        destination <<- destfiles
        testthat::expect_match(urls, "EDDI_ETrs_01mn_20200107\\.asc$")
        suppressWarnings(terra::writeRaster(
          terra::rast(fixture)[[1L]],
          destfiles,
          filetype = "AAIGrid",
          overwrite = TRUE
        ))
        list(success = 1L, failed = 0L, skipped = 0L)
      }
    )

    download_data(
      dataset_name = "eddi",
      date = "2020-01-07",
      timescale = 1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_true(file.exists(destination))
    testthat::expect_identical(
      basename(destination),
      "EDDI_ETrs_01mn_20200107.asc"
    )
  }
)
```

### Recommended process contracts

```r
eddi <- process_covariates(
  covariate = "eddi",
  path = download_directory,
  date = "2020-01-07",
  timescale = 1L
)

testthat::expect_s4_class(eddi, "SpatRaster")
testthat::expect_equal(terra::nlyr(eddi), 1L)
testthat::expect_identical(names(eddi), "eddi_01_2020-01-07")
testthat::expect_identical(
  as.Date(terra::time(eddi)),
  as.Date("2020-01-07")
)
testthat::expect_true(terra::same.crs(eddi, "EPSG:4326"))
testthat::expect_true(terra::hasValues(eddi))
```

### Recommended calculate contracts

```r
result <- calculate_covariates(
  covariate = "eddi",
  from = eddi,
  locs = locations,
  locs_id = "site_id",
  radius = 0
)

testthat::expect_s3_class(result, "data.frame")
testthat::expect_equal(nrow(result), terra::nrow(locations))
testthat::expect_identical(result$site_id, locations$site_id)
testthat::expect_true("eddi_01_0" %in% names(result))
testthat::expect_identical(
  unique(as.Date(result$time)),
  as.Date("2020-01-07")
)
```

## USDM findings and recommendations

### Findings

- USDM requires complete shapefile bundles. Copying only `.shp` is not a valid
  process-ready simulation; `.dbf`, `.shx`, `.prj`, and normally `.cpg` must be
  installed too.
- Download, extraction, date parsing, polygon processing, point overlay, and
  buffered proportions are separate contracts.
- The linked comparison found and reproduced an attribute-collision defect in
  buffered calculation. That defect has been fixed in
  `calculate_covariates.R`.
- The strongest regression input is a location object derived from a USDM
  polygon because it deliberately carries colliding `DM`, `date`, and `source`
  fields.

### Recommended download and extraction mock

```r
testthat::test_that(
  "download_data(dataset_name=usdm): installs complete dated shapefile bundles",
  {
    fixture_directory <- testthat::test_path(
      "..", "testdata", "drought", "usdm"
    )
    extracted_files <- character()

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        testthat::expect_length(urls, 2L)
        vapply(destfiles, function(path) {
          dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
          writeBin(charToRaw("mock archive"), path)
          TRUE
        }, logical(1))
        list(success = 2L, failed = 0L, skipped = 0L)
      },
      download_unzip = function(file_name, directory_to_unzip, ...) {
        date <- sub(".*USDM_([0-9]{8})_M\\.zip$", "\\1", basename(file_name))
        files <- list.files(
          fixture_directory,
          pattern = paste0("USDM_", date, "\\."),
          full.names = TRUE
        )
        dir.create(directory_to_unzip, recursive = TRUE, showWarnings = FALSE)
        file.copy(files, directory_to_unzip, overwrite = TRUE)
        extracted_files <<- c(extracted_files, file.path(
          directory_to_unzip,
          basename(files)
        ))
      }
    )

    download_data(
      dataset_name = "usdm",
      date = c("2020-01-07", "2020-01-14"),
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_length(extracted_files, 10L)
    testthat::expect_true(all(file.exists(extracted_files)))
  }
)
```

### Recommended process contracts

```r
usdm <- process_covariates(
  covariate = "usdm",
  path = extracted_directory,
  date = c("2020-01-07", "2020-01-14")
)

testthat::expect_s4_class(usdm, "SpatVector")
testthat::expect_equal(terra::nrow(usdm), 2L)
testthat::expect_true(all(c("DM", "date", "source") %in% names(usdm)))
testthat::expect_equal(
  as.Date(sort(unique(usdm$date))),
  as.Date(c("2020-01-07", "2020-01-14"))
)
testthat::expect_true(terra::same.crs(usdm, "EPSG:4326"))
testthat::expect_true(all(usdm$DM %in% 0:4))
```

### Recommended buffered calculate regression

```r
locations <- terra::centroids(usdm[1L, ])
locations$site_id <- "001"

result <- calculate_covariates(
  covariate = "usdm",
  from = usdm,
  locs = locations,
  locs_id = "site_id",
  radius = 1000L
)

proportion_columns <- paste0("usdm_dm_", 0:4, "_1000")
proportions <- as.matrix(result[, proportion_columns, drop = FALSE])

testthat::expect_s3_class(result, "data.frame")
testthat::expect_equal(nrow(result), 2L)
testthat::expect_true(all(proportion_columns %in% names(result)))
testthat::expect_equal(result$usdm_dm_0, c(2, 2))
testthat::expect_equal(result$usdm_dm_2_1000, c(1, 1))
testthat::expect_true(all(proportions >= 0 & proportions <= 1))
testthat::expect_equal(
  rowSums(proportions),
  c(1, 1),
  tolerance = 1e-6
)
```

## CropScape findings and recommendations

### Findings

- CropScape has USDA ZIP and GMU `tar.gz` variants.
- It calls `archive::archive_extract()` directly, so the ecoregion
  `download_unzip()` mock cannot be reused unchanged.
- Existing tests already check per-file extraction directories, but older
  process and calculate blocks rely heavily on `expect_no_error()` and broad
  inheritance checks.
- Year metadata, CRS, raster values, location IDs, class-fraction bounds, and
  fraction sums provide stronger reproducibility contracts.

### Recommended download and extraction mock

```r
testthat::test_that(
  "download_data(dataset_name=cropscape, source=GMU): extracts process-ready TIFF",
  {
    fixture <- testthat::test_path(
      "..", "testdata", "cropscape", "cdl_30m_r_nc_2019_sub.tif"
    )
    extracted_path <- NULL

    local_download_mocks(
      download_run_method = function(urls, destfiles, ...) {
        testthat::expect_match(urls, "2019_cdls\\.tar\\.gz$")
        writeBin(charToRaw("mock archive"), destfiles)
        list(success = 1L, failed = 0L, skipped = 0L)
      }
    )
    testthat::local_mocked_bindings(
      archive_extract = function(archive, file = NULL, dir = ".", ...) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        extracted_path <<- file.path(dir, basename(fixture))
        file.copy(fixture, extracted_path, overwrite = TRUE)
      },
      .package = "archive"
    )

    download_data(
      dataset_name = "cropscape",
      year = 2019,
      source = "GMU",
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE,
      show_progress = FALSE,
      rate_limit = 0
    )

    testthat::expect_true(file.exists(extracted_path))
    testthat::expect_match(dirname(extracted_path), "2019_cdls$")
  }
)
```

The implemented tests also cover USDA with the expected
`2019_30m_cdls.zip` archive and `2019_30m_cdls` extraction directory.

### Recommended process contracts

```r
cropscape <- process_covariates(
  covariate = "cropscape",
  path = extracted_directory,
  year = 2019
)
metadata <- terra::metags(cropscape)

testthat::expect_s4_class(cropscape, "SpatRaster")
testthat::expect_equal(terra::nlyr(cropscape), 1L)
testthat::expect_gt(terra::ncell(cropscape), 0L)
testthat::expect_true(terra::hasValues(cropscape))
testthat::expect_false(is.na(terra::crs(cropscape)))
testthat::expect_identical(
  unname(metadata[metadata$name == "year", "value"]),
  "2019"
)
```

### Recommended calculate contracts

```r
result <- calculate_covariates(
  covariate = "cropscape",
  from = cropscape,
  locs = locations,
  locs_id = "site_id",
  radius = 300L
)

class_columns <- grep("^cropscape_300_", names(result), value = TRUE)
fractions <- as.matrix(result[, class_columns, drop = FALSE])

testthat::expect_s3_class(result, "data.frame")
testthat::expect_equal(nrow(result), terra::nrow(locations))
testthat::expect_identical(result$site_id, locations$site_id)
testthat::expect_gt(length(class_columns), 0L)
testthat::expect_true(all(fractions >= 0 & fractions <= 1))
testthat::expect_equal(
  rowSums(fractions),
  rep(1, nrow(result)),
  tolerance = 1e-6
)
```

## Package changes completed during this review

1. `download_data()` now dispatches `cropscape`, `cdl`, `drought`, `spei`,
   `eddi`, and `usdm` in the active wrapper.
2. `spei`, `eddi`, and `usdm` aliases automatically select the corresponding
   drought source.
3. Dispatcher regression tests cover all requested aliases.
4. `compare-test-strategies.r` now uses one shared runner and five dataset
   specifications rather than copied benchmark implementations.
5. USDM buffered intersections now retain only a collision-safe internal site
   index from locations, preventing provider-field collisions.
6. A USDM regression test covers colliding attributes and verifies two dates,
   dominant class, five proportions, bounds, and unit row sums.
7. The linked USDM comparison now runs with `radius = 1000`, rather than
   bypassing buffered behavior with `radius = 0`.
8. `test-cropscape.R` now creates process-ready GMU and USDA extraction
   artifacts, exercises all three public wrappers, uses typed raster and data
   frame expectations, validates year/CRS/value contracts, preserves location
   identifiers, and verifies that 300 m class fractions are numeric, bounded,
   nonmissing, and sum to one.

## Recommended implementation priority

The detailed, status-tracked sequence is maintained in
`vignettes/testing-implementation-plan.md`. The priorities below summarize that
implementation plan.

### Priority 1: make routine tests deterministic

- Move all provider-dependent assertions into `*-live.R` files.
- Gate them with `skip_if_no_live_tests()`.
- Ensure default `devtools::test()` performs no remote HTTP, FTP, or archive
  discovery.

### Priority 2: complete linked fixture tests

- Keep one linked download-to-process-to-calculate test for each concrete file
  format, not merely each high-level family.
- Treat SPEI, EDDI, and USDM as distinct specifications.
- Parameterize genuine variants such as CropScape GMU and USDA when their
  archive behavior differs.

### Priority 3: replace weak assertions

- Replace `expect_true(inherits(x, ...))` with `expect_s3_class()` or
  `expect_s4_class()`.
- Replace `expect_true(length(x) > 0)` with `expect_gt()` or `expect_length()`.
- Do not use `expect_no_error()` as the final assertion; validate the returned
  object and its scientific meaning.
- Use exact names, dates, CRS, row counts, IDs, value domains, and invariants.

### Priority 4: standardize failure descriptions

Use:

```text
<function>(<argument=value>, ...): <expected behavior>
```

For example:

```r
testthat::test_that(
  "calculate_drought(source=usdm, radius=1000): proportions sum to one",
  { ... }
)
```

This makes CI failures understandable without opening the test source.

### Priority 5: scale through specifications

For a new source, add a specification containing:

```r
list(
  expected_downloads = 1L,
  download = function(directory) { ... },
  write_download = function(url, destination) { ... },
  extract = function(archive, directory) { ... },
  linked_input = function(state, directory) { ... },
  original_input = function(directory) { ... },
  process = function(path) { ... },
  process_checks = function(processed) { ... },
  calculate = function(processed) { ... },
  calculate_checks = function(result) { ... }
)
```

Do not copy timing, repetitions, CSV generation, or report formatting.

## How to reproduce the evidence

Run focused mocked and unit tests:

```r
devtools::load_all(".")
testthat::test_file("tests/testthat/test-download-dispatch.R")
testthat::test_file("tests/testthat/test-drought-download-mock.R")
testthat::test_file("tests/testthat/test-drought.R")
testthat::test_file("tests/testthat/test-cropscape.R")
```

Run all five comparison specifications:

```sh
Rscript vignettes/scripts/compare-test-strategies.r \
  --repository=. \
  --iterations=5 \
  --output=/tmp/amadeus-comparison-all \
  --dataset=all
```

The outputs are:

- `strategy-comparison-report.txt` — management-readable summary;
- `strategy-comparison-summary.csv` — per-dataset aggregated evidence;
- `strategy-comparison-runs.csv` — iteration-level timing and contracts;
- `simulation-component-map.csv` — simulated versus real boundaries.

## Interpretation for management

The linked tests may be slightly slower than no-op mocks because they perform
real local file operations and more assertions. This small local cost buys:

- no routine dependency on provider uptime;
- reproducible CI behavior;
- coverage of public dispatchers and actual filesystem layouts;
- earlier detection of incompatible stage boundaries;
- explicit validation of scientific outputs;
- faster diagnosis when a test fails;
- an extensible pattern for adding future datasets.

Performance results should therefore be reported as test-suite reliability and
coverage improvements. Any claim about production runtime requires a separate
like-for-like benchmark using equivalent data volumes and environments.

## Known repository-level follow-up

Package loading currently warns that `download_aqs` is exported but absent from
the active working version of `R/download.R`. This is separate from the dataset
changes in this report, but it should be resolved before package release so the
namespace and implementation remain consistent.
