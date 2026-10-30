# gridMET: explicit Amadeus request and schema integration

**Everyone follows the same [parallel team test guide](docs/PARALLEL-TEAM-TEST.md) after the branch is manually pushed.** There is no owner-first sequence and no default variables, dates, sites or engine. Each tester chooses an explicit request and uses a separate workspace/output directory.

Amadeus's scientific package is unchanged. The visible public calls in example_gridmet.R perform downloading, processing and calculation. An explicit external hand-off then constructs/validates schema records and optionally loads native R database engines. No earlier pilot drivers, outputs, caches, checkpoints, fault hooks or personal library paths are required.

## Required request inputs

```text
Rscript example_gridmet.R --variables CODE[,CODE...]|all --start YYYY-MM-DD --end YYYY-MM-DD --sites YOUR.csv --engine none|duckdb|sedonadb|both
```

Supply real values for all five flags; these placeholders are not a runnable request. `Rscript example_gridmet.R --help` prints usage without loading the scientific package or contacting the provider. Missing/empty choices fail rather than silently selecting a sample request.

The R values go to ordinary Amadeus calls:

```r
amadeus::download_data(dataset_name = "gridmet", variables = request$variables, year = request$year, directory_to_save = request$download_dir, ...)
amadeus::process_covariates(covariate = "gridmet", variable = code, date = as.character(request$dates), path = ..., extent = ...)
amadeus::calculate_covariates(covariate = "gridmet", from = from, locs = locs, locs_id = "site_id", radius = 0, fun = "mean", .by_time = NULL, geom = FALSE)
```

These snippets explain the boundary; the full executable argument lists are in the example. The code fixes this declared point-extraction profile, not a universal dataset dispatcher. Schema metadata does not select a different scientific algorithm.

Choose16daily-profile codes from inputs/variables.csv, one calendar year per request, inclusive dates, and a CSV with unique simple site_id,lon,lat columns in WGS84 and the gridMET domain. inputs/sites.csv is an optional template only and is not automatically used. `all` explicitly selects the16listed daily variables; PDSI/other provider products are excluded. No numeric conversions, monthly products or zero filling are performed.

Optional inputs are --output DIR, --download-dir DIR and --phi-status no_phi|aggregated_no_phi|phi_present. Without --output, each run has a new folder under outputs/. Without --download-dir, raw files start in that new folder and the provider is contacted. A supplied download directory deliberately uses Amadeus's existing-file behavior; freshness/reuse is recorded. Even a short request downloads annual files. Plan network/disk/time for selected variables and keep writable caches separate across concurrent testers.

The example sets UTC in its own process and explicitly materializes bounded FLT8S tiles. Time values are nominal UTC00 date labels, not verified physical sample intervals. no_phi is a public-test declaration, not a privacy assessment. See [validation and limits](docs/VALIDATION-AND-LIMITS.md).

## Environment

From the cloned NIEHS/amadeus root, use the approved existing environment read-only or install into your individual library as described in the shared guide. Install the checked-out package and required dependencies with [pak local installation](https://pak.r-lib.org/reference/local_install.html). Extra test tools are checkmate,digest,ncdf4,jsonlite,yaml,jsonvalidate and optionally DBI/duckdb. Amadeus's normal sf/terra/GIS system prerequisites still apply. The check_setup.R script reports versions/capabilities; it never installs software.

The optional dependencies belong to this example, not Amadeus's DESCRIPTION or NAMESPACE. Normal scientific package use does not load these helpers. Native R SedonaDB is optional: [official installation](https://sedona.apache.org/sedonadb/latest/r/index.html) documents `install.packages("sedonadb", repos="https://community.r-multiverse.org")` and platform/build requirements. No Python fallback, uv, just or DuckDB CLI is needed by runtime testers. LinkML/Python is developer-build tooling only for changing the pinned model release.

Database package requirements, file locations, count queries and a provenance SQL example are detailed in [the shared guide, section6](docs/PARALLEL-TEAM-TEST.md#6-view-the-database-records-and-run-your-own-sql).

## Test and query your run

After your request command succeeds, use its printed run directory. For the ordinary output base:

```bash
GRIDMET_TEST_RUN=$(cat outputs/LATEST.txt)
Rscript tests/schema_tests.R "$GRIDMET_TEST_RUN"
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb docs/results.sql
```

Run DuckDB query only when DuckDB was selected; for SedonaDB use engine sedonadb. For a custom --output base, take LATEST.txt from that base or use the printed run path. A previous successful LATEST pointer can survive a later failure, so stop on error instead of accidentally testing an older run.

DuckDB opens its saved database read-only. A SedonaDB query reconstructs a fresh native R session; no saved SedonaDB database is claimed. Loading the same accepted run into a new, previously unselected engine is possible with `Rscript R/database.R RUN_DIR ENGINE`; existing output directories are not overwritten. Each selected engine uses the pinned core/WKT schema and102generated integrity queries, plus record/value/unit comparisons. Spatial SQL is not qualified.

Expected records = selected variables x requested locations x inclusive days. Counts are calculated from the actual saved request. Do not assume the count from a different tester or earlier qualification. The16schema cases include14intended rejections and2positive cases; descriptive text is deliberately not an automatic scientific truth check.

## Outputs and reporting

| Run path | Meaning |
|---|---|
| request.rds / requested-sites.csv | Actual user request and points |
| raw/ | Provider files unless explicitly directed elsewhere |
| processed/ | Bounded tiles and any metadata companions |
| ordinary/ | Unchanged Amadeus result objects saved as RDS |
| results.csv | Native scientific values with variables/sites/dates |
| calls.rds / source-existed-before.rds | Call context and truthful source reuse evidence |
| source-metadata.json | Source units, attributes, grid spacing, hashes and acquisition mode |
| schema-instance.json / .yaml | Linked catalog/request/location/asset/run/value records |
| acceptance.json | Accepted hand-off receipt; production approval false |
| schema-tests.csv / .json | Actual positive/negative test outcomes |
| database/ENGINE/ | Query rows, columns and integrity/precision receipt |
| database/duckdb/gridmet.duckdb | Saved/reopened DuckDB |
| session-info.txt | Actual environment information |

Results/inputs stay in each person's own workspace. Fill your own docs/TEAM-RESULTS.csv and compare commits/requests/outcomes after tests finish. For matching request/source/package/options, compare scientific values and declared database tolerances; run IDs, paths and timestamps intentionally differ. Do not concurrently edit one shared result sheet or commit raw/private/output data.

## Files and responsibility

| File | Role |
|---|---|
| example_gridmet.R | Explicit request choices, ordinary public calls, then visible optional hand-off |
| R/schema_export.R | Metadata construction, source inspection, serialization, IDs/hashes |
| R/schema_validator.R | Generated R/JSON constraints plus explicit relationship/date/geometry/native-value checks |
| R/database.R | Optional native R loading/checks/query; no scientific calculation |
| check_setup.R | Read-only dependency/capability check |
| tests/schema_tests.R | Actual-record and in-memory invalid-fixture QA |
| schema/ | Pinned model/generated contracts/DDL with provenance; no runtime YAML reflection |
| inputs/ | Declared daily-variable profile and optional site template |
| air.toml | Developer formatting only |

Existing observations in docs/LOCAL-QUALIFICATION.json are not proof of your new request. The all16-variable profile, fresh-machine portability, scientific time/ontology interpretation and production operations require their own review. There is no checkpoint/fault/publication service inherited from earlier experiments. Ordinary package behavior is unchanged, while external argument selection, materialization and acceptance gates remain explicit influences.
