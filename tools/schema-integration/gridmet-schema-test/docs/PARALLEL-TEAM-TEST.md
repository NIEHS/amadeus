# One direct-request workflow for every tester

You and colleagues can test concurrently after the branch is manually pushed. Everyone follows these same instructions, chooses an explicit request and records an individual result. There is no owner-first requirement and no default scientific request.

The branch must exist before cloning. Nothing in this guide pushes code or changes Amadeus functions. Existing local qualification is evidence for earlier requests, not proof of a tester's new request.

## 1. Separate workspace and exact commit

Run in Bash, one command at a time. Stop if a command fails.

```bash
GRIDMET_TEST_WORKSPACE=$(mktemp -d "$HOME/amadeus-gridmet-review.XXXXXX")
git clone --branch amadeus-schema-gridmet-test --single-branch https://github.com/NIEHS/amadeus.git "$GRIDMET_TEST_WORKSPACE/amadeus"
cd "$GRIDMET_TEST_WORKSPACE/amadeus"
git remote get-url origin
git rev-parse HEAD
```

Record the commit. The unique directory allows simultaneous tests even on a shared host/account. Do not share a writable checkout, run output directory or writable source cache. Different commits or environments should be reported as such; do not pull halfway through a test.

## 2. Check or install the environment

An existing approved R/Amadeus environment can be used read-only; record its versions. If installing packages, use an individual library rather than upgrading a shared library while others run:

```bash
mkdir -p "$GRIDMET_TEST_WORKSPACE/R-library"
export R_LIBS_USER="$GRIDMET_TEST_WORKSPACE/R-library"
Rscript -e 'install.packages("pak", repos="https://cloud.r-project.org")'
Rscript -e 'pak::local_install(root=".", upgrade=FALSE, dependencies=NA, ask=FALSE)'
Rscript -e 'install.packages(c("checkmate","digest","ncdf4","jsonlite","yaml","jsonvalidate","DBI","duckdb"), repos="https://cloud.r-project.org")'
```

R and the normal GIS/system prerequisites for Amadeus must be available. Installs concern the optional test environment; DESCRIPTION and NAMESPACE are unchanged. If using an approved existing installation instead, do not run upgrades concurrently in its shared library. Same-host testing does not prove a fresh environment is portable.

```bash
cd tools/schema-integration/gridmet-schema-test
Rscript check_setup.R
```

Optional native R SedonaDB installation is documented in README. DuckDB requires its R package, not the DuckDB CLI. There is no Python runtime adapter or fallback.

## 3. Choose your request explicitly

Prepare a CSV with unique simple site_id values and WGS84 longitude/latitude:

```text
site_id,lon,lat
YOUR_SITE_ID,YOUR_LONGITUDE,YOUR_LATITUDE
```

Use real numeric coordinates in the gridMET domain. The example CSV is only a template; it is never selected automatically:

```bash
mkdir -p local-inputs
cp inputs/sites.csv local-inputs/my-sites.csv
```

Open that copy in your editor and replace/select the sites you want, or use an existing compatible CSV and its path. local-inputs/ and outputs/ are Git-ignored. Keep this terminal open so the variables below remain available.

```bash
read -r -p "Variables (comma-separated codes, or all): " GRIDMET_TEST_VARIABLES
read -r -p "Start date (YYYY-MM-DD): " GRIDMET_TEST_START
read -r -p "End date (YYYY-MM-DD): " GRIDMET_TEST_END
read -r -p "Path to your site CSV: " GRIDMET_TEST_SITES
read -r -p "Engine (duckdb or both for database testing): " GRIDMET_TEST_ENGINE
```

Choose the variables/dates/locations yourself. The supported profile is the16daily codes in inputs/variables.csv, one calendar year per request, WGS84 points and native daily values. 'all' selects those16codes, not every provider product. Dates are inclusive. Each tester can choose a different request; for reproducibility comparisons, intentionally use the same request and commit in separate workspaces.

Use engine duckdb for an end-to-end database test, or both when the optional native R SedonaDB package is installed. The program also supports none for R/schema-only tests and sedonadb for that engine alone; those choices do not demonstrate a DuckDB test.

## 4. Run those inputs through ordinary Amadeus

```bash
Rscript example_gridmet.R --variables "$GRIDMET_TEST_VARIABLES" --start "$GRIDMET_TEST_START" --end "$GRIDMET_TEST_END" --sites "$GRIDMET_TEST_SITES" --engine "$GRIDMET_TEST_ENGINE"
```

All five flags are required. Empty/missing inputs produce a usage error instead of running a previous/sample request. The wrapper passes the R request values to the visible public download_data/process_covariates/calculate_covariates calls; it does not dispatch scientific algorithms from schema metadata. Schema export/validation follows the ordinary scientific results.

For this first live test, do not supply --download-dir. Each run has its own empty raw directory. Annual files are downloaded even for a short date range; plan space/network/time accordingly. This is live retrieval of the selected historical year's files, not necessarily a current-year live feed.

If using sensitive locations, set the appropriate --phi-status declaration documented in README; no_phi is a declared public-test metadata convention, not an automatic privacy assessment. Source-date labels use the documented nominal UTC00 representation; physical sampling intervals and ontology approval remain separate.

## 5. Test and inspect this exact run

Only continue after the example exits successfully. A failed later attempt can leave an older LATEST pointer, so check the path printed by the successful command.

```bash
GRIDMET_TEST_RUN=$(cat outputs/LATEST.txt)
printf '%s\n' "$GRIDMET_TEST_RUN"
Rscript tests/schema_tests.R "$GRIDMET_TEST_RUN"
```

For DuckDB/both:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb docs/results.sql
```

For SedonaDB/both:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" sedonadb docs/results.sql
```

Run only the query commands for the engines you selected. DuckDB opens its saved database read-only. SedonaDB reconstructs a new R context; no persistent SedonaDB file is promised. The queries do not rerun environmental processing.

Inspect results.csv, ordinary/*.rds, schema-instance.json/.yaml, acceptance.json, source-metadata.json, schema-tests.csv and selected database receipts/results. The expected result count comes from your request:

```text
selected variables x location rows x inclusive requested days
```

Do not assume12or96rows. A one-variable,one-site,two-day request expects2; a two-variable,three-site,seven-day request expects42. These are count illustrations, not default requests. Incomplete/nonfinite results fail acceptance rather than being zero-filled.

This executable check calculates expectations from your saved actual R request and checks the selected engine receipts:

```bash
Rscript -e 'a<-commandArgs(TRUE);p<-a[1];engine<-a[2];r<-readRDS(file.path(p,"request.rds"));n<-length(r$variables)*nrow(r$locations)*(as.integer(r$dates[2]-r$dates[1])+1L);s<-jsonlite::read_json(file.path(p,"acceptance.json"));t<-jsonlite::read_json(file.path(p,"schema-tests.json"));m<-jsonlite::read_json(file.path(p,"source-metadata.json"));stopifnot(s$accepted,s$records==n,s$JSON_YAML_agree,t$cases==16,t$failed==0,all(vapply(m,function(x)x$acquisition=="file absent before public downloader call",logical(1))));for(e in if(engine=="both")c("duckdb","sedonadb")else if(engine=="none")character()else engine){d<-jsonlite::read_json(file.path(p,"database",e,"receipt.json"));stopifnot(d$records==n,d$integrity_queries==102,d$all_within_declared_tolerances);if(e=="duckdb")stopifnot(d$persistent_reopen_passed)};cat("Expected and accepted records:",n,"; selected scope passed. Physical-time/ontology and production approval remain separate.\n")' "$GRIDMET_TEST_RUN" "$GRIDMET_TEST_ENGINE"
```

The freshness assertion is for this guide's first live run. For a later deliberate --download-dir reuse experiment, record reuse explicitly and do not apply that freshness assertion as though it were a new download.

## 6. View the database records and run your own SQL

### What needs to be installed?

The branch contains code and pinned schema artifacts, not a prepopulated database. Your successful request creates records on your own machine/workspace.

- DuckDB: R packages DBI and duckdb. No database server or DuckDB command-line executable is needed.
- SedonaDB: the optional native R sedonadb package. No Python adapter is used. Its platform prerequisites/installation link are in README.
- You do not need both engines. Choose duckdb for a saved database, sedonadb for a session-based check, or both to compare both paths. none is R/schema-only and produces no database until you explicitly load one.

For DuckDB packages, in R:

```r
install.packages(c("DBI", "duckdb"), repos = "https://cloud.r-project.org")
```

### Create the database from your request

The request command in section4 already loads the selected engine after schema acceptance. Do not repeat a successful load into the same engine directory; the loader refuses overwrite.

If you originally chose none, or want to add an engine you did not select, load the accepted records explicitly without repeating download/process/calculate:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb
```

For an unselected SedonaDB engine, once its optional native R package is installed:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" sedonadb
```

Run only the load you need. The adapter checks the accepted instance, pinned schema and asset hashes before loading. Record any additional engine in your result sheet.

### Find the files

Use the run directory printed by the successful request; GRIDMET_TEST_RUN was set in section5. For a custom --output base, read that base's LATEST.txt or set GRIDMET_TEST_RUN to the actual printed run path, not another tester's directory.

| Path relative to your run directory | Purpose |
|---|---|
| database/duckdb/gridmet.duckdb | Saved binary DuckDB; open with a database client, not a text editor |
| database/duckdb/results.csv | Readable database result rows |
| database/duckdb/columns.csv | Table/column names for writing queries |
| database/duckdb/results.sql | SQL used for the saved result extract |
| database/duckdb/receipt.json | Actual row count, integrity checks, precision and reopen result |
| database/sedonadb/results.csv | Readable extract from the SedonaDB load |
| database/sedonadb/columns.csv | SedonaDB table/column information |
| database/sedonadb/receipt.json | Actual SedonaDB load/check results |

Open CSV/JSON files directly in VS Code. There is no persistent SedonaDB database file in this implementation: its query command reconstructs the accepted records in a fresh native R session. Do not try to open the DuckDB binary as CSV/text.

### Query actual records

For a saved DuckDB:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb docs/results.sql
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb 'SELECT COUNT(*) AS records FROM "AmbientValueAtLocation"'
```

The first command displays values with variable, site, nominal date label, units, asset and processing-run links. The second counts stored results. Compare the count to your variables x sites x inclusive days, not a fixed sample number. DuckDB queries open the database read-only.

For SedonaDB:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" sedonadb docs/results.sql
Rscript R/database.R "$GRIDMET_TEST_RUN" sedonadb 'SELECT COUNT(*) AS records FROM "AmbientValueAtLocation"'
```

Each SedonaDB query reconstructs a separate context and runs checks before executing the query. It can be slower than opening the saved DuckDB, but does not repeat environmental processing. It does not overwrite the original load's result files. No persistent SedonaDB connection is shared between testers.

### Write your own SQL

Create a file such as local-inputs/my-query.sql in your editor. Example linking actual values to their calculation run and result asset:

```sql
SELECT p.amadeus_variable_code AS variable,
       l.location_key AS site_id,
       CAST(v.valid_time_start AS VARCHAR) AS nominal_date_label,
       v.value,
       v.value_unit_ucum,
       r.function_name,
       a.sha256 AS result_asset_sha256
FROM "AmbientValueAtLocation" v
JOIN "ProductVariable" p ON v.product_variable = p.id
JOIN "Location" l ON v.location = l.id
JOIN "ToolRun" r ON v.processing_run = r.id
JOIN "Asset" a ON v.asset = a.id
ORDER BY p.amadeus_variable_code, l.location_key, v.valid_time_start;
```

Run it on an engine you have loaded:

```bash
Rscript R/database.R "$GRIDMET_TEST_RUN" duckdb local-inputs/my-query.sql
```

Use sedonadb instead for that engine. The referenced asset here is the saved calculation result; upstream runs/input assets provide further lineage to processing and provider files. Inspect columns.csv for additional fields/tables. The two core engines should preserve the accepted records within declared numeric tolerances; this does not qualify spatial SQL extensions or all empty schema classes.

### What demonstrates a successful database test?

Require acceptance.json with accepted=true, the expected request-derived record count, successful schema-tests.json and the selected engine's receipt. Check records, integrity_queries, all_within_declared_tolerances and, for DuckDB, persistent_reopen_passed. The loader checks102generated queries, some against empty tables. A binary file's existence or an old CSV alone is not acceptance evidence. Scientific values should agree; generated SQL FLOAT storage can round within inputs/variables.csv tolerances.

Preserve the first meaningful error if loading/querying fails. Do not overwrite a failed database directory or edit accepted records to hide the error. These outputs are local test results, with production/scientific-policy approval still separate.

## 7. Report independently, then compare

Fill docs/TEAM-RESULTS.csv with reviewer, commit, versions, exact request, fresh/reused source status, expected/actual counts, schema-test failures, selected engines and findings. Each person keeps a separate worksheet/result file; do not concurrently write to one shared CSV.

Compare invariant outcomes and values for identical requests, not run IDs or file hashes for whole instances: run-specific IDs, paths and timestamps intentionally differ. For identical source bytes/package/options/request, scientific values should agree; database rounding is governed by the declared per-variable tolerances. Different requests need different expected counts, so their numerical values should not be treated as an equality test.

Keep session-info.txt and the first meaningful error. Do not edit accepted records to make tests pass. Do not commit annual files, private inputs or outputs. Human approval is pending until actually recorded. Passing16cases and102queries does not certify every schema class, physical time meaning, arbitrary failures, full-variable scale or production readiness.

Everyone can run in parallel after the manual push. No separate owner-first/default exercise is required.
