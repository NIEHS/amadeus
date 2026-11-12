# Run from the review branch

All testers, including the author, follow the same steps. Use a writable clone. Commands below are Bash commands unless marked R.

## Clone

```bash
git clone --single-branch --branch amadeus-schema-storage-boundary-review https://github.com/NIEHS/amadeus.git amadeus-storage-review
cd amadeus-storage-review
git rev-parse HEAD
cd tools/schema-integration/gridmet-storage-boundary
```

Use SSH instead if configured. NIEHS repository permissions/SSO are separate from the pilot. GitHub account passwords do not authenticate HTTPS Git pushes.

## Dependencies

Use an installed, compatible Amadeus and its scientific dependencies. In R, install the integration/setup packages if missing:

```r
install.packages(c("terra", "sf", "jsonlite", "yaml", "digest", "jsonvalidate", "ncdf4", "DBI", "duckdb"))
```

The repository clone is source code, not an installed R package. Follow the repository's Amadeus installation instructions, or run `R CMD INSTALL .` from the clone root after its package dependencies are installed. Record which installed Amadeus is used; cloning this branch does not silently replace it. Do not patch process_gridmet or change md/window behavior without reporting the exact change.

SedonaDB is optional and not qualified in this latest branch. Parquet I/O uses DuckDB: arrow/nanoparquet, Python and an AI service are not required by this runtime. The existing setup gate requires all nine packages above plus Amadeus. Compiled spatial/database libraries and filesystem behavior still matter; no universal minimum version or platform guarantee is claimed.

```bash
mkdir -p outputs raw-cache
Rscript integration/check_setup.R outputs
```

## Saved-record demonstration (no provider download)

```bash
Rscript integration/R/database.R demo/snapshot duckdb 'SELECT id,value,value_unit_ucum,processing_run FROM "AmbientValueAtLocation" ORDER BY id'
```

Expected: four historical native results after automatic publication verification. Inspect demo/snapshot/results.csv, schema-instance.json, source-metadata.json, provenance/, acceptance.json, publication-manifest.json and workflow-receipt.json. This is a relocated author-generated snapshot; it is not a fresh execution or a tester success. Its absolute raw source paths are historical and do not need to exist for saved queries. Do not edit published artifacts.

## Supply your direct scientific request

Create `inputs/my-sites.csv` using a plain-text editor or the following Bash command. Each row is a site; longitude/latitude are decimal degrees, EPSG:4326. Use simple unique site IDs without slashes.

```bash
cat > inputs/my-sites.csv <<'CSV'
site_id,lon,lat
raleigh,-78.6382,35.7796
denver,-104.9903,39.7392
CSV
```

Change sites, variables and dates to your scientific request within the tested profile. Source cache must exist before execution. Leave it empty for ordinary provider downloads or populate variable/variable_YEAR.nc from a trusted cache. Cache reuse is explicitly recorded and is not described as a new download.

```bash
sh tests/replay.sh outputs/my-request-01 tmmx,pr 2025-02-01 2025-02-02 inputs/my-sites.csv raw-cache
```

For this exact request, expect 2 variables × 2 sites × 2 days = 8 finite values. Each run needs a NEW output path. For a different request, compute the expected count from its actual variables/sites/days. The test caller performs ordinary Amadeus scientific calls, explicit recording, then post-calculation export. It also captures environment, runs a verified saved query and performs separate direct-source QA. Source QA needs the original bytes; saved queries do not.

The source download can be annual even for two requested days; all-variable runs may require substantial disk/network/time. Start small. The tested profile is 16 native daily variables, point extraction, radius 0, mean, no temporal aggregation and one year per request. Broader spatial/scientific settings are not qualified here.

## All 16 qualified native daily variables

```bash
sh tests/replay.sh outputs/my-full-01 tmmx,tmmn,pr,pet,etr,rmax,rmin,sph,vpd,srad,vs,th,erc,bi,fm100,fm1000 2025-02-10 2025-02-16 inputs/sites.csv raw-cache
```

The included sites.csv has Raleigh, Denver and Phoenix: expect 336 values. Seventeen additional recognized package codes are outside this qualification. No conversion or monthly aggregation is introduced.

## Inspect your fresh results and query DuckDB

```bash
cat outputs/my-request-01/replay-summary.txt
cat outputs/my-request-01/query.log
cat outputs/my-request-01/LATEST.txt
RUN=$(cat outputs/my-request-01/LATEST.txt)
Rscript integration/R/database.R "$RUN" duckdb 'SELECT id,value,value_unit_ucum,processing_run FROM "AmbientValueAtLocation" ORDER BY id LIMIT 20'
Rscript integration/R/database.R "$RUN" duckdb 'SELECT table_name FROM information_schema.tables WHERE table_schema = current_schema() ORDER BY table_name'
```

Inspect the run's results.csv, source-metadata.json, schema-instance.json, provenance/, acceptance.json, database/duckdb/receipt.json, publication-manifest.json and workflow-receipt.json. The actual database is database/duckdb/gridmet.duckdb; accepted-values.parquet sits beside it on the default path. SQL checks include empty collections and do not prove every class was exercised.

Use the supported query entry point above to enforce publication verification. A direct DBI/IDE connection bypasses that protection. No Sedona installation is needed for this DuckDB release.

## Portable publication/rejection tests

```bash
Rscript tests/publication_tests.R demo/snapshot scratch/publication-01
```

Expected: 26 checks pass, including unaltered snapshot acceptance and altered/incomplete publication rejection. Tests modify owned copies only; use a new scratch path. Report is scratch/publication-01/reports/publication-tests.csv. You may substitute a completed fresh run path for demo/snapshot. These tests qualify publication integrity, not every semantic rule or scientific truth. The test adaptation changes paths only; runtime fingerprints remain identical.

## Return your evidence

Record clone commit (`git rev-parse HEAD`), actual request, environment.json, source hashes, counts/missingness, acceptance/completion, SQL results and source QA status. Complete review/independent-replay-results.csv in your own working copy and submit through the agreed review process. Complete scientific/unit and AI/human review sheets only when actually reviewed; do not prefill approval from a test pass. Raw caches and outputs are ignored by Git. Submit selected logs/receipts under a new reviewed evidence folder only after checking content and scope.

If a run fails, retain its output and environment evidence. Do not replace NA values, disable rules, rewrite receipts or rerun into the same directory. Distinguish ordinary processing failure, schema rejection, database publication failure, verified-open failure and separate source-QA failure. Inspect replay.log, environment.log, query.log and source-check.log.
