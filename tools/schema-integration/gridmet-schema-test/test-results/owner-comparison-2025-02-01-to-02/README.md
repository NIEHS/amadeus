# Owner comparison: gridMET February 1–2, 2025

This folder records the successful owner run using the same **stated** request as a tester who reported all missing values. It is frozen evidence, not an automated fresh test, a container reproduction, or a resolution of the tester failure.

## What was requested

- All 16 supported native daily variables; exact codes in `request.json`.
- Raleigh and Denver, public coordinates in `sites.csv`.
- February 1–2, 2025, inclusive.
- Expected values: 16 × 2 × 2 = 64.
- Original run ID: `run-ae1adb1cc291ba7a91cf`.

## Observed owner outcome

64 results, zero missing; schema acceptance and JSON/YAML agreement; DuckDB64 results,488total inserted table/junction rows,102SQL checks and successful persistent reopen. Database values agree within the declared tolerances, not bit-for-bit equality for SQL FLOAT.

The 16-case schema pack was run **later during evidence preparation** against a byte-identical copy of the saved schema instance, scientific CSV and acceptance receipt. All16passed. It was not present in the original run, and no downloading/scientific calculation/database loading was repeated to prepare this evidence. The expected malformed-WKT fixture may print an OGR message; case outcomes are recorded in schema-tests.csv.

## Read in this order

1. `request.json` and `sites.csv`: stated input scope.
2. `results.csv`:64ordinary values; for Raleigh tmmx,288.1K and281.1K.
3. `schema-instance.yaml` or `.json`: inspect variable/location/run/asset/value links.
4. `acceptance.json`: original acceptance receipt; its hashes match the unchanged instance/CSV included here.
5. `schema-tests.csv` and `.json`: later record-test pack evidence.
6. `duckdb-results.csv`, `duckdb-receipt.json` and `duckdb-results.sql`: original query results and database checks.
7. `comparison.csv` and `SUMMARY.json`: interpretation and unresolved questions.

These are separate records:64calculated values versus488total inserted rows across metadata/junction/value tables, and102SQL integrity checks, some on empty tables. None is a production certification.

## Comparison limits

The tester reported the same location/date/variable scope and a matching tmmx source checksum. Their saved Raleigh tmmx tile had zero finite cells and ordinary calculation NaN. Their exact pre-write behavior, code/argument identity and final acceptance/database status remain pending. Other-source hashes have not all been independently compared. Separate precipitation experiments are not substituted for this temperature evidence.

The saved run used Amadeus2.0.4/R4.3.2. The current checkout commit recorded in SUMMARY.json was observed at evidence review, not captured at original execution. Identical version labels do not establish identical package source builds. No tester failure is attributed to a particular version based on this folder.

## Repeating the request

From `tools/schema-integration/gridmet-schema-test`, in an installed environment:

```bash
Rscript example_gridmet.R --variables all --start 2025-02-01 --end 2025-02-02 --sites test-results/owner-comparison-2025-02-01-to-02/sites.csv --engine duckdb
```

This is a **new** run with annual downloads unless explicitly directed to an existing source directory. Use its printed run path. On the currently pushed runner the schema test pack is a separate command; the proposed portability update makes it automatic. Check which code revision you run. Follow docs/PARALLEL-TEAM-TEST.md for tests and database queries.

## Storage, privacy and reproducibility

No raw NetCDF, TIFF, R-library, RDS or database binary is included. The CSVs are original query extracts, not a queryable database; run the pilot to create a local database. The frozen instances retain original host-local paths to preserve the original receipt hashes. They describe public test locations but should still be reviewed before sharing. Those paths are provenance, not portable file locations; the frozen instance alone cannot recreate all asset-dependent operations on another machine.

FILE-HASHES.json covers the included evidence files; it detects changes but is not a digital signature. Source code and the schema model remain the existing pilot artifacts. No scientific algorithm, validator, package or runtime dependency is changed by this addition.
