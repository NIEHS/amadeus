# Test Reports

This directory holds reproducible reports about the amadeus test suite.

| File | Purpose |
|---|---|
| `test_report.Rmd` | Renders quality scorecards over the whole suite (assertion quality, naming, mocked-binding inventory, untested files, …). |
| `cran_checklist.Rmd` | CRAN readiness assessment for the current state of the package. |
| `render_report.R` | Helper that renders `test_report.Rmd` with the correct knit root. |

Render either report with:

```bash
Rscript -e 'rmarkdown::render("tests/test_report/test_report.Rmd",   knit_root_dir = getwd())'
Rscript -e 'rmarkdown::render("tests/test_report/cran_checklist.Rmd", knit_root_dir = getwd())'
```

Generated HTML/PDF outputs are git-ignored; only `.Rmd` sources and
`render_report.R` are committed.

---

## Testing Protocol (Human-Readable Guide)

This is the canonical, human-readable description of how testing works in
`amadeus`. For machine-actionable contributor docs see `vignettes/testing.Rmd`.
For the AI testing agent's system prompt see `agents/test-agent.md`.

### Two tiers of tests

| Tier | File pattern | When it runs | Network? | Credentials? |
|---|---|---|---|---|
| **Mocked / fixture** | `tests/testthat/test-<dataset>.R` | Every CI run, every `devtools::test()`, CRAN | No (mocked) | No |
| **Live API** | `tests/testthat/test-<dataset>-live.R` | Scheduled (weekly) + `workflow_dispatch` via `.github/workflows/test-live.yaml` | Yes | Yes (read from env vars) |

The split is enforced by:

- **`skip_if_no_live_tests()`** — defined in `tests/testthat/helper-skips.R`,
  reads `AMADEUS_LIVE_TESTS`. The live workflow sets it to `"true"`; no other
  workflow does, so live tests skip everywhere else.
- **File-name regex** — the live workflow runs
  `testthat::test_dir(filter = "-live$")`, picking up only `test-*-live.R`.

### Helpers (auto-loaded by testthat)

| File | Provides |
|---|---|
| `helper-skips.R` | `skip_if_no_live_tests()`, `skip_if_no_credentials(var)`, `skip_if_pkg_missing(pkg)` |
| `helper-mocks-download.R` | `mocks_download_stack()`, `mocks_token_stack()`, `local_download_mocks()`, `local_token_mocks()` — factories for the network/IO stack used by every `download_*` |
| `helper-mocks-process.R` | Canned terra/sf objects and file-listing mocks for `process_*` tests |
| `helper-fixtures.R` | `fixture_spatraster`, `fixture_points`, `fixture_aoi`, `fixture_dates` — canonical small inputs |

### Mocking convention

Every mocked `download_*` test uses `testthat::local_mocked_bindings(...,
.package = "amadeus")` to intercept the network stack. The convenience wrapper
`local_download_mocks()` collapses the common boilerplate:

```r
testthat::test_that(
  "download_aqs(hash=TRUE): returns hash string",
  {
    local_download_mocks(hash_value = "abc")
    out <- amadeus::download_aqs(
      year = 2022, hash = TRUE,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    )
    testthat::expect_equal(out, "abc")
  }
)
```

Override any binding inline:

```r
local_download_mocks(success = 0L, failed = 1L)
```

For Earthdata-style flows that hit `get_token()`:

```r
local_token_mocks(token = "test-token")
```

### Naming convention

Test descriptions encode the input combination under test:

```
test_that("<fn>(<arg=value>, ...): <expected behavior>", { ... })
```

Examples:

- `"download_aqs(resolution_temporal='daily', hash=TRUE): returns hash string"`
- `"download_geos(collection=<bad>): errors on bad collection"`
- `"process_modis_swath(path=<missing>): errors on non-existent path"`

For matrix-style cases use `patrick::with_parameters_test_that()` so each row
appears as a separate test in failure output.

### Assertion conventions

- Always namespace: `testthat::expect_*`.
- Prefer typed expectations (see table in `vignettes/testing.Rmd`).
- The advisory linter `tests/lint_tests.R` flags weak patterns:
  - `expect_true(inherits(x, "C"))` → use `expect_s3_class` / `expect_s4_class`
  - `expect_true(file.exists(p))` alone → add `expect_gt(file.info(p)$size, 0)`
  - `expect_true(length(x) > 0)` → use `expect_gt(length(x), 0)` or `expect_length`
  - `expect_no_error(f(...))` → assign result and assert on its class/value

Run advisory:

```bash
Rscript tests/lint_tests.R           # advisory: always exit 0
Rscript tests/lint_tests.R --strict  # fail build on any hit
```

### Adding tests for a new dataset

1. Add mocked tests in `tests/testthat/test-<name>.R` using
   `local_download_mocks()` / `local_token_mocks()` and the fixture helpers.
2. Add a live test in `tests/testthat/test-<name>-live.R`. The first lines
   must be `skip_if_no_live_tests()` (and `skip_if_no_credentials(...)` if
   credentials are required).
3. If the live test needs credentials, add the env var to
   `.github/workflows/test-live.yaml`.
4. Re-render `tests/test_report/test_report.Rmd` and inspect the scorecards.
5. Verify with:
   ```bash
   Rscript -e 'testthat::test_dir("tests/testthat", filter="<name>")'
   ```

### Removing or modifying tests

- Never delete a `test-*-live.R` file just because credentials are unavailable
  locally — it will skip cleanly.
- When refactoring a mocked test, re-render `test_report.Rmd` and confirm the
  "weak assertion %" and "naming audit" scorecards do not regress.

### Running the suite

| Goal | Command |
|---|---|
| Default (mocked) | `Rscript -e 'devtools::test()'` |
| Filter to one file | `Rscript -e 'testthat::test_file("tests/testthat/test-aqs.R")'` |
| Filter by regex | `Rscript -e 'testthat::test_dir("tests/testthat", filter="aqs")'` |
| Live tests only (locally) | `AMADEUS_LIVE_TESTS=true Rscript -e 'testthat::test_dir("tests/testthat", filter="-live$")'` |
| Coverage | `Rscript -e 'covr::package_coverage()'` |
| Lint (whole package) | `Rscript -e 'lintr::lint_package()'` |
| Test-assertion lint | `Rscript tests/lint_tests.R` |

### CI matrix

| Workflow | Triggers | What it runs |
|---|---|---|
| `check-standard.yaml` (R-CMD-check) | push, PR | `rcmdcheck::rcmdcheck()` |
| `test-coverage-local.yaml` | push, PR, daily cron | `covr::package_coverage()` over mocked suite |
| `test-live.yaml` | weekly cron, `workflow_dispatch` | `testthat::test_dir(filter = "-live$")` with credentials |
| `lint.yaml` | push, PR | `lintr::lint_package()` + `tests/lint_tests.R` (advisory) |
| `pkgdown.yaml` | push to main | Builds documentation site |
