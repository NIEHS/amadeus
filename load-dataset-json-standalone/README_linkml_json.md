# Standalone dataset JSON loader

This folder is a portable development and test bundle for `load_dataset_json.R`.
Copy or zip **this entire folder**; no Amadeus checkout, installed Amadeus package,
Git configuration, Excel files, credentials, or provider downloads are required.
The R loader, serializer, Python bridges, shared helpers, schemas, catalogs, and
regression tests are unchanged source snapshots. `SOURCE_MANIFEST.json` records
their SHA-256 hashes. Do not copy a virtual environment between computers.

The supplied [portability report](verification/portability_report.json) records
successful tests after copying the bundle outside the original checkout, into
a path with spaces. Reference test logs are under `verification/loader-tests/`.
This check reused installed runtimes/packages; it did not test a fresh internet
installation. New test runs write separate results under `test-results/`.

## 1. Requirements and setup

Install R and Python 3.12 on the recipient's computer. The reference environment
was R 4.6.0, `yaml` 2.3.12, `jsonlite` 2.0.0, and Python 3.12.15. Other R versions
may work; run the tests to verify. `testthat`, `reticulate`, `just`, and `uv` are
not required. All dependency declarations are inside this folder.

Open a terminal **in this folder**. On macOS/Linux:

```sh
python3.12 -m venv .venv-catalog
.venv-catalog/bin/python -m pip install -r requirements.txt
Rscript --vanilla setup.R
Rscript --vanilla run_tests.R --python .venv-catalog/bin/python
```

On Windows PowerShell:

```powershell
py -3.12 -m venv .venv-catalog
.\.venv-catalog\Scripts\python.exe -m pip install -r requirements.txt
Rscript --vanilla setup.R
Rscript --vanilla run_tests.R --python .venv-catalog/Scripts/python.exe
```

`setup.R` installs missing R packages from CRAN, with the repository specified
explicitly, and prints installed versions. Python's four direct dependencies are
pinned in `dataset_spec_objects/catalog_requirements.txt`: LinkML 1.12.1,
linkml-runtime 1.12.1, jsonschema 4.26.0, and PyYAML 6.0.3. Pip installs their
transitive dependencies. To reproduce the full reference Python environment,
use `-r requirements-tested.txt` instead; that snapshot includes development
extras that the loader does not require. Run `python -m pip check` using the
chosen interpreter after installation if dependency compatibility is uncertain.

Package installation requires internet access and may need platform build tools
if binary packages are unavailable. Package distributions and interpreter
executables are **not vendored**; this is independent of project files, not an
offline software installer. Once dependencies are installed, generation and
tests use bundled files locally. No shell activation is necessary.

## 2. Generate and read a catalog

Start R in this folder:

```r
source("linkml-json-iplementation/R/load_dataset_json.R")
catalog <- jsonlite::read_json(load_merra2_json(), simplifyVector = FALSE)
length(catalog$products)           # 7
length(catalog$product_variables)  # 45

path <- load_aqs_json()
receipt <- jsonlite::read_json(attr(path, "validation_report"))
stopifnot(identical(receipt$status, "PASS"))

# Any included dataset can use the same implementation:
path <- load_dataset_json("huc")
load_prism_json <- make_dataset_json_loader("prism")
```

The loader discovers `.venv-catalog` inside this standalone folder. Alternatively,
pass `python = "/path/to/python"` or set `AMADEUS_CATALOG_PYTHON` explicitly.
After sourcing, calls work from a different working directory. Source using an
absolute path if R starts elsewhere. Generated files default to
`linkml-json-iplementation/output/single-dataset/<dataset>.json` and
`<dataset>.validation.json`. They contain catalog metadata, not observations.
Each call regenerates and validates; reuse the returned catalog outside loops.

For example, choose an output directory without changing the bundled catalogs:

```r
path <- load_merra2_json(output_file = file.path(tempdir(), "merra2.json"))
catalog <- jsonlite::read_json(path, simplifyVector = FALSE)
```

### Reuse JSON that has already been generated

**Read the existing file directly for discovery or routine workflow use.**
`load_dataset_json()` and its wrappers always regenerate and validate; they
currently have no read-only or cache mode. Sourcing `load_dataset_json.R` alone
does not regenerate anything, but it is unnecessary when only reading JSON.
Existing JSON can be read with R and `jsonlite`, without Python or LinkML.

Run this example from the standalone folder after generating MERRA-2 JSON:

```r
# Single-dataset loader output:
catalog_file <- "linkml-json-iplementation/output/single-dataset/merra2.json"
# Alternatively, after the complete build:
# catalog_file <- "linkml-json-iplementation/output/catalogs/merra2.json"
stopifnot(file.exists(catalog_file))
catalog <- jsonlite::read_json(catalog_file, simplifyVector = FALSE)

# Discovery: include every field present in any product-variable binding.
fields <- unique(unlist(lapply(catalog$product_variables, names)))
variables <- do.call(rbind, lapply(catalog$product_variables, function(binding) {
  values <- lapply(fields, function(field) {
    value <- binding[[field]]
    if (is.null(value)) return(NA_character_)
    paste(unlist(value, use.names = FALSE), collapse = "; ")
  })
  names(values) <- fields
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}))
head(variables)
selected <- catalog$product_variables[[1L]]
product <- Filter(function(x) identical(x$id, selected$product), catalog$products)
```

This display table includes all fields populated anywhere in MERRA-2's
`product_variables`, including fields absent from some bindings. Its columns are
text; arrays are joined with `; `, empty arrays display as empty strings, and
missing/null fields display as `NA`. Keep `catalog` for original types and array
structure. Product, grid, and canonical-variable attributes remain in their own
collections; they are not automatically joined into this binding table.

For cross-dataset discovery, read `output/catalogs/global_catalog.json` instead
(relative to `linkml-json-iplementation/`). It uses the same collections. Follow
each binding's `canonical_variable` and `product` IDs to the corresponding
records to inspect definitions, units, coverage, access details, and limitations.
An empty gap list means no recorded flags; it does not certify scientific
correctness or adapter support.

Read the catalog **once per workflow** and reuse the resulting list:

| Use | How to reuse the metadata |
|---|---|
| Standalone discovery | Search canonical variables and inspect their dataset bindings before selecting a product. No installed Amadeus package is needed. |
| `download_data(dataset_name = ...)` | Resolve the selected product into the existing source-specific collection, version, and date arguments. Check recorded coverage/access requirements; retain the source-specific URL and filename logic. |
| `process_covariates(covariate = ...)` | Use selected binding metadata to check native variable names, units, packing, fill values, and grid information before applying the existing processing logic. Apply scale/offset only at the appropriate stage to avoid double scaling. |
| `calculate_covariates(covariate = ...)` | Check spatial support, temporal meaning, extensivity, and unresolved gaps before choosing supported extraction and aggregation options. Record the selected product/binding IDs with results. |

The current Amadeus wrappers do **not** accept a `catalog` argument or consume
these JSON files automatically. Using them within those functions requires a
small adapter/helper that reads or receives the already-loaded catalog, maps
catalog IDs to existing arguments, and performs the relevant checks. Do not pass
canonical IDs directly as native variable names. Execution requires Amadeus and
its normal dependencies; those download/process/calculate functions are not
included in this standalone loader bundle.

Use a trusted, previously validated JSON snapshot and retain its validation
report. Reading JSON checks parsing, not schema conformance or freshness. After
changing catalog YAML or the LinkML model, explicitly regenerate with
`load_dataset_json()` (or the batch pipeline for the global catalog), then read
the new JSON. Refresh an in-memory catalog after replacing its source file.

## 3. Files and architecture

| Location | Purpose |
|---|---|
| `linkml-json-iplementation/R/load_dataset_json.R` | Generic API, dataset wrappers, and orchestration. |
| `linkml-json-iplementation/R/convert_catalog.R` | R serialization preserving schema-defined arrays, scalars, nulls, and precision. |
| `linkml-json-iplementation/python/{dataset_bridge,linkml_bridge}.py` | Unchanged LinkML compilation, validation, reference checks, parity checks, and safe publication. |
| `dataset_spec_objects/catalog_workflow.py` | Existing strict YAML loading, catalog relationships, and global union helpers. |
| `dataset_spec_objects/catalog_updates_20261109/read_catalog.R` | Existing YAML reader with large-integer safeguards. |
| `validated_catalog_handles/validate_catalogs.py` | Existing dataset registry, reused by the batch bridge. |
| `amadeus_schema/src/amadeus_schema/schema/*.yaml` | Shared LinkML model and all six local schema modules. |
| `global_dataset_catalogs/*.yaml` | 23 dataset catalogs and the global catalog snapshot. |
| `linkml-json-iplementation/output/amadeus_schema.schema.json` | Generated JSON Schema fixture used by tests; the loader recompiles its own schema on each call. |
| `linkml-json-iplementation/tests/` | Existing R serialization/integration and Python validation tests. |
| `setup.R`, `run_tests.R` | Standalone dependency setup and combined test runner. |

The internal layout intentionally matches the original project so no bridge
functions need modification. Paths resolve inside this folder. The shared
helper files contain additional original project commands; **do not invoke their
standalone CLIs here**. Only their imported functions used by this loader and
the included batch pipeline are supported. Historical evidence audits and
provider adapters are outside this bundle.

The original [README_load_dataset_json.md](linkml-json-iplementation/R/README_load_dataset_json.md)
is included unchanged. In that snapshot, “repository root” means this folder;
pre-existing `output/catalogs` files are generated by the optional batch command
below, and proposed package integration features are not implemented here.

## 4. Develop and test

```sh
Rscript --vanilla run_tests.R --python .venv-catalog/bin/python
```

The runner uses explicit paths and records command logs, R/Python versions, and
the overall result under `test-results/`. A failed test exits nonzero. Expected
coverage: 18 R serialization checks, 29 R loader integration checks, and 10
Python tests. Integration tests use private temporary inputs/outputs and cover
invalid types/enums, required fields, broken YAML/schema, dangling references,
null preservation, overwrite protection, and preserving previous valid JSON.
Python tests additionally cover generated conditional rules, asset/binding
consistency, duplicate YAML keys, non-finite JSON, and dependency hash changes.

Tests use a bundled generated schema fixture. After intentionally changing the
LinkML model, refresh it before testing:

```sh
.venv-catalog/bin/python -c "import sys; sys.path.insert(0, 'linkml-json-iplementation/python'); import linkml_bridge as b; b.write_json(b.IMPLEMENTATION / 'output/amadeus_schema.schema.json', b.compile_schema())"
```

Edit the generic implementation once; wrappers created by
`make_dataset_json_loader()` share its logic. To add a dataset, provide a
self-contained YAML catalog with all referenced records and use its filename
stem with `load_dataset_json()`. Extending the **batch** collection also requires
updating the bundled `DATASETS` registry and global union. A dataset YAML is an
instance of the shared model, not a separate LinkML schema.

`SOURCE_MANIFEST.json` identifies the original snapshot; deliberate development
edits will change those hashes. Keep reviewed catalogs as the source and
regenerate JSON. Never hand-edit a validation receipt to claim success.

## 5. Optional complete build and validation limits

All 24 YAML inputs are bundled, so the unchanged batch pipeline also works:

```sh
Rscript --vanilla linkml-json-iplementation/run_pipeline.R --python .venv-catalog/bin/python
```

It generates `output/catalogs/*.json`, `output/amadeus_schema.schema.json`,
`conversion_plan.json`, `r_conversion_report.json`, `validation_report.json`,
and `manifest.json` inside `linkml-json-iplementation/`. Expect 24 source and
24 output documents checked with report status `PASS`. Plans and receipts
contain absolute paths for the current run; regenerate them after moving the
folder. Do not distribute a virtual environment or rely on old receipts.

Validation covers schema conformance, selected catalog relationships, source
parity, and dependency hashes. Scientific/provider correctness is explicitly
`NOT_RUN`; existing metadata gaps are preserved. Request execution, download,
processing, and automatic scientific provenance are proposed integrations in
the original guide, not supplied implementations. See the included license files
for source licensing; provider data terms still apply to the cataloged sources.

## 6. Folder reference

Paths below are relative to `load-dataset-json-standalone/`. Container folders
retain the original layout so the bundled loader and bridges work unchanged.

| Folder | Files Inside | Purpose |
|---|---|---|
| `./` | `README_linkml_json.md`, `setup.R`, `run_tests.R`, `requirements.txt`, `requirements-tested.txt`, `SOURCE_MANIFEST.json`, license files | Setup, usage, dependency specifications, test entry point, and source snapshot hashes. |
| `global_dataset_catalogs/` | 23 dataset YAML files, including `merra2.yaml`, plus `global_catalog.yaml` | Reviewed catalog inputs for individual conversion and the complete global build. |
| `linkml-json-iplementation/` | `README.md`, `run_pipeline.R`, `requirements.txt` | Conversion workflow documentation, complete-build runner, and bridge dependency reference. |
| `linkml-json-iplementation/R/` | `load_dataset_json.R`, `convert_catalog.R`, `README_load_dataset_json.md` | Reusable R loader, schema-aware JSON serialization, and original usage guide. |
| `linkml-json-iplementation/python/` | `dataset_bridge.py`, `linkml_bridge.py` | LinkML compilation, validation, relationship/parity checks, and publication of validated JSON. |
| `linkml-json-iplementation/tests/` | `test_conversion.R`, `test_dataset_loaders.R`, `test_bridge.py` | Serialization, loader integration, and Python validation regression tests. |
| `linkml-json-iplementation/output/` | `amadeus_schema.schema.json`; additional plans, reports, and manifests after a complete build | Bundled generated schema fixture and destination for generated build artifacts. |
| `linkml-json-iplementation/output/single-dataset/` | Initially empty; `<dataset>.json` and `<dataset>.validation.json` after loader calls | Default destination for individual catalog JSON and validation receipts. |
| `amadeus_schema/` | `LICENSE`; schema files under `src/` | Bundled schema subset and its license, rather than a complete schema development checkout. |
| `amadeus_schema/src/` | Schema tree under `amadeus_schema/` | Preserves the source layout expected by the bridge. |
| `amadeus_schema/src/amadeus_schema/` | YAML model files under `schema/` | Container for the shared Amadeus model. |
| `amadeus_schema/src/amadeus_schema/schema/` | `amadeus_schema.yaml`, `amadeus_common.yaml`, `amadeus_catalog.yaml`, `amadeus_value.yaml`, `amadeus_request.yaml`, `amadeus_provenance.yaml` | Root LinkML model and local modules defining types, catalogs, values, requests, and provenance. |
| `dataset_spec_objects/` | `catalog_workflow.py`, `catalog_requirements.txt` | Shared strict YAML loading, catalog relationship/union helpers, and pinned Python requirements. |
| `dataset_spec_objects/catalog_updates_20261109/` | `read_catalog.R` | YAML reader that protects large integer values during R conversion. |
| `validated_catalog_handles/` | `validate_catalogs.py` | Supplies the existing dataset membership registry to the batch workflow; discovery-handle YAML files are not bundled here. |
| `verification/` | `portability_report.json` | Reference results from testing a copied bundle outside the original checkout. |
| `verification/loader-tests/` | `test_report.json`, `dependencies.log`, `serialization.log`, `bridge.log`, `loaders.log` | Saved reference test outcomes and dependency versions. |

Setup creates `.venv-catalog/` when the documented virtual-environment command
is used. New test runs create `test-results/`; the complete build creates
`linkml-json-iplementation/output/catalogs/` for dataset and global JSON files.
These generated folders are separate from the bundled reference results.
