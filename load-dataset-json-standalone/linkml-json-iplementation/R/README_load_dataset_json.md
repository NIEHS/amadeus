# Using `load_dataset_json.R`

`load_dataset_json.R` generates **validated catalog metadata**, not environmental observations. It reads one dataset YAML, compiles the shared Amadeus LinkML model, converts the data in R, validates the candidate JSON and publishes it. These are sourceable helper functions; they are not currently exported by the installed `amadeus` package.

## Setup and required files

Run the examples from the Amadeus repository root. Keep the repository layout intact: copying the R loader alone is insufficient.

| Resource | Location / requirement |
|---|---|
| R API and converter | [`load_dataset_json.R`](load_dataset_json.R), [`convert_catalog.R`](convert_catalog.R). |
| Dataset input | [`global_dataset_catalogs/<dataset>.yaml`](../../global_dataset_catalogs/), e.g. `merra2.yaml`. This is catalog data, not a separate LinkML schema. |
| Shared model | [`amadeus_schema.yaml`](../../amadeus_schema/src/amadeus_schema/schema/amadeus_schema.yaml) and its imported modules in the same schema tree. |
| Validation bridge | [`dataset_bridge.py`](../python/dataset_bridge.py) and [`linkml_bridge.py`](../python/linkml_bridge.py). They also reuse `dataset_spec_objects/catalog_workflow.py`, `catalog_updates_20261109/read_catalog.R` and the registry/helper module `validated_catalog_handles/validate_catalogs.py`. |
| R dependencies | `yaml` and `jsonlite`. |
| Python dependencies | LinkML 1.12.1, linkml-runtime 1.12.1, jsonschema 4.26.0 and PyYAML 6.0.3, pinned through [`requirements.txt`](../requirements.txt). |

Install missing dependencies once; reuse an existing configured environment when available:

```sh
python3.12 -m venv .venv-catalog
.venv-catalog/bin/python -m pip install -r linkml-json-iplementation/requirements.txt
Rscript -e 'install.packages(c("yaml", "jsonlite"), repos="https://cloud.r-project.org")'
```

A virtual environment is optional. Any Python interpreter with these packages can be selected with `python = "python3"` or `Sys.setenv(AMADEUS_CATALOG_PYTHON = "/path/to/python")`. Otherwise the loader tries the repository `.venv-catalog`, then `python3` on PATH. Python is required for the **current generation/validation function**, but not for reading JSON that has already been generated. No provider downloads occur.

## Working usage

```r
source("linkml-json-iplementation/R/load_dataset_json.R")

catalog <- jsonlite::read_json(
  load_merra2_json(),
  simplifyVector = FALSE
)
length(catalog$products)           # 7
length(catalog$product_variables)  # 45
catalog$product_variables[[1]]$native_name
```

`load_merra2_json()` first generates the file and invisibly returns its absolute path. `read_json()` reads that file; `simplifyVector = FALSE` keeps the nested objects/arrays as R lists. Each loader call regenerates and validates, so call it **once outside download/processing loops** and reuse `catalog`.

For an explicit output location and access to the validation report:

```r
path <- load_merra2_json(
  output_file = "linkml-json-iplementation/output/single-dataset/merra2.json",
  overwrite = TRUE
)
catalog <- jsonlite::read_json(path, simplifyVector = FALSE)
report <- jsonlite::read_json(attr(path, "validation_report"))
stopifnot(identical(report$status, "PASS"))

# Reuse the same implementation for other datasets:
aqs_path <- load_aqs_json()
huc_path <- load_dataset_json("huc")
load_prism_json <- make_dataset_json_loader("prism")
```

Default output: `linkml-json-iplementation/output/single-dataset/merra2.json`; receipt: `merra2.validation.json` beside it. The existing `output/catalogs/*.json` files belong to the separate complete build and are untouched by these calls. Defaults are anchored to the sourced repository; explicit relative paths use the caller's working directory. Optional `input_dir`, `schema_yaml`, `python`, `overwrite` and `quiet` arguments are documented in the [full README](../README.md#7-generate-one-dataset-with-reusable-r-functions).

Before publication, checks cover schema types, required fields, enums, generated conditional rules, IDs/references, source/output parity and input hashes. A validation failure raises an R error, records details where possible and preserves existing JSON. This does not resolve scientific metadata gaps or certify download/extraction correctness. The conversion receipt is **not** a download/process/calculate provenance record. Use [`run_pipeline.R`](../run_pipeline.R) for the complete dataset/global build; `load_dataset_json("global_catalog")` is intentionally unsupported.

## Required enhancements for integration

The following are **proposed extensions**, not existing loader behavior. Each strategy can be implemented independently using the same catalog metadata.

| Strategy | Practical implementation guidance |
|---|---|
| **Catalog-backed discovery and input selection** | Separate **reading** a released catalog from explicitly **regenerating** it. Add `read_catalog()`, `find_variables()` and `list_products()` helpers: use the global JSON for cross-source searches and dataset JSON for native names, units, versions, coverage and limitations. Resolve canonical → binding → product IDs and return arguments the current wrappers accept. Ship validated snapshots under `inst/catalog/`, load with `system.file()`/`jsonlite`, and cache in the R session. This avoids requiring Python merely for discovery. Keep reviewed YAML as the maintained source. |
| **Catalog-aware download–process–calculate pipeline** | Add an optional product/binding selection to `R/download.R`, `R/process.R` and `R/calculate_covariates.R`. Use an explicit, tested adapter registry to translate catalog IDs into existing arguments. Reuse access/version/coverage metadata for downloads, verify native variables/packing/fill/units during processing, and check spatial support, extensivity and valid aggregation during calculation. Keep source-specific URL templates and filename rules where catalogs do not encode them. Attach selected IDs and catalog version/hash to the workflow; stop only operations affected by unresolved evidence. Existing source-specific functions remain the execution layer. |
| **Schema-validated requests and automatic provenance** | Add a separate request runner that resolves catalog IDs, validates `ExtractionRequest` and location records from `amadeus_request.yaml`, and invokes existing adapters. Supplement schema validation with cross-object and scientific checks. Record real files/URLs/hashes as `Asset`, each operation's arguments/version/status/input/output links as `ToolRun`, and its ordered upstream history as `ProvenanceChain` from `amadeus_provenance.yaml`. Capture download file manifests even when existing functions return NULL. Store requests and execution records in a user-selected run directory, not the curated catalogs; honor PHI/export restrictions. No database is required—schema-conformant JSON receipts can be the initial storage. |

For package adoption, declare R dependencies in `DESCRIPTION`, install catalog snapshots/generated validation artifacts under `inst/`, and replace repository-relative helper assumptions with installed-file lookup. Keep LinkML compilation as a development/release step unless users explicitly request regeneration. If runtime request validation must be entirely R-only, adapt and test the existing `amadeus_schema/scripts/gen_r_validators.py` outputs plus reference checks; do not assume they cover every rule automatically.

Maintain one reviewed YAML source and regenerate snapshots through the existing pipeline/CI rather than hand-editing JSON. Discovery can supply selections, catalog-aware wrappers can execute them, and the request runner can record the workflow—but none needs the others to be implemented first. See the [full pipeline README](../README.md) for reproducibility commands and validation limits.
