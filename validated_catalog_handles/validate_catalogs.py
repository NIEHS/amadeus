#!/usr/bin/env python3
"""Validate the catalog artifacts against the unchanged archived schema.

Requires PyYAML and LinkML. Run with the interpreter that provides
``linkml-validate``; no downloads or schema generation are performed.
"""

from __future__ import annotations

import argparse
import datetime
import hashlib
import importlib.metadata
import json
import math
from pathlib import Path
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
import zipfile

import yaml


ROOT = Path(__file__).resolve().parents[1]
HERE = Path(__file__).resolve().parent
SPECS = ROOT / "dataset_spec_objects"
SCHEMA = ROOT / "amadeus_schema_archived/src/amadeus_schema/schema/amadeus_schema.yaml"
DATASETS = ("gridmet", "aqs", "hms", "nlcd")
CATALOG_COLLECTIONS = (
    "data_sources", "products", "grid_definitions", "canonical_variables", "product_variables"
)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path: Path) -> dict:
    return yaml.safe_load(path.read_text())


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def index(bundle: dict, collection: str) -> dict:
    rows = bundle.get(collection, [])
    result = {r["id"]: r for r in rows}
    require(len(result) == len(rows), f"Duplicate IDs in {collection}")
    return result


def workbook_cells(path: Path) -> dict:
    """Read actual saved workbook values independently of the audit report."""
    ns = {"m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
    result = {}
    with zipfile.ZipFile(path) as archive:
        strings = ["".join(si.itertext()) for si in ET.fromstring(
            archive.read("xl/sharedStrings.xml")).findall("m:si", ns)] if (
                "xl/sharedStrings.xml" in archive.namelist()) else []
        rels = {r.get("Id"): r.get("Target") for r in ET.fromstring(
            archive.read("xl/_rels/workbook.xml.rels"))}
        for sheet in ET.fromstring(archive.read("xl/workbook.xml")).findall("m:sheets/m:sheet", ns):
            target = rels[sheet.get("{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id")]
            part = target.lstrip("/") if target.startswith("/") else "xl/" + target
            cells = {}
            for cell in ET.fromstring(archive.read(part)).findall("m:sheetData/m:row/m:c", ns):
                value, inline = cell.find("m:v", ns), cell.find("m:is", ns)
                cells[cell.get("r")] = (
                    "".join(inline.itertext()) if inline is not None else
                    strings[int(value.text)] if cell.get("t") == "s" and value is not None else
                    value.text if value is not None else ""
                )
            result[sheet.get("name")] = cells
    return result


def semantic_checks() -> list[str]:
    results = []
    bundles = {dataset: load(SPECS / f"{dataset}.yaml") for dataset in DATASETS}
    global_catalog = load(HERE / "global_catalog.yaml")
    schema_documents = [load(p) for p in sorted(SCHEMA.parent.glob("*.yaml"))]
    classes = {k: v for s in schema_documents for k, v in (s.get("classes") or {}).items()}
    slots = {k: v for s in schema_documents for k, v in (s.get("slots") or {}).items()}
    containers = {
        "DataSource": "data_sources", "Product": "products", "GridDefinition": "grid_definitions",
        "CanonicalVariable": "canonical_variables", "ProductVariable": "product_variables",
        "OmopConceptBinding": "omop_concept_binding", "Asset": "assets",
    }
    for dataset, bundle in list(bundles.items()) + [("global_catalog", global_catalog)]:
        by_collection = {name: index(bundle, name) for name in CATALOG_COLLECTIONS}
        ids = [key for records in by_collection.values() for key in records]
        require(len(ids) == len(set(ids)), f"Cross-class duplicate IDs: {dataset}")
        for class_name, collection in containers.items():
            if collection not in by_collection:
                continue
            for row in by_collection[collection].values():
                for field, value in row.items():
                    target = slots.get(field, {}).get("range")
                    if target not in containers or target == "OmopConceptBinding":
                        continue
                    target_rows = index(bundle, containers[target])
                    for ref in value if isinstance(value, list) else [value]:
                        require(ref in target_rows, f"Unresolved {dataset}: {field} -> {ref}")
                require(not row.get("omop_concept_binding"), "No verified OMOP bindings are supplied")
        for row in by_collection["canonical_variables"].values():
            if row["value_data_type"] == "categorical":
                require(row["extensivity"] == "categorical", "Categorical extensivity mismatch")
                require("plausible_min" not in row and "plausible_max" not in row,
                        "Categorical code bounds must not become physical plausibility limits")
        for row in by_collection["products"].values():
            for field, wrapper, source_file in (
                ("amadeus_dataset_name", "download_data", "download.R"),
                ("amadeus_process_covariate", "process_covariates", "process.R"),
                ("amadeus_calculate_covariate", "calculate_covariates", "calculate_covariates.R"),
            ):
                if field not in row:
                    continue
                text = (ROOT / "R" / source_file).read_text()
                require(bool(re.search(r"\b" + re.escape(row[field]) + r"\s*=", text)),
                        f"Unknown {wrapper} dispatch: {row[field]}")
        results.append(f"PASS {dataset}: unique IDs, closed references, categorical constraints, dispatch declarations")
    for collection in CATALOG_COLLECTIONS:
        union = {}
        for bundle in bundles.values():
            for key, row in index(bundle, collection).items():
                require(key not in union or union[key] == row, f"Conflicting identity: {key}")
                union[key] = row
        require(index(global_catalog, collection) == union, f"Global union differs: {collection}")
    for dataset, spec in bundles.items():
        handle = load(HERE / f"{dataset}.yaml")
        expected = {k: v for k, v in spec.items()
                    if k in ("schema_version", "data_sources", "products", "grid_definitions")}
        require(handle == expected, f"Handle/spec drift: {dataset}")
    results.append("PASS global union and all four discovery projections match their source specs exactly")

    gridmet = bundles["gridmet"]
    grid = gridmet["grid_definitions"][0]
    require(grid["crs"] == "EPSG:4326" and grid["n_columns"] == 1386 and grid["n_rows"] == 585,
            "gridMET grid identity/dimensions mismatch")
    axes = {a.get("name"): a for a in ET.parse(SPECS / "evidence/gridmet_tmmx.xml").getroot().findall("axis")}
    x, y = axes["lon"].find("values"), axes["lat"].find("values")
    require(math.isclose(grid["origin_x"], float(x.get("start")) - grid["resolution_x"] / 2),
            "gridMET cell-centre/corner convention mismatch")
    require(math.isclose(grid["origin_y"], float(y.get("start")) + grid["resolution_y"] / 2),
            "gridMET upper-left corner mismatch")
    for pv in gridmet["product_variables"]:
        code = pv["amadeus_variable_code"]
        node = ET.parse(SPECS / f"evidence/gridmet_{code}.xml").getroot().find(".//grid")
        attrs = {a.get("name"): a.get("value") for a in node.findall("attribute")}
        require(pv["native_name"] == node.get("name"), f"Incorrect netCDF variable: {code}")
        require(pv["missing_value_sentinel"] == attrs["_FillValue"], f"Incorrect fill: {code}")
        require(pv["native_value_scale_factor"] == float(attrs["scale_factor"]), f"Incorrect scale: {code}")
        require(pv["native_value_offset"] == float(attrs["add_offset"]), f"Incorrect offset: {code}")
        require(pv["cf_standard_name"] == attrs["standard_name"], f"Raw CF attribute changed: {code}")
        require("unsigned" in pv["harmonization_note"], f"Unsigned packing qualification missing: {code}")
    require("Mountain Standard Time" in gridmet["products"][0]["description"], "Missing fixed-MST qualification")
    direction = next(c for c in gridmet["canonical_variables"] if "direction" in c["name"])
    require(direction["default_aggregation_method"] == "other", "Directions require circular statistics")
    require(len(gridmet["product_variables"]) == 16, "Daily gridMET profile changed scope")
    results.append("PASS gridMET: provider attributes, 16 daily fields, centre/corner geometry, fixed MST and circular wind semantics")

    aqs = bundles["aqs"]
    require(aqs["data_sources"][0]["requires_authentication"] is False, "Bulk AQS is public")
    require(len(aqs["products"]) == 7 and len(aqs["canonical_variables"]) == 6, "AQS parameter scope changed")
    for product in aqs["products"]:
        require("crs" not in product and "srid" not in product, "AQS Datum is row-specific")
        require("amadeus_calculate_covariate" not in product, "AQS has no calculate dispatch")
        require("default_data_status" not in product, "No archive-wide certification assertion is justified")
        require(product.get("update_cadence_iso") == "P6M", "AQS bulk publication cadence must remain distinct from daily support")
        require(all(word in product["description"] for word in ("Sample Duration", "Pollutant Standard", "POC")),
                "AQS row selection qualifiers missing")
    results.append("PASS AQS: public bulk route, seven parameters, shared PM2.5 identity and explicit row-selection caveats")

    hms = bundles["hms"]
    require(hms["canonical_variables"][0]["value_data_type"] == "categorical", "HMS density is a class")
    require(hms["product_variables"][0]["native_units_ucum"] == "1", "HMS density is not surface ug/m3")
    require(hms["products"][0]["spatial_support_type"] == "polygon", "HMS smoke is not a raster")
    require("Start/End" in hms["products"][0]["description"], "HMS interval qualification missing")
    require(set(hms["products"][0]["data_genre"]) == {"wildfire_smoke", "air_pollution"},
            "HMS source themes were not reconciled")
    results.append("PASS HMS: polygon/class semantics and feature-time qualifications")

    nlcd = bundles["nlcd"]
    ng = nlcd["grid_definitions"][0]
    require(ng["crs"] == "AMADEUS:100001" and "srid" not in ng, "Do not invent an EPSG/database SRID")
    require("+datum=WGS84" in ng["proj_string"], "Annual NLCD native datum must remain WGS84")
    require(ng["resolution_x"] == ng["resolution_y"] == 30, "Annual NLCD grid spacing mismatch")
    source = ET.parse(SPECS / "evidence/nlcd_native_metadata.xml").getroot()
    require(ng["n_rows"] == int(source.findtext(".//rastinfo/rowcount")), "NLCD row count mismatch")
    require(ng["n_columns"] == int(source.findtext(".//rastinfo/colcount")), "NLCD column count mismatch")
    require(len(nlcd["products"]) == 6, "Annual NLCD profile scope changed")
    expected_fill = {"LndCov": "250", "LndChg": "9999", "LndCnf": "250",
                     "FctImp": "250", "ImpDsc": "250", "SpcChg": "9999"}
    for pv in nlcd["product_variables"]:
        require(pv["missing_value_sentinel"] == expected_fill[pv["native_name"]], "Annual/Legacy nodata confusion")
    for product in nlcd["products"]:
        require(product["crs"] == ng["crs"] and "srid" not in product, "Incorrect NLCD CRS inheritance")
        require("C1V1" in product["product_version"], "NLCD version pin lost")
    results.append("PASS NLCD: six C1V1 products, verified native projection, local CRS registration and product-specific nodata")

    manifest = json.loads((SPECS / "source_manifest.json").read_text())
    for relative, expected in manifest["source_files_sha256"].items():
        require(digest(ROOT / relative) == expected, f"Source/schema changed since catalog verification: {relative}")
    for relative, expected in manifest["evidence_files_sha256"].items():
        require(digest(SPECS / relative) == expected, f"Provider evidence changed: {relative}")
    results.append("PASS input schemas/workbooks/documentation and provider-evidence checksums")

    audit_path = SPECS / manifest["resource_reconciliation"]["report"]
    require(digest(audit_path) == manifest["resource_reconciliation"]["sha256"], "Resource audit checksum mismatch")
    audit = json.loads(audit_path.read_text())
    source_cells = workbook_cells(ROOT / "environmental_datasets_table.xlsx")
    mapping_cells = workbook_cells(ROOT / "catalog_schema_excel_mapping_with_unmapped.xlsx")
    for trace in audit["field_trace"]:
        collection = containers[trace["target_class"]]
        require(trace["field"] in classes[trace["target_class"]]["slots"], "Trace points to an unattached field")
        records = index(bundles[trace["dataset"]], collection)
        for target in trace["objects"]:
            row = records[target["id"]]
            require(row.get(trace["field"]) == target["value"], "Resource trace value drift")
            require((trace["field"] in row) == target["field_present"], "Resource trace presence drift")
        for source in trace["excel_evidence"]:
            require(source_cells[source["sheet"]].get(source["cell"], "") == source["value"],
                    "Resource trace changed an Excel value")
        for mapping in trace["mapping_workbook_rows"]:
            row = str(mapping["row"])
            require(mapping_cells[mapping["sheet"]].get("B" + row) == mapping["item"],
                    "Mapping-workbook item reference mismatch")
            require(mapping_cells[mapping["sheet"]].get("C" + row) == mapping["status"],
                    "Mapping-workbook status reference mismatch")
    for record in audit["core_excel_cell_dispositions"]:
        require(source_cells[record["sheet"]].get(record["cell"], "") == record["value"],
                "Core Excel-cell disposition changed the source value")
    defined = {(p.name, kind, name)
               for p in sorted(SCHEMA.parent.glob("*.yaml"))
               for section, kind in (("classes", "Class"), ("enums", "Enum"), ("slots", "Slot"))
               for name in (load(p).get(section) or {})}
    traced = {(r["schema_file"], r["item_type"], r["item_name"])
              for r in audit["mapping_definitions"]}
    require(defined == traced and len(traced) == 286, "Resource audit misses a schema definition")
    require(len(audit["proposed_definition_dispositions"]) == 46 and audit["new_schema_definitions"] == 0,
            "Proposed versus implemented definitions were conflated")
    require(len(audit["hms_programme_contributions"]) == 21, "HMS programme table is incomplete")
    for history in audit["hms_programme_contributions"]:
        require(history["system_label"] in hms["data_sources"][0]["description"], "HMS history was not retained")
        if history["end_status"].startswith("not asserted"):
            require(history["end"] is None, "In use/chart endpoint became a retirement date")
    require(any(r["sheet"] == "Amadeus data sources" and r["row"] == 20
                for r in manifest["workbook_rows"]), "AQS bulk source row was omitted")
    results.append("PASS three-resource audit: actual Excel values, mapping rows/statuses, target fields, all definitions and scoped HMS history")
    return results


def r_checks() -> dict:
    """Check the actual R objects and interpret the custom projection with PROJ."""
    rscript = shutil.which("Rscript")
    require(rscript is not None, "Rscript is needed to verify the supplied RDS objects")
    code = r'''
checks <- list()
for (dataset in c('gridmet', 'aqs', 'hms', 'nlcd')) {
  expected <- yaml::read_yaml(file.path('dataset_spec_objects', paste0(dataset,'.yaml')))
  actual <- readRDS(file.path('dataset_spec_objects', paste0(dataset,'.rds')))
  stopifnot(identical(expected, actual))
  checks[[paste0(dataset,'_rds')]] <- 'PASS: RDS equals YAML-loaded named list'
}
grid <- readRDS('dataset_spec_objects/nlcd.rds')$grid_definitions[[1]]
crs <- sf::st_crs(grid$proj_string)
stopifnot(!is.na(crs), is.na(crs$epsg))
xy <- sf::st_coordinates(sf::st_transform(
  sf::st_sfc(sf::st_point(c(-96,23)),crs=4326),crs))
stopifnot(max(abs(xy)) < 1e-5)
checks$nlcd_projection <- 'PASS: exact WGS84 Albers parses; (-96,23) maps to (0,0); no EPSG match asserted'
packages <- list(R=as.character(getRversion()),yaml=as.character(utils::packageVersion('yaml')),
                 sf=as.character(utils::packageVersion('sf')))
cat(jsonlite::toJSON(list(checks=checks,packages=packages),auto_unbox=TRUE))
'''
    proc = subprocess.run([rscript, "-"], input=code, cwd=ROOT, text=True, capture_output=True)
    require(proc.returncode == 0, f"R object/projection verification failed: {proc.stderr}")
    return json.loads(proc.stdout)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--report", type=Path, help="Write a JSON validation report")
    args = parser.parse_args()
    executable = Path(sys.executable).parent / "linkml-validate"
    if not executable.exists():
        found = shutil.which("linkml-validate")
        require(found is not None, "Run using an environment containing LinkML")
        executable = Path(found)
    files = sorted(SPECS.glob("*.yaml")) + sorted(HERE.glob("*.yaml"))
    results = []
    for path in files:
        command = [str(executable), "-s", str(SCHEMA), "-C", "AmadeusDatabase", str(path)]
        proc = subprocess.run(command, capture_output=True, text=True)
        output = (proc.stdout + proc.stderr).strip()
        passed = proc.returncode == 0 and "ERROR" not in output and "Traceback" not in output
        results.append({"file": str(path.relative_to(ROOT)), "passed": passed,
                        "sha256": digest(path), "validator_output": output})
        print(f'{"PASS" if passed else "FAIL"} {path.relative_to(ROOT)}', flush=True)
    errors = []
    r_results = {}
    try:
        checks = semantic_checks()
        r_results = r_checks()
    except (ValueError, KeyError, OSError) as exc:
        checks = []
        errors.append(str(exc))
    for message in checks + errors:
        print(message, flush=True)
    passed = all(row["passed"] for row in results) and not errors
    report = {
        "validated_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "schema": str(SCHEMA.relative_to(ROOT)), "target_class": "AmadeusDatabase",
        "package_versions": {name: importlib.metadata.version(name)
                             for name in ("linkml", "linkml-runtime", "PyYAML")},
        "linkml_results": results, "semantic_checks": checks, "semantic_errors": errors,
        "r_object_and_projection_checks": r_results,
        "passed": passed,
        "scope": "Metadata schema, references and stated scientific checks. No runtime data access or database ingestion is certified.",
        "open_verification_items": "../dataset_spec_objects/verification_gaps.json",
        "database_backend_execution_tested": False,
    }
    if args.report:
        args.report.write_text(json.dumps(report, indent=2) + "\n")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
