"""Offline regeneration of derived catalogs from reviewed dataset YAML.

The family YAML documents are curated inputs, not inferred from Excel blanks.
Historical provider builders are not replayed. Run --help for output controls.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import importlib.metadata
import importlib.util
import json
from pathlib import Path
import runpy
import shutil
import subprocess
import sys

import yaml
from jsonschema import Draft201909Validator, FormatChecker
from linkml.generators.jsonschemagen import JsonSchemaGenerator

ROOT = Path(__file__).resolve().parents[1]
SOURCES = ROOT / "global_dataset_catalogs"
EVIDENCE = ROOT / "dataset_spec_objects"
SCHEMA = ROOT / "amadeus_schema/src/amadeus_schema/schema/amadeus_schema.yaml"
COLLECTIONS = ("data_sources", "products", "grid_definitions",
               "canonical_variables", "product_variables", "assets")
HANDLE_FIELDS = ("schema_version", "data_sources", "products", "grid_definitions")
CLASS_COLLECTION = dict(zip(
    ("DataSource", "Product", "GridDefinition", "CanonicalVariable", "ProductVariable", "Asset"),
    COLLECTIONS,
))


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate YAML mapping keys instead of silently discarding data."""


def unique_mapping(loader, node, deep=False):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in result:
            raise ValueError(f"Duplicate YAML key: {key!r}")
        result[key] = loader.construct_object(value_node, deep=deep)
    return result


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def load(path):
    return yaml.load(Path(path).read_text(), Loader=UniqueLoader)


def require(condition, message):
    if not condition:
        raise ValueError(message)


def module(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def registry():
    # Keep the existing dataset membership registry; do not invoke its old main().
    return tuple(sorted(module(ROOT / "validated_catalog_handles/validate_catalogs.py",
                               "catalog_membership").DATASETS))


def load_bundles(folder):
    paths = {p.stem: p for p in Path(folder).glob("*.yaml") if p.stem != "global_catalog"}
    require(set(paths) == set(registry()), "Dataset YAML filenames differ from the DATASETS registry")
    return {name: load(paths[name]) for name in sorted(paths)}


def assemble(bundles):
    versions = {b.get("schema_version") for b in bundles.values()}
    require(len(versions) == 1 and None not in versions, "Missing/conflicting schema_version")
    united = {"schema_version": versions.pop()}
    for collection in COLLECTIONS:
        records = {}
        for family in sorted(bundles):
            seen = set()
            for record in bundles[family].get(collection, []):
                identity = record["id"]
                require(identity not in seen, f"Duplicate {collection} ID within {family}: {identity}")
                seen.add(identity)
                require(identity not in records or records[identity] == record,
                        f"Conflicting shared definition: {identity}")
                records[identity] = copy.deepcopy(record)
        united[collection] = list(records.values())
    return united


def handle(bundle):
    return {k: v for k, v in bundle.items() if k in HANDLE_FIELDS}


def check_references(bundle, slots):
    indexed = {c: {r["id"]: r for r in bundle.get(c, [])} for c in COLLECTIONS}
    all_ids = [r["id"] for c in COLLECTIONS for r in bundle.get(c, [])]
    require(len(all_ids) == len(set(all_ids)), "Duplicate ID within/across catalog collections")
    for collection in COLLECTIONS:
        for record in bundle.get(collection, []):
            for field, value in record.items():
                target = CLASS_COLLECTION.get(slots.get(field, {}).get("range"))
                if target:
                    for ref in value if isinstance(value, list) else [value]:
                        require(ref in indexed[target], f"Unresolved {record['id']}: {field} -> {ref}")
    for asset in bundle.get("assets", []):
        for identity in asset.get("variables_present", []):
            require(indexed["product_variables"][identity]["product"] == asset["product"],
                    f"Asset contains a binding from another Product: {asset['id']}")


def validate_models(bundles, united):
    schema = json.loads(JsonSchemaGenerator(str(SCHEMA)).serialize())
    validator = Draft201909Validator(schema, format_checker=FormatChecker())
    slots = {}
    for path in SCHEMA.parent.glob("*.yaml"):
        slots.update(load(path).get("slots", {}))
    documents = [(name, b) for name, b in bundles.items()]
    documents += [("global_catalog", united)]
    documents += [(name + " handle", handle(b)) for name, b in bundles.items()]
    for name, document in documents:
        errors = list(validator.iter_errors(document))
        require(not errors, f"{name}: " + "; ".join(
            f"{list(e.path)}: {e.message}" for e in errors[:5]))
        check_references(document, slots)
    return {"documents_checked": len(documents), "schema": str(SCHEMA.relative_to(ROOT)),
            "unique_ids": True, "closed_references": True,
            "asset_product_binding_consistency": True}


def metadata_report(bundles, united):
    helper = module(ROOT / "validated_catalog_handles/validate_catalogs.py", "metadata_readiness")
    flagged = [p for p in united["product_variables"] if p.get("metadata_gaps")]
    affected = {p["canonical_variable"] for p in flagged}
    by_family = {}
    for name, bundle in bundles.items():
        pvs = bundle.get("product_variables", [])
        counts = {}
        for pv in pvs:
            for flag in pv.get("metadata_gaps", []):
                counts[flag] = counts.get(flag, 0) + 1
        by_family[name] = {"bindings": len(pvs), "flag_counts": counts,
                           "absent": sum("metadata_gaps" not in p for p in pvs),
                           "empty_list": sum(p.get("metadata_gaps") == [] for p in pvs)}
    return {"binding_count": len(united["product_variables"]),
            "flagged_binding_count": len(flagged),
            "canonical_variable_count": len(united["canonical_variables"]),
            "canonical_variables_with_flagged_bindings": len(affected),
            "families": by_family,
            "flagged_bindings": [{"id": p["id"], "product": p["product"],
                                  "canonical_variable": p["canonical_variable"],
                                  "metadata_gaps": p["metadata_gaps"],
                                  "catalog_note": p.get("harmonization_note", "")}
                                 for p in flagged],
            "crs_readiness": helper.crs_requirements(united),
            "scope": "Flags and CRS readiness describe current catalog metadata. They do not certify native arrays, scientific interpretation, observation QA or backend execution."}


def native_checks(bundles, united):
    """Run snapshot-independent checks against retained evidence, not old builders."""
    helper = module(ROOT / "validated_catalog_handles/validate_catalogs.py", "native_coherence")
    result = helper.import_coherence(united)
    require(not result["unresolved_errors"], f"Unresolved coherence errors: {result['unresolved_errors']}")
    # Keep both raw checker messages and evidence-scoped time/area resolutions.
    sys.path.insert(0, str(ROOT / "catalog_gap_resolution"))
    check = module(ROOT / "catalog_gap_resolution/verify_native.py", "native_gridmet_modis")
    messages = [check.check_drought(bundles["gridmet"]), check.check_decoders(bundles["modis"])]
    geos = runpy.run_path(str(EVIDENCE / "geos_review_20261109/verify.py"))
    messages.append(geos["check_with_legacy"](bundles))
    return {"retained_evidence_checks": messages, "coherence": result,
            "scope": "Retained GridMET/MODIS/GEOS evidence and other five-family invariants; not the historical all-file checksum gates or a complete 23-provider revalidation."}


def write_json(path, data):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2, ensure_ascii=False, allow_nan=False) + "\n")


def r_objects(mode, folder):
    require(shutil.which("Rscript") is not None, "Rscript with yaml/jsonlite is required")
    proc = subprocess.run([
        "Rscript", str(EVIDENCE / "catalog_workflow.R"), mode, str(folder),
        str(EVIDENCE / "catalog_updates_20261109/read_catalog.R"),
    ], cwd=ROOT, text=True, capture_output=True)
    require(proc.returncode == 0, proc.stderr or proc.stdout)
    return json.loads(proc.stdout)


def disk_check(base, bundles, united):
    full, handles = base / "global_dataset_catalogs", base / "validated_catalog_handles"
    for name, b in bundles.items():
        require(load(full / f"{name}.yaml") == b, f"Dataset YAML drift: {name}")
        require(json.loads((full / f"{name}.json").read_text()) == b, f"Dataset JSON drift: {name}")
        require(load(handles / f"{name}.yaml") == handle(b), f"Discovery projection drift: {name}")
        optional_json = handles / f"{name}.json"
        if optional_json.exists():
            require(json.loads(optional_json.read_text()) == handle(b), f"Handle JSON drift: {name}")
    for folder in (full, handles):
        require(load(folder / "global_catalog.yaml") == united, f"Global YAML union drift: {folder}")
        jp = folder / "global_catalog.json"
        require(jp.exists() or folder == handles, "Full global JSON is missing")
        if jp.exists():
            require(json.loads(jp.read_text()) == united, f"Global JSON union drift: {folder}")


def manifest(bundles, united, output):
    inputs = [p for p in SOURCES.glob("*.yaml") if p.stem != "global_catalog"]
    inputs += list(SCHEMA.parent.glob("*.yaml"))
    inputs += [Path(__file__), EVIDENCE / "catalog_workflow.R",
               EVIDENCE / "catalog_updates_20261109/read_catalog.R",
               EVIDENCE / "catalog_requirements.txt",
               ROOT / "validated_catalog_handles/validate_catalogs.py",
               EVIDENCE / "evidence/nlcd_native_metadata.xml"]
    context = [ROOT / "catalog_gap_resolution/policy.json",
               ROOT / "environmental_datasets_table.xlsx",
               ROOT / "catalog_schema_excel_mapping_with_unmapped.xlsx"]
    digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    return {"input_role": "Reviewed dataset YAML is the source. Global YAML is checked for drift but is not used to assemble records.",
            "input_sha256": {str(p.relative_to(ROOT)): digest(p) for p in sorted(inputs)},
            "context_sha256": {str(p.relative_to(ROOT)): digest(p) for p in context if p.exists()},
            "families": list(bundles),
            "global_counts": {c: len(united[c]) for c in COLLECTIONS},
            "python_version": sys.version,
            "python_packages": {n: importlib.metadata.version(n) for n in
                                ("PyYAML", "linkml", "linkml-runtime", "jsonschema")},
            "generated_sha256": {str(p.relative_to(output)): digest(p)
                                 for dirname in ("global_dataset_catalogs", "validated_catalog_handles")
                                 for p in sorted((output / dirname).iterdir())
                                 if p.suffix in (".yaml", ".json", ".rds")}}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    generate = sub.add_parser("generate", help="Render family JSON/RDS, projected handles and the global union")
    target = generate.add_mutually_exclusive_group(required=True)
    target.add_argument("--output-dir", type=Path, help="Build a separate tree for review")
    target.add_argument("--in-place", action="store_true", help="Update derived artifacts in this checkout")
    validate = sub.add_parser("validate", help="Validate current or staged YAML/JSON/RDS artifacts")
    validate.add_argument("--catalog-root", type=Path, default=ROOT)
    validate.add_argument("--native-evidence", action="store_true")
    validate.add_argument("--report", type=Path, required=True)
    gaps = sub.add_parser("gaps", help="Report explicit flags and shared grid readiness")
    gaps.add_argument("--catalog-root", type=Path, default=ROOT)
    gaps.add_argument("--report", type=Path, required=True)
    args = parser.parse_args(argv)
    base = (ROOT if args.in_place else args.output_dir.resolve()) if args.command == "generate" else args.catalog_root.resolve()
    if args.command == "generate":
        require(args.in_place or not base.is_relative_to(SOURCES) and base != ROOT,
                "Use --in-place explicitly to regenerate this checkout")
        bundles = load_bundles(SOURCES)
    else:
        bundles = load_bundles(base / "global_dataset_catalogs")
    united = assemble(bundles)
    model_result = validate_models(bundles, united)
    if args.command == "generate":
        full, handles = base / "global_dataset_catalogs", base / "validated_catalog_handles"
        full.mkdir(parents=True, exist_ok=True)
        handles.mkdir(parents=True, exist_ok=True)
        for name, b in bundles.items():
            dest = full / f"{name}.yaml"
            if dest.resolve() != (SOURCES / dest.name).resolve():
                shutil.copyfile(SOURCES / dest.name, dest)
            write_json(full / f"{name}.json", b)
            (handles / f"{name}.yaml").write_text(yaml.safe_dump(handle(b), sort_keys=False, allow_unicode=True, width=100))
            if (handles / f"{name}.json").exists():
                write_json(handles / f"{name}.json", handle(b))
        for folder in (full, handles):
            (folder / "global_catalog.yaml").write_text(yaml.safe_dump(united, sort_keys=False, allow_unicode=True, width=100))
            write_json(folder / "global_catalog.json", united)
        r_result = r_objects("generate", full)
        require(load_bundles(full) == bundles, "Generated dataset directory contains unexpected YAML")
        disk_check(base, bundles, united)
        write_json(base / "catalog_build_manifest.json", {**manifest(bundles, united, base), "r_objects": r_result})
        write_json(base / "metadata_gap_report.json", metadata_report(bundles, united))
        print(json.dumps({"status": "PASS", "output": str(base), **model_result,
                          "global_counts": {c: len(united[c]) for c in COLLECTIONS}}))
    elif args.command == "validate":
        disk_check(base, bundles, united)
        report = {"status": "PASS", **model_result, "catalog_root": str(base),
                  "global_counts": {c: len(united[c]) for c in COLLECTIONS},
                  "yaml_json_and_projection_parity": True,
                  "r_objects": r_objects("validate", base / "global_dataset_catalogs"),
                  "metadata": metadata_report(bundles, united)}
        if args.native_evidence:
            report["native_evidence"] = native_checks(bundles, united)
        write_json(args.report, report)
        print(json.dumps({"status": "PASS", "report": str(args.report), "native_evidence": args.native_evidence}))
    else:
        write_json(args.report, metadata_report(bundles, united))
        print(json.dumps({"status": "PASS", "report": str(args.report)}))


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        # Do not leave a previous PASS report looking like the failed run's result.
        if "--report" in sys.argv:
            position = sys.argv.index("--report")
            if position + 1 < len(sys.argv):
                write_json(sys.argv[position + 1], {"status": "FAIL", "error": str(exc)})
        print(f"FAIL: {exc}", file=sys.stderr)
        sys.exit(1)
