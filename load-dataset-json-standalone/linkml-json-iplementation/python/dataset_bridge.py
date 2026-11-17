"""Validate one catalog and publish R-generated JSON only after every check.

Shares LinkML compilation, JSON Schema validation, reference checks, strict
JSON parsing and source-parity logic with the batch bridge.
"""
from __future__ import annotations

import argparse
from datetime import datetime, timezone
import os
from pathlib import Path
import re
import shutil
import sys
import tempfile

import linkml_bridge as bridge


class DatasetValidationError(ValueError):
    def __init__(self, errors):
        self.errors = errors
        super().__init__("; ".join(
            f"{e['document']}{e['path']}: {e['message']}" for e in errors[:10]))


def checked_dataset_name(name):
    if not re.fullmatch(r"[a-z][a-z0-9_]*", name):
        raise ValueError("Dataset name must contain lowercase letters, digits and underscores, without paths or extensions")
    if name == "global_catalog":
        raise ValueError("Use run_pipeline.R to validate the global catalog and its complete dataset union")
    return name


def check_destination(destination, source, schema_source):
    destination = destination.resolve()
    for protected in [bridge.ROOT / "global_dataset_catalogs", bridge.ROOT / "validated_catalog_handles", bridge.ROOT / "amadeus_schema"]:
        if destination == protected or destination.is_relative_to(protected):
            raise ValueError(f"Output/report is inside a protected catalog/schema tree: {destination}")
    if destination.suffix.lower() != ".json":
        raise ValueError(f"Output must have a .json extension: {destination}")
    if destination in {source.resolve(), schema_source.resolve()}:
        raise ValueError("Output/report must not replace an input file")
    return destination


def implementation_inputs(schema_source):
    return sorted(schema_source.parent.glob("*.yaml")) + [
        Path(__file__).resolve(), Path(bridge.__file__).resolve(),
        bridge.IMPLEMENTATION / "R/load_dataset_json.R",
        bridge.IMPLEMENTATION / "R/convert_catalog.R",
        bridge.ROOT / "dataset_spec_objects/catalog_workflow.py",
        bridge.ROOT / "dataset_spec_objects/catalog_updates_20261109/read_catalog.R",
        bridge.ROOT / "validated_catalog_handles/validate_catalogs.py",
        bridge.IMPLEMENTATION / "requirements.txt",
        bridge.ROOT / "dataset_spec_objects/catalog_requirements.txt",
    ]


def prepare_dataset(dataset, input_dir, schema_source, work, output, report):
    checked_dataset_name(dataset)
    source = input_dir / f"{dataset}.yaml"
    if not source.is_file():
        available = ", ".join(p.stem for p in sorted(input_dir.glob("*.yaml")) if p.stem != "global_catalog")
        raise ValueError(f"Dataset '{dataset}' not found: {source}. Available datasets: {available or '(none)'}")
    check_destination(output, source, schema_source)
    check_destination(report, source, schema_source)
    if output == report:
        raise ValueError("Output JSON and validation report must have different paths")
    # No public output exists at this stage. Failure leaves previous JSON intact.
    document = bridge.catalog.load(source)
    if not isinstance(document, dict):
        raise ValueError(f"{source}: expected a catalog mapping at the YAML root")
    unexpected = set(document) - {"schema_version", *bridge.catalog.COLLECTIONS}
    if unexpected:
        raise ValueError(f"{source}: expected a dataset catalog instance, unexpected fields: {sorted(unexpected)}")
    # Restrict the schema to the catalog root while retaining all existing rules.
    schema = bridge.compile_schema(schema_source)
    validator = bridge.validator_for(schema)
    errors = bridge.check_document(document, validator, bridge.schema_slots(schema_source), dataset)
    if errors:
        raise DatasetValidationError(errors)
    # Reject source values with no standard JSON representation (e.g. NaN).
    import json
    json.dumps(document, allow_nan=False)
    work.mkdir(parents=True, exist_ok=True)
    exported_schema = work / "amadeus_schema.schema.json"
    bridge.write_json(exported_schema, schema)
    plan = {
        "dataset": dataset, "target_class": "AmadeusDatabase",
        "input": str(source), "input_sha256": bridge.sha256(source),
        "schema_source": str(schema_source), "schema_file": str(exported_schema),
        "schema_sha256": bridge.sha256(exported_schema),
        "candidate_file": str(work / f"{dataset}.json"),
        "output": str(output), "report": str(report),
        "dependency_sha256": {str(p): bridge.sha256(p) for p in implementation_inputs(schema_source)},
        "python_packages": bridge.package_versions(), "python_version": sys.version,
    }
    bridge.write_json(work / "dataset_plan.json", plan)
    bridge.write_json(report, {"status": "NOT_RUN", "dataset": dataset,
                              "phase": "validated_source_conversion_pending",
                              "output": str(output)})
    return plan


def verify_dataset_plan(plan):
    fingerprints = {**plan["dependency_sha256"],
                    plan["input"]: plan["input_sha256"],
                    plan["schema_file"]: plan["schema_sha256"]}
    for path, expected in fingerprints.items():
        if not Path(path).is_file() or bridge.sha256(path) != expected:
            raise ValueError(f"Source/schema/implementation changed during generation: {path}; rerun the loader")
    if bridge.package_versions() != plan["python_packages"]:
        raise ValueError("Python package versions changed during generation")


def publish_dataset(work, overwrite):
    plan = bridge.read_json(work / "dataset_plan.json")
    verify_dataset_plan(plan)
    source = bridge.catalog.load(plan["input"])
    candidate = bridge.read_json(plan["candidate_file"])
    schema = bridge.read_json(plan["schema_file"])
    errors = bridge.check_document(candidate, bridge.validator_for(schema),
                                   bridge.schema_slots(Path(plan["schema_source"])), plan["dataset"])
    if not bridge.semantic_equal(source, candidate):
        errors.append({"document": plan["dataset"], "path": "/", "check": "source_json_parity",
                       "message": "R-generated JSON differs from source YAML values or structure"})
    if errors:
        raise DatasetValidationError(errors)
    output = Path(plan["output"])
    report = Path(plan["report"])
    if output.exists() and not overwrite:
        raise ValueError(f"Output already exists: {output}; use overwrite=TRUE to replace it")
    # Copy the validated bytes to the destination filesystem, hash them again,
    # then atomically replace the final path. Python does not serialize data.
    output.parent.mkdir(parents=True, exist_ok=True)
    digest = bridge.sha256(plan["candidate_file"])
    descriptor, staged_name = tempfile.mkstemp(prefix=f".{plan['dataset']}-", suffix=".json", dir=output.parent)
    os.close(descriptor)
    staged = Path(staged_name)
    try:
        shutil.copyfile(plan["candidate_file"], staged)
        if bridge.sha256(staged) != digest:
            raise ValueError("Validated bytes changed while staging the final JSON")
        # Test report serialization/writability before touching existing output.
        result = {
            "status": "PASS", "dataset": plan["dataset"], "target_class": plan["target_class"],
            "validated_utc": datetime.now(timezone.utc).isoformat(),
            "input": plan["input"], "input_sha256": plan["input_sha256"],
            "schema_source": plan["schema_source"], "schema_sha256": plan["schema_sha256"],
            "output": str(output), "output_sha256": digest,
            "checks": {"source_json_schema": "PASS", "output_json_schema": "PASS",
                       "types_required_fields_enums_generated_rules": "PASS",
                       "catalog_references": "PASS", "source_parity": "PASS",
                       "input_schema_dependency_hashes": "PASS", "scientific_provider_evidence": "NOT_RUN",
                       "other_datasets_and_global_union": "NOT_RUN"},
            "counts": {c: len(candidate.get(c) or []) for c in bridge.catalog.COLLECTIONS},
            "python_packages": plan["python_packages"],
            "dependency_sha256": plan["dependency_sha256"],
            "R_conversion": bridge.read_json(work / "r_conversion_report.json"),
            "scope": "Single catalog conformance and exact conversion; not native-array or adapter/scientific certification.",
        }
        staged_report = work / "completed_report.json"
        bridge.write_json(staged_report, result)
        report.parent.mkdir(parents=True, exist_ok=True)
        # Write the report as pending until the output has been published.
        bridge.write_json(report, {"status": "NOT_RUN", "dataset": plan["dataset"],
                                  "phase": "publishing_validated_output", "output": str(output)})
        os.replace(staged, output)
        bridge.write_json(report, result)
    finally:
        staged.unlink(missing_ok=True)
    print(f"PASS: {output}\nValidation report: {report}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["prepare", "publish"])
    parser.add_argument("--dataset")
    parser.add_argument("--input-dir", type=Path, default=bridge.ROOT / "global_dataset_catalogs")
    parser.add_argument("--schema-yaml", type=Path, default=bridge.SCHEMA)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--output-file", type=Path)
    parser.add_argument("--report-file", type=Path, required=True)
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()
    report = args.report_file.resolve()
    source = args.input_dir.resolve() / f"{args.dataset}.yaml"
    schema_source = args.schema_yaml.resolve()
    try:
        # Even failure-report paths must be outside protected input/schema trees.
        check_destination(report, source, schema_source)
        if args.command == "prepare":
            if not args.dataset or args.output_file is None:
                raise ValueError("prepare requires --dataset and --output-file")
            prepare_dataset(args.dataset, args.input_dir.resolve(), schema_source,
                            args.work_dir.resolve(), args.output_file.resolve(), report)
        else:
            publish_dataset(args.work_dir.resolve(), args.overwrite)
        return 0
    except Exception as exc:
        try:
            check_destination(report, source, schema_source)
            bridge.write_json(report, {"status": "FAIL", "dataset": args.dataset,
                 "phase": args.command, "error": str(exc),
                 "errors": getattr(exc, "errors", []), "output_accepted": False,
                 "scope": "No final output was accepted by this invocation."})
        except Exception:
            pass
        print(f"Dataset JSON {args.command} failed: {exc}\nReport: {report}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
