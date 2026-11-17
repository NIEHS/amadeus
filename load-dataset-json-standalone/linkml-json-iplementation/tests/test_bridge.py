"""Negative controls for the exported schema and relationship checks."""
import copy
import importlib.util
from pathlib import Path
import tempfile
import unittest

HERE = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("linkml_bridge", HERE / "python/linkml_bridge.py")
bridge = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bridge)


class BridgeChecks(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.schema = bridge.read_json(HERE / "output/amadeus_schema.schema.json")
        cls.validator = bridge.validator_for(cls.schema)
        cls.slots = bridge.schema_slots()
        cls.document = bridge.catalog.load(bridge.ROOT / "global_dataset_catalogs/gridmet.yaml")

    def test_preserved_json_schema_accepts_current_example(self):
        self.assertEqual(bridge.check_document(self.document, self.validator, self.slots, "gridmet"), [])

    def test_nullable_optional_collection_remains_valid(self):
        doc = copy.deepcopy(self.document)
        doc["assets"] = None
        self.assertEqual(bridge.check_document(doc, self.validator, self.slots, "nullable_assets"), [])
        self.assertIsNone(doc["assets"])

    def test_singleton_array_must_not_be_unboxed(self):
        doc = copy.deepcopy(self.document)
        doc["product_variables"][0]["metadata_gaps"] = "under_investigation"
        self.assertTrue(bridge.schema_errors(self.validator, doc, "scalar_flag"))

    def test_nonexistent_completion_flag_is_rejected(self):
        doc = copy.deepcopy(self.document)
        doc["product_variables"][0]["metadata_gaps"] = ["complete"]
        self.assertTrue(bridge.schema_errors(self.validator, doc, "bad_enum"))

    def test_packing_requires_native_units_under_generated_condition(self):
        doc = copy.deepcopy(self.document)
        binding = doc["product_variables"][0]
        binding["native_value_scale_factor"] = 0.01
        binding.pop("native_units_ucum", None)
        self.assertTrue(bridge.schema_errors(self.validator, doc, "missing_native_units"))

    def test_dangling_reference_is_rejected(self):
        doc = copy.deepcopy(self.document)
        doc["product_variables"][0]["canonical_variable"] = "amadeus:var/nonexistent"
        errors = bridge.check_document(doc, self.validator, self.slots, "dangling")
        self.assertTrue(any(e["check"] == "references" for e in errors))

    def test_asset_cannot_include_another_products_binding(self):
        doc = copy.deepcopy(self.document)
        doc["assets"][0]["variables_present"] = [doc["product_variables"][0]["id"]]
        errors = bridge.check_document(doc, self.validator, self.slots, "asset_mismatch")
        self.assertTrue(any(e["check"] == "references" for e in errors))

    def test_boolean_does_not_compare_equal_to_number(self):
        self.assertFalse(bridge.semantic_equal({"x": True}, {"x": 1}))
        self.assertFalse(bridge.semantic_equal({"x": [1]}, {"x": 1}))
        self.assertTrue(bridge.semantic_equal({"x": 1.0}, {"x": 1}))

    def test_duplicate_yaml_keys_and_nonfinite_json_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "bad.yaml"
            p.write_text('schema_version: "0.1.0"\nschema_version: "9.0.0"\n')
            with self.assertRaisesRegex(ValueError, "Duplicate YAML"):
                bridge.catalog.load(p)
            p = Path(tmp) / "bad.json"
            p.write_text('{"value": NaN}')
            with self.assertRaisesRegex(ValueError, "Non-finite"):
                bridge.read_json(p)

    def test_schema_hash_changes_invalidate_the_plan(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "schema.json"
            p.write_text('{}')
            plan = {"dependency_sha256": {}, "schema_file": str(p),
                    "schema_sha256": bridge.sha256(p), "documents": [],
                    "python_packages": bridge.package_versions()}
            bridge.verify_plan(plan)
            p.write_text('{"additionalProperties": true}')
            with self.assertRaisesRegex(ValueError, "changed after prepare"):
                bridge.verify_plan(plan)


if __name__ == "__main__":
    unittest.main()
