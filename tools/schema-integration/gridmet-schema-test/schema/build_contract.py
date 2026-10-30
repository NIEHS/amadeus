"""Developer/build only. Requires the schema project's LinkML environment, never R runtime."""
from pathlib import Path
import hashlib
import json
from linkml_runtime.utils.schemaview import SchemaView

schema = Path(__file__).resolve().parent
view = SchemaView(str(schema / "source/amadeus_schema.yaml"))
classes = view.all_classes()
contract = {
    "collections": {s.name: s.range for s in view.class_induced_slots("AmadeusDatabase") if s.range in classes},
    "classes": {},
}
for name in classes:
    slots = view.class_induced_slots(name)
    contract["classes"][name] = {
        "multiple": [s.name for s in slots if s.multivalued],
        "inline": {s.name: s.range for s in slots if s.range in classes and (s.inlined or s.inlined_as_list)},
        "references": {s.name: s.range for s in slots if s.range in classes and not (s.inlined or s.inlined_as_list)},
    }
(schema / "contract.json").write_text(json.dumps(contract, indent=2) + "\n")
provenance_path = schema / "PROVENANCE.json"
provenance = json.loads(provenance_path.read_text())
provenance["contract_generation"] = "LinkML SchemaView at build time; no runtime Python reflection"
provenance["files"] = {
    str(p.relative_to(schema)): hashlib.sha256(p.read_bytes()).hexdigest()
    for p in sorted(schema.rglob("*"))
    if p.is_file() and p != provenance_path and "__pycache__" not in p.parts
}
provenance_path.write_text(json.dumps(provenance, indent=2) + "\n")
