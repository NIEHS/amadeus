"""Build-only LinkML generation. Run in the maintainer's LinkML environment."""
from pathlib import Path
from linkml.generators.jsonschemagen import JsonSchemaGenerator
import hashlib, json, importlib.metadata
root=Path(__file__).resolve().parents[1]
src=root/'approval/approval_schema.yaml';out=root/'approval/approval.schema.json'
out.write_text(JsonSchemaGenerator(str(src)).serialize())
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
(root/'approval/GENERATION.json').write_text(json.dumps(dict(source_sha256=sha(src),json_schema_sha256=sha(out),generator='linkml.generators.jsonschemagen.JsonSchemaGenerator',linkml_version=importlib.metadata.version('linkml')),indent=2))
print(out)
