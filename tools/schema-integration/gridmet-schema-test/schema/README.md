# Pinned model release

The YAML definitions and generated JSON Schema, R validators, SQL DDL and integrity SQL come from the schema repository identified in PROVENANCE.json. No earlier experiment's driver, catalog instance, outputs or data are required.

The R runtime uses these fixed generated artifacts plus contract.json. Source YAML is included for review, not reflected on by R at runtime. A scalar record check does not supply every cross-record semantic rule; the external validator explicitly owns the additional gridMET point-profile rules.

To propose a model change, work in a separate developer copy of the schema repository, use its documented generators, review the resulting artifacts and replace this pinned release deliberately. Then execute build_contract.py in that schema project's LinkML environment and review/update provenance. Do not hand-edit generated validators or change YAML alone and assume runtime behavior changed. Normal testers do not need Python, LinkML, uv or just installed.

PROVENANCE.json lists the actual supplied bytes. A source-checkout commit identifies the source checkout; it is not a claim every generated/uncommitted byte is represented by that commit. File hashes are authoritative for this review package.
