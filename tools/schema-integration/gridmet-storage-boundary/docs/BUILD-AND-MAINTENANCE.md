# Maintainer build and review

Users run pinned artifacts; they do not rebuild LinkML contracts per request. LinkML YAML/source, current approval catalog, generators, R/SQL/Parquet mappings and dev QA are retained in integration/. Amadeus code/labels are build metadata inputs; policy unit authority and source facts are distinct. Scientific approval remains pending.

A deliberate build can run `Rscript integration/dev/validate_release.R` from this folder. It writes release/runtime-contract.json and BUILD-RECEIPT.json; do not run casually against an archived demo and expect old publication identities to stay valid. Review/requalify changed release identities and preserve old releases for old snapshots. This is consistency QA, not full LinkML regeneration or scientific approval.

Review source YAML, generators and their recorded provenance before full regeneration. Python/LinkML build tooling is a developer dependency, not a scientific/export/query runtime requirement. Pin tool versions before claiming reproducible full regeneration. No full clean regeneration is claimed for this packaging step.

Follow review-materials/AI-AND-HUMAN-TRACEABILITY.txt: stable rule IDs, authoritative evidence, code, valid/invalid tests and real reviewer decisions. Historical suite references still need individual rule mapping. Changed semantics invalidate corresponding approval and require affected tests. No additional automatic human-approval service was introduced.

Performance checks must use matched requests/source bytes/scientific calls, fresh processes and explicit component timings. The corrected release needs a new controlled full-workflow benchmark before efficiency claims.

air.toml is editor formatting configuration only; it has no scientific runtime role. Formatting pinned runtime files changes hashes and requires a deliberate rebuilt/requalified release.
