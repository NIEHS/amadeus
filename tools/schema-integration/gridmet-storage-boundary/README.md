# Amadeus Schema Integration at the Storage Boundary

Review branch: `amadeus-schema-storage-boundary-review` in NIEHS/amadeus.

Ordinary Amadeus download/process/calculate calls remain unchanged. An explicit optional recorder observes execution alongside science. After calculation, the external writer enforces the schema and scoped semantic contract, writes/reads typed Parquet, loads DuckDB and verifies integrity/fidelity. A manifest binds the completed publication; the supported query entry point verifies it before opening read-only. No runtime AI or Python adapter. Amadeus does not depend on this folder.

Start with [RUNBOOK](docs/RUNBOOK.md). Everyone runs the same instructions from a clone: choose a request and a new output/cache location. No spreadsheet, handwritten schema document or separate user-facing validation call is needed. The test caller is a convenience example, not a new Amadeus API or dispatcher.

- [Architecture and dependencies](docs/IMPLEMENTATION.txt)
- [AI assistance, deterministic execution and human decisions](review-materials/AI-AND-HUMAN-TRACEABILITY.txt)
- [Stage-by-stage register](review-materials/stage-traceability.csv)
- [Individual storage SQL rules](review-materials/sql-rule-register.csv)
- [Human decisions pending](review-materials/human-review-decisions.csv)
- [Dependency register](review-materials/dependency-and-execution-register.csv)
- [Visual workflow](review-materials/workflow-visual.html) and [Mermaid source](docs/workflow.mmd)
- [Editable PowerPoint](review-materials/amadeus-storage-boundary-review.pptx)
- [Pilot versus current release](review-materials/comparison-for-decision.csv)
- [Evidence limitations](docs/EVIDENCE-AND-SCOPE.md)
- [Build/maintenance](docs/BUILD-AND-MAINTENANCE.md)

Status: bounded technical qualification; human scientific approval pending; independent environment replay pending; production approval false. Testers must report their own results. Historical evidence does not constitute an independent test.

The active integration does not use the earlier pilot's example_gridmet.R orchestration or schema_validator.R checks. Existing core Amadeus checks remain. Retained generated R validators are pinned build artifacts, not the candidate's executed validation route. No annual provider files, package libraries, original-pilot implementation or old executable comparison arm is bundled.
