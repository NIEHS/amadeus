# gridMET gaps and priority actions

Review inventory — 12 November 2026. **Documentation only: no runtime, test outcome or approval change.**

[Editable detailed register](../review-materials/gridmet-detailed-gap-register.csv)

This register consolidates 50 items from the inspected pilot/release limitations, scientific questions and action plan. It distinguishes demonstrated limits, pending human decisions, missing qualification and optional expansion. A missing test is not an observed implementation defect. Earlier finding IDs are topic cross-references, not proof that historical defects remain. This is comprehensive against inspected categories, not every possible defect or future product.

| Priority | Items | When to address | Closure expectation |
| --- | ---: | --- | --- |
| P1 | 28 | Before a bounded operational trial | Close or explicitly bound the issue with evidence, human decision and supported scope. |
| P2 | 16 | Before the corresponding broader production or efficiency claim | Qualify the relevant deployment, recovery, scale or cost behavior. Requirements depend on intended use. |
| P3 | 6 | When optional scope is selected | Qualify new codes, metadata inputs, methods, datasets, engines or joins before claiming support. |

These counts are review records, not new runtime checks, scripts or manual steps. All action-completion entries remain pending until actual evidence and human decisions are recorded. Current implemented protections remain in place.

## Detailed priority table

| ID | Priority | Area / responsibility | Concrete action | Closure evidence |
| --- | --- | --- | --- | --- |
| M01 | P1 | Metadata field authority | Define the authority for each field: package code/label, provider source fact, LinkML structure or reviewed policy. | Versioned field-authority map; conflicts resolved or explicitly excluded. |
| M02 | P1 | Scientific variable binding | Review all 16 code-to-source-to-canonical bindings; test valid-ID wrong bindings. | Per-code decisions and linked wrong-binding rejections. |
| M03 | P1 | Units and aliases | Approve native units and aliases; test relabeled Kelvin/Celsius and percent/fraction errors without silently converting values. | Approved unit evidence and control/rejection cases; conversion remains explicit if ever introduced. |
| M04 | P1 | Packed scale/offset and tolerance | Review source packing/scale/offset and per-code tolerance; add representative edge/fill-value fixtures where required. | Documented decoding/tolerance authority and finite/missing-value tests against actual source facts. |
| M05 | P1 | Physical temporal meaning | Establish physical period, anchoring, inclusivity and calendar from authoritative evidence; retain uncertainty if unresolved. | Reviewer decision per variable/time family; no physical-interval claim inferred solely from date agreement. |
| M06 | P1 | Descriptions and ambiguous definitions | Treat free text as a claim with source/version/review; flag contradictory definitions without inventing corrections. | Sourced descriptions and unresolved conflicts visible; controlled policy only where reviewed. |
| M07 | P3 | External metadata ingestion | If adopted, compile a reviewed keyed catalog at build with field authority, required fields and conflict rules. | One effective versioned catalog; tested conflict handling; no runtime spreadsheet dependency. |
| R01 | P1 | Request interval validity | Link controls and reversed/empty/out-of-range windows to implemented interval rules on the exact release. | Explicit passing control and intended rejection receipts with rule/release IDs. |
| R02 | P1 | Result date-label binding | Mutate timestamps while keeping plausible dates; test endpoints under the approved request policy. | Clock/date membership tests pass; physical interval approval remains separately recorded. |
| R03 | P1 | Requested variable/location membership | Test alternative valid sites, unrequested variables and expected-key/value mismatch. | Exact requested membership demonstrated by controls and rejections. |
| R04 | P1 | Completeness and duplicates | Test a missing row plus duplicate replacement, duplicate IDs and extra rows. | Expected keys/multiplicity checked, not just total count; approved completeness policy documented. |
| R05 | P1 | Missingness and true zero | Approve handling of NA/NaN/Inf, source fill values and legitimate zero; test each. | Missing/invalid values follow approved policy; valid zero survives unchanged. |
| R06 | P1 | Geometry and CRS meaning | Test malformed/empty geometry plus wrong coordinate order/CRS using reviewed source/site references. | Supported point geometry and spatial meaning demonstrated; unsupported cases explicit. |
| R07 | P1 | Spatial extraction meaning | Review coordinate/grid selection; add exact-grid/edge examples where relevant without changing science silently. | Approved point method/scope and comparisons against source cells. |
| R08 | P2 | Full-model keys/references | Add small positive/negative fixtures for required empty classes before supporting them. | Coverage distinguishes queried, populated and negatively tested classes; no full-model claim from query count. |
| P01 | P1 | Value producer lineage | Test calculation result bound to download/process run and inconsistent output asset. | Correct producer/asset/run bindings pass and wrong-role bindings reject. |
| P02 | P1 | Provenance cycles | Exercise self-link and a multi-run cycle on the exact supported graph; mark uncovered cases. | Observed cycle rejection and a valid acyclic control with actual rule links. |
| P03 | P1 | Truth versus plausible history | Keep observed versus supplied claims explicit; test request/call/output substitution through the public writer. | Mismatched recorded context rejected; remaining unobserved-history limits acknowledged by humans. |
| P04 | P1 | Cached acquisition history | Record reuse and observation time; leave earlier acquisition unknown unless supported by evidence. | No fresh-download or earlier-history claim without observed supporting evidence. |
| P05 | P2 | Raster content fingerprint | Determine whether source/output file hashes and audits meet the trust requirement; add processed-file hashing only if justified and measured. | Documented assurance boundary; extra hashing, if selected, tested for detection and cost. |
| P06 | P2 | Input mutation during execution | Use immutable/read-only staged source versions or a justified before/after identity check in qualification. | Supported source-lifecycle policy and mutation tests; no continuous-immutability claim without evidence. |
| P07 | P2 | Source retention and reconstruction | Agree retention/location/checksum policy for raw and processed assets; test reconstruction from retained artifacts. | Either successful scoped reconstruction or explicit query-only/retention limits. |
| E01 | P1 | Independent environment replay | Replay a small matched request/source/release on an independent and previously affected stack where available. | Ordinary and stored values agree; NA source localized; supported/excluded environments documented. |
| E02 | P1 | Code and installation identity | Record branch commit, runtime hash, installed package provenance/function hashes and source hashes; leave unavailable identity unknown. | Comparison can distinguish code, source, environment and processing differences. |
| E03 | P1 | Dependency and user burden | Have a colleague follow only the runbook with a direct request; distinguish setup from per-run actions. | Dependencies, user inputs and one-command test path are accurate; no hidden package hook/model/Python dependency. |
| E04 | P2 | Long-lived R contract cache | Prefer immutable versioned release paths and fresh processes; test/review hot reload only if needed. | Supported cache/release lifecycle demonstrated; no in-place contract replacement silently reused. |
| S01 | P1 | Parquet semantics versus typing | Retain compiled JSON/shared SQL checks after calculation; assign each rule to one actual enforcement point. | Rule-placement matrix shows no required semantic rule lost or needlessly duplicated. |
| S02 | P2 | Readback types and edge values | Add representative IDs, duplicates, precision, timestamp and approved null edge cases to the shared roundtrip tests. | Mapped columns and multiplicity preserved under supported engine/type versions. |
| S03 | P1 | Schema acceptance versus DB completion | Repeat blocked load/readback failure and fresh retry on target filesystem. | No false completion or pointer advance; separate stage statuses and preserved diagnostics. |
| S04 | P2 | Publication authenticity | Agree threat model; use deployment write/read separation and immutable versions; consider signing only if required. | Access-control/authenticity claims match implemented, tested controls. |
| S05 | P2 | Concurrent mutation/publication | For bounded trial exclude concurrent writers; before production test serialization/locking or immutable-store controls. | Supported writer/read lifecycle and race tests; unsupported concurrency explicit. |
| S06 | P2 | Power-loss durability | Define required crash guarantees; test interruption/fsync/commit protocol only if those guarantees are needed. | Recovery never advertises incomplete publication; residual power-loss limits documented. |
| S07 | P1 | Supported query versus direct access | Make verified-open the supported path; label direct/raw access outside its guarantee. | Demonstrate altered-store rejection via supported command; users understand bypass. |
| S08 | P2 | External-file SQL scope | Limit assurance statements to published artifacts; separately validate/register external inputs if adopted. | Query result assurance explicitly identifies included and excluded data sources. |
| S09 | P2 | Old snapshots and schema evolution | Preserve matching releases; define versioned migration/republication with before/after identity/value tests. | Historical access works under matching release or explicit reviewed migration. |
| S10 | P1 | Filesystem behavior | Run existing setup probe and small publication/failure test on intended output filesystem. | Actual filesystem behavior documented; readonly bits not misrepresented as immutability. |
| Q01 | P1 | Rule-specific rejection coverage | Link each critical rule to valid/invalid/boundary cases first; expand remaining rules and non-SQL families. | Register distinguishes current direct evidence, historical suite evidence and untested cases. |
| Q02 | P1 | Version-specific test receipts | Bind each test run to request/source/code/runtime identity and actual acceptance/load/open outcomes. | A reviewer can reproduce which release passed which case; missing receipts stay missing. |
| Q03 | P1 | Human scientific/release sign-off | Record reviewer/date/decision/rationale against exact policy/rule/release scope; retain exclusions. | No pending assumption represented as approval; signed-off scope distinguishable from technical success. |
| Q04 | P1 | AI/human contribution attribution | Record known AI assistance and human decisions for new changes; do not invent retrospective authorship. | Contribution log and review records explicit; no runtime AI model or AI-derived approval claim. |
| Q05 | P2 | Generated artifacts and generator QA | Qualify generators and generated patterns at build when changed; retain inactive generated artifacts as such. | Reproducible generation or explicit provenance limits; inactive R validator not treated as current execution. |
| C01 | P3 | All recognized gridMET codes | Treat SPEI/EDDI/PDSI or other codes as separate scientific/metadata profiles; review category ambiguity without silently altering provider data. | Per-code capability/metadata/scientific tests and explicit supported/excluded inventory. |
| C02 | P3 | Broader dates, calendars and geometry | Add one scope dimension at a time with native-result and contract-equivalence tests. | Each new profile has independent evidence; unsupported routes rejected/documented. |
| C03 | P3 | Cross-dataset reuse | After gridMET critical gaps, apply same shared boundary to a contrasting dataset; isolate its policies as versioned metadata. | Shared checks reused; dataset-specific missingness/time rules explicit; no hidden gridMET assumptions. |
| C04 | P3 | Sedona/amadeusdb computation and batching | If selected, test backend computation versus ordinary Amadeus, temporal batching, union/Parquet behavior and geometry differences separately. | Per-engine value equivalence, rejection coverage and batching/performance evidence. |
| C05 | P3 | Cross-time and multi-source joins | Add small cardinality/time-alignment/unit/missingness fixtures only when join scope is selected. | No unintended duplicates, temporal shifts or provenance loss in qualified joins. |
| F01 | P2 | Latest full-workflow runtime and memory | Run matched ordinary/pilot/latest release cases with randomized repeated fresh processes and component timings. | Correctness plus distributions and overhead for science/record/export/load/open; no query-only speed extrapolation. |
| F02 | P2 | Download, scale and workload generalization | Separate acquisition from cached processing; increase workload dimensions deliberately; report bytes and deliverables. | Measured scale/network/throughput limits, not inferred full-CONUS or production performance. |
| F03 | P2 | Memory measurement meaning | Keep main-process HWM, sampled tree RSS and R object sizes distinct; retain sampling method and limitations. | Reproducible metrics with correctly labeled interpretation and uncertainty. |
| F04 | P2 | Maintenance and duplicate-check burden | Assign stable checks to build and actual-data checks to output/load/open; benchmark suspected duplicate scans before removing them. | Rule-placement and change-impact map; necessary coverage preserved; user steps/dependencies explicit. |

## How to track a decision

Gap ID → authoritative source or policy/version → implementation/rule/hash → valid and invalid test IDs/outcomes → reviewer/date/rationale → approved release and scope.

Use the CSV for evidence status, parent action IDs, earlier finding cross-references and human decision fields. Keep unknowns and exclusions visible. Machine acceptance, workflow completion and human scientific approval are separate.

Start with scientific policy, critical rule rejection cases, independent small-request replay and publication failure/retry. Reuse current build QA, recorder, shared boundary and publication tests. Do not create a new scientific workflow or weaken checks to obtain a pass.

## Existing protections and remaining boundaries

Retain the successful-build guard, pinned runtime fingerprints, automatic post-calculation JSON/R/SQL checks, typed Parquet/readback, database integrity/fidelity/reopen and manifest-verified saved queries. Bounded successful tests do not establish universal scientific validity, authenticated history, full crash durability or all environments/backends.

No AI model runs in science, acceptance, loading or queries. AI assistance is limited to development, review and documentation; humans approve scientific meaning and release decisions. This table was prepared with AI assistance and is not human scientific sign-off.

## Relationship to packaging manifests

These two added files are review documentation, not runtime contract artifacts. They do not alter release/runtime-contract.json or the original PACKAGE-MANIFEST.json, which describes the earlier packaging snapshot. No new approval gate or dependency is introduced.
