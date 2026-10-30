# Validation boundary and current limits

The scientific package is unchanged. The clean implementation has newly written hand-off code; it does not inherit earlier drivers, output fixtures, checkpoint machinery or their qualification results.

| Boundary | What is checked | Purpose |
|---|---|---|
| Before the example | Installed dependencies, public routes, declared variable/date/site profile | Fail clearly for unsupported inputs/environment |
| Ordinary scientific processing | Existing Amadeus checks plus returned layer/date shape | Preserve existing processing/extraction behavior |
| Source metadata | Actual NetCDF variable, native unit spelling, regular grid and source hashes | Avoid silently describing the wrong quantity/grid |
| Schema structure | Pinned JSON Schema and generated `check_Class` functions | Types, required fields, enumerations and cardinality |
| Record graph | Unique IDs and existing references | Prevent disconnected records |
| Additional profile semantics | Canonical binding/units, location membership, ordered request window, result date labels, asset/run association, acyclic provenance, WKT point validity, finite/native values and result completeness | Check relationships that field types/reference existence alone do not establish |
| Serialized outputs | Actual JSON validation, YAML roundtrip and asset hashes | Verify what is written, not just what was in memory |
| Optional database | Accepted-instance/schema/file hashes, mapped columns/junctions,102 generated integrity queries, values/units within declared precision | Verify stored/queryable records |

The generated R file currently needs a scoped PCRE `grepl` compatibility binding for its lookahead patterns. That binding exists only in the private generated-check environment; no base or Amadeus function is replaced. Generated code is not edited manually.

YAML readers may simplify single-element sequences into R scalars. The roundtrip restores the model-declared array representation before comparing with JSON. The actual JSON instance is independently validated strictly.

New profile guards address previously identified categories of cross-record/time/geometry mistakes. Their tests apply to this clean gridMET point profile, not to every schema class or other dataset. The complete suite records intended rejection reasons; it is not a claim that every field or condition is exhaustively qualified.

Important limits:

- Source dates are represented as **nominal UTC00 date labels**, with no `valid_time_end` physical interval asserted. Product metadata declares observation-dependent day convention and describes the limitation. This differs from an older fixed-clock assumption; it changes metadata representation, not the package's daily numeric values. The physical source-day boundary/timezone needs domain review before time-sensitive production interpretation.
- Canonical IDs are local candidates. No external ontology/OMOP equivalence is asserted.
- Extensivity is a spatial point-profile classification required by the model, not authorization for arbitrary temporal aggregation. No monthly products or scientific conversions are added.
- Only the locally recorded small request is qualified here. All16daily variables are offered for further team testing; PDSI, multiple years, other calendars, polygons and other datasets are outside this example.
- SQL uses the schema's generated numeric types. FLOAT rounding is checked against quantity-specific tolerances; DOUBLE/type decisions are not silently changed. Some of the102integrity queries concern empty classes/tables.
- Core/WKT storage is tested, not SQL spatial functions/extensions. Point WKT is checked in R.
- There is no checkpoint resume, fault injection, production publication service, concurrent-writer protection, cross-request upsert or arbitrary crash-recovery claim in this clean example. New run folders prevent ordinary overwrite. Failed partial outputs are retained; choose a fresh run or explicitly inspect them.
- Prior pilot performance/recovery claims do not automatically apply to this new implementation. Different machines/package releases need their own results. No runtime speedup claim is made.
- The schema metadata does not control Amadeus's algorithms. External code does select request parameters and gate export/storage; that influence is explicit.

A successful team test supports a review decision. It does not grant production or human scientific approval. No Amadeus package changes are required to run it.
