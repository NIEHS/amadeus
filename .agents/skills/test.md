# Testing: Evidence and Test Design

Use the shared protocol for workflow and governance, `agents/test-agent.md`
for specialist context, and `vignettes/testing.Rmd` for test conventions.
Use the matching tier skill for domain-specific acceptance checks.

This guide addresses whether a test actually proves the intended behavior.

## Choose the boundary

Within the existing offline and live suites, select the boundary that exposes
the defect:

| Behavior under test | Exercise directly | Replace or control |
| --- | --- | --- |
| Dispatch | Routing and argument forwarding | Source function |
| Download selection | URL and destination construction | Transfer executor |
| Transfer handling | Response and failure handling | HTTP responses |
| File discovery | Selection from representative directory contents | File contents if irrelevant |
| Format interpretation | Actual reader and metadata decoding | Small local source file |
| Scientific calculation | Actual spatial or temporal operation | Small controlled inputs |
| Output assembly | Joins, reshaping, names, and geometry attachment | Extraction values if irrelevant |

Do not mock the behavior the assertion is intended to verify. Confirm that
each mock intercepts the actual call, including namespace-qualified calls.

## Construct discriminating fixtures

Choose inputs that distinguish correct behavior from plausible mistakes:

- Unequal cell values expose wrong-cell selection.
- Shuffled character IDs expose positional joins and unwanted coercion.
- Multiple variables and timestamps expose layer misalignment.
- Mixed filenames expose overly broad discovery patterns.
- Valid zeros alongside missing values expose incorrect nodata handling.
- Partial overlap exposes coverage and denominator mistakes.

Use constant rasters only when variation is irrelevant. Place points away from
boundaries unless boundary behavior is the subject of the test.

Create fresh spatial objects and retain backing files while objects use them.

## Establish an independent expected result

- Derive expected values manually or from an independent reference.
- Do not reuse the implementation helper to compute the expected result.
- Assert values together with the keys and metadata needed to interpret them.
- Justify numerical tolerances from the operation.
- Do not sort, round, or discard attributes merely to make a comparison pass
  when those properties are part of the contract.

Useful supplementary checks include:

- Reordering locations preserves the association between IDs and values.
- Constant valid input produces the expected constant mean.
- Positive rescaling of weights leaves a weighted mean unchanged.
- Splitting and recombining inputs preserves results when the operation is
  mathematically independent across those inputs.

Apply such properties only when supported by the source-specific semantics.

## Verify handoffs

For cross-tier changes, test the actual boundary:

- Downloaded filenames are discoverable by processing.
- Processed variables and timestamps remain aligned during calculation.
- Returned file-backed objects remain readable after the creating function exits.
- Custom location IDs survive extraction and geometry attachment.

Two functions returning objects separately does not verify their compatibility.

## Diagnose failures without weakening evidence

Classify a failure before editing:

- Implementation defect.
- Incorrect expectation.
- Unrepresentative fixture.
- Ineffective mock.
- Dependency or environment limitation.
- Live provider failure.

For a regression, identify the assertion that detects the original defect and
confirm failure before the fix where practical.

Do not replace numerical assertions with existence checks, broaden tolerances
without justification, or convert unexplained failures into skips.

## Execution details

Use package-aware execution such as `devtools::test(filter = ...)` and confirm
that the filter selected the intended tests.

The current `skip_if_no_live_tests()` enables live tests for any nonempty
`AMADEUS_LIVE_TESTS` value. Unset the variable for offline runs; setting it to
`"false"` still enables live tests.

Separate executed, skipped, and failed checks. A skipped test provides no
verification of the behavior it would have exercised.

## Task-specific evidence

| Task | Additional evidence to deliver |
| --- | --- |
| Regression test | The assertion that detects the reported defect |
| Coverage review | Missing behavioral cases, not just uncovered lines |
| Mock replacement | Which real operations remain exercised |
| Refactor verification | Equivalent values, keys, and metadata |
| Failure diagnosis | Failure classification and minimal reproducer |
| Test execution | Selected scope, results, skips, and remaining gaps |