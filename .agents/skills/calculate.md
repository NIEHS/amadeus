# Calculation: Numerical Meaning and Result Alignment

Use the shared protocol for workflow and governance,
`agents/calculate-agent.md` for specialist context, and
`.agents/skills/test.md` for test design.

This guide addresses whether extracted values represent the intended locations,
spatial supports, and time periods.

## Define the result key

Identify what uniquely describes a result row: location, time, radius,
variable, level, or other source-specific dimensions.

- Preserve character identifiers, including leading zeros.
- Do not confuse extraction row numbers with user identifiers.
- Verify joins against the complete result key.
- Do not remove repeated location IDs that represent different periods or
  supports.
- Check geometry alignment after reshaping or attaching results.

Use shuffled, nonsequential IDs to detect reliance on row position.

## Check input interpretation

Before extraction, verify that the processed input contains the expected
variables, units, dimensions, and time metadata.

Determine where scaling occurs. Some routes expose calculation-stage scaling;
others receive already scaled values. Apply the source-specific conversion
exactly once.

Confirm that wrapper arguments reach the intended source function, particularly
`weights`, `.by_time`, and source-specific options.

## Spatial support

Establish what each result represents:

- A point sample or interpolated value.
- A buffer summary.
- A polygon intersection or coverage statistic.
- A distance, density, count, or classification.

Then verify:

- CRS compatibility and actual distance or area units.
- Zero-radius behavior where supported.
- Boundary inclusion and partial-overlap rules.
- Treatment of locations outside data coverage.
- Whether transforming locations avoids unnecessary raster resampling.

Do not assume different extraction methods are numerically interchangeable.

## Weighting and denominators

For each weighted or proportional result, identify:

- The contributing observations or cells.
- Coverage fractions and any area factors.
- User-supplied weights.
- Exclusions caused by missing values.
- The numerator and denominator.

Distinguish cell coverage, cell area, population, and other weights. Determine
whether the extraction backend already incorporates a factor before applying it.

Check negative or missing weights according to the supported contract, and
establish the result when effective total weight is zero.

Use an independent example: values 10 and 20 with effective weights 1 and 3
have weighted mean 17.5. Ensure the fixture's geometry produces the intended
effective weights.

For classifications, verify category fractions or membership rather than
averaging category codes.

## Temporal summaries

- Keep timestamps aligned with values and variables.
- Establish `.by_time = NULL` behavior for the affected route.
- Retain location and additional dimensions in grouping keys.
- Verify lag direction and interval boundaries.
- Distinguish averaging records from weighting by represented duration.
- Account for missing periods without treating them as observed zeros.
- Check output cardinality as well as summary values.

Use boundary dates only where relevant to the change: month end, year end,
leap day, or time-zone transition.

## Missingness and output assembly

Distinguish:

- Valid zero.
- Missing source value.
- No spatial overlap.
- Empty temporal selection.
- All-missing summary.
- Zero effective denominator.

Preserve the route's established result for each condition. Check that output
assembly does not silently remove locations or periods.

Verify names, types, units, ordering, and geometry against the full result key.
A plausible numeric column attached to the wrong identifier is a failed result.

## Focused verification

| Risk | Discriminating check |
| --- | --- |
| Wrong location association | Shuffled character IDs and unequal values |
| Wrong point selection | Known values at interior cell coordinates |
| Incorrect spatial summary | Independently known overlap |
| Incorrect weighting | Unequal values and effective weights |
| Time misalignment | Distinct values across multiple timestamps |
| Group collapse | Multiple levels or radii sharing a location and time |
| Missingness confusion | Valid zero, partial missingness, and no overlap |
| Geometry misalignment | Compare returned geometry with each result's ID |

Exercise real extraction when verifying its numerical behavior. Mocked
extraction values can verify joins and output assembly only.

## Task-specific evidence

| Task | Additional evidence to deliver |
| --- | --- |
| Incorrect value diagnosis | Independent expected result and first divergent operation |
| Identifier correction | Complete-key and geometry-alignment checks |
| Weighting change | Explicit numerator, denominator, and missingness rules |
| Temporal correction | Expected grouping, boundaries, values, and row counts |
| New calculation route | Accepted processed input and independently verified output |
| Performance change | Equivalent values, result keys, and ordering |
| Calculate user covariates | Result artifact or object, units, support, time coverage, and missingness summary |