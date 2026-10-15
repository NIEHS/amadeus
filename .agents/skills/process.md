# Processing: Source Interpretation and Handoffs

Use the shared protocol for workflow and governance, `agents/process-agent.md`
for specialist context, and `.agents/skills/test.md` for test design.

This guide addresses the conversion of downloaded files into correctly
interpreted inputs for calculation.

## Establish the two handoffs

For the affected route, identify:

| Boundary | Questions to resolve |
| --- | --- |
| Download → process | Which paths, filenames, formats, variables, and periods are expected? |
| Process → calculate | Which return class, dimensions, names, units, CRS, and time metadata are required? |

Verify the route's actual return type; processing does not always return a raster.

## File selection and dimensional interpretation

- Check selection against mixed directory contents, not just one valid file.
- Distinguish missing files from files containing no matching variable or period.
- Resolve duplicate periods or tiles using the established policy.
- Do not infer temporal order from filenames without verifying their convention.
- Select variables and subdatasets explicitly rather than assuming the first
  available one is correct.
- Trace dimensions through reading, subsetting, stacking, and reshaping.
- Keep time, variable, and vertical-level labels aligned with their values.

When format interpretation changes, verify it with a representative local file.
A mocked reader cannot establish that metadata are decoded correctly.

## Decode values exactly once

Check both provider metadata and reader behavior:

- Does the reader already apply scale and offset?
- Are fill values converted to missing values before scaling?
- Are valid zeros retained?
- Are quantities totals, rates, densities, or interval averages?
- Are category codes and labels preserved?
- Are quality flags interpreted independently from data values?

Use at least one known raw-to-processed value to detect double scaling,
incorrect offsets, or unintended unit conversion.

## Spatial transformations

Apply the repository's CRS rule and any documented source-specific exception.

Before changing a spatial operation, verify:

- Coordinate order and longitude convention.
- Latitude orientation and grid alignment.
- Expected extent and resolution.
- Whether the operation assigns CRS metadata or transforms coordinates.
- Whether crop, mask, resample, and mosaic operations have distinct intended
  effects.
- How overlapping tiles and nodata boundaries are resolved.

Choose resampling according to the variable's meaning. Category codes must not
acquire interpolated values.

For antimeridian crossings, irregular grids, or rotated coordinates, use a
targeted example rather than assuming standard rectangular-grid behavior.

## Temporal interpretation

- Decode source time units, origin, and calendar together.
- Do not silently coerce unsupported calendars.
- Distinguish observation time from the interval represented by a value.
- Verify filtering at the requested start and end boundaries.
- Preserve repeated timestamps when they represent different variables or levels.
- Check layer-to-time alignment after sorting, combining, or dropping layers.
- Do not manufacture dates from layer positions when time metadata are missing.

A correct timestamp vector attached to incorrectly ordered values is still an
incorrect result.

## Output usability and lifetime

- Pass representative output directly to its downstream calculation route.
- Verify required metadata survive conversions between object classes.
- Confirm file-backed objects remain readable after the function returns.
- Do not remove intermediate files still referenced by returned objects.
- When changing chunking or storage, compare values and metadata rather than
  object class alone.

## Focused verification

| Risk | Discriminating check |
| --- | --- |
| Wrong file or variable | Mixed inputs with distinct known values |
| Double scaling | Known raw value, scale, and offset |
| Reversed grid | Different values at identifiable coordinates |
| Misaligned time | Multiple timestamps with distinct values |
| Incorrect mosaic | Known overlap and nodata boundary |
| Category corruption | Output codes remain within the valid classification |
| Broken output lifetime | Read values after processing has returned |
| Broken handoff | Calculate from the processed result without manual edits |

## Task-specific evidence

| Task | Additional evidence to deliver |
| --- | --- |
| Discovery failure | Expected versus observed file selection |
| Format update | Changed source fields or dimensions and their interpretation |
| CRS correction | Independently checked coordinates and grid properties |
| Time correction | Source time encoding and expected layer associations |
| New processing route | Download-layout compatibility and calculation-ready output |
| Performance change | Equivalent values, metadata, and backing-file lifetime |
| Process user data | Output location or object, variables, units, coverage, and limitations |