# Dataset specification objects

Each `.yaml` file is a self-contained, schema-validated `AmadeusDatabase` catalog bundle using the archived catalog/common definitions. Each matching `.rds` stores the same YAML-loaded R named list; no new R class or schema is introduced.

[RESOURCE_USAGE.md](RESOURCE_USAGE.md) explains exactly how the mapping workbook, integration review and environmental workbook informed the objects, identifies limitations in the original build, and documents the subsequent reconciliation. [resource_audit.json](resource_audit.json) provides the cell-to-field trace. The catalog bundles are stored in the existing `validated_catalog_handles/` directory.

```r
spec <- readRDS("dataset_spec_objects/gridmet.rds")
spec$products[[1]]
spec$product_variables

# Equivalent portable representation:
spec <- yaml::read_yaml("dataset_spec_objects/gridmet.yaml")
```

The RDS round trip is checked against the YAML-loaded object. If a YAML spec is deliberately changed, regenerate its RDS with `saveRDS(yaml::read_yaml(path), sub("yaml$", "rds", path), version = 3)` and rerun catalog validation.

## Scientific interpretation

| Object | Interpretation and safeguards |
|---|---|
| [gridmet.yaml](gridmet.yaml) | Sixteen verified daily fields on the native 1/24-degree grid. A source day uses fixed Mountain Standard Time, 07:00 UTC to 07:00 UTC the following date. Native coordinate arrays contain cell centres; grid origins here are upper-left outer cell corners. Provider packing and temperature conversion to Celsius are separate, explicit steps. Wind bearings require circular statistics. PDSI, five-day indices and elevation need separate profiles. |
| [aqs.yaml](aqs.yaml) | Seven parameter-scoped bulk daily-summary products. `Arithmetic Mean` must be interpreted within its `Sample Duration`, `Pollutant Standard`, POC, method and event context. The catalog does not choose one row combination for an analysis. Coordinate datum is row-specific; no uniform EPSG code is asserted. Bulk files need no API credentials. Their nominal `P6M` publication cadence is distinct from daily observation resolution; exact release months/dates are not guaranteed. AQS is not supported by `calculate_covariates()`. |
| [hms.yaml](hms.yaml) | Raw smoke polygons, with an ordinal density class rather than a measured surface concentration. Daily refers to archive reporting, while individual `Start`/`End` UTC intervals may cross midnight or have equal endpoints. Missing imagery/files do not mean smoke-free conditions. The workbook's 21-entry contribution history is preserved as attributed programme context on DataSource, not asserted as smoke-specific sensor/date coverage. Third-party HMS-Smoke gap filling and QA attributes are not attributed to the raw NOAA product. |
| [nlcd.yaml](nlcd.yaml) | Six Annual NLCD C1V1 products, map years 1985–2024. Legacy NLCD and current C1V2 are distinct products/releases. Preserve categorical classes, transitions, confidence scores, impervious percentages and spectral dates according to their individual semantics. Annual nodata values are product-specific, not the old legacy 0/255 policy. Only land cover is linked to the verified categorical covariate dispatch; download/process support does not prove scientific extraction support for all six products. |

## Custom NLCD CRS registration

The provider's native metadata specifies WGS84 Albers, 30 m pixels, with central meridian −96°, origin latitude 23°, standard parallels 29.5°/45.5° and zero false offsets. It does not establish an external EPSG authority code.

The archived `GridDefinition` requires a code-like `crs` string even when a complete `proj_string` is available. `AMADEUS:100001` is therefore an **explicitly assigned project-local catalog identifier** for the exact projection in the linked grid. It is not provider metadata, an EPSG assignment, a PROJ-recognized authority or a database SRID. Its registration and exact projection are recorded in [source_manifest.json](source_manifest.json). No numeric `srid` is assigned. Consumers must resolve `proj_string`, and must fail rather than default to EPSG:4326 or EPSG:5070. No CRS registry table or new schema definition was added.

Affine origin coordinates not supplied by the inspected NLCD XML are omitted. Obtain the actual requested raster header before cell indexing. The six products share the grid characteristics documented by the official Collection 1 guide; only Land Cover's native archive XML was directly inspected in this build.

## Verification still required

These are verified **metadata profiles for the stated scopes**, not a claim of complete knowledge of all four dataset families. [verification_gaps.json](verification_gaps.json) identifies each unresolved field, consequence and next action. Important remaining work includes exact per-file release/QA state, AQS row selection, historical HMS density encodings, NLCD affine origins and product-specific headers, and runtime extraction support.

Proposed catalog fields are not emitted into the baseline bundles. Native encoded ranges are not placed in canonical `plausible_min/max`. New ontology equivalences, physical plausibility bounds, blanket data-quality statuses and unobserved asset records are not invented. Complete source-wording and verification evidence remain available in existing notes and the audit files.

## Sources

The [source manifest](source_manifest.json) records input hashes and field-level evidence. Primary sources reviewed include:

- [Climatology Lab gridMET](https://www.climatologylab.org/gridmet.html) and the sixteen provider THREDDS dataset metadata files preserved in [evidence/](evidence/).
- [EPA AirData file definitions](https://aqs.epa.gov/aqsweb/airdata/FileFormats.html), [parameter codes](https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv) and [public download inventory](https://aqs.epa.gov/aqsweb/airdata/download_files.html).
- [NOAA HMS](https://www.ospo.noaa.gov/products/land/hms.html) and raw shapefile samples already available in the repository.
- [Annual NLCD Collection 1 user guide](https://www.mrlc.gov/sites/default/files/docs/LSDS-2103%20Annual%20National%20Land%20Cover%20Database%20%28NLCD%29%20Collection%201%20Science%20Product%20User%20Guide%20-v1.1%202025_06_11.pdf), [the release record](https://www.usgs.gov/data/annual-national-land-cover-database-nlcd-collection-1-products-ver-12-june-2026), and native C1V1 archive XML preserved in [evidence/nlcd_native_metadata.xml](evidence/nlcd_native_metadata.xml).
- [NIEHS/amadeus at the reviewed revision](https://github.com/NIEHS/amadeus/tree/3fa6161de984dacac096cf18147c8022c595c875) for adapter declarations, and [HMS-Smoke at the reviewed revision](https://github.com/tianjialiu/HMS-Smoke/tree/731d8b00d7401af047e2672afe114f7bd8dd458d) for documented historical processing caveats. The local adapter files were checked separately; their hashes are recorded.

Provider metadata supersedes conflicting workbook inventory statements. The workbook and integration report remain useful crosswalks and gap analyses, not substitutes for product-specific scientific metadata.
