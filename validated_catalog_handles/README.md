# Initial global and dataset catalog

The four dataset handles and `global_catalog.yaml` are **catalog-only `AmadeusDatabase` instances** validated against the unchanged archived schema, version 0.1.0. They introduce no new classes, slots, enums or database tables.

This directory contains the catalog bundles referred to as `validated_catalog_bundles/` in the resource-review request. The actual existing path remains `validated_catalog_handles/`. See [the resource-use audit](../dataset_spec_objects/RESOURCE_USAGE.md) for original versus current use of all three review resources and the subsequent corrections.

| Handle | Defined scope | Full spec |
|---|---|---|
| [gridmet.yaml](gridmet.yaml) | Sixteen daily surface-meteorology variables | [GridMET](../dataset_spec_objects/gridmet.yaml) |
| [aqs.yaml](aqs.yaml) | Daily summary products for parameters 88101, 88502, 81102, 44201, 42602, 42401 and 42101 | [AQS](../dataset_spec_objects/aqs.yaml) |
| [hms.yaml](hms.yaml) | NOAA smoke polygons and their density class | [HMS](../dataset_spec_objects/hms.yaml) |
| [nlcd.yaml](nlcd.yaml) | Six Annual NLCD Collection 1 Version 1.1 CONUS products | [NLCD](../dataset_spec_objects/nlcd.yaml) |

A handle is the discovery projection containing sources, products and referenced grids. A dataset spec adds the canonical variables and native variable bindings. The handle records are identical to their counterparts in the spec; they are not independently maintained definitions. [global_catalog.yaml](global_catalog.yaml) is their complete union, including variables: **4 sources, 15 products, 2 grids, 29 canonical variables and 30 product variables**. Common PM2.5 identity is reused across the two distinct AQS parameter deliveries.

These files describe metadata. They contain no fabricated observations, assets, checksums of remote data, execution history or ontology bindings. Validation establishes schema conformance and the checks listed below. It does **not** establish that every provider product or historical file has been verified. Outstanding verification requirements are recorded in [verification_gaps.json](../dataset_spec_objects/verification_gaps.json).

## Validation

Using a Python environment containing `linkml` and `PyYAML`, plus R with `yaml`, `jsonlite` and `sf`, run from the repository root:

```sh
python validated_catalog_handles/validate_catalogs.py \
  --report validated_catalog_handles/validation_report.json
```

The command validates all nine YAML documents against [the archived root schema](../amadeus_schema_archived/src/amadeus_schema/schema/amadeus_schema.yaml), then checks reference closure, identity uniqueness, handle/spec equivalence, global assembly, declared dispatch routes, provider packing attributes, grid geometry, categorical semantics, CRS qualifications and evidence hashes. It also compares all four RDS objects with their YAML representations and checks the custom NLCD projection using R/PROJ. It performs no downloads. [validation_report.json](validation_report.json) records the results and package versions.

SedonaDB/DuckDB ingestion and live adapter execution have not been tested by this metadata validation. In particular, **do not pass NLCD's catalog-local `AMADEUS:100001` to a database as an EPSG code or numeric SRID**. Its verified projection is stored in `GridDefinition.proj_string`; an ingestion layer must resolve that exact definition explicitly. The archived backend generator's fallback SRID of 4326 must not be used for this grid.

## Evidence and scope

- [source_manifest.json](../dataset_spec_objects/source_manifest.json) records source workbook cells, unchanged input/schema hashes, provider retrievals, field evidence and pinned GitHub revisions. Hidden worksheet copies were excluded from the workbook crosswalk.
- [resource_audit.json](../dataset_spec_objects/resource_audit.json) records the parsed crosswalk, 57 field traces, relevant source-cell dispositions, 21 paired HMS programme-history entries, and decisions for all 46 candidate definitions. Reconciliation preserves the existing schema and propagates the same records to discovery bundles and full specs.
- [evidence/](../dataset_spec_objects/evidence/) retains the small provider XML/CSV metadata used for direct checks. The NLCD metadata was extracted using HTTP ranges from a C1V1 archive; its multi-gigabyte raster was not downloaded.
- Existing archived examples provided reusable entity identifiers and the bundle pattern. Demonstration versions, ontology mappings, arbitrary physical bounds, credentials and stale time/CRS assertions were not copied.
- The mapping workbook's proposed elements remain proposals. Information without an implemented slot is retained in an existing description or harmonization note, or documented as a verification gap.
