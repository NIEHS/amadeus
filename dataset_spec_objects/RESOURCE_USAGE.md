# Audit of the three catalog integration resources

The catalog bundles in this checkout are in [validated_catalog_handles/](../validated_catalog_handles/), not a directory named `validated_catalog_bundles/`. This review updates those existing bundles and [dataset_spec_objects/](.) together; it does not create a duplicate catalog directory.

## What actually informed the original build

The original objects were manually curated from provider metadata, archived schema/examples and adapter inspection. GitHub/local R code established implementation names and supported routes; provider XML/CSV/pages supplied most detailed scientific metadata. The three requested resources were used unevenly:

| Resource | Original use | Limitation found | Use after this audit |
|---|---|---|---|
| [Mapping workbook](../catalog_schema_excel_mapping_with_unmapped.xlsx) | Reviewed for existing/proposed fields and ownership; its checksum was recorded. | The initial generator did not parse its rows or execute a field-level crosswalk. | All seven worksheets are now parsed. The 286 existing definitions are checked against their owning YAML files; all 46 remaining candidates are reviewed separately. Relevant mapping rows are attached to the field trace. |
| [Integration review](../catalog_dataset_integration.md) | Manually informed the catalog spine, shared vocabularies, packing/units distinctions and extension safeguards. | No explicit section-to-field audit trail; its older candidate table could be mistaken for an approved extension list. | Named sections now have recorded applications. The final **Remaining gaps in schema coverage and recommended placement** section governs extension decisions; earlier candidates are not automatically adopted. |
| [Environmental workbook](../environmental_datasets_table.xlsx) | Selected visible source rows were retained and some inventory facts corroborated the objects. | The row selection missed the specifically named AQS bulk-file source and did not retain the HMS contribution table. Coverage of source assertions was incomplete. | All thirteen worksheets are parsed. Explicit source, bibliography, taxonomy and programme-history blocks are reconciled with the existing objects. The audit accounts for 402 relevant core metadata cells and 21 paired HMS history entries. |

This was not originally an automated Excel-to-YAML import. The new [reconciliation script](reconcile_resources.py) makes this resource use repeatable and explicit. The [machine-readable audit](resource_audit.json) contains 57 field traces with source values, mapping-workbook row references, output object IDs/values, transformations and disposition reasons.

## Where the resources now affect the objects

| Source location | Existing schema destination or disposition | Result |
|---|---|---|
| `Amadeus data sources`, rows 4/6/15/20, plus the main inventory | Source identity, homepage, suffix; Product genre, format, extent and support fields | Names and support assertions are reconciled at the correct product scope. Actual R code still corroborates dispatch strings. Generic netCDF/CSV/raster labels do not establish detailed file metadata. |
| `Amadeus data sources!K20`, compared with `Environmental Datasets!G2` | `Product.update_cadence_iso` and `Product.description`, AQS | Seven bulk-summary products now use **nominal `P6M`**. Daily observations and database updates are separate from bulk publication. EPA corroborates twice-yearly bulk refresh, while its page sections differ on month labels; no exact release-day guarantee is asserted. [EPA download documentation](https://aqs.epa.gov/aqsweb/airdata/download_files.html). |
| `Amadeus Supp Refs!B116:C116`, matched to the AQS source | `DataSource.citation_apa` | Reuses the explicit 2023 AQS Data Mart bibliography entry, without treating 2023 as a dataset version or release date. |
| Source-row licensing/citation, status and first-release/history columns | Attributed statements in `DataSource.description`; detailed audit entries | Preserves the source wording while distinguishing release date, record start, programme launch, source lifecycle, implementation support and measurement QA. Government terms are not converted into an unsupported SPDX identifier. |
| Coverage columns `Environmental Datasets!J` and `Amadeus data sources!N` | `Product.description`, alongside existing extent/support fields | Preserves completeness and sampling qualifications without adding the proposed coverage slots or treating a bounding rectangle as complete observation coverage. |
| `Environmental Datasets!A103` plus the HMS source genre | Existing `Product.data_genre` / `DataGenreEnum` | HMS now retains both `wildfire_smoke` and `air_pollution`; this is thematic classification, not a claim that density classes are measured concentrations. |
| `NOAA Hazard Mapping System!B5:F25` | Attributed **programme-level** history in `DataSource.description`; raw/normalized pairs in the audit | Preserves 21 system/start/end associations. The table includes programme inputs before smoke polygons began, so the relationships are not forced onto the smoke-only Product. `In use` has no asserted end; chart value `46324` is not an instrument retirement date. |
| Legacy NLCD dates/citations in the main inventory, source/product table and bibliography | Explicit scope exclusions in the audit | Legacy DOI/year/cadence information is not assigned to the Annual C1V1 products. The two Tree Canopy Cover entries represent one separately reviewed family under two themes, not additional Annual C1V1 products. |

All changed source/product records are propagated consistently into the four discovery bundles, the full global catalog and the four YAML/RDS specs. No catalog identity is duplicated or newly invented to represent a repeated inventory entry.

## Information deliberately not inferred

- The MODIS variable sheets contain units, data types, ranges and packing for **MODIS**. They cannot populate the native variables of gridMET, AQS, HMS or NLCD. The verified provider metadata remains necessary for those details.
- Independent taxonomy columns are vocabulary lists. Row 4's gridMET name in one block does not make the GOES/license/land-cover metadata in another block gridMET attributes. Source names and category keys establish the limited valid joins.
- Hidden copies provide duplicate evidence. Chart-helper columns, source row numbers, missing-classification indicators, empty input columns and the MERRA acronym glossary do not require additional catalog definitions for these four profiles.
- A proposed field in the mapping workbook is not an implemented YAML field. Rights, history, coverage and release qualifiers are retained through existing scoped text fields. Typed filtering of those concepts requires a separately approved extension.
- The final Markdown gap review recommends reuse over synonyms. These profiles need no second geography enum or variable-specific grid field because the scoped products already use existing support types and one grid per product.

## Remaining verification and completeness

The audit closes missing resource use, traceability and preservable metadata gaps. It does not establish facts absent from the resources: exact AQS analysis-row selection, instrument-specific historical smoke participation, per-file QA/release state, every gridMET variant, NLCD affine origins and additional product families still require evidence. These remain explicit in [verification_gaps.json](verification_gaps.json).

The earlier NLCD project-local CRS registration is unchanged. It is a catalog identifier for the verified full projection, not an external EPSG code or a ready-to-use database SRID. No new schema definition is introduced to conceal that constraint.

## Repeat the reconciliation and validation

From the repository root, using Python with PyYAML/LinkML and R with yaml/jsonlite/sf:

```sh
python dataset_spec_objects/reconcile_resources.py
python validated_catalog_handles/validate_catalogs.py \
  --report validated_catalog_handles/validation_report.json
```

The reconciliation reads the three original resources and writes only the generated objects and their audit files. The original resources, archived YAML schemas and R adapters remain unchanged.
