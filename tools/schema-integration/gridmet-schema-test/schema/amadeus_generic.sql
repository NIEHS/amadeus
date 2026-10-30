-- # Class: AmadeusDatabase Description: Container for a complete or partial AmadeusDB instance. Exists so a catalog snapshot, a sample dataset or an import bundle can be validated as one document with `linkml-validate`, and so the SQL generator has a root from which to reach every table.All collections are optional: a catalog-only bundle (the normal shape of a curation pull request) is as valid as a full instance.
--     * Slot: id
--     * Slot: schema_version Description: Version of the AmadeusDB schema a record conforms to, so consumers can branch on schema evolution rather than guess.
-- # Class: DataSource Description: A provider programme that publishes one or more products: EPA AQS, NASA MODIS, NOAA NARR, Climatology Lab. One row per row of the amadeus README source table.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: label Description: A human-readable display label.
--     * Slot: description
--     * Slot: producer_institution
--     * Slot: homepage
--     * Slot: access_protocol Description: Primary access mechanism. Determines whether "subset early" is achievable at all for this source.
--     * Slot: requires_authentication Description: Whether a credential is needed. True for all NASA Earthdata sources (`modis`, `merra2`, `geos`, SEDAC `population`).
--     * Slot: auth_mechanism
--     * Slot: rate_limit_requests_per_second Description: Provider rate limit, as `download_data(rate_limit=)` already takes. In the catalog so a query planner can respect it without hard-coding.
--     * Slot: license_spdx
--     * Slot: citation_apa
--     * Slot: doi
--     * Slot: amadeus_function_suffix Description: The suffix amadeus uses for this source's functions (`_narr` → `download_narr`, `process_narr`, `calculate_narr`). Recorded so the catalog and the R API can be checked against each other mechanically — a catalog entry with no implementing function, or a function with no catalog entry, is a test failure.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: Product Description: A specific, versioned data product from a source — the unit a user actually requests, and the unit that carries the *profile*: spatial support, temporal support, format, calendar, day boundary.This is where the answer to "do we need different schemas for different types of source data?" lives. We do not. The variation across the amadeus catalog decomposes into five orthogonal axes recorded here (`spatial_support_type`, `temporal_resolution`, `value` kind via the variables, `native_format`, and extensivity via the variables). One schema, one product table; a MODIS swath and an AQS monitor differ in their *values on these axes*, not in their structure.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: label Description: A human-readable display label.
--     * Slot: description
--     * Slot: data_source Description: The publishing programme.
--     * Slot: product_version
--     * Slot: native_format
--     * Slot: spatial_support_type
--     * Slot: grid_definition Description: Required when `spatial_support_type` is a grid cell type, forbidden otherwise. Enforced by the rules below rather than by two separate classes — the project lead's "grid size is a dataset-level property" becomes a foreign key, not a copied field.
--     * Slot: crs Description: Coordinate reference system as an authority code. Stored as the string form (`EPSG:4326`) because that is what both `terra` and the SQL engines accept; the numeric SRID is derived for `ST_SetSRID`.
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: spatial_extent_bbox_wkt Description: Coverage footprint as WKT, so extent is a spatial predicate.
--     * Slot: spatial_extent_descriptor
--     * Slot: native_spatial_resolution_m Description: Nominal resolution in metres. Computable, and therefore the one a query planner uses — but meaningless for non-grid supports, hence the descriptor alongside it.
--     * Slot: native_spatial_resolution_descriptor
--     * Slot: temporal_resolution
--     * Slot: temporal_resolution_iso Description: The same fact as `temporal_resolution`, as an ISO 8601 duration. Both are kept because the enum is validatable and the duration is expressive (`P3D` has no enum member). HEW uses the duration, EnVar the enum; this is the reconciliation, and it is generated, not hand-maintained.
--     * Slot: temporal_alignment
--     * Slot: day_boundary_convention
--     * Slot: calendar
--     * Slot: temporal_coverage_start
--     * Slot: temporal_coverage_end Description: Absent means "ongoing".
--     * Slot: update_cadence_iso Description: How often the source publishes new data.
--     * Slot: typical_latency_iso Description: Typical delay between observation and availability. The field that makes the AQS-versus-AirNow trade-off explicit: AQS is quality-controlled and months late, AirNow is preliminary and hourly.
--     * Slot: default_data_status Description: The status values from this product carry unless a row says otherwise.
--     * Slot: homogenisation_status
--     * Slot: doi
--     * Slot: license_spdx
--     * Slot: citation_apa
--     * Slot: access_url
--     * Slot: amadeus_dataset_name Description: The exact string accepted by `download_data(dataset_name=)`. The bridge between the catalog and the existing R API.
--     * Slot: amadeus_process_covariate Description: The string accepted by `process_covariates(covariate=)`.
--     * Slot: amadeus_calculate_covariate Description: The string accepted by `calculate_covariates(covariate=)`. Distinct from the process name — `modis_swath`/`modis_merge` both calculate as `modis`, and `aqs` processes but does not calculate.
--     * Slot: supports_server_side_subset Description: Whether the provider will return a geometry/time/variable subset. Drives the achievable `MaterializationModeEnum`.
--     * Slot: stac_collection_id Description: STAC collection identifier, where the product is STAC-published.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: GridDefinition Description: A reusable description of a raster or model grid. Its own entity because several products share one grid (all NARR monolevel variables) and because resolution, projection and vertical levels must be computable, not prose.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: label Description: A human-readable display label.
--     * Slot: crs Description: Coordinate reference system as an authority code. Stored as the string form (`EPSG:4326`) because that is what both `terra` and the SQL engines accept; the numeric SRID is derived for `ST_SetSRID`.
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: grid_mapping_name Description: CF grid mapping name.
--     * Slot: proj_string Description: Full PROJ or WKT2 CRS definition, for grids with no authority code.
--     * Slot: resolution_x
--     * Slot: resolution_y
--     * Slot: resolution_unit
--     * Slot: n_columns
--     * Slot: n_rows
--     * Slot: origin_x
--     * Slot: origin_y
--     * Slot: vertical_level_type
--     * Slot: vertical_level_unit
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: CanonicalVariable Description: A harmonised environmental quantity, independent of who publishes it. The cross-source identity anchor: `gridmet.tmmx`, `prism.tmax` and `daymet.tmax` are three `ProductVariable`s over *one* `CanonicalVariable`, and that is what makes "is it the same variable?" answerable by a join rather than by a conversation.Carries the semantics that must be identical across sources (`standard_name`, canonical units, extensivity, plausible range, concept bindings). Everything that legitimately differs per source lives on `ProductVariable`.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: label Description: A human-readable display label.
--     * Slot: description
--     * Slot: standard_name Description: The standard-name identifier for the physical quantity, as a CURIE so no single naming authority is privileged. Prefer a CF standard name where one exists; fall back to ECTO/ENVO or a minted `AMADEUS:` term. The prefix carries the authority; the slot name does not.
--     * Slot: standard_name_authority
--     * Slot: units_ucum Description: Canonical units in UCUM notation, bare (not CURIE-prefixed). HEW prefixes (`UCUM:Cel`), EnVar does not (`Cel`); we follow EnVar and note the difference, because it is exactly the kind of thing a mechanical mapper gets wrong silently.
--     * Slot: units_display
--     * Slot: value_data_type
--     * Slot: extensivity Description: Intensive, extensive or categorical. Determines which aggregation methods are legal for this variable — see `ExtractionRequest`.
--     * Slot: default_aggregation_method Description: The aggregation applied when a request does not name one. Must be legal for the declared extensivity.
--     * Slot: plausible_min
--     * Slot: plausible_max
--     * Slot: envar_variable_family Description: Which EnVar variable family this belongs to, and therefore which variable-specific extension block an emitted EnVar record must carry (`derived_heat_metric` for the heat family).
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
--     * Slot: omop_concept_binding_id
-- # Class: ProductVariable Description: One source's delivery of one canonical variable: the native name, the native units and conversion, the CF metadata, the aggregation actually applied, and the layer-name template amadeus parses.Every slot here exists because it is a reason two sources of "the same" variable are not interchangeable. `native_units_ucum` + `native_value_scale_factor` + `native_value_offset` are the gridMET Kelvin→Celsius case, where the conversion happened at render time and was recorded nowhere.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product
--     * Slot: canonical_variable
--     * Slot: native_name Description: The variable's name in the source: a netCDF variable name, a MODIS subdataset, an AQS parameter code.
--     * Slot: label Description: A human-readable display label.
--     * Slot: native_units_ucum
--     * Slot: native_value_scale_factor
--     * Slot: native_value_offset
--     * Slot: unit_conversion_formula
--     * Slot: cf_standard_name Description: The CF `standard_name` attribute as it appears in the source file. A Tier 1 field: already in the netCDF, currently discarded on conversion to `terra`.
--     * Slot: cf_cell_methods Description: The CF `cell_methods` attribute verbatim. CF already has a standard way to say "daily maximum" and it is sitting unread in the file.
--     * Slot: aggregation_method
--     * Slot: aggregation_window_iso
--     * Slot: temporal_alignment
--     * Slot: vertical_level_type
--     * Slot: missing_value_sentinel
--     * Slot: quality_flag_vocabulary
--     * Slot: layer_name_template Description: How amadeus composes `terra` layer names for this variable. Recorded because `calc_worker()` currently recovers variable, level and time by splitting the layer name on `_` and indexing by position; a declared template turns that from an implicit convention into a checkable one.
--     * Slot: amadeus_variable_code Description: The string a user passes as `variable=` to the amadeus download/process functions, where it differs from `native_name`.
--     * Slot: value_data_type Description: Optional override. Absent means "as the canonical variable declares". Present means this source genuinely differs — e.g. a source that publishes a binary exceedance flag for a quantity that is continuous elsewhere.
--     * Slot: extensivity Description: Optional override, same semantics as `value_data_type`. Overriding it is a strong signal something is wrong and should be reviewed.
--     * Slot: harmonization_note Description: Free text on why this delivery is not interchangeable with a sibling delivery of the same canonical variable. Deliberately unstructured: it is the intake queue for future first-class slots, not a permanent home.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: OmopConceptBinding Description: Binding of a canonical variable to an OMOP concept. Taken structurally from HEW so the two schemas exchange bindings without an adapter.
--     * Slot: id
--     * Slot: omop_concept_id
--     * Slot: omop_concept_name
--     * Slot: omop_vocabulary_id
--     * Slot: omop_domain_id
--     * Slot: omop_standard_concept
--     * Slot: concept_status
-- # Class: Asset Description: A concrete byte stream: a file on disk, an object in a bucket, a STAC asset, or a provider API response. The unit of download, caching, integrity checking and idempotency.**Asset-centric by design.** The modernization plan's own instruction is "adopt an asset-centric schema; avoid mandatory cell-row ingestion". An asset can be registered — catalogued, discoverable, queryable by extent and time — without any of its values ever being materialised into a value table. `materialization_mode` records how much was actually read, so the four streaming modes of plan §4.7.2 become auditable properties of the catalog rather than an ungoverned side path.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product
--     * Slot: asset_key Description: STAC asset key or other within-item key.
--     * Slot: url Description: Where the bytes live upstream. Retained even after local caching so a result can be re-derived from source.
--     * Slot: local_path Description: Present only when bytes were actually written locally. Absent for `direct_remote_scan` and `server_side_subset`.
--     * Slot: media_type
--     * Slot: native_format
--     * Slot: size_bytes
--     * Slot: sha256 Description: SHA-256 of the byte stream, for integrity and idempotency.
--     * Slot: source_last_modified
--     * Slot: download_timestamp_utc
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: bbox_wkt Description: Bounding geometry as WKT.
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: stac_collection_id Description: STAC collection identifier, where the product is STAC-published.
--     * Slot: stac_item_id
--     * Slot: materialization_mode
--     * Slot: cache_expires_at
--     * Slot: value_state Description: Whether rows for this asset exist in a value table. `registered_only` is the normal case for the asset-centric discovery path.
--     * Slot: produced_by_run Description: Identifier of the tool run that registered or materialised this asset. Typed as a string rather than an object reference to keep the catalog and provenance modules independently loadable; the FK is added by the SQL post-processor.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Abstract Class: AmbientValue Description: One value of one variable, at one place, over one time interval, from one asset, produced by one run. A property of the environment; not an exposure.Abstract: no table of its own. The SQL generator flattens these slots into each concrete child, which is the right physical design — a single polymorphic value table with mostly-null geometry columns would defeat both partitioning and spatial indexing.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
-- # Class: StationObservation Description: A value from a fixed instrument with a persistent identity: EPA AQS, AirNow, IMPROVE, USGS water sites.The distinguishing feature is not the point geometry — it is that the *instrument* is an entity with a history. Monitors move, get replaced, and report the same parameter through several collocated units (the AQS POC). A grid cell has none of that. Collapsing stations into a generic point table loses the keys that make a revision traceable.
--     * Slot: station_id Description: The source's site identifier — an AQS state+county+site key, an IMPROVE site code, a USGS gauge number.
--     * Slot: station_name
--     * Slot: monitor_id Description: Instrument identity, where distinct from the site.
--     * Slot: parameter_code Description: The source's parameter code, retained verbatim alongside the canonical variable because it is part of the source's primary key.
--     * Slot: poc Description: Parameter Occurrence Code — which of several collocated monitors measuring the same parameter this is. Part of the AQS primary key and routinely dropped by naive ingests, which then silently pick one monitor per site.
--     * Slot: sampling_duration_iso Description: The instrument's integration time — distinct from the reporting resolution. A 1-hour reported value from a 24-hour filter sample is not an hourly measurement.
--     * Slot: station_geom_wkt
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: elevation_m
--     * Slot: station_valid_from Description: Station moves are modelled as a validity interval on the observation's station identity rather than by mutating the station. "Station moves/POC changes" is a named edge case in the plan's test matrix.
--     * Slot: station_valid_to
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: GridCellValue Description: A value at one cell of one grid — the flattened form of a raster layer. This is the class the SedonaDB decision hangs on: Sedona's raster support is thinner than `terra`'s, so the plan's proposal is to flatten raster to point or polygon and persist as GeoParquet.The cell is identified *twice*: by integer grid index (`cell_x`, `cell_y`) and by geometry. The index is the cheap, exact, join-stable key and the one to partition and deduplicate on; the geometry is what spatial predicates need. Storing only geometry makes idempotent re-ingestion a floating-point comparison, which is how duplicate rows appear.
--     * Slot: grid_definition
--     * Slot: cell_x Description: Zero-based column index in the grid.
--     * Slot: cell_y Description: Zero-based row index in the grid.
--     * Slot: cell_centroid_wkt
--     * Slot: cell_polygon_wkt Description: The cell footprint. Optional: needed for area-weighted extraction and for honest polygon intersection, redundant for nearest-cell lookup. Materialising it for a continental hourly grid is expensive, so this is a deliberate per-product choice, not a default.
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: AreaValue Description: A value attached to a named or delineated polygon: HUC watersheds, EPA ecoregions, county-level NEI emissions, USDM drought polygons, NOAA HMS smoke plumes.Two sub-cases that must not be merged, distinguished by `area_role`: an *administrative* area whose identity is external and stable (a county FIPS), and an *episodic* polygon that exists only because something happened there (a smoke plume on one day). Aggregating across the second as if it were the first produces nonsense.
--     * Slot: area_id
--     * Slot: area_type
--     * Slot: area_role
--     * Slot: area_code Description: The external code identifying the area.
--     * Slot: area_name
--     * Slot: area_geom_wkt
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: area_km2 Description: Materialised area. Needed to make extensive values (an emission mass) convertible to intensive ones (a flux density) without recomputing geodesic area per query.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: HexCellValue Description: A value aggregated to an H3 cell — the harmonization target for the C-HER collaboration (objective O-09).Hexification is lossy and the loss must be recorded, not assumed. Three slots do that: `source_support_type` (what was aggregated), `coverage_fraction` (how much of the hex the source actually covered) and `hexification_method` (how). A hex value whose provenance says "area_weighted_mean from raster_grid_cell at 0.62 coverage" is usable; the same number without them is not.
--     * Slot: h3_cell
--     * Slot: h3_resolution
--     * Slot: hex_centroid_wkt
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: source_support_type Description: The support type that was aggregated into this hex.
--     * Slot: hexification_method Description: How source supports were combined into the hex value. Must be legal for the canonical variable's extensivity — a population count hexifies by `sum`, a temperature by `area_weighted_mean`, and swapping them is a silent error.
--     * Slot: coverage_fraction Description: Fraction of the hex actually covered by contributing source support.
--     * Slot: contributing_cell_count
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: VectorFeature Description: A reference geometry with attributes but no measured value over time: SEDAC gROADS road segments, hydrography lines, building footprints.Deliberately **not** an `AmbientValue`. These are inputs to density and distance covariates, not observations, and they have no valid-time interval in any meaningful sense. The distinction matters because the legal operations differ: you compute a length or a distance from these, never a mean.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product
--     * Slot: feature_id
--     * Slot: feature_type
--     * Slot: geometry_type
--     * Slot: feature_geom_wkt
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: feature_attributes Description: Source attributes as a JSON object string. Deliberately opaque: road classifications and similar source-specific schemas should not be forced into the core model, and promoting one to a first-class slot is a decision to take when a real query needs it.
--     * Slot: length_m
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: LocationSet Description: A user-supplied collection of locations — the `locs` argument, promoted to an entity. Carries the identifier field name (`locs_id`), the CRS the user supplied, and a PHI declaration.`phi_status` defaults to `no_phi` and is the gate on everything else: if a location set is flagged, results derived from it must not be written to shared catalog tables or embedded in an exported EnVar record, whose `phi_status` is a required field precisely so this cannot be left implicit.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: description
--     * Slot: locs_id_field Description: Name of the user's identifier column. amadeus already takes this as `locs_id`; recording it means the output column names are derivable rather than conventional.
--     * Slot: crs Description: Declared, never assumed. `calculate_covariates()` currently reprojects `locs` to `crs(from)` silently; an undeclared input CRS is the most common way a spatial join goes quietly wrong.
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: geometry_type
--     * Slot: location_count
--     * Slot: phi_status Description: Whether these locations are or derive from protected health information. Gates persistence and export. Same slot name, range and value set as EnVar's required `phi_status`, so an exported record carries it verbatim.
--     * Slot: provided_by
--     * Slot: created_at
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: Location Description: One location in a set, with an optional validity interval.`valid_from` / `valid_to` exist because a residential address is not timeless. Address histories, known travel intervals and synthetic residence periods all need them, and without them a value is joined to a place the subject may not have occupied on that date. amadeus does not need to model *why* the interval exists — that is the health layer's business — only to honour it in the join.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: location_set
--     * Slot: location_key Description: The user's own identifier value, preserved verbatim.
--     * Slot: location_geom_wkt
--     * Slot: srid Description: Numeric spatial reference identifier, derived from `crs`. Materialised because `ST_SetSRID` takes an integer and the geometry views need it without a lookup.
--     * Slot: buffer_radius_m Description: Buffer radius in metres. Metres, explicitly — amadeus currently interprets `radius` in the units of the *projected* geometry's CRS, which for a geographic CRS means degrees, and the docs describe it as metres. Declaring the unit in the schema removes the ambiguity.
--     * Slot: valid_from
--     * Slot: valid_to
--     * Slot: target_geography_type
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: ExtractionRequest Description: A declarative, reproducible specification of an extraction: which variables, over which locations, for which time window, with which buffer, aggregation, weighting and lags.The rules below are the ones that make the request *checkable before it runs*, which is the point. The extensivity rule in particular turns the project lead's "intensive vs extensive should be explicit" from documentation into a validation error.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: name Description: A short machine-friendly name.
--     * Slot: location_set
--     * Slot: time_window_start
--     * Slot: time_window_end
--     * Slot: temporal_grouping Description: The `.by_time` argument.
--     * Slot: buffer_radius_m Description: Buffer radius in metres. Metres, explicitly — amadeus currently interprets `radius` in the units of the *projected* geometry's CRS, which for a geographic CRS means degrees, and the docs describe it as metres. Declaring the unit in the schema removes the ambiguity.
--     * Slot: extraction_method
--     * Slot: aggregation_method
--     * Slot: weighting_product_variable Description: The product variable used as weights (a population density grid).
--     * Slot: target_geography_type
--     * Slot: output_orientation
--     * Slot: materialization_mode
--     * Slot: source_priority_policy Description: Ordered source preference for reconciliation, e.g. `aqs > airnow`. Required whenever `status_mixing_policy` is `allow_with_policy`.
--     * Slot: status_mixing_policy Description: Whether preliminary and quality-controlled values may appear in one result. Defaults to refusing.
--     * Slot: strict_extensivity_check Description: Whether to reject a request whose aggregation is illegal for the variable's extensivity, or whose variable declares `extensivity: unknown`. Defaults to on: an aggregation that cannot be justified should fail loudly at request time, not produce a plausible number.
--     * Slot: requested_by
--     * Slot: request_timestamp_utc
--     * Slot: request_hash Description: Hash over the normalised request. The idempotency key: the same request re-submitted resolves to the existing result set instead of recomputing it, and a changed request is visibly a different request.
--     * Slot: generated_sql Description: The SQL actually issued. Stored so the result is auditable against the plan the optimiser chose, and so a user can take the query away and run it themselves — which is the "same contract in R, SQL, Python or a container" promise, made concrete.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: AmbientValueAtLocation Description: The output row: an ambient value assigned to a user location for a time interval, with the extraction that produced it recorded alongside. This is what `calculate_covariates()` returns today, plus the metadata it currently drops.Not an exposure. The location may be a person's address and the row may carry a cohort key, but the value is a property of the place: swap the person, keep the address, and it does not change.What today's output has, that this keeps: location key, time, value. What today's output lacks, that this adds: which product variable and which canonical variable (today encoded positionally in a column name like `weasd_0`), the extraction method and buffer actually used, how many source values contributed, coverage, distance to the contributing station, the quality and status of the inputs, and the run that produced it.
--     * Slot: location
--     * Slot: extraction_request
--     * Slot: buffer_radius_m Description: Buffer radius in metres. Metres, explicitly — amadeus currently interprets `radius` in the units of the *projected* geometry's CRS, which for a geographic CRS means degrees, and the docs describe it as metres. Declaring the unit in the schema removes the ambiguity.
--     * Slot: extraction_method
--     * Slot: aggregation_method
--     * Slot: contributing_value_count Description: How many source values went into this one. A buffer mean over one cell and over four hundred cells are different claims; today both come back as an undistinguished number.
--     * Slot: coverage_fraction Description: Fraction of the hex actually covered by contributing source support.
--     * Slot: distance_to_source_m Description: Distance to the contributing station, for nearest-station extraction. Paired with the request's max-distance setting, this is what makes a nearest-station assignment defensible.
--     * Slot: lag_days_applied
--     * Slot: temporal_grouping Description: The `.by_time` argument.
--     * Slot: output_column_name Description: The wide-format column name this value would occupy, for backward compatibility with existing amadeus and beethoven consumers (`weasd_0`, `tmmx_10000`). Generated from the request, not parsed back out of it — the long form is canonical and the wide form is a projection.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: product_variable Description: Which source delivery this is. Carries the native units, the CF metadata and the aggregation — so the row does not have to.
--     * Slot: canonical_variable Description: Denormalised from `product_variable` on purpose. Cross-source queries ("all daily maximum temperature within this polygon") are the common case, and making them a two-hop join is a performance tax on the query the whole architecture exists to serve.
--     * Slot: asset Description: The byte stream this value came from. Every number resolves to a file and a hash — half of objective O-02's "every derived output resolves to source assets and processing runs".
--     * Slot: processing_run Description: Identifier of the `ToolRun` that produced this value. The other half of O-02. A plain string rather than an object reference so the value and provenance modules stay independently loadable; the SQL post-processor adds the foreign key.
--     * Slot: value Description: The value in the canonical variable's units. Null when `null_semantics` explains why.
--     * Slot: value_unit_ucum Description: Units of `value`, denormalised from the canonical variable. Redundant by design: a value row that travels out of the database as CSV or Parquet must not lose its units on the way.
--     * Slot: native_value Description: The value exactly as the source shipped it, before scale, offset and unit conversion. Kept so a conversion bug is discoverable after the fact instead of being baked in irreversibly.
--     * Slot: native_value_unit_ucum
--     * Slot: valid_time_start Description: Inclusive start of the interval the value is valid for. Left-closed, right-open by default (`[start, end)`) — declared here once so no individual dataset has to litigate it.
--     * Slot: valid_time_end Description: Exclusive end of the validity interval.
--     * Slot: reference_time Description: Model initialisation / cycle time, distinct from valid time. Required for HRRR and any forecast product: "valid versus reference time" is called out explicitly in the P0 subdaily work.
--     * Slot: retrieval_time_utc Description: When amadeus obtained the value. Required by the AQS/AirNow rule: with revisable sources, "when we asked" is part of the value's identity.
--     * Slot: vertical_level Description: Pressure or height level for multi-level products (NARR, MERRA-2, GEOS-CF). Part of the value's identity, not an attribute of it.
--     * Slot: vertical_level_unit
--     * Slot: quality_flag Description: The source's quality flag, verbatim, interpreted against the `quality_flag_vocabulary` on the product variable. Never normalised on ingest — normalising discards information, and the vocabularies do not align.
--     * Slot: data_status Description: Per-row, not per-product. The AQS/AirNow rule requires it: a preliminary AirNow value and a quality-controlled AQS value for the same monitor, parameter and hour must be able to coexist as distinct rows.
--     * Slot: source_system Description: Which system supplied this row (`aqs`, `airnow`). Required on every row by the harmonization rule so preliminary and quality-controlled values for the same monitor-hour coexist rather than overwrite.
--     * Slot: source_version Description: The source's own version or revision identifier for this value, where it publishes one.
--     * Slot: null_semantics
--     * Slot: superseded_by Description: Identifier of the row that replaces this one. Revisions append; they do not mutate.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: ToolRun Description: One execution of one amadeus operation, with everything needed to decide whether it can be trusted and whether it can be skipped."Whether it can be skipped" is the idempotency requirement: re-running a workflow with the same inputs and configuration should reuse valid outputs and recover from partial failures without silent duplication. That needs `input_file_sha256`, `output_file_sha256`, `run_arguments` and `status` — not a timestamp and a log line.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: run_role
--     * Slot: tool_name
--     * Slot: tool_version
--     * Slot: function_name Description: The specific amadeus function called (`download_narr`, `process_narr`, `calculate_narr`). More useful than `tool_name` alone, since one package version contains many operations with different failure modes.
--     * Slot: run_arguments Description: The call's arguments as a JSON object string. The single field that decides whether a result is reproducible. Kept opaque because the argument sets differ per function and per source; validating them is the `ExtractionRequest`'s job for the calculate path, and an adapter contract test's job for the download path.
--     * Slot: container_image_repository
--     * Slot: container_image_digest Description: Immutable image digest, not a tag. Tags move; a digest is the only thing that makes "the same container" a checkable claim, and the plan requires digest references in run manifests.
--     * Slot: run_timestamp_utc
--     * Slot: run_duration_seconds
--     * Slot: run_environment Description: Runtime environment as a JSON object string — R version, platform, key package versions (`terra`, `sf`, `exactextractr`), and the database engine and version. The engine matters: a SedonaDB result and a DuckDB result of the same request are not guaranteed identical, and the capability matrix the plan calls for is only auditable if each run says which engine ran it.
--     * Slot: input_file_sha256
--     * Slot: input_row_count
--     * Slot: output_file_sha256
--     * Slot: output_row_count
--     * Slot: status
--     * Slot: log_excerpt
--     * Slot: materialization_mode
--     * Slot: bytes_transferred Description: Network bytes for this run. The benchmark plan's acceptance target is "at least 75% fewer bytes transferred for a remote subset request where the upstream format/API supports pushdown" — which requires this to be measured per run rather than estimated.
--     * Slot: request_count Description: Number of HTTP or API requests issued, for rate-limit accounting.
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: ProvenanceChain Description: A materialised walk of `upstream_runs` from a derived value back to its terminus, plus what the terminus is. Stored rather than always recomputed because it is what an exported EnVar record carries, and because the recursive query is expensive to run per row at export time.
--     * Slot: id Description: Stable identifier for this entity within an AmadeusDB instance.
--     * Slot: terminus_type
--     * Slot: AmadeusDatabase_id Description: Autocreated FK slot
-- # Class: Product_data_genre
--     * Slot: Product_id Description: Autocreated FK slot
--     * Slot: data_genre Description: Thematic classification, from the README source table.
-- # Class: GridDefinition_vertical_levels
--     * Slot: GridDefinition_id Description: Autocreated FK slot
--     * Slot: vertical_levels Description: The pressure or height levels present, when the product is multi-level. NARR and MERRA-2/GEOS pressure-level products encode the level in the `terra` layer name (`variable_level_YYYYMMDD`); recording the level set here is what lets that be parsed reliably instead of positionally.
-- # Class: CanonicalVariable_concept_mappings
--     * Slot: CanonicalVariable_id Description: Autocreated FK slot
--     * Slot: concept_mappings Description: Mappings to any target vocabulary. One generic list rather than a slot per standard, so adding a vocabulary is a new prefix, not a schema change. EnVar and HEW arrived at this identical design independently.
-- # Class: ProductVariable_metadata_gaps
--     * Slot: ProductVariable_id Description: Autocreated FK slot
--     * Slot: metadata_gaps Description: Which metadata this delivery is missing, and why. Makes the Tier 1 backlog ("in the file, discarded by amadeus") countable and therefore reportable as progress.
-- # Class: Asset_variables_present
--     * Slot: Asset_id Description: Autocreated FK slot
--     * Slot: variables_present_id Description: Which variables this asset actually contains. Lets a request skip assets that cannot satisfy it without opening them.
-- # Class: Asset_vertical_levels_present
--     * Slot: Asset_id Description: Autocreated FK slot
--     * Slot: vertical_levels_present
-- # Class: ExtractionRequest_requested_canonical_variables
--     * Slot: ExtractionRequest_id Description: Autocreated FK slot
--     * Slot: requested_canonical_variables_id Description: Request by canonical variable to let the planner choose a source. The preferred form: the user asks for daily maximum temperature, not for `gridmet.tmmx`.
-- # Class: ExtractionRequest_requested_product_variables
--     * Slot: ExtractionRequest_id Description: Autocreated FK slot
--     * Slot: requested_product_variables_id Description: Request a specific source delivery, pinning the answer. Required when reproducing a published result, since the planner's choice may change.
-- # Class: ExtractionRequest_lag_days
--     * Slot: ExtractionRequest_id Description: Autocreated FK slot
--     * Slot: lag_days Description: Lags to compute, in days. `calculate_lagged()` already does this; naming them in the request is what lets the result carry which lag it is.
-- # Class: ToolRun_input_asset_ids
--     * Slot: ToolRun_id Description: Autocreated FK slot
--     * Slot: input_asset_ids Description: Assets consumed by this run.
-- # Class: ToolRun_upstream_runs
--     * Slot: ToolRun_id Description: Autocreated FK slot
--     * Slot: upstream_runs Description: Identifiers of the runs this one consumed. The DAG edge. Typed as strings rather than object references so the table is self-contained and loadable before its own foreign keys resolve — a run may be recorded before its parents are fully written.
-- # Class: ProvenanceChain_chain_steps
--     * Slot: ProvenanceChain_id Description: Autocreated FK slot
--     * Slot: chain_steps Description: Run identifiers, ordered from the derived value back to the terminus.
-- # Class: ProvenanceChain_compatibility_assertions
--     * Slot: ProvenanceChain_id Description: Autocreated FK slot
--     * Slot: compatibility_assertions Description: Assertions that consecutive steps were compatible — matching CRS, units, calendar, day boundary. The seam between two steps belongs to neither step, and this is where a mismatch gets recorded instead of silently absorbed.

CREATE TABLE "AmadeusDatabase" (
	id INTEGER NOT NULL,
	schema_version TEXT NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE "OmopConceptBinding" (
	id INTEGER NOT NULL,
	omop_concept_id INTEGER,
	omop_concept_name TEXT,
	omop_vocabulary_id TEXT,
	omop_domain_id TEXT,
	omop_standard_concept TEXT,
	concept_status VARCHAR(18) NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE "DataSource" (
	id TEXT NOT NULL,
	name TEXT NOT NULL,
	label TEXT,
	description TEXT,
	producer_institution TEXT,
	homepage TEXT,
	access_protocol VARCHAR(15),
	requires_authentication BOOLEAN,
	auth_mechanism TEXT,
	rate_limit_requests_per_second FLOAT,
	license_spdx TEXT,
	citation_apa TEXT,
	doi TEXT,
	amadeus_function_suffix TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "GridDefinition" (
	id TEXT NOT NULL,
	name TEXT,
	label TEXT,
	crs TEXT NOT NULL,
	srid INTEGER,
	grid_mapping_name TEXT,
	proj_string TEXT,
	resolution_x FLOAT NOT NULL,
	resolution_y FLOAT NOT NULL,
	resolution_unit TEXT,
	n_columns INTEGER,
	n_rows INTEGER,
	origin_x FLOAT,
	origin_y FLOAT,
	vertical_level_type TEXT,
	vertical_level_unit TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "CanonicalVariable" (
	id TEXT NOT NULL,
	name TEXT NOT NULL,
	label TEXT,
	description TEXT,
	standard_name TEXT NOT NULL,
	standard_name_authority TEXT,
	units_ucum TEXT NOT NULL,
	units_display TEXT,
	value_data_type VARCHAR(18) NOT NULL,
	extensivity VARCHAR(11) NOT NULL,
	default_aggregation_method VARCHAR(24),
	plausible_min FLOAT,
	plausible_max FLOAT,
	envar_variable_family TEXT,
	"AmadeusDatabase_id" INTEGER,
	omop_concept_binding_id INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id),
	FOREIGN KEY(omop_concept_binding_id) REFERENCES "OmopConceptBinding" (id)
);

CREATE TABLE "LocationSet" (
	id TEXT NOT NULL,
	name TEXT,
	description TEXT,
	locs_id_field TEXT NOT NULL,
	crs TEXT NOT NULL,
	srid INTEGER,
	geometry_type VARCHAR(18),
	location_count INTEGER,
	phi_status VARCHAR(17) NOT NULL,
	provided_by TEXT,
	created_at DATETIME,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "ToolRun" (
	id TEXT NOT NULL,
	run_role VARCHAR(9) NOT NULL,
	tool_name TEXT NOT NULL,
	tool_version TEXT NOT NULL,
	function_name TEXT,
	run_arguments TEXT,
	container_image_repository TEXT,
	container_image_digest TEXT,
	run_timestamp_utc DATETIME NOT NULL,
	run_duration_seconds FLOAT,
	run_environment TEXT,
	input_file_sha256 TEXT,
	input_row_count INTEGER,
	output_file_sha256 TEXT,
	output_row_count INTEGER,
	status VARCHAR(9) NOT NULL,
	log_excerpt TEXT,
	materialization_mode VARCHAR(20),
	bytes_transferred INTEGER,
	request_count INTEGER,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "ProvenanceChain" (
	id TEXT NOT NULL,
	terminus_type VARCHAR(28) NOT NULL,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "Product" (
	id TEXT NOT NULL,
	name TEXT NOT NULL,
	label TEXT,
	description TEXT,
	data_source TEXT NOT NULL,
	product_version TEXT,
	native_format VARCHAR(24),
	spatial_support_type VARCHAR(19) NOT NULL,
	grid_definition TEXT,
	crs TEXT,
	srid INTEGER,
	spatial_extent_bbox_wkt TEXT,
	spatial_extent_descriptor TEXT,
	native_spatial_resolution_m FLOAT,
	native_spatial_resolution_descriptor TEXT,
	temporal_resolution VARCHAR(16) NOT NULL,
	temporal_resolution_iso TEXT,
	temporal_alignment VARCHAR(8),
	day_boundary_convention VARCHAR(21),
	calendar VARCHAR(19),
	temporal_coverage_start DATE,
	temporal_coverage_end DATE,
	update_cadence_iso TEXT,
	typical_latency_iso TEXT,
	default_data_status VARCHAR(18),
	homogenisation_status VARCHAR(15),
	doi TEXT,
	license_spdx TEXT,
	citation_apa TEXT,
	access_url TEXT,
	amadeus_dataset_name TEXT,
	amadeus_process_covariate TEXT,
	amadeus_calculate_covariate TEXT,
	supports_server_side_subset BOOLEAN,
	stac_collection_id TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(data_source) REFERENCES "DataSource" (id),
	FOREIGN KEY(grid_definition) REFERENCES "GridDefinition" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "Location" (
	id TEXT NOT NULL,
	location_set TEXT NOT NULL,
	location_key TEXT NOT NULL,
	location_geom_wkt TEXT NOT NULL,
	srid INTEGER,
	buffer_radius_m FLOAT,
	valid_from DATE,
	valid_to DATE,
	target_geography_type VARCHAR(19),
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(location_set) REFERENCES "LocationSet" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "GridDefinition_vertical_levels" (
	"GridDefinition_id" TEXT,
	vertical_levels FLOAT,
	PRIMARY KEY ("GridDefinition_id", vertical_levels),
	FOREIGN KEY("GridDefinition_id") REFERENCES "GridDefinition" (id)
);

CREATE TABLE "CanonicalVariable_concept_mappings" (
	"CanonicalVariable_id" TEXT,
	concept_mappings TEXT,
	PRIMARY KEY ("CanonicalVariable_id", concept_mappings),
	FOREIGN KEY("CanonicalVariable_id") REFERENCES "CanonicalVariable" (id)
);

CREATE TABLE "ToolRun_input_asset_ids" (
	"ToolRun_id" TEXT,
	input_asset_ids TEXT,
	PRIMARY KEY ("ToolRun_id", input_asset_ids),
	FOREIGN KEY("ToolRun_id") REFERENCES "ToolRun" (id)
);

CREATE TABLE "ToolRun_upstream_runs" (
	"ToolRun_id" TEXT,
	upstream_runs TEXT,
	PRIMARY KEY ("ToolRun_id", upstream_runs),
	FOREIGN KEY("ToolRun_id") REFERENCES "ToolRun" (id)
);

CREATE TABLE "ProvenanceChain_chain_steps" (
	"ProvenanceChain_id" TEXT,
	chain_steps TEXT,
	PRIMARY KEY ("ProvenanceChain_id", chain_steps),
	FOREIGN KEY("ProvenanceChain_id") REFERENCES "ProvenanceChain" (id)
);

CREATE TABLE "ProvenanceChain_compatibility_assertions" (
	"ProvenanceChain_id" TEXT,
	compatibility_assertions TEXT,
	PRIMARY KEY ("ProvenanceChain_id", compatibility_assertions),
	FOREIGN KEY("ProvenanceChain_id") REFERENCES "ProvenanceChain" (id)
);

CREATE TABLE "ProductVariable" (
	id TEXT NOT NULL,
	product TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	native_name TEXT NOT NULL,
	label TEXT,
	native_units_ucum TEXT,
	native_value_scale_factor FLOAT,
	native_value_offset FLOAT,
	unit_conversion_formula TEXT,
	cf_standard_name TEXT,
	cf_cell_methods TEXT,
	aggregation_method VARCHAR(24),
	aggregation_window_iso TEXT,
	temporal_alignment VARCHAR(8),
	vertical_level_type TEXT,
	missing_value_sentinel TEXT,
	quality_flag_vocabulary TEXT,
	layer_name_template TEXT,
	amadeus_variable_code TEXT,
	value_data_type VARCHAR(18),
	extensivity VARCHAR(11),
	harmonization_note TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product) REFERENCES "Product" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "Asset" (
	id TEXT NOT NULL,
	product TEXT NOT NULL,
	asset_key TEXT,
	url TEXT,
	local_path TEXT,
	media_type TEXT,
	native_format VARCHAR(24),
	size_bytes INTEGER,
	sha256 TEXT,
	source_last_modified DATETIME,
	download_timestamp_utc DATETIME,
	valid_time_start DATETIME,
	valid_time_end DATETIME,
	reference_time DATETIME,
	bbox_wkt TEXT,
	srid INTEGER,
	stac_collection_id TEXT,
	stac_item_id TEXT,
	materialization_mode VARCHAR(20) NOT NULL,
	cache_expires_at DATETIME,
	value_state VARCHAR(15) NOT NULL,
	produced_by_run TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product) REFERENCES "Product" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "Product_data_genre" (
	"Product_id" TEXT,
	data_genre VARCHAR(22),
	PRIMARY KEY ("Product_id", data_genre),
	FOREIGN KEY("Product_id") REFERENCES "Product" (id)
);

CREATE TABLE "AmbientValue" (
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id)
);

CREATE TABLE "StationObservation" (
	station_id TEXT NOT NULL,
	station_name TEXT,
	monitor_id TEXT,
	parameter_code TEXT,
	poc INTEGER,
	sampling_duration_iso TEXT,
	station_geom_wkt TEXT NOT NULL,
	srid INTEGER,
	elevation_m FLOAT,
	station_valid_from DATE,
	station_valid_to DATE,
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "GridCellValue" (
	grid_definition TEXT NOT NULL,
	cell_x INTEGER NOT NULL,
	cell_y INTEGER NOT NULL,
	cell_centroid_wkt TEXT NOT NULL,
	cell_polygon_wkt TEXT,
	srid INTEGER,
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	UNIQUE (product_variable, grid_definition, cell_x, cell_y, vertical_level, valid_time_start, source_system, data_status),
	FOREIGN KEY(grid_definition) REFERENCES "GridDefinition" (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "AreaValue" (
	area_id TEXT NOT NULL,
	area_type VARCHAR(19) NOT NULL,
	area_role VARCHAR(14) NOT NULL,
	area_code TEXT,
	area_name TEXT,
	area_geom_wkt TEXT NOT NULL,
	srid INTEGER,
	area_km2 FLOAT,
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "HexCellValue" (
	h3_cell TEXT NOT NULL,
	h3_resolution INTEGER NOT NULL,
	hex_centroid_wkt TEXT,
	srid INTEGER,
	source_support_type VARCHAR(19) NOT NULL,
	hexification_method VARCHAR(24) NOT NULL,
	coverage_fraction FLOAT NOT NULL,
	contributing_cell_count INTEGER,
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "VectorFeature" (
	id TEXT NOT NULL,
	product TEXT NOT NULL,
	feature_id TEXT,
	feature_type TEXT,
	geometry_type VARCHAR(18) NOT NULL,
	feature_geom_wkt TEXT NOT NULL,
	srid INTEGER,
	feature_attributes TEXT,
	length_m FLOAT,
	asset TEXT,
	processing_run TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(product) REFERENCES "Product" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "ExtractionRequest" (
	id TEXT NOT NULL,
	name TEXT,
	location_set TEXT NOT NULL,
	time_window_start DATETIME NOT NULL,
	time_window_end DATETIME NOT NULL,
	temporal_grouping VARCHAR(5),
	buffer_radius_m FLOAT,
	extraction_method VARCHAR(41) NOT NULL,
	aggregation_method VARCHAR(24),
	weighting_product_variable TEXT,
	target_geography_type VARCHAR(19),
	output_orientation VARCHAR(4),
	materialization_mode VARCHAR(20),
	source_priority_policy TEXT,
	status_mixing_policy VARCHAR(17),
	strict_extensivity_check VARCHAR(10),
	requested_by TEXT,
	request_timestamp_utc DATETIME,
	request_hash TEXT NOT NULL,
	generated_sql TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(location_set) REFERENCES "LocationSet" (id),
	FOREIGN KEY(weighting_product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "ProductVariable_metadata_gaps" (
	"ProductVariable_id" TEXT,
	metadata_gaps VARCHAR(28),
	PRIMARY KEY ("ProductVariable_id", metadata_gaps),
	FOREIGN KEY("ProductVariable_id") REFERENCES "ProductVariable" (id)
);

CREATE TABLE "Asset_variables_present" (
	"Asset_id" TEXT,
	variables_present_id TEXT,
	PRIMARY KEY ("Asset_id", variables_present_id),
	FOREIGN KEY("Asset_id") REFERENCES "Asset" (id),
	FOREIGN KEY(variables_present_id) REFERENCES "ProductVariable" (id)
);

CREATE TABLE "Asset_vertical_levels_present" (
	"Asset_id" TEXT,
	vertical_levels_present FLOAT,
	PRIMARY KEY ("Asset_id", vertical_levels_present),
	FOREIGN KEY("Asset_id") REFERENCES "Asset" (id)
);

CREATE TABLE "AmbientValueAtLocation" (
	location TEXT NOT NULL,
	extraction_request TEXT NOT NULL,
	buffer_radius_m FLOAT,
	extraction_method VARCHAR(41) NOT NULL,
	aggregation_method VARCHAR(24),
	contributing_value_count INTEGER,
	coverage_fraction FLOAT,
	distance_to_source_m FLOAT,
	lag_days_applied INTEGER,
	temporal_grouping VARCHAR(5),
	output_column_name TEXT,
	id TEXT NOT NULL,
	product_variable TEXT NOT NULL,
	canonical_variable TEXT NOT NULL,
	asset TEXT,
	processing_run TEXT,
	value FLOAT,
	value_unit_ucum TEXT,
	native_value FLOAT,
	native_value_unit_ucum TEXT,
	valid_time_start DATETIME NOT NULL,
	valid_time_end DATETIME,
	reference_time DATETIME,
	retrieval_time_utc DATETIME,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	"AmadeusDatabase_id" INTEGER,
	PRIMARY KEY (id),
	FOREIGN KEY(location) REFERENCES "Location" (id),
	FOREIGN KEY(extraction_request) REFERENCES "ExtractionRequest" (id),
	FOREIGN KEY(product_variable) REFERENCES "ProductVariable" (id),
	FOREIGN KEY(canonical_variable) REFERENCES "CanonicalVariable" (id),
	FOREIGN KEY(asset) REFERENCES "Asset" (id),
	FOREIGN KEY("AmadeusDatabase_id") REFERENCES "AmadeusDatabase" (id)
);

CREATE TABLE "ExtractionRequest_requested_canonical_variables" (
	"ExtractionRequest_id" TEXT,
	requested_canonical_variables_id TEXT,
	PRIMARY KEY ("ExtractionRequest_id", requested_canonical_variables_id),
	FOREIGN KEY("ExtractionRequest_id") REFERENCES "ExtractionRequest" (id),
	FOREIGN KEY(requested_canonical_variables_id) REFERENCES "CanonicalVariable" (id)
);

CREATE TABLE "ExtractionRequest_requested_product_variables" (
	"ExtractionRequest_id" TEXT,
	requested_product_variables_id TEXT,
	PRIMARY KEY ("ExtractionRequest_id", requested_product_variables_id),
	FOREIGN KEY("ExtractionRequest_id") REFERENCES "ExtractionRequest" (id),
	FOREIGN KEY(requested_product_variables_id) REFERENCES "ProductVariable" (id)
);

CREATE TABLE "ExtractionRequest_lag_days" (
	"ExtractionRequest_id" TEXT,
	lag_days INTEGER,
	PRIMARY KEY ("ExtractionRequest_id", lag_days),
	FOREIGN KEY("ExtractionRequest_id") REFERENCES "ExtractionRequest" (id)
);

