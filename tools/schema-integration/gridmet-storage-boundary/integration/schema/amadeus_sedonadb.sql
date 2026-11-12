-- AmadeusDB DDL — dialect: sedonadb
-- Generated from amadeus_schema.yaml via linkml gen-sqltables + scripts/gen_backend_ddl.py
-- 32 tables, 8 geometry views

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
	PRIMARY KEY (id)
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
	PRIMARY KEY (id)
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
	omop_concept_binding_id INTEGER,
	PRIMARY KEY (id)
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
	created_at TIMESTAMP,
	PRIMARY KEY (id)
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
	run_timestamp_utc TIMESTAMP NOT NULL,
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
	PRIMARY KEY (id)
);

CREATE TABLE "ProvenanceChain" (
	id TEXT NOT NULL,
	terminus_type VARCHAR(28) NOT NULL,
	PRIMARY KEY (id)
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
	PRIMARY KEY (id)
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
	PRIMARY KEY (id)
);

CREATE TABLE "GridDefinition_vertical_levels" (
	"GridDefinition_id" TEXT,
	vertical_levels FLOAT,
	PRIMARY KEY ("GridDefinition_id", vertical_levels)
);

CREATE TABLE "CanonicalVariable_concept_mappings" (
	"CanonicalVariable_id" TEXT,
	concept_mappings TEXT,
	PRIMARY KEY ("CanonicalVariable_id", concept_mappings)
);

CREATE TABLE "ToolRun_input_asset_ids" (
	"ToolRun_id" TEXT,
	input_asset_ids TEXT,
	PRIMARY KEY ("ToolRun_id", input_asset_ids)
);

CREATE TABLE "ToolRun_upstream_runs" (
	"ToolRun_id" TEXT,
	upstream_runs TEXT,
	PRIMARY KEY ("ToolRun_id", upstream_runs)
);

CREATE TABLE "ProvenanceChain_chain_steps" (
	"ProvenanceChain_id" TEXT,
	chain_steps TEXT,
	PRIMARY KEY ("ProvenanceChain_id", chain_steps)
);

CREATE TABLE "ProvenanceChain_compatibility_assertions" (
	"ProvenanceChain_id" TEXT,
	compatibility_assertions TEXT,
	PRIMARY KEY ("ProvenanceChain_id", compatibility_assertions)
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
	PRIMARY KEY (id)
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
	source_last_modified TIMESTAMP,
	download_timestamp_utc TIMESTAMP,
	valid_time_start TIMESTAMP,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	bbox_wkt TEXT,
	srid INTEGER,
	stac_collection_id TEXT,
	stac_item_id TEXT,
	materialization_mode VARCHAR(20) NOT NULL,
	cache_expires_at TIMESTAMP,
	value_state VARCHAR(15) NOT NULL,
	produced_by_run TEXT,
	PRIMARY KEY (id)
);

CREATE TABLE "Product_data_genre" (
	"Product_id" TEXT,
	data_genre VARCHAR(22),
	PRIMARY KEY ("Product_id", data_genre)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id),
	UNIQUE (product_variable, grid_definition, cell_x, cell_y, vertical_level, valid_time_start, source_system, data_status)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id)
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
	PRIMARY KEY (id)
);

CREATE TABLE "ExtractionRequest" (
	id TEXT NOT NULL,
	name TEXT,
	location_set TEXT NOT NULL,
	time_window_start TIMESTAMP NOT NULL,
	time_window_end TIMESTAMP NOT NULL,
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
	request_timestamp_utc TIMESTAMP,
	request_hash TEXT NOT NULL,
	generated_sql TEXT,
	PRIMARY KEY (id)
);

CREATE TABLE "ProductVariable_metadata_gaps" (
	"ProductVariable_id" TEXT,
	metadata_gaps VARCHAR(28),
	PRIMARY KEY ("ProductVariable_id", metadata_gaps)
);

CREATE TABLE "Asset_variables_present" (
	"Asset_id" TEXT,
	variables_present_id TEXT,
	PRIMARY KEY ("Asset_id", variables_present_id)
);

CREATE TABLE "Asset_vertical_levels_present" (
	"Asset_id" TEXT,
	vertical_levels_present FLOAT,
	PRIMARY KEY ("Asset_id", vertical_levels_present)
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
	valid_time_start TIMESTAMP NOT NULL,
	valid_time_end TIMESTAMP,
	reference_time TIMESTAMP,
	retrieval_time_utc TIMESTAMP,
	vertical_level FLOAT,
	vertical_level_unit TEXT,
	quality_flag TEXT,
	data_status VARCHAR(18) NOT NULL,
	source_system TEXT NOT NULL,
	source_version TEXT,
	null_semantics VARCHAR(15) NOT NULL,
	superseded_by TEXT,
	PRIMARY KEY (id)
);

CREATE TABLE "ExtractionRequest_requested_canonical_variables" (
	"ExtractionRequest_id" TEXT,
	requested_canonical_variables_id TEXT,
	PRIMARY KEY ("ExtractionRequest_id", requested_canonical_variables_id)
);

CREATE TABLE "ExtractionRequest_requested_product_variables" (
	"ExtractionRequest_id" TEXT,
	requested_product_variables_id TEXT,
	PRIMARY KEY ("ExtractionRequest_id", requested_product_variables_id)
);

CREATE TABLE "ExtractionRequest_lag_days" (
	"ExtractionRequest_id" TEXT,
	lag_days INTEGER,
	PRIMARY KEY ("ExtractionRequest_id", lag_days)
);


-- ==========================================================================
-- Geometry views. LinkML has no spatial type, and SedonaDB refuses a
-- GEOMETRY column in CREATE TABLE, so `WktLiteral` slots are stored as text
-- and promoted here. Query the views, load the tables.
-- ==========================================================================

-- geometry promotion for AreaValue: WKT text -> native geometry
CREATE OR REPLACE VIEW "AreaValue_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(area_geom_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS area_geom
FROM "AreaValue";

-- geometry promotion for Asset: WKT text -> native geometry
CREATE OR REPLACE VIEW "Asset_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(bbox_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS bbox
FROM "Asset";

-- geometry promotion for GridCellValue: WKT text -> native geometry
CREATE OR REPLACE VIEW "GridCellValue_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(cell_centroid_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS cell_centroid,
	ST_SetSRID(ST_GeomFromText(cell_polygon_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS cell_polygon
FROM "GridCellValue";

-- geometry promotion for HexCellValue: WKT text -> native geometry
CREATE OR REPLACE VIEW "HexCellValue_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(hex_centroid_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS hex_centroid
FROM "HexCellValue";

-- geometry promotion for Location: WKT text -> native geometry
CREATE OR REPLACE VIEW "Location_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(location_geom_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS location_geom
FROM "Location";

-- geometry promotion for Product: WKT text -> native geometry
CREATE OR REPLACE VIEW "Product_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(spatial_extent_bbox_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS spatial_extent_bbox
FROM "Product";

-- geometry promotion for StationObservation: WKT text -> native geometry
CREATE OR REPLACE VIEW "StationObservation_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(station_geom_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS station_geom
FROM "StationObservation";

-- geometry promotion for VectorFeature: WKT text -> native geometry
CREATE OR REPLACE VIEW "VectorFeature_geo" AS
SELECT
	*,
	ST_SetSRID(ST_GeomFromText(feature_geom_wkt), CAST(COALESCE(srid, 4326) AS INTEGER)) AS feature_geom
FROM "VectorFeature";

