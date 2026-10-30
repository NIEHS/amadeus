-- Integrity checks LinkML cannot express: referential integrity,
-- uniqueness, and enum domains. Generated from the FK graph,
-- unique_keys and enum ranges. Every query must return n = 0.
-- 102 checks

-- [FK] CanonicalVariable.omop_concept_binding_id -> OmopConceptBinding.id
SELECT COUNT(*) AS n FROM "CanonicalVariable" c
  LEFT JOIN "OmopConceptBinding" p ON c."omop_concept_binding_id" = p."id"
  WHERE c."omop_concept_binding_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] Product.data_source -> DataSource.id
SELECT COUNT(*) AS n FROM "Product" c
  LEFT JOIN "DataSource" p ON c."data_source" = p."id"
  WHERE c."data_source" IS NOT NULL AND p."id" IS NULL;

-- [FK] Product.grid_definition -> GridDefinition.id
SELECT COUNT(*) AS n FROM "Product" c
  LEFT JOIN "GridDefinition" p ON c."grid_definition" = p."id"
  WHERE c."grid_definition" IS NOT NULL AND p."id" IS NULL;

-- [FK] Location.location_set -> LocationSet.id
SELECT COUNT(*) AS n FROM "Location" c
  LEFT JOIN "LocationSet" p ON c."location_set" = p."id"
  WHERE c."location_set" IS NOT NULL AND p."id" IS NULL;

-- [FK] GridDefinition_vertical_levels.GridDefinition_id -> GridDefinition.id
SELECT COUNT(*) AS n FROM "GridDefinition_vertical_levels" c
  LEFT JOIN "GridDefinition" p ON c."GridDefinition_id" = p."id"
  WHERE c."GridDefinition_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] CanonicalVariable_concept_mappings.CanonicalVariable_id -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "CanonicalVariable_concept_mappings" c
  LEFT JOIN "CanonicalVariable" p ON c."CanonicalVariable_id" = p."id"
  WHERE c."CanonicalVariable_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ToolRun_input_asset_ids.ToolRun_id -> ToolRun.id
SELECT COUNT(*) AS n FROM "ToolRun_input_asset_ids" c
  LEFT JOIN "ToolRun" p ON c."ToolRun_id" = p."id"
  WHERE c."ToolRun_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ToolRun_upstream_runs.ToolRun_id -> ToolRun.id
SELECT COUNT(*) AS n FROM "ToolRun_upstream_runs" c
  LEFT JOIN "ToolRun" p ON c."ToolRun_id" = p."id"
  WHERE c."ToolRun_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ProvenanceChain_chain_steps.ProvenanceChain_id -> ProvenanceChain.id
SELECT COUNT(*) AS n FROM "ProvenanceChain_chain_steps" c
  LEFT JOIN "ProvenanceChain" p ON c."ProvenanceChain_id" = p."id"
  WHERE c."ProvenanceChain_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ProvenanceChain_compatibility_assertions.ProvenanceChain_id -> ProvenanceChain.id
SELECT COUNT(*) AS n FROM "ProvenanceChain_compatibility_assertions" c
  LEFT JOIN "ProvenanceChain" p ON c."ProvenanceChain_id" = p."id"
  WHERE c."ProvenanceChain_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ProductVariable.product -> Product.id
SELECT COUNT(*) AS n FROM "ProductVariable" c
  LEFT JOIN "Product" p ON c."product" = p."id"
  WHERE c."product" IS NOT NULL AND p."id" IS NULL;

-- [FK] ProductVariable.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "ProductVariable" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] Asset.product -> Product.id
SELECT COUNT(*) AS n FROM "Asset" c
  LEFT JOIN "Product" p ON c."product" = p."id"
  WHERE c."product" IS NOT NULL AND p."id" IS NULL;

-- [FK] Product_data_genre.Product_id -> Product.id
SELECT COUNT(*) AS n FROM "Product_data_genre" c
  LEFT JOIN "Product" p ON c."Product_id" = p."id"
  WHERE c."Product_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValue.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "AmbientValue" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValue.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "AmbientValue" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValue.asset -> Asset.id
SELECT COUNT(*) AS n FROM "AmbientValue" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] StationObservation.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "StationObservation" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] StationObservation.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "StationObservation" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] StationObservation.asset -> Asset.id
SELECT COUNT(*) AS n FROM "StationObservation" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] GridCellValue.grid_definition -> GridDefinition.id
SELECT COUNT(*) AS n FROM "GridCellValue" c
  LEFT JOIN "GridDefinition" p ON c."grid_definition" = p."id"
  WHERE c."grid_definition" IS NOT NULL AND p."id" IS NULL;

-- [FK] GridCellValue.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "GridCellValue" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] GridCellValue.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "GridCellValue" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] GridCellValue.asset -> Asset.id
SELECT COUNT(*) AS n FROM "GridCellValue" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [UNIQUE] GridCellValue (product_variable, grid_definition, cell_x, cell_y, vertical_level, valid_time_start, source_system, data_status)
SELECT COUNT(*) AS n FROM (
  SELECT COALESCE(CAST("product_variable" AS VARCHAR), '␀NULL'), COALESCE(CAST("grid_definition" AS VARCHAR), '␀NULL'), COALESCE(CAST("cell_x" AS VARCHAR), '␀NULL'), COALESCE(CAST("cell_y" AS VARCHAR), '␀NULL'), COALESCE(CAST("vertical_level" AS VARCHAR), '␀NULL'), COALESCE(CAST("valid_time_start" AS VARCHAR), '␀NULL'), COALESCE(CAST("source_system" AS VARCHAR), '␀NULL'), COALESCE(CAST("data_status" AS VARCHAR), '␀NULL') FROM "GridCellValue"
  GROUP BY COALESCE(CAST("product_variable" AS VARCHAR), '␀NULL'), COALESCE(CAST("grid_definition" AS VARCHAR), '␀NULL'), COALESCE(CAST("cell_x" AS VARCHAR), '␀NULL'), COALESCE(CAST("cell_y" AS VARCHAR), '␀NULL'), COALESCE(CAST("vertical_level" AS VARCHAR), '␀NULL'), COALESCE(CAST("valid_time_start" AS VARCHAR), '␀NULL'), COALESCE(CAST("source_system" AS VARCHAR), '␀NULL'), COALESCE(CAST("data_status" AS VARCHAR), '␀NULL') HAVING COUNT(*) > 1
) d;

-- [FK] AreaValue.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "AreaValue" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AreaValue.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "AreaValue" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AreaValue.asset -> Asset.id
SELECT COUNT(*) AS n FROM "AreaValue" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] HexCellValue.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "HexCellValue" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] HexCellValue.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "HexCellValue" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] HexCellValue.asset -> Asset.id
SELECT COUNT(*) AS n FROM "HexCellValue" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] VectorFeature.product -> Product.id
SELECT COUNT(*) AS n FROM "VectorFeature" c
  LEFT JOIN "Product" p ON c."product" = p."id"
  WHERE c."product" IS NOT NULL AND p."id" IS NULL;

-- [FK] VectorFeature.asset -> Asset.id
SELECT COUNT(*) AS n FROM "VectorFeature" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest.location_set -> LocationSet.id
SELECT COUNT(*) AS n FROM "ExtractionRequest" c
  LEFT JOIN "LocationSet" p ON c."location_set" = p."id"
  WHERE c."location_set" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest.weighting_product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "ExtractionRequest" c
  LEFT JOIN "ProductVariable" p ON c."weighting_product_variable" = p."id"
  WHERE c."weighting_product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] ProductVariable_metadata_gaps.ProductVariable_id -> ProductVariable.id
SELECT COUNT(*) AS n FROM "ProductVariable_metadata_gaps" c
  LEFT JOIN "ProductVariable" p ON c."ProductVariable_id" = p."id"
  WHERE c."ProductVariable_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] Asset_variables_present.Asset_id -> Asset.id
SELECT COUNT(*) AS n FROM "Asset_variables_present" c
  LEFT JOIN "Asset" p ON c."Asset_id" = p."id"
  WHERE c."Asset_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] Asset_variables_present.variables_present_id -> ProductVariable.id
SELECT COUNT(*) AS n FROM "Asset_variables_present" c
  LEFT JOIN "ProductVariable" p ON c."variables_present_id" = p."id"
  WHERE c."variables_present_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] Asset_vertical_levels_present.Asset_id -> Asset.id
SELECT COUNT(*) AS n FROM "Asset_vertical_levels_present" c
  LEFT JOIN "Asset" p ON c."Asset_id" = p."id"
  WHERE c."Asset_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValueAtLocation.location -> Location.id
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation" c
  LEFT JOIN "Location" p ON c."location" = p."id"
  WHERE c."location" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValueAtLocation.extraction_request -> ExtractionRequest.id
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation" c
  LEFT JOIN "ExtractionRequest" p ON c."extraction_request" = p."id"
  WHERE c."extraction_request" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValueAtLocation.product_variable -> ProductVariable.id
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation" c
  LEFT JOIN "ProductVariable" p ON c."product_variable" = p."id"
  WHERE c."product_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValueAtLocation.canonical_variable -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation" c
  LEFT JOIN "CanonicalVariable" p ON c."canonical_variable" = p."id"
  WHERE c."canonical_variable" IS NOT NULL AND p."id" IS NULL;

-- [FK] AmbientValueAtLocation.asset -> Asset.id
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation" c
  LEFT JOIN "Asset" p ON c."asset" = p."id"
  WHERE c."asset" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest_requested_canonical_variables.ExtractionRequest_id -> ExtractionRequest.id
SELECT COUNT(*) AS n FROM "ExtractionRequest_requested_canonical_variables" c
  LEFT JOIN "ExtractionRequest" p ON c."ExtractionRequest_id" = p."id"
  WHERE c."ExtractionRequest_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest_requested_canonical_variables.requested_canonical_variables_id -> CanonicalVariable.id
SELECT COUNT(*) AS n FROM "ExtractionRequest_requested_canonical_variables" c
  LEFT JOIN "CanonicalVariable" p ON c."requested_canonical_variables_id" = p."id"
  WHERE c."requested_canonical_variables_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest_requested_product_variables.ExtractionRequest_id -> ExtractionRequest.id
SELECT COUNT(*) AS n FROM "ExtractionRequest_requested_product_variables" c
  LEFT JOIN "ExtractionRequest" p ON c."ExtractionRequest_id" = p."id"
  WHERE c."ExtractionRequest_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest_requested_product_variables.requested_product_variables_id -> ProductVariable.id
SELECT COUNT(*) AS n FROM "ExtractionRequest_requested_product_variables" c
  LEFT JOIN "ProductVariable" p ON c."requested_product_variables_id" = p."id"
  WHERE c."requested_product_variables_id" IS NOT NULL AND p."id" IS NULL;

-- [FK] ExtractionRequest_lag_days.ExtractionRequest_id -> ExtractionRequest.id
SELECT COUNT(*) AS n FROM "ExtractionRequest_lag_days" c
  LEFT JOIN "ExtractionRequest" p ON c."ExtractionRequest_id" = p."id"
  WHERE c."ExtractionRequest_id" IS NOT NULL AND p."id" IS NULL;

-- [ENUM] DataSource.access_protocol in AccessProtocolEnum
SELECT COUNT(*) AS n FROM "DataSource"
  WHERE "access_protocol" IS NOT NULL AND "access_protocol" NOT IN ('https_bulk_file', 'ftp', 's3_object_store', 'stac_api', 'opendap', 'thredds', 'rest_api', 'harmony', 'arcgis_service');

-- [ENUM] Product.native_format in NativeFormatEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "native_format" IS NOT NULL AND "native_format" NOT IN ('netcdf4_cf', 'netcdf3', 'hdf4', 'hdf5', 'geotiff', 'cloud_optimized_geotiff', 'ascii_grid', 'grib1', 'grib2', 'bil', 'shapefile', 'geodatabase', 'kml', 'csv_station_observations', 'pipe_delimited_text', 'fixed_width_text', 'zarr', 'parquet', 'geoparquet');

-- [ENUM] Product.spatial_support_type in SpatialSupportTypeEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "spatial_support_type" IS NOT NULL AND "spatial_support_type" NOT IN ('point', 'monitoring_station', 'raster_grid_cell', 'model_grid_cell', 'polygon', 'administrative_area', 'statistical_area', 'hex_cell', 'line_feature', 'other');

-- [ENUM] Product.temporal_resolution in TemporalResolutionEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "temporal_resolution" IS NOT NULL AND "temporal_resolution" NOT IN ('instantaneous', 'subhourly', 'hourly', 'three_hourly', 'daily', 'monthly', 'seasonal', 'annual', 'multi_year_epoch', 'timeless');

-- [ENUM] Product.temporal_alignment in TemporalAlignmentEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "temporal_alignment" IS NOT NULL AND "temporal_alignment" NOT IN ('start', 'center', 'end', 'interval', 'unknown');

-- [ENUM] Product.day_boundary_convention in DayBoundaryConventionEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "day_boundary_convention" IS NOT NULL AND "day_boundary_convention" NOT IN ('local_midnight', 'utc_midnight', 'ending_1200_gmt', 'solar_noon_centered', 'observation_dependent', 'not_applicable');

-- [ENUM] Product.calendar in CalendarEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "calendar" IS NOT NULL AND "calendar" NOT IN ('gregorian', 'proleptic_gregorian', 'noleap', 'daymet_365', 'all_leap', 'day_360', 'julian');

-- [ENUM] Product.default_data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "default_data_status" IS NOT NULL AND "default_data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] Product.homogenisation_status in HomogenisationStatusEnum
SELECT COUNT(*) AS n FROM "Product"
  WHERE "homogenisation_status" IS NOT NULL AND "homogenisation_status" NOT IN ('homogenised', 'not_homogenised', 'partial');

-- [ENUM] CanonicalVariable.value_data_type in ValueDataTypeEnum
SELECT COUNT(*) AS n FROM "CanonicalVariable"
  WHERE "value_data_type" IS NOT NULL AND "value_data_type" NOT IN ('continuous_numeric', 'categorical', 'binary_flag', 'count', 'event_marker', 'proportion');

-- [ENUM] CanonicalVariable.extensivity in VariableExtensivityEnum
SELECT COUNT(*) AS n FROM "CanonicalVariable"
  WHERE "extensivity" IS NOT NULL AND "extensivity" NOT IN ('intensive', 'extensive', 'categorical', 'unknown');

-- [ENUM] CanonicalVariable.default_aggregation_method in AggregationMethodEnum
SELECT COUNT(*) AS n FROM "CanonicalVariable"
  WHERE "default_aggregation_method" IS NOT NULL AND "default_aggregation_method" NOT IN ('instantaneous', 'mean', 'minimum', 'maximum', 'median', 'sum', 'count', 'percentile', 'cumulative', 'area_weighted_mean', 'population_weighted_mean', 'mode', 'class_proportion', 'density', 'nearest', 'other');

-- [ENUM] ProductVariable.aggregation_method in AggregationMethodEnum
SELECT COUNT(*) AS n FROM "ProductVariable"
  WHERE "aggregation_method" IS NOT NULL AND "aggregation_method" NOT IN ('instantaneous', 'mean', 'minimum', 'maximum', 'median', 'sum', 'count', 'percentile', 'cumulative', 'area_weighted_mean', 'population_weighted_mean', 'mode', 'class_proportion', 'density', 'nearest', 'other');

-- [ENUM] ProductVariable.temporal_alignment in TemporalAlignmentEnum
SELECT COUNT(*) AS n FROM "ProductVariable"
  WHERE "temporal_alignment" IS NOT NULL AND "temporal_alignment" NOT IN ('start', 'center', 'end', 'interval', 'unknown');

-- [ENUM] ProductVariable.value_data_type in ValueDataTypeEnum
SELECT COUNT(*) AS n FROM "ProductVariable"
  WHERE "value_data_type" IS NOT NULL AND "value_data_type" NOT IN ('continuous_numeric', 'categorical', 'binary_flag', 'count', 'event_marker', 'proportion');

-- [ENUM] ProductVariable.extensivity in VariableExtensivityEnum
SELECT COUNT(*) AS n FROM "ProductVariable"
  WHERE "extensivity" IS NOT NULL AND "extensivity" NOT IN ('intensive', 'extensive', 'categorical', 'unknown');

-- [ENUM] OmopConceptBinding.concept_status in ConceptStatusEnum
SELECT COUNT(*) AS n FROM "OmopConceptBinding"
  WHERE "concept_status" IS NOT NULL AND "concept_status" NOT IN ('mapped_standard', 'mapped_nonstandard', 'candidate_mapping', 'vocabulary_gap', 'not_evaluated');

-- [ENUM] Asset.native_format in NativeFormatEnum
SELECT COUNT(*) AS n FROM "Asset"
  WHERE "native_format" IS NOT NULL AND "native_format" NOT IN ('netcdf4_cf', 'netcdf3', 'hdf4', 'hdf5', 'geotiff', 'cloud_optimized_geotiff', 'ascii_grid', 'grib1', 'grib2', 'bil', 'shapefile', 'geodatabase', 'kml', 'csv_station_observations', 'pipe_delimited_text', 'fixed_width_text', 'zarr', 'parquet', 'geoparquet');

-- [ENUM] Asset.materialization_mode in MaterializationModeEnum
SELECT COUNT(*) AS n FROM "Asset"
  WHERE "materialization_mode" IS NOT NULL AND "materialization_mode" NOT IN ('direct_remote_scan', 'server_side_subset', 'bounded_cache', 'full_materialization');

-- [ENUM] Asset.value_state in ValueMaterializationEnum
SELECT COUNT(*) AS n FROM "Asset"
  WHERE "value_state" IS NOT NULL AND "value_state" NOT IN ('registered_only', 'materialized');

-- [ENUM] StationObservation.data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "StationObservation"
  WHERE "data_status" IS NOT NULL AND "data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] StationObservation.null_semantics in NullSemanticsEnum
SELECT COUNT(*) AS n FROM "StationObservation"
  WHERE "null_semantics" IS NOT NULL AND "null_semantics" NOT IN ('present', 'structural_null', 'derived_null', 'true_zero');

-- [ENUM] GridCellValue.data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "GridCellValue"
  WHERE "data_status" IS NOT NULL AND "data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] GridCellValue.null_semantics in NullSemanticsEnum
SELECT COUNT(*) AS n FROM "GridCellValue"
  WHERE "null_semantics" IS NOT NULL AND "null_semantics" NOT IN ('present', 'structural_null', 'derived_null', 'true_zero');

-- [ENUM] AreaValue.area_type in AreaTypeEnum
SELECT COUNT(*) AS n FROM "AreaValue"
  WHERE "area_type" IS NOT NULL AND "area_type" NOT IN ('county', 'state', 'census_tract', 'census_block_group', 'zcta', 'hydrologic_unit', 'ecoregion', 'smoke_plume', 'drought_area', 'public_water_system', 'custom');

-- [ENUM] AreaValue.area_role in AreaRoleEnum
SELECT COUNT(*) AS n FROM "AreaValue"
  WHERE "area_role" IS NOT NULL AND "area_role" NOT IN ('administrative', 'episodic');

-- [ENUM] AreaValue.data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "AreaValue"
  WHERE "data_status" IS NOT NULL AND "data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] AreaValue.null_semantics in NullSemanticsEnum
SELECT COUNT(*) AS n FROM "AreaValue"
  WHERE "null_semantics" IS NOT NULL AND "null_semantics" NOT IN ('present', 'structural_null', 'derived_null', 'true_zero');

-- [ENUM] HexCellValue.source_support_type in SpatialSupportTypeEnum
SELECT COUNT(*) AS n FROM "HexCellValue"
  WHERE "source_support_type" IS NOT NULL AND "source_support_type" NOT IN ('point', 'monitoring_station', 'raster_grid_cell', 'model_grid_cell', 'polygon', 'administrative_area', 'statistical_area', 'hex_cell', 'line_feature', 'other');

-- [ENUM] HexCellValue.hexification_method in AggregationMethodEnum
SELECT COUNT(*) AS n FROM "HexCellValue"
  WHERE "hexification_method" IS NOT NULL AND "hexification_method" NOT IN ('instantaneous', 'mean', 'minimum', 'maximum', 'median', 'sum', 'count', 'percentile', 'cumulative', 'area_weighted_mean', 'population_weighted_mean', 'mode', 'class_proportion', 'density', 'nearest', 'other');

-- [ENUM] HexCellValue.data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "HexCellValue"
  WHERE "data_status" IS NOT NULL AND "data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] HexCellValue.null_semantics in NullSemanticsEnum
SELECT COUNT(*) AS n FROM "HexCellValue"
  WHERE "null_semantics" IS NOT NULL AND "null_semantics" NOT IN ('present', 'structural_null', 'derived_null', 'true_zero');

-- [ENUM] VectorFeature.geometry_type in GeometryTypeEnum
SELECT COUNT(*) AS n FROM "VectorFeature"
  WHERE "geometry_type" IS NOT NULL AND "geometry_type" NOT IN ('point', 'multipoint', 'linestring', 'multilinestring', 'polygon', 'multipolygon', 'geometrycollection');

-- [ENUM] LocationSet.geometry_type in GeometryTypeEnum
SELECT COUNT(*) AS n FROM "LocationSet"
  WHERE "geometry_type" IS NOT NULL AND "geometry_type" NOT IN ('point', 'multipoint', 'linestring', 'multilinestring', 'polygon', 'multipolygon', 'geometrycollection');

-- [ENUM] LocationSet.phi_status in PhiStatusEnum
SELECT COUNT(*) AS n FROM "LocationSet"
  WHERE "phi_status" IS NOT NULL AND "phi_status" NOT IN ('no_phi', 'aggregated_no_phi', 'phi_present');

-- [ENUM] Location.target_geography_type in TargetGeographyTypeEnum
SELECT COUNT(*) AS n FROM "Location"
  WHERE "target_geography_type" IS NOT NULL AND "target_geography_type" NOT IN ('point_location', 'buffered_point', 'census_block_group', 'census_tract', 'zcta', 'county', 'state', 'hydrologic_unit', 'h3_hex', 'grid_cell', 'public_water_system');

-- [ENUM] ExtractionRequest.temporal_grouping in TemporalGroupingEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "temporal_grouping" IS NOT NULL AND "temporal_grouping" NOT IN ('none', 'hour', 'day', 'month', 'year');

-- [ENUM] ExtractionRequest.extraction_method in ExtractionMethodEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "extraction_method" IS NOT NULL AND "extraction_method" NOT IN ('nearest_cell', 'bilinear', 'inverse_distance_weighted_4_nearest_cells', 'area_weighted_polygon_mean', 'population_weighted_mean', 'point_station_lookup', 'nearest_station', 'intersection_area_proportion', 'raster_zonal_sum', 'feature_density', 'exact_extract');

-- [ENUM] ExtractionRequest.aggregation_method in AggregationMethodEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "aggregation_method" IS NOT NULL AND "aggregation_method" NOT IN ('instantaneous', 'mean', 'minimum', 'maximum', 'median', 'sum', 'count', 'percentile', 'cumulative', 'area_weighted_mean', 'population_weighted_mean', 'mode', 'class_proportion', 'density', 'nearest', 'other');

-- [ENUM] ExtractionRequest.target_geography_type in TargetGeographyTypeEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "target_geography_type" IS NOT NULL AND "target_geography_type" NOT IN ('point_location', 'buffered_point', 'census_block_group', 'census_tract', 'zcta', 'county', 'state', 'hydrologic_unit', 'h3_hex', 'grid_cell', 'public_water_system');

-- [ENUM] ExtractionRequest.output_orientation in TableOrientationEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "output_orientation" IS NOT NULL AND "output_orientation" NOT IN ('wide', 'long');

-- [ENUM] ExtractionRequest.materialization_mode in MaterializationModeEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "materialization_mode" IS NOT NULL AND "materialization_mode" NOT IN ('direct_remote_scan', 'server_side_subset', 'bounded_cache', 'full_materialization');

-- [ENUM] ExtractionRequest.status_mixing_policy in StatusMixingPolicyEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "status_mixing_policy" IS NOT NULL AND "status_mixing_policy" NOT IN ('refuse', 'allow_with_policy');

-- [ENUM] ExtractionRequest.strict_extensivity_check in StrictnessEnum
SELECT COUNT(*) AS n FROM "ExtractionRequest"
  WHERE "strict_extensivity_check" IS NOT NULL AND "strict_extensivity_check" NOT IN ('strict', 'permissive');

-- [ENUM] AmbientValueAtLocation.extraction_method in ExtractionMethodEnum
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"
  WHERE "extraction_method" IS NOT NULL AND "extraction_method" NOT IN ('nearest_cell', 'bilinear', 'inverse_distance_weighted_4_nearest_cells', 'area_weighted_polygon_mean', 'population_weighted_mean', 'point_station_lookup', 'nearest_station', 'intersection_area_proportion', 'raster_zonal_sum', 'feature_density', 'exact_extract');

-- [ENUM] AmbientValueAtLocation.aggregation_method in AggregationMethodEnum
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"
  WHERE "aggregation_method" IS NOT NULL AND "aggregation_method" NOT IN ('instantaneous', 'mean', 'minimum', 'maximum', 'median', 'sum', 'count', 'percentile', 'cumulative', 'area_weighted_mean', 'population_weighted_mean', 'mode', 'class_proportion', 'density', 'nearest', 'other');

-- [ENUM] AmbientValueAtLocation.temporal_grouping in TemporalGroupingEnum
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"
  WHERE "temporal_grouping" IS NOT NULL AND "temporal_grouping" NOT IN ('none', 'hour', 'day', 'month', 'year');

-- [ENUM] AmbientValueAtLocation.data_status in DataStatusEnum
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"
  WHERE "data_status" IS NOT NULL AND "data_status" NOT IN ('preliminary', 'provisional', 'quality_controlled', 'final', 'forecast', 'reanalysis', 'superseded', 'unknown');

-- [ENUM] AmbientValueAtLocation.null_semantics in NullSemanticsEnum
SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"
  WHERE "null_semantics" IS NOT NULL AND "null_semantics" NOT IN ('present', 'structural_null', 'derived_null', 'true_zero');

-- [ENUM] ToolRun.run_role in RunRoleEnum
SELECT COUNT(*) AS n FROM "ToolRun"
  WHERE "run_role" IS NOT NULL AND "run_role" NOT IN ('download', 'process', 'calculate', 'register', 'harmonize', 'import', 'export');

-- [ENUM] ToolRun.status in RunStatusEnum
SELECT COUNT(*) AS n FROM "ToolRun"
  WHERE "status" IS NOT NULL AND "status" NOT IN ('succeeded', 'failed', 'partial', 'cancelled');

-- [ENUM] ToolRun.materialization_mode in MaterializationModeEnum
SELECT COUNT(*) AS n FROM "ToolRun"
  WHERE "materialization_mode" IS NOT NULL AND "materialization_mode" NOT IN ('direct_remote_scan', 'server_side_subset', 'bounded_cache', 'full_materialization');

-- [ENUM] ProvenanceChain.terminus_type in ProvenanceChainTerminusEnum
SELECT COUNT(*) AS n FROM "ProvenanceChain"
  WHERE "terminus_type" IS NOT NULL AND "terminus_type" NOT IN ('raw_source_download', 'remote_asset_scan', 'pre_existing_curated_dataset', 'externally_imported_dataset', 'synthetic_data');

