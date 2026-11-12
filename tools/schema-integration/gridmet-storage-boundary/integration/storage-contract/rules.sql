-- required
SELECT count(*) FROM v WHERE "location" IS NULL OR "extraction_request" IS NULL OR "extraction_method" IS NULL OR "id" IS NULL OR "product_variable" IS NULL OR "canonical_variable" IS NULL OR "valid_time_start" IS NULL OR "data_status" IS NULL OR "source_system" IS NULL OR "null_semantics" IS NULL OR id IS NULL;

-- finite_native
SELECT count(*) FROM v WHERE value IS NULL OR native_value IS NULL OR NOT isfinite(value) OR NOT isfinite(native_value) OR value <> native_value;

-- unique_id
SELECT count(*)-count(DISTINCT id) FROM v;

-- unit_binding
SELECT count(*) FROM v LEFT JOIN product_variables p ON v.product_variable=p."id" LEFT JOIN canonical_variables c ON v.canonical_variable=c."id" WHERE p.id IS NULL OR c.id IS NULL OR (p."canonical_variable") IS DISTINCT FROM v.canonical_variable OR (p."native_units_ucum") IS DISTINCT FROM v.native_value_unit_ucum OR (c."units_ucum") IS DISTINCT FROM v.value_unit_ucum;

-- request_membership
SELECT count(*) FROM v LEFT JOIN extraction_requests q ON v.extraction_request=q."id" LEFT JOIN locations l ON v.location=l."id" WHERE q.id IS NULL OR l.id IS NULL OR (l."location_set") IS DISTINCT FROM (q."location_set") OR NOT EXISTS (SELECT 1 FROM request_variables z WHERE z.request=q.id AND z.variable=v.product_variable);

-- request_interval
SELECT count(*) FROM extraction_requests WHERE try_cast("time_window_start" AS TIMESTAMPTZ) IS NULL OR try_cast("time_window_end" AS TIMESTAMPTZ) IS NULL OR CAST("time_window_start" AS TIMESTAMPTZ)>=CAST("time_window_end" AS TIMESTAMPTZ);

-- result_interval
SELECT count(*) FROM v JOIN extraction_requests q ON v.extraction_request=q."id" WHERE try_cast(v.valid_time_start AS TIMESTAMPTZ) IS NULL OR CAST(v.valid_time_start AS TIMESTAMPTZ)<CAST(q."time_window_start" AS TIMESTAMPTZ) OR CAST(v.valid_time_start AS TIMESTAMPTZ)>=CAST(q."time_window_end" AS TIMESTAMPTZ);

-- lineage
SELECT count(*) FROM v LEFT JOIN assets a ON v.asset=a."id" LEFT JOIN tool_runs r ON v.processing_run=r."id" WHERE a.id IS NULL OR r.id IS NULL OR (a."produced_by_run") IS DISTINCT FROM v.processing_run OR (r."run_role") IS DISTINCT FROM 'calculate';

-- zero_semantics
SELECT count(*) FROM v WHERE null_semantics IS DISTINCT FROM CASE WHEN value=0 THEN 'true_zero' ELSE 'present' END;

-- expected_keys_values
SELECT count(*) FROM (SELECT p."amadeus_variable_code" AS variable,l."location_key" AS site_id,substr(CAST(v.valid_time_start AS VARCHAR),1,10) AS date,v.valid_time_start,v.value FROM v LEFT JOIN product_variables p ON v.product_variable=p."id" LEFT JOIN locations l ON v.location=l."id") a FULL OUTER JOIN expected e USING(variable,site_id,date) WHERE a.value IS NULL OR e.value IS NULL OR abs(a.value-e.value)>1e-10 OR a.valid_time_start IS DISTINCT FROM CAST(e.date||'T00:00:00Z' AS TIMESTAMP);

-- expected_count
SELECT abs((SELECT count(*) FROM v)-(SELECT count(*) FROM expected));

-- unique_data_sources
SELECT count(*)-count(DISTINCT id) FROM "data_sources";

-- unique_products
SELECT count(*)-count(DISTINCT id) FROM "products";

-- ref_products_data_source
SELECT count(*) FROM "products" s LEFT JOIN "data_sources" t ON s."data_source"=t.id WHERE s."data_source" IS NOT NULL AND t.id IS NULL;

-- ref_products_grid_definition
SELECT count(*) FROM "products" s LEFT JOIN "grid_definitions" t ON s."grid_definition"=t.id WHERE s."grid_definition" IS NOT NULL AND t.id IS NULL;

-- unique_grid_definitions
SELECT count(*)-count(DISTINCT id) FROM "grid_definitions";

-- unique_canonical_variables
SELECT count(*)-count(DISTINCT id) FROM "canonical_variables";

-- unique_product_variables
SELECT count(*)-count(DISTINCT id) FROM "product_variables";

-- ref_product_variables_product
SELECT count(*) FROM "product_variables" s LEFT JOIN "products" t ON s."product"=t.id WHERE s."product" IS NOT NULL AND t.id IS NULL;

-- ref_product_variables_canonical_variable
SELECT count(*) FROM "product_variables" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- unique_assets
SELECT count(*)-count(DISTINCT id) FROM "assets";

-- ref_assets_product
SELECT count(*) FROM "assets" s LEFT JOIN "products" t ON s."product"=t.id WHERE s."product" IS NOT NULL AND t.id IS NULL;

-- ref_assets_variables_present
SELECT count(*) FROM "assets__variables_present" e LEFT JOIN "product_variables" t ON e.target=t.id WHERE t.id IS NULL;

-- unique_station_observations
SELECT count(*)-count(DISTINCT id) FROM "station_observations";

-- ref_station_observations_product_variable
SELECT count(*) FROM "station_observations" s LEFT JOIN "product_variables" t ON s."product_variable"=t.id WHERE s."product_variable" IS NOT NULL AND t.id IS NULL;

-- ref_station_observations_canonical_variable
SELECT count(*) FROM "station_observations" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- ref_station_observations_asset
SELECT count(*) FROM "station_observations" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_grid_cell_values
SELECT count(*)-count(DISTINCT id) FROM "grid_cell_values";

-- ref_grid_cell_values_grid_definition
SELECT count(*) FROM "grid_cell_values" s LEFT JOIN "grid_definitions" t ON s."grid_definition"=t.id WHERE s."grid_definition" IS NOT NULL AND t.id IS NULL;

-- ref_grid_cell_values_product_variable
SELECT count(*) FROM "grid_cell_values" s LEFT JOIN "product_variables" t ON s."product_variable"=t.id WHERE s."product_variable" IS NOT NULL AND t.id IS NULL;

-- ref_grid_cell_values_canonical_variable
SELECT count(*) FROM "grid_cell_values" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- ref_grid_cell_values_asset
SELECT count(*) FROM "grid_cell_values" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_area_values
SELECT count(*)-count(DISTINCT id) FROM "area_values";

-- ref_area_values_product_variable
SELECT count(*) FROM "area_values" s LEFT JOIN "product_variables" t ON s."product_variable"=t.id WHERE s."product_variable" IS NOT NULL AND t.id IS NULL;

-- ref_area_values_canonical_variable
SELECT count(*) FROM "area_values" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- ref_area_values_asset
SELECT count(*) FROM "area_values" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_hex_cell_values
SELECT count(*)-count(DISTINCT id) FROM "hex_cell_values";

-- ref_hex_cell_values_product_variable
SELECT count(*) FROM "hex_cell_values" s LEFT JOIN "product_variables" t ON s."product_variable"=t.id WHERE s."product_variable" IS NOT NULL AND t.id IS NULL;

-- ref_hex_cell_values_canonical_variable
SELECT count(*) FROM "hex_cell_values" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- ref_hex_cell_values_asset
SELECT count(*) FROM "hex_cell_values" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_vector_features
SELECT count(*)-count(DISTINCT id) FROM "vector_features";

-- ref_vector_features_product
SELECT count(*) FROM "vector_features" s LEFT JOIN "products" t ON s."product"=t.id WHERE s."product" IS NOT NULL AND t.id IS NULL;

-- ref_vector_features_asset
SELECT count(*) FROM "vector_features" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_location_sets
SELECT count(*)-count(DISTINCT id) FROM "location_sets";

-- unique_locations
SELECT count(*)-count(DISTINCT id) FROM "locations";

-- ref_locations_location_set
SELECT count(*) FROM "locations" s LEFT JOIN "location_sets" t ON s."location_set"=t.id WHERE s."location_set" IS NOT NULL AND t.id IS NULL;

-- unique_extraction_requests
SELECT count(*)-count(DISTINCT id) FROM "extraction_requests";

-- ref_extraction_requests_location_set
SELECT count(*) FROM "extraction_requests" s LEFT JOIN "location_sets" t ON s."location_set"=t.id WHERE s."location_set" IS NOT NULL AND t.id IS NULL;

-- ref_extraction_requests_requested_canonical_variables
SELECT count(*) FROM "extraction_requests__requested_canonical_variables" e LEFT JOIN "canonical_variables" t ON e.target=t.id WHERE t.id IS NULL;

-- ref_extraction_requests_requested_product_variables
SELECT count(*) FROM "extraction_requests__requested_product_variables" e LEFT JOIN "product_variables" t ON e.target=t.id WHERE t.id IS NULL;

-- ref_extraction_requests_weighting_product_variable
SELECT count(*) FROM "extraction_requests" s LEFT JOIN "product_variables" t ON s."weighting_product_variable"=t.id WHERE s."weighting_product_variable" IS NOT NULL AND t.id IS NULL;

-- unique_ambient_values_at_location
SELECT count(*)-count(DISTINCT id) FROM "ambient_values_at_location";

-- ref_ambient_values_at_location_location
SELECT count(*) FROM "ambient_values_at_location" s LEFT JOIN "locations" t ON s."location"=t.id WHERE s."location" IS NOT NULL AND t.id IS NULL;

-- ref_ambient_values_at_location_extraction_request
SELECT count(*) FROM "ambient_values_at_location" s LEFT JOIN "extraction_requests" t ON s."extraction_request"=t.id WHERE s."extraction_request" IS NOT NULL AND t.id IS NULL;

-- ref_ambient_values_at_location_product_variable
SELECT count(*) FROM "ambient_values_at_location" s LEFT JOIN "product_variables" t ON s."product_variable"=t.id WHERE s."product_variable" IS NOT NULL AND t.id IS NULL;

-- ref_ambient_values_at_location_canonical_variable
SELECT count(*) FROM "ambient_values_at_location" s LEFT JOIN "canonical_variables" t ON s."canonical_variable"=t.id WHERE s."canonical_variable" IS NOT NULL AND t.id IS NULL;

-- ref_ambient_values_at_location_asset
SELECT count(*) FROM "ambient_values_at_location" s LEFT JOIN "assets" t ON s."asset"=t.id WHERE s."asset" IS NOT NULL AND t.id IS NULL;

-- unique_tool_runs
SELECT count(*)-count(DISTINCT id) FROM "tool_runs";

-- unique_provenance_chains
SELECT count(*)-count(DISTINCT id) FROM "provenance_chains";

-- provenance_cycles
WITH RECURSIVE reach(start,node) AS (SELECT owner,target FROM tool_runs__upstream_runs UNION SELECT r.start,e.target FROM reach r JOIN tool_runs__upstream_runs e ON r.node=e.owner) SELECT count(*) FROM reach WHERE start=node;