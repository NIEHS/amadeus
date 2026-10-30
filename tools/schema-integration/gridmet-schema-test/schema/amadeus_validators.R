# ---------------------------------------------------------------------------
# amadeus_validators.R — GENERATED, DO NOT EDIT
#
# Generated from src/amadeus_schema/schema/amadeus_schema.yaml by
# scripts/gen_r_validators.py. Regenerate with `just gen-r`.
#
# Pure R + checkmate. No Python at runtime — LinkML is a build-time tool,
# and this file is one of its outputs (assumption A-01: R and SQL first).
#
# Each class gets:
#   check_<Class>(x)   TRUE, or a character message (checkmate convention)
#   assert_<Class>(x)  invisible(x), or stop() with the message
#
# `x` is a named list — one record. For a data.frame, apply row-wise.
#
# NOT checked here: cross-object referential integrity. Those constraints
# span a join and belong to the database or the query planner.
# ---------------------------------------------------------------------------

#' Validate one DataSource record
#' A provider programme that publishes one or more products: EPA AQS, NASA MODIS, NOAA NARR, Climatology Lab. One row per row of the amadeus README sourc
check_DataSource <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("DataSource: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("DataSource$id: ", .e))
  }
  if (is.null(x[["name"]])) return("DataSource: required slot `name` is missing")
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$name: ", .e))
  }
  if (!is.null(x[["label"]])) {
    .e <- checkmate::check_string(x[["label"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$label: ", .e))
  }
  if (!is.null(x[["description"]])) {
    .e <- checkmate::check_string(x[["description"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$description: ", .e))
  }
  if (!is.null(x[["producer_institution"]])) {
    .e <- checkmate::check_string(x[["producer_institution"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$producer_institution: ", .e))
  }
  if (!is.null(x[["homepage"]])) {
    .e <- checkmate::check_string(x[["homepage"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("DataSource$homepage: ", .e))
  }
  if (!is.null(x[["access_protocol"]])) {
    .e <- checkmate::check_choice(x[["access_protocol"]], c("https_bulk_file", "ftp", "s3_object_store", "stac_api", "opendap", "thredds", "rest_api", "harmony", "arcgis_service"))
    if (!isTRUE(.e)) return(paste0("DataSource$access_protocol: ", .e))
  }
  if (!is.null(x[["requires_authentication"]])) {
    .e <- checkmate::check_flag(x[["requires_authentication"]])
    if (!isTRUE(.e)) return(paste0("DataSource$requires_authentication: ", .e))
  }
  if (!is.null(x[["auth_mechanism"]])) {
    .e <- checkmate::check_string(x[["auth_mechanism"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$auth_mechanism: ", .e))
  }
  if (!is.null(x[["rate_limit_requests_per_second"]])) {
    .e <- checkmate::check_number(x[["rate_limit_requests_per_second"]])
    if (!isTRUE(.e)) return(paste0("DataSource$rate_limit_requests_per_second: ", .e))
  }
  if (!is.null(x[["license_spdx"]])) {
    .e <- checkmate::check_string(x[["license_spdx"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$license_spdx: ", .e))
  }
  if (!is.null(x[["citation_apa"]])) {
    .e <- checkmate::check_string(x[["citation_apa"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$citation_apa: ", .e))
  }
  if (!is.null(x[["doi"]])) {
    .e <- checkmate::check_string(x[["doi"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$doi: ", .e))
  }
  if (!is.null(x[["doi"]]) && !grepl("^10\\.[0-9]{4,9}/.+$", x[["doi"]]))
    return("DataSource$doi: does not match required pattern")
  if (!is.null(x[["amadeus_function_suffix"]])) {
    .e <- checkmate::check_string(x[["amadeus_function_suffix"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("DataSource$amadeus_function_suffix: ", .e))
  }
  TRUE
}

assert_DataSource <- function(x) {
  res <- check_DataSource(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one Product record
#' A specific, versioned data product from a source — the unit a user actually requests, and the unit that carries the *profile*: spatial support, tempor
check_Product <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("Product: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Product$id: ", .e))
  }
  if (is.null(x[["name"]])) return("Product: required slot `name` is missing")
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$name: ", .e))
  }
  if (!is.null(x[["label"]])) {
    .e <- checkmate::check_string(x[["label"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$label: ", .e))
  }
  if (!is.null(x[["description"]])) {
    .e <- checkmate::check_string(x[["description"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$description: ", .e))
  }
  if (is.null(x[["data_source"]])) return("Product: required slot `data_source` is missing")
  if (!is.null(x[["data_source"]])) {
    .e <- checkmate::check_string(x[["data_source"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Product$data_source: ", .e))
  }
  if (!is.null(x[["product_version"]])) {
    .e <- checkmate::check_string(x[["product_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$product_version: ", .e))
  }
  if (!is.null(x[["native_format"]])) {
    .e <- checkmate::check_choice(x[["native_format"]], c("netcdf4_cf", "netcdf3", "hdf4", "hdf5", "geotiff", "cloud_optimized_geotiff", "ascii_grid", "grib1", "grib2", "bil", "shapefile", "geodatabase", "kml", "csv_station_observations", "pipe_delimited_text", "fixed_width_text", "zarr", "parquet", "geoparquet"))
    if (!isTRUE(.e)) return(paste0("Product$native_format: ", .e))
  }
  if (is.null(x[["spatial_support_type"]])) return("Product: required slot `spatial_support_type` is missing")
  if (!is.null(x[["spatial_support_type"]])) {
    .e <- checkmate::check_choice(x[["spatial_support_type"]], c("point", "monitoring_station", "raster_grid_cell", "model_grid_cell", "polygon", "administrative_area", "statistical_area", "hex_cell", "line_feature", "other"))
    if (!isTRUE(.e)) return(paste0("Product$spatial_support_type: ", .e))
  }
  if (!is.null(x[["grid_definition"]])) {
    .e <- checkmate::check_string(x[["grid_definition"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Product$grid_definition: ", .e))
  }
  if (!is.null(x[["crs"]])) {
    .e <- checkmate::check_string(x[["crs"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$crs: ", .e))
  }
  if (!is.null(x[["crs"]]) && !grepl("^[A-Za-z]+:[0-9]+$", x[["crs"]]))
    return("Product$crs: does not match required pattern")
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("Product$srid: ", .e))
  }
  if (!is.null(x[["spatial_extent_bbox_wkt"]])) {
    .e <- checkmate::check_string(x[["spatial_extent_bbox_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$spatial_extent_bbox_wkt: ", .e))
  }
  if (!is.null(x[["spatial_extent_descriptor"]])) {
    .e <- checkmate::check_string(x[["spatial_extent_descriptor"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$spatial_extent_descriptor: ", .e))
  }
  if (!is.null(x[["native_spatial_resolution_m"]])) {
    .e <- checkmate::check_number(x[["native_spatial_resolution_m"]])
    if (!isTRUE(.e)) return(paste0("Product$native_spatial_resolution_m: ", .e))
  }
  if (!is.null(x[["native_spatial_resolution_descriptor"]])) {
    .e <- checkmate::check_string(x[["native_spatial_resolution_descriptor"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$native_spatial_resolution_descriptor: ", .e))
  }
  if (is.null(x[["temporal_resolution"]])) return("Product: required slot `temporal_resolution` is missing")
  if (!is.null(x[["temporal_resolution"]])) {
    .e <- checkmate::check_choice(x[["temporal_resolution"]], c("instantaneous", "subhourly", "hourly", "three_hourly", "daily", "monthly", "seasonal", "annual", "multi_year_epoch", "timeless"))
    if (!isTRUE(.e)) return(paste0("Product$temporal_resolution: ", .e))
  }
  if (!is.null(x[["temporal_resolution_iso"]])) {
    .e <- checkmate::check_string(x[["temporal_resolution_iso"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$temporal_resolution_iso: ", .e))
  }
  if (!is.null(x[["temporal_resolution_iso"]]) && !grepl("^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$", x[["temporal_resolution_iso"]]))
    return("Product$temporal_resolution_iso: does not match required pattern")
  if (!is.null(x[["temporal_alignment"]])) {
    .e <- checkmate::check_choice(x[["temporal_alignment"]], c("start", "center", "end", "interval", "unknown"))
    if (!isTRUE(.e)) return(paste0("Product$temporal_alignment: ", .e))
  }
  if (!is.null(x[["day_boundary_convention"]])) {
    .e <- checkmate::check_choice(x[["day_boundary_convention"]], c("local_midnight", "utc_midnight", "ending_1200_gmt", "solar_noon_centered", "observation_dependent", "not_applicable"))
    if (!isTRUE(.e)) return(paste0("Product$day_boundary_convention: ", .e))
  }
  if (!is.null(x[["calendar"]])) {
    .e <- checkmate::check_choice(x[["calendar"]], c("gregorian", "proleptic_gregorian", "noleap", "daymet_365", "all_leap", "day_360", "julian"))
    if (!isTRUE(.e)) return(paste0("Product$calendar: ", .e))
  }
  if (!is.null(x[["temporal_coverage_start"]])) {
    .e <- checkmate::check_string(x[["temporal_coverage_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("Product$temporal_coverage_start: ", .e))
  }
  if (!is.null(x[["temporal_coverage_end"]])) {
    .e <- checkmate::check_string(x[["temporal_coverage_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("Product$temporal_coverage_end: ", .e))
  }
  if (!is.null(x[["update_cadence_iso"]])) {
    .e <- checkmate::check_string(x[["update_cadence_iso"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$update_cadence_iso: ", .e))
  }
  if (!is.null(x[["update_cadence_iso"]]) && !grepl("^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$", x[["update_cadence_iso"]]))
    return("Product$update_cadence_iso: does not match required pattern")
  if (!is.null(x[["typical_latency_iso"]])) {
    .e <- checkmate::check_string(x[["typical_latency_iso"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$typical_latency_iso: ", .e))
  }
  if (!is.null(x[["typical_latency_iso"]]) && !grepl("^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$", x[["typical_latency_iso"]]))
    return("Product$typical_latency_iso: does not match required pattern")
  if (!is.null(x[["default_data_status"]])) {
    .e <- checkmate::check_choice(x[["default_data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("Product$default_data_status: ", .e))
  }
  if (!is.null(x[["homogenisation_status"]])) {
    .e <- checkmate::check_choice(x[["homogenisation_status"]], c("homogenised", "not_homogenised", "partial"))
    if (!isTRUE(.e)) return(paste0("Product$homogenisation_status: ", .e))
  }
  if (!is.null(x[["doi"]])) {
    .e <- checkmate::check_string(x[["doi"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$doi: ", .e))
  }
  if (!is.null(x[["doi"]]) && !grepl("^10\\.[0-9]{4,9}/.+$", x[["doi"]]))
    return("Product$doi: does not match required pattern")
  if (!is.null(x[["license_spdx"]])) {
    .e <- checkmate::check_string(x[["license_spdx"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$license_spdx: ", .e))
  }
  if (!is.null(x[["citation_apa"]])) {
    .e <- checkmate::check_string(x[["citation_apa"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$citation_apa: ", .e))
  }
  if (!is.null(x[["access_url"]])) {
    .e <- checkmate::check_string(x[["access_url"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Product$access_url: ", .e))
  }
  if (!is.null(x[["amadeus_dataset_name"]])) {
    .e <- checkmate::check_string(x[["amadeus_dataset_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$amadeus_dataset_name: ", .e))
  }
  if (!is.null(x[["amadeus_process_covariate"]])) {
    .e <- checkmate::check_string(x[["amadeus_process_covariate"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$amadeus_process_covariate: ", .e))
  }
  if (!is.null(x[["amadeus_calculate_covariate"]])) {
    .e <- checkmate::check_string(x[["amadeus_calculate_covariate"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$amadeus_calculate_covariate: ", .e))
  }
  if (!is.null(x[["supports_server_side_subset"]])) {
    .e <- checkmate::check_flag(x[["supports_server_side_subset"]])
    if (!isTRUE(.e)) return(paste0("Product$supports_server_side_subset: ", .e))
  }
  if (!is.null(x[["stac_collection_id"]])) {
    .e <- checkmate::check_string(x[["stac_collection_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Product$stac_collection_id: ", .e))
  }
  # rule 1: A gridded product must declare its grid. Without this, "1 km" lives in a description string and no query can r
  if ((!is.null(x[["spatial_support_type"]]) && as.character(x[["spatial_support_type"]]) %in% c("raster_grid_cell", "model_grid_cell"))) {
    if (is.null(x[["grid_definition"]])) return("Product: rule 1 — `grid_definition` is required here")
  }
  # rule 2: A daily product must declare where its day starts. This rule alone would have caught the Daymet/GridMET 1.26 °
  if ((!is.null(x[["temporal_resolution"]]) && identical(as.character(x[["temporal_resolution"]]), "daily"))) {
    if (is.null(x[["day_boundary_convention"]])) return("Product: rule 2 — `day_boundary_convention` is required here")
  }
  # rule 3: A timeless product (GMTED elevation, Köppen-Geiger) asserts no temporal coverage start. Modelled as absence ra
  if ((!is.null(x[["temporal_resolution"]]) && identical(as.character(x[["temporal_resolution"]]), "timeless"))) {
    if (!is.null(x[["temporal_coverage_start"]])) return("Product: rule 3 — `temporal_coverage_start` must be absent here")
  }
  # rule 4: A timeless product asserts no temporal coverage end.
  if ((!is.null(x[["temporal_resolution"]]) && identical(as.character(x[["temporal_resolution"]]), "timeless"))) {
    if (!is.null(x[["temporal_coverage_end"]])) return("Product: rule 4 — `temporal_coverage_end` must be absent here")
  }
  TRUE
}

assert_Product <- function(x) {
  res <- check_Product(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one GridDefinition record
#' A reusable description of a raster or model grid. Its own entity because several products share one grid (all NARR monolevel variables) and because re
check_GridDefinition <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("GridDefinition: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridDefinition$id: ", .e))
  }
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$name: ", .e))
  }
  if (!is.null(x[["label"]])) {
    .e <- checkmate::check_string(x[["label"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$label: ", .e))
  }
  if (is.null(x[["crs"]])) return("GridDefinition: required slot `crs` is missing")
  if (!is.null(x[["crs"]])) {
    .e <- checkmate::check_string(x[["crs"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$crs: ", .e))
  }
  if (!is.null(x[["crs"]]) && !grepl("^[A-Za-z]+:[0-9]+$", x[["crs"]]))
    return("GridDefinition$crs: does not match required pattern")
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$srid: ", .e))
  }
  if (!is.null(x[["grid_mapping_name"]])) {
    .e <- checkmate::check_string(x[["grid_mapping_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$grid_mapping_name: ", .e))
  }
  if (!is.null(x[["proj_string"]])) {
    .e <- checkmate::check_string(x[["proj_string"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$proj_string: ", .e))
  }
  if (is.null(x[["resolution_x"]])) return("GridDefinition: required slot `resolution_x` is missing")
  if (!is.null(x[["resolution_x"]])) {
    .e <- checkmate::check_number(x[["resolution_x"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$resolution_x: ", .e))
  }
  if (is.null(x[["resolution_y"]])) return("GridDefinition: required slot `resolution_y` is missing")
  if (!is.null(x[["resolution_y"]])) {
    .e <- checkmate::check_number(x[["resolution_y"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$resolution_y: ", .e))
  }
  if (!is.null(x[["resolution_unit"]])) {
    .e <- checkmate::check_string(x[["resolution_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$resolution_unit: ", .e))
  }
  if (!is.null(x[["n_columns"]])) {
    .e <- checkmate::check_int(x[["n_columns"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$n_columns: ", .e))
  }
  if (!is.null(x[["n_rows"]])) {
    .e <- checkmate::check_int(x[["n_rows"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$n_rows: ", .e))
  }
  if (!is.null(x[["origin_x"]])) {
    .e <- checkmate::check_number(x[["origin_x"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$origin_x: ", .e))
  }
  if (!is.null(x[["origin_y"]])) {
    .e <- checkmate::check_number(x[["origin_y"]])
    if (!isTRUE(.e)) return(paste0("GridDefinition$origin_y: ", .e))
  }
  if (!is.null(x[["vertical_level_type"]])) {
    .e <- checkmate::check_string(x[["vertical_level_type"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$vertical_level_type: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridDefinition$vertical_level_unit: ", .e))
  }
  TRUE
}

assert_GridDefinition <- function(x) {
  res <- check_GridDefinition(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one CanonicalVariable record
#' A harmonised environmental quantity, independent of who publishes it. The cross-source identity anchor: `gridmet.tmmx`, `prism.tmax` and `daymet.tmax`
check_CanonicalVariable <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("CanonicalVariable: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$id: ", .e))
  }
  if (is.null(x[["name"]])) return("CanonicalVariable: required slot `name` is missing")
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$name: ", .e))
  }
  if (!is.null(x[["label"]])) {
    .e <- checkmate::check_string(x[["label"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$label: ", .e))
  }
  if (!is.null(x[["description"]])) {
    .e <- checkmate::check_string(x[["description"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$description: ", .e))
  }
  if (is.null(x[["standard_name"]])) return("CanonicalVariable: required slot `standard_name` is missing")
  if (!is.null(x[["standard_name"]])) {
    .e <- checkmate::check_string(x[["standard_name"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$standard_name: ", .e))
  }
  if (!is.null(x[["standard_name_authority"]])) {
    .e <- checkmate::check_string(x[["standard_name_authority"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$standard_name_authority: ", .e))
  }
  if (is.null(x[["units_ucum"]])) return("CanonicalVariable: required slot `units_ucum` is missing")
  if (!is.null(x[["units_ucum"]])) {
    .e <- checkmate::check_string(x[["units_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$units_ucum: ", .e))
  }
  if (!is.null(x[["units_display"]])) {
    .e <- checkmate::check_string(x[["units_display"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$units_display: ", .e))
  }
  if (is.null(x[["value_data_type"]])) return("CanonicalVariable: required slot `value_data_type` is missing")
  if (!is.null(x[["value_data_type"]])) {
    .e <- checkmate::check_choice(x[["value_data_type"]], c("continuous_numeric", "categorical", "binary_flag", "count", "event_marker", "proportion"))
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$value_data_type: ", .e))
  }
  if (is.null(x[["extensivity"]])) return("CanonicalVariable: required slot `extensivity` is missing")
  if (!is.null(x[["extensivity"]])) {
    .e <- checkmate::check_choice(x[["extensivity"]], c("intensive", "extensive", "categorical", "unknown"))
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$extensivity: ", .e))
  }
  if (!is.null(x[["default_aggregation_method"]])) {
    .e <- checkmate::check_choice(x[["default_aggregation_method"]], c("instantaneous", "mean", "minimum", "maximum", "median", "sum", "count", "percentile", "cumulative", "area_weighted_mean", "population_weighted_mean", "mode", "class_proportion", "density", "nearest", "other"))
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$default_aggregation_method: ", .e))
  }
  if (!is.null(x[["plausible_min"]])) {
    .e <- checkmate::check_number(x[["plausible_min"]])
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$plausible_min: ", .e))
  }
  if (!is.null(x[["plausible_max"]])) {
    .e <- checkmate::check_number(x[["plausible_max"]])
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$plausible_max: ", .e))
  }
  if (!is.null(x[["omop_concept_binding"]])) {
    .e <- checkmate::check_string(x[["omop_concept_binding"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$omop_concept_binding: ", .e))
  }
  if (!is.null(x[["envar_variable_family"]])) {
    .e <- checkmate::check_string(x[["envar_variable_family"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("CanonicalVariable$envar_variable_family: ", .e))
  }
  # rule 1: A categorical value must declare categorical extensivity.
  if ((!is.null(x[["value_data_type"]]) && identical(as.character(x[["value_data_type"]]), "categorical"))) {
    if (is.null(x[["extensivity"]]) || !identical(as.character(x[["extensivity"]]), "categorical"))
      return("CanonicalVariable: rule 1 — `extensivity` must be 'categorical' here")
  }
  # rule 2: A plausible numeric minimum is meaningless for a nominal class label.
  if ((!is.null(x[["extensivity"]]) && identical(as.character(x[["extensivity"]]), "categorical"))) {
    if (!is.null(x[["plausible_min"]])) return("CanonicalVariable: rule 2 — `plausible_min` must be absent here")
  }
  # rule 3: A plausible numeric maximum is meaningless for a nominal class label.
  if ((!is.null(x[["extensivity"]]) && identical(as.character(x[["extensivity"]]), "categorical"))) {
    if (!is.null(x[["plausible_max"]])) return("CanonicalVariable: rule 3 — `plausible_max` must be absent here")
  }
  TRUE
}

assert_CanonicalVariable <- function(x) {
  res <- check_CanonicalVariable(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one ProductVariable record
#' One source's delivery of one canonical variable: the native name, the native units and conversion, the CF metadata, the aggregation actually applied, 
check_ProductVariable <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("ProductVariable: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ProductVariable$id: ", .e))
  }
  if (is.null(x[["product"]])) return("ProductVariable: required slot `product` is missing")
  if (!is.null(x[["product"]])) {
    .e <- checkmate::check_string(x[["product"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ProductVariable$product: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("ProductVariable: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ProductVariable$canonical_variable: ", .e))
  }
  if (is.null(x[["native_name"]])) return("ProductVariable: required slot `native_name` is missing")
  if (!is.null(x[["native_name"]])) {
    .e <- checkmate::check_string(x[["native_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$native_name: ", .e))
  }
  if (!is.null(x[["label"]])) {
    .e <- checkmate::check_string(x[["label"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$label: ", .e))
  }
  if (!is.null(x[["native_units_ucum"]])) {
    .e <- checkmate::check_string(x[["native_units_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$native_units_ucum: ", .e))
  }
  if (!is.null(x[["native_value_scale_factor"]])) {
    .e <- checkmate::check_number(x[["native_value_scale_factor"]])
    if (!isTRUE(.e)) return(paste0("ProductVariable$native_value_scale_factor: ", .e))
  }
  if (!is.null(x[["native_value_offset"]])) {
    .e <- checkmate::check_number(x[["native_value_offset"]])
    if (!isTRUE(.e)) return(paste0("ProductVariable$native_value_offset: ", .e))
  }
  if (!is.null(x[["unit_conversion_formula"]])) {
    .e <- checkmate::check_string(x[["unit_conversion_formula"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$unit_conversion_formula: ", .e))
  }
  if (!is.null(x[["cf_standard_name"]])) {
    .e <- checkmate::check_string(x[["cf_standard_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$cf_standard_name: ", .e))
  }
  if (!is.null(x[["cf_cell_methods"]])) {
    .e <- checkmate::check_string(x[["cf_cell_methods"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$cf_cell_methods: ", .e))
  }
  if (!is.null(x[["aggregation_method"]])) {
    .e <- checkmate::check_choice(x[["aggregation_method"]], c("instantaneous", "mean", "minimum", "maximum", "median", "sum", "count", "percentile", "cumulative", "area_weighted_mean", "population_weighted_mean", "mode", "class_proportion", "density", "nearest", "other"))
    if (!isTRUE(.e)) return(paste0("ProductVariable$aggregation_method: ", .e))
  }
  if (!is.null(x[["aggregation_window_iso"]])) {
    .e <- checkmate::check_string(x[["aggregation_window_iso"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$aggregation_window_iso: ", .e))
  }
  if (!is.null(x[["aggregation_window_iso"]]) && !grepl("^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$", x[["aggregation_window_iso"]]))
    return("ProductVariable$aggregation_window_iso: does not match required pattern")
  if (!is.null(x[["temporal_alignment"]])) {
    .e <- checkmate::check_choice(x[["temporal_alignment"]], c("start", "center", "end", "interval", "unknown"))
    if (!isTRUE(.e)) return(paste0("ProductVariable$temporal_alignment: ", .e))
  }
  if (!is.null(x[["vertical_level_type"]])) {
    .e <- checkmate::check_string(x[["vertical_level_type"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$vertical_level_type: ", .e))
  }
  if (!is.null(x[["missing_value_sentinel"]])) {
    .e <- checkmate::check_string(x[["missing_value_sentinel"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$missing_value_sentinel: ", .e))
  }
  if (!is.null(x[["quality_flag_vocabulary"]])) {
    .e <- checkmate::check_string(x[["quality_flag_vocabulary"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$quality_flag_vocabulary: ", .e))
  }
  if (!is.null(x[["layer_name_template"]])) {
    .e <- checkmate::check_string(x[["layer_name_template"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$layer_name_template: ", .e))
  }
  if (!is.null(x[["amadeus_variable_code"]])) {
    .e <- checkmate::check_string(x[["amadeus_variable_code"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$amadeus_variable_code: ", .e))
  }
  if (!is.null(x[["value_data_type"]])) {
    .e <- checkmate::check_choice(x[["value_data_type"]], c("continuous_numeric", "categorical", "binary_flag", "count", "event_marker", "proportion"))
    if (!isTRUE(.e)) return(paste0("ProductVariable$value_data_type: ", .e))
  }
  if (!is.null(x[["extensivity"]])) {
    .e <- checkmate::check_choice(x[["extensivity"]], c("intensive", "extensive", "categorical", "unknown"))
    if (!isTRUE(.e)) return(paste0("ProductVariable$extensivity: ", .e))
  }
  if (!is.null(x[["harmonization_note"]])) {
    .e <- checkmate::check_string(x[["harmonization_note"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ProductVariable$harmonization_note: ", .e))
  }
  # rule 1: A scale factor without the unit it scales from is uninterpretable.
  if ((!is.null(x[["native_value_scale_factor"]]))) {
    if (is.null(x[["native_units_ucum"]])) return("ProductVariable: rule 1 — `native_units_ucum` is required here")
  }
  # rule 2: A multi-level variable's layer names must carry the level, or the level cannot be recovered when the raster is
  if ((!is.null(x[["vertical_level_type"]]))) {
    if (is.null(x[["layer_name_template"]]) || !grepl(".*\\{level\\}.*", x[["layer_name_template"]]))
      return("ProductVariable: rule 2 — `layer_name_template` must match the required pattern here")
  }
  TRUE
}

assert_ProductVariable <- function(x) {
  res <- check_ProductVariable(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one OmopConceptBinding record
#' Binding of a canonical variable to an OMOP concept. Taken structurally from HEW so the two schemas exchange bindings without an adapter.
check_OmopConceptBinding <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (!is.null(x[["omop_concept_id"]])) {
    .e <- checkmate::check_int(x[["omop_concept_id"]])
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$omop_concept_id: ", .e))
  }
  if (!is.null(x[["omop_concept_name"]])) {
    .e <- checkmate::check_string(x[["omop_concept_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$omop_concept_name: ", .e))
  }
  if (!is.null(x[["omop_vocabulary_id"]])) {
    .e <- checkmate::check_string(x[["omop_vocabulary_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$omop_vocabulary_id: ", .e))
  }
  if (!is.null(x[["omop_domain_id"]])) {
    .e <- checkmate::check_string(x[["omop_domain_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$omop_domain_id: ", .e))
  }
  if (!is.null(x[["omop_standard_concept"]])) {
    .e <- checkmate::check_string(x[["omop_standard_concept"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$omop_standard_concept: ", .e))
  }
  if (is.null(x[["concept_status"]])) return("OmopConceptBinding: required slot `concept_status` is missing")
  if (!is.null(x[["concept_status"]])) {
    .e <- checkmate::check_choice(x[["concept_status"]], c("mapped_standard", "mapped_nonstandard", "candidate_mapping", "vocabulary_gap", "not_evaluated"))
    if (!isTRUE(.e)) return(paste0("OmopConceptBinding$concept_status: ", .e))
  }
  TRUE
}

assert_OmopConceptBinding <- function(x) {
  res <- check_OmopConceptBinding(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one Asset record
#' A concrete byte stream: a file on disk, an object in a bucket, a STAC asset, or a provider API response. The unit of download, caching, integrity chec
check_Asset <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("Asset: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Asset$id: ", .e))
  }
  if (is.null(x[["product"]])) return("Asset: required slot `product` is missing")
  if (!is.null(x[["product"]])) {
    .e <- checkmate::check_string(x[["product"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Asset$product: ", .e))
  }
  if (!is.null(x[["asset_key"]])) {
    .e <- checkmate::check_string(x[["asset_key"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$asset_key: ", .e))
  }
  if (!is.null(x[["url"]])) {
    .e <- checkmate::check_string(x[["url"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Asset$url: ", .e))
  }
  if (!is.null(x[["local_path"]])) {
    .e <- checkmate::check_string(x[["local_path"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$local_path: ", .e))
  }
  if (!is.null(x[["media_type"]])) {
    .e <- checkmate::check_string(x[["media_type"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$media_type: ", .e))
  }
  if (!is.null(x[["native_format"]])) {
    .e <- checkmate::check_choice(x[["native_format"]], c("netcdf4_cf", "netcdf3", "hdf4", "hdf5", "geotiff", "cloud_optimized_geotiff", "ascii_grid", "grib1", "grib2", "bil", "shapefile", "geodatabase", "kml", "csv_station_observations", "pipe_delimited_text", "fixed_width_text", "zarr", "parquet", "geoparquet"))
    if (!isTRUE(.e)) return(paste0("Asset$native_format: ", .e))
  }
  if (!is.null(x[["size_bytes"]])) {
    .e <- checkmate::check_int(x[["size_bytes"]])
    if (!isTRUE(.e)) return(paste0("Asset$size_bytes: ", .e))
  }
  if (!is.null(x[["sha256"]])) {
    .e <- checkmate::check_string(x[["sha256"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$sha256: ", .e))
  }
  if (!is.null(x[["sha256"]]) && !grepl("^[0-9a-f]{64}$", x[["sha256"]]))
    return("Asset$sha256: does not match required pattern")
  if (!is.null(x[["source_last_modified"]])) {
    .e <- checkmate::check_string(x[["source_last_modified"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$source_last_modified: ", .e))
  }
  if (!is.null(x[["download_timestamp_utc"]])) {
    .e <- checkmate::check_string(x[["download_timestamp_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$download_timestamp_utc: ", .e))
  }
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$reference_time: ", .e))
  }
  if (!is.null(x[["bbox_wkt"]])) {
    .e <- checkmate::check_string(x[["bbox_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$bbox_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("Asset$srid: ", .e))
  }
  if (!is.null(x[["stac_collection_id"]])) {
    .e <- checkmate::check_string(x[["stac_collection_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$stac_collection_id: ", .e))
  }
  if (!is.null(x[["stac_item_id"]])) {
    .e <- checkmate::check_string(x[["stac_item_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$stac_item_id: ", .e))
  }
  if (is.null(x[["materialization_mode"]])) return("Asset: required slot `materialization_mode` is missing")
  if (!is.null(x[["materialization_mode"]])) {
    .e <- checkmate::check_choice(x[["materialization_mode"]], c("direct_remote_scan", "server_side_subset", "bounded_cache", "full_materialization"))
    if (!isTRUE(.e)) return(paste0("Asset$materialization_mode: ", .e))
  }
  if (!is.null(x[["cache_expires_at"]])) {
    .e <- checkmate::check_string(x[["cache_expires_at"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("Asset$cache_expires_at: ", .e))
  }
  if (is.null(x[["value_state"]])) return("Asset: required slot `value_state` is missing")
  if (!is.null(x[["value_state"]])) {
    .e <- checkmate::check_choice(x[["value_state"]], c("registered_only", "materialized"))
    if (!isTRUE(.e)) return(paste0("Asset$value_state: ", .e))
  }
  if (!is.null(x[["produced_by_run"]])) {
    .e <- checkmate::check_string(x[["produced_by_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Asset$produced_by_run: ", .e))
  }
  # rule 1: Anything cached or fully downloaded must record where it went and what it hashes to — otherwise "resumable, id
  if ((!is.null(x[["materialization_mode"]]) && as.character(x[["materialization_mode"]]) %in% c("bounded_cache", "full_materialization"))) {
    if (is.null(x[["local_path"]])) return("Asset: rule 1 — `local_path` is required here")
    if (is.null(x[["sha256"]])) return("Asset: rule 1 — `sha256` is required here")
  }
  # rule 2: If values were written into a value table, the run that wrote them must be identified. This is the link that m
  if ((!is.null(x[["value_state"]]) && identical(as.character(x[["value_state"]]), "materialized"))) {
    if (is.null(x[["produced_by_run"]])) return("Asset: rule 2 — `produced_by_run` is required here")
  }
  TRUE
}

assert_Asset <- function(x) {
  res <- check_Asset(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one StationObservation record
#' A value from a fixed instrument with a persistent identity: EPA AQS, AirNow, IMPROVE, USGS water sites. The distinguishing feature is not the point ge
check_StationObservation <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["station_id"]])) return("StationObservation: required slot `station_id` is missing")
  if (!is.null(x[["station_id"]])) {
    .e <- checkmate::check_string(x[["station_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$station_id: ", .e))
  }
  if (!is.null(x[["station_name"]])) {
    .e <- checkmate::check_string(x[["station_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$station_name: ", .e))
  }
  if (!is.null(x[["monitor_id"]])) {
    .e <- checkmate::check_string(x[["monitor_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$monitor_id: ", .e))
  }
  if (!is.null(x[["parameter_code"]])) {
    .e <- checkmate::check_string(x[["parameter_code"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$parameter_code: ", .e))
  }
  if (!is.null(x[["poc"]])) {
    .e <- checkmate::check_int(x[["poc"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$poc: ", .e))
  }
  if (!is.null(x[["sampling_duration_iso"]])) {
    .e <- checkmate::check_string(x[["sampling_duration_iso"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$sampling_duration_iso: ", .e))
  }
  if (!is.null(x[["sampling_duration_iso"]]) && !grepl("^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$", x[["sampling_duration_iso"]]))
    return("StationObservation$sampling_duration_iso: does not match required pattern")
  if (is.null(x[["station_geom_wkt"]])) return("StationObservation: required slot `station_geom_wkt` is missing")
  if (!is.null(x[["station_geom_wkt"]])) {
    .e <- checkmate::check_string(x[["station_geom_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$station_geom_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$srid: ", .e))
  }
  if (!is.null(x[["elevation_m"]])) {
    .e <- checkmate::check_number(x[["elevation_m"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$elevation_m: ", .e))
  }
  if (!is.null(x[["station_valid_from"]])) {
    .e <- checkmate::check_string(x[["station_valid_from"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("StationObservation$station_valid_from: ", .e))
  }
  if (!is.null(x[["station_valid_to"]])) {
    .e <- checkmate::check_string(x[["station_valid_to"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("StationObservation$station_valid_to: ", .e))
  }
  if (is.null(x[["id"]])) return("StationObservation: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("StationObservation$id: ", .e))
  }
  if (is.null(x[["product_variable"]])) return("StationObservation: required slot `product_variable` is missing")
  if (!is.null(x[["product_variable"]])) {
    .e <- checkmate::check_string(x[["product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("StationObservation$product_variable: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("StationObservation: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("StationObservation$canonical_variable: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("StationObservation$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$processing_run: ", .e))
  }
  if (!is.null(x[["value"]])) {
    .e <- checkmate::check_number(x[["value"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$value: ", .e))
  }
  if (!is.null(x[["value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$value_unit_ucum: ", .e))
  }
  if (!is.null(x[["native_value"]])) {
    .e <- checkmate::check_number(x[["native_value"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$native_value: ", .e))
  }
  if (!is.null(x[["native_value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["native_value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$native_value_unit_ucum: ", .e))
  }
  if (is.null(x[["valid_time_start"]])) return("StationObservation: required slot `valid_time_start` is missing")
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("StationObservation$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("StationObservation$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("StationObservation$reference_time: ", .e))
  }
  if (!is.null(x[["retrieval_time_utc"]])) {
    .e <- checkmate::check_string(x[["retrieval_time_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("StationObservation$retrieval_time_utc: ", .e))
  }
  if (!is.null(x[["vertical_level"]])) {
    .e <- checkmate::check_number(x[["vertical_level"]])
    if (!isTRUE(.e)) return(paste0("StationObservation$vertical_level: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$vertical_level_unit: ", .e))
  }
  if (!is.null(x[["quality_flag"]])) {
    .e <- checkmate::check_string(x[["quality_flag"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$quality_flag: ", .e))
  }
  if (is.null(x[["data_status"]])) return("StationObservation: required slot `data_status` is missing")
  if (!is.null(x[["data_status"]])) {
    .e <- checkmate::check_choice(x[["data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("StationObservation$data_status: ", .e))
  }
  if (is.null(x[["source_system"]])) return("StationObservation: required slot `source_system` is missing")
  if (!is.null(x[["source_system"]])) {
    .e <- checkmate::check_string(x[["source_system"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$source_system: ", .e))
  }
  if (!is.null(x[["source_version"]])) {
    .e <- checkmate::check_string(x[["source_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$source_version: ", .e))
  }
  if (is.null(x[["null_semantics"]])) return("StationObservation: required slot `null_semantics` is missing")
  if (!is.null(x[["null_semantics"]])) {
    .e <- checkmate::check_choice(x[["null_semantics"]], c("present", "structural_null", "derived_null", "true_zero"))
    if (!isTRUE(.e)) return(paste0("StationObservation$null_semantics: ", .e))
  }
  if (!is.null(x[["superseded_by"]])) {
    .e <- checkmate::check_string(x[["superseded_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("StationObservation$superseded_by: ", .e))
  }
  TRUE
}

assert_StationObservation <- function(x) {
  res <- check_StationObservation(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one GridCellValue record
#' A value at one cell of one grid — the flattened form of a raster layer. This is the class the SedonaDB decision hangs on: Sedona's raster support is t
check_GridCellValue <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["grid_definition"]])) return("GridCellValue: required slot `grid_definition` is missing")
  if (!is.null(x[["grid_definition"]])) {
    .e <- checkmate::check_string(x[["grid_definition"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridCellValue$grid_definition: ", .e))
  }
  if (is.null(x[["cell_x"]])) return("GridCellValue: required slot `cell_x` is missing")
  if (!is.null(x[["cell_x"]])) {
    .e <- checkmate::check_int(x[["cell_x"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$cell_x: ", .e))
  }
  if (is.null(x[["cell_y"]])) return("GridCellValue: required slot `cell_y` is missing")
  if (!is.null(x[["cell_y"]])) {
    .e <- checkmate::check_int(x[["cell_y"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$cell_y: ", .e))
  }
  if (is.null(x[["cell_centroid_wkt"]])) return("GridCellValue: required slot `cell_centroid_wkt` is missing")
  if (!is.null(x[["cell_centroid_wkt"]])) {
    .e <- checkmate::check_string(x[["cell_centroid_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$cell_centroid_wkt: ", .e))
  }
  if (!is.null(x[["cell_polygon_wkt"]])) {
    .e <- checkmate::check_string(x[["cell_polygon_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$cell_polygon_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$srid: ", .e))
  }
  if (is.null(x[["id"]])) return("GridCellValue: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridCellValue$id: ", .e))
  }
  if (is.null(x[["product_variable"]])) return("GridCellValue: required slot `product_variable` is missing")
  if (!is.null(x[["product_variable"]])) {
    .e <- checkmate::check_string(x[["product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridCellValue$product_variable: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("GridCellValue: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridCellValue$canonical_variable: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("GridCellValue$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$processing_run: ", .e))
  }
  if (!is.null(x[["value"]])) {
    .e <- checkmate::check_number(x[["value"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$value: ", .e))
  }
  if (!is.null(x[["value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$value_unit_ucum: ", .e))
  }
  if (!is.null(x[["native_value"]])) {
    .e <- checkmate::check_number(x[["native_value"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$native_value: ", .e))
  }
  if (!is.null(x[["native_value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["native_value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$native_value_unit_ucum: ", .e))
  }
  if (is.null(x[["valid_time_start"]])) return("GridCellValue: required slot `valid_time_start` is missing")
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("GridCellValue$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("GridCellValue$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("GridCellValue$reference_time: ", .e))
  }
  if (!is.null(x[["retrieval_time_utc"]])) {
    .e <- checkmate::check_string(x[["retrieval_time_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("GridCellValue$retrieval_time_utc: ", .e))
  }
  if (!is.null(x[["vertical_level"]])) {
    .e <- checkmate::check_number(x[["vertical_level"]])
    if (!isTRUE(.e)) return(paste0("GridCellValue$vertical_level: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$vertical_level_unit: ", .e))
  }
  if (!is.null(x[["quality_flag"]])) {
    .e <- checkmate::check_string(x[["quality_flag"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$quality_flag: ", .e))
  }
  if (is.null(x[["data_status"]])) return("GridCellValue: required slot `data_status` is missing")
  if (!is.null(x[["data_status"]])) {
    .e <- checkmate::check_choice(x[["data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("GridCellValue$data_status: ", .e))
  }
  if (is.null(x[["source_system"]])) return("GridCellValue: required slot `source_system` is missing")
  if (!is.null(x[["source_system"]])) {
    .e <- checkmate::check_string(x[["source_system"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$source_system: ", .e))
  }
  if (!is.null(x[["source_version"]])) {
    .e <- checkmate::check_string(x[["source_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$source_version: ", .e))
  }
  if (is.null(x[["null_semantics"]])) return("GridCellValue: required slot `null_semantics` is missing")
  if (!is.null(x[["null_semantics"]])) {
    .e <- checkmate::check_choice(x[["null_semantics"]], c("present", "structural_null", "derived_null", "true_zero"))
    if (!isTRUE(.e)) return(paste0("GridCellValue$null_semantics: ", .e))
  }
  if (!is.null(x[["superseded_by"]])) {
    .e <- checkmate::check_string(x[["superseded_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("GridCellValue$superseded_by: ", .e))
  }
  TRUE
}

assert_GridCellValue <- function(x) {
  res <- check_GridCellValue(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one AreaValue record
#' A value attached to a named or delineated polygon: HUC watersheds, EPA ecoregions, county-level NEI emissions, USDM drought polygons, NOAA HMS smoke p
check_AreaValue <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["area_id"]])) return("AreaValue: required slot `area_id` is missing")
  if (!is.null(x[["area_id"]])) {
    .e <- checkmate::check_string(x[["area_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$area_id: ", .e))
  }
  if (is.null(x[["area_type"]])) return("AreaValue: required slot `area_type` is missing")
  if (!is.null(x[["area_type"]])) {
    .e <- checkmate::check_choice(x[["area_type"]], c("county", "state", "census_tract", "census_block_group", "zcta", "hydrologic_unit", "ecoregion", "smoke_plume", "drought_area", "public_water_system", "custom"))
    if (!isTRUE(.e)) return(paste0("AreaValue$area_type: ", .e))
  }
  if (is.null(x[["area_role"]])) return("AreaValue: required slot `area_role` is missing")
  if (!is.null(x[["area_role"]])) {
    .e <- checkmate::check_choice(x[["area_role"]], c("administrative", "episodic"))
    if (!isTRUE(.e)) return(paste0("AreaValue$area_role: ", .e))
  }
  if (!is.null(x[["area_code"]])) {
    .e <- checkmate::check_string(x[["area_code"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$area_code: ", .e))
  }
  if (!is.null(x[["area_name"]])) {
    .e <- checkmate::check_string(x[["area_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$area_name: ", .e))
  }
  if (is.null(x[["area_geom_wkt"]])) return("AreaValue: required slot `area_geom_wkt` is missing")
  if (!is.null(x[["area_geom_wkt"]])) {
    .e <- checkmate::check_string(x[["area_geom_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$area_geom_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("AreaValue$srid: ", .e))
  }
  if (!is.null(x[["area_km2"]])) {
    .e <- checkmate::check_number(x[["area_km2"]])
    if (!isTRUE(.e)) return(paste0("AreaValue$area_km2: ", .e))
  }
  if (is.null(x[["id"]])) return("AreaValue: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AreaValue$id: ", .e))
  }
  if (is.null(x[["product_variable"]])) return("AreaValue: required slot `product_variable` is missing")
  if (!is.null(x[["product_variable"]])) {
    .e <- checkmate::check_string(x[["product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AreaValue$product_variable: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("AreaValue: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AreaValue$canonical_variable: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AreaValue$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$processing_run: ", .e))
  }
  if (!is.null(x[["value"]])) {
    .e <- checkmate::check_number(x[["value"]])
    if (!isTRUE(.e)) return(paste0("AreaValue$value: ", .e))
  }
  if (!is.null(x[["value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$value_unit_ucum: ", .e))
  }
  if (!is.null(x[["native_value"]])) {
    .e <- checkmate::check_number(x[["native_value"]])
    if (!isTRUE(.e)) return(paste0("AreaValue$native_value: ", .e))
  }
  if (!is.null(x[["native_value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["native_value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$native_value_unit_ucum: ", .e))
  }
  if (is.null(x[["valid_time_start"]])) return("AreaValue: required slot `valid_time_start` is missing")
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AreaValue$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AreaValue$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AreaValue$reference_time: ", .e))
  }
  if (!is.null(x[["retrieval_time_utc"]])) {
    .e <- checkmate::check_string(x[["retrieval_time_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AreaValue$retrieval_time_utc: ", .e))
  }
  if (!is.null(x[["vertical_level"]])) {
    .e <- checkmate::check_number(x[["vertical_level"]])
    if (!isTRUE(.e)) return(paste0("AreaValue$vertical_level: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$vertical_level_unit: ", .e))
  }
  if (!is.null(x[["quality_flag"]])) {
    .e <- checkmate::check_string(x[["quality_flag"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$quality_flag: ", .e))
  }
  if (is.null(x[["data_status"]])) return("AreaValue: required slot `data_status` is missing")
  if (!is.null(x[["data_status"]])) {
    .e <- checkmate::check_choice(x[["data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("AreaValue$data_status: ", .e))
  }
  if (is.null(x[["source_system"]])) return("AreaValue: required slot `source_system` is missing")
  if (!is.null(x[["source_system"]])) {
    .e <- checkmate::check_string(x[["source_system"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$source_system: ", .e))
  }
  if (!is.null(x[["source_version"]])) {
    .e <- checkmate::check_string(x[["source_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$source_version: ", .e))
  }
  if (is.null(x[["null_semantics"]])) return("AreaValue: required slot `null_semantics` is missing")
  if (!is.null(x[["null_semantics"]])) {
    .e <- checkmate::check_choice(x[["null_semantics"]], c("present", "structural_null", "derived_null", "true_zero"))
    if (!isTRUE(.e)) return(paste0("AreaValue$null_semantics: ", .e))
  }
  if (!is.null(x[["superseded_by"]])) {
    .e <- checkmate::check_string(x[["superseded_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AreaValue$superseded_by: ", .e))
  }
  TRUE
}

assert_AreaValue <- function(x) {
  res <- check_AreaValue(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one HexCellValue record
#' A value aggregated to an H3 cell — the harmonization target for the C-HER collaboration (objective O-09). Hexification is lossy and the loss must be r
check_HexCellValue <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["h3_cell"]])) return("HexCellValue: required slot `h3_cell` is missing")
  if (!is.null(x[["h3_cell"]])) {
    .e <- checkmate::check_string(x[["h3_cell"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$h3_cell: ", .e))
  }
  if (!is.null(x[["h3_cell"]]) && !grepl("^[0-9a-f]{15}$", x[["h3_cell"]]))
    return("HexCellValue$h3_cell: does not match required pattern")
  if (is.null(x[["h3_resolution"]])) return("HexCellValue: required slot `h3_resolution` is missing")
  if (!is.null(x[["h3_resolution"]])) {
    .e <- checkmate::check_int(x[["h3_resolution"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$h3_resolution: ", .e))
  }
  if (!is.null(x[["h3_resolution"]]) && x[["h3_resolution"]] < 0)
    return("HexCellValue$h3_resolution: below minimum 0")
  if (!is.null(x[["h3_resolution"]]) && x[["h3_resolution"]] > 15)
    return("HexCellValue$h3_resolution: above maximum 15")
  if (!is.null(x[["hex_centroid_wkt"]])) {
    .e <- checkmate::check_string(x[["hex_centroid_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$hex_centroid_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$srid: ", .e))
  }
  if (is.null(x[["source_support_type"]])) return("HexCellValue: required slot `source_support_type` is missing")
  if (!is.null(x[["source_support_type"]])) {
    .e <- checkmate::check_choice(x[["source_support_type"]], c("point", "monitoring_station", "raster_grid_cell", "model_grid_cell", "polygon", "administrative_area", "statistical_area", "hex_cell", "line_feature", "other"))
    if (!isTRUE(.e)) return(paste0("HexCellValue$source_support_type: ", .e))
  }
  if (is.null(x[["hexification_method"]])) return("HexCellValue: required slot `hexification_method` is missing")
  if (!is.null(x[["hexification_method"]])) {
    .e <- checkmate::check_choice(x[["hexification_method"]], c("instantaneous", "mean", "minimum", "maximum", "median", "sum", "count", "percentile", "cumulative", "area_weighted_mean", "population_weighted_mean", "mode", "class_proportion", "density", "nearest", "other"))
    if (!isTRUE(.e)) return(paste0("HexCellValue$hexification_method: ", .e))
  }
  if (is.null(x[["coverage_fraction"]])) return("HexCellValue: required slot `coverage_fraction` is missing")
  if (!is.null(x[["coverage_fraction"]])) {
    .e <- checkmate::check_number(x[["coverage_fraction"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$coverage_fraction: ", .e))
  }
  if (!is.null(x[["coverage_fraction"]]) && x[["coverage_fraction"]] < 0.0)
    return("HexCellValue$coverage_fraction: below minimum 0.0")
  if (!is.null(x[["coverage_fraction"]]) && x[["coverage_fraction"]] > 1.0)
    return("HexCellValue$coverage_fraction: above maximum 1.0")
  if (!is.null(x[["contributing_cell_count"]])) {
    .e <- checkmate::check_int(x[["contributing_cell_count"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$contributing_cell_count: ", .e))
  }
  if (is.null(x[["id"]])) return("HexCellValue: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("HexCellValue$id: ", .e))
  }
  if (is.null(x[["product_variable"]])) return("HexCellValue: required slot `product_variable` is missing")
  if (!is.null(x[["product_variable"]])) {
    .e <- checkmate::check_string(x[["product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("HexCellValue$product_variable: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("HexCellValue: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("HexCellValue$canonical_variable: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("HexCellValue$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$processing_run: ", .e))
  }
  if (!is.null(x[["value"]])) {
    .e <- checkmate::check_number(x[["value"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$value: ", .e))
  }
  if (!is.null(x[["value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$value_unit_ucum: ", .e))
  }
  if (!is.null(x[["native_value"]])) {
    .e <- checkmate::check_number(x[["native_value"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$native_value: ", .e))
  }
  if (!is.null(x[["native_value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["native_value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$native_value_unit_ucum: ", .e))
  }
  if (is.null(x[["valid_time_start"]])) return("HexCellValue: required slot `valid_time_start` is missing")
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("HexCellValue$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("HexCellValue$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("HexCellValue$reference_time: ", .e))
  }
  if (!is.null(x[["retrieval_time_utc"]])) {
    .e <- checkmate::check_string(x[["retrieval_time_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("HexCellValue$retrieval_time_utc: ", .e))
  }
  if (!is.null(x[["vertical_level"]])) {
    .e <- checkmate::check_number(x[["vertical_level"]])
    if (!isTRUE(.e)) return(paste0("HexCellValue$vertical_level: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$vertical_level_unit: ", .e))
  }
  if (!is.null(x[["quality_flag"]])) {
    .e <- checkmate::check_string(x[["quality_flag"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$quality_flag: ", .e))
  }
  if (is.null(x[["data_status"]])) return("HexCellValue: required slot `data_status` is missing")
  if (!is.null(x[["data_status"]])) {
    .e <- checkmate::check_choice(x[["data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("HexCellValue$data_status: ", .e))
  }
  if (is.null(x[["source_system"]])) return("HexCellValue: required slot `source_system` is missing")
  if (!is.null(x[["source_system"]])) {
    .e <- checkmate::check_string(x[["source_system"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$source_system: ", .e))
  }
  if (!is.null(x[["source_version"]])) {
    .e <- checkmate::check_string(x[["source_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$source_version: ", .e))
  }
  if (is.null(x[["null_semantics"]])) return("HexCellValue: required slot `null_semantics` is missing")
  if (!is.null(x[["null_semantics"]])) {
    .e <- checkmate::check_choice(x[["null_semantics"]], c("present", "structural_null", "derived_null", "true_zero"))
    if (!isTRUE(.e)) return(paste0("HexCellValue$null_semantics: ", .e))
  }
  if (!is.null(x[["superseded_by"]])) {
    .e <- checkmate::check_string(x[["superseded_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("HexCellValue$superseded_by: ", .e))
  }
  # rule 1: A hex covered less than half by its source must be flagged. Partial coverage at the edge of a product's extent
  if ((!is.null(x[["coverage_fraction"]]) && x[["coverage_fraction"]] <= 0.5)) {
    if (is.null(x[["quality_flag"]])) return("HexCellValue: rule 1 — `quality_flag` is required here")
  }
  TRUE
}

assert_HexCellValue <- function(x) {
  res <- check_HexCellValue(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one VectorFeature record
#' A reference geometry with attributes but no measured value over time: SEDAC gROADS road segments, hydrography lines, building footprints. Deliberately
check_VectorFeature <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("VectorFeature: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("VectorFeature$id: ", .e))
  }
  if (is.null(x[["product"]])) return("VectorFeature: required slot `product` is missing")
  if (!is.null(x[["product"]])) {
    .e <- checkmate::check_string(x[["product"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("VectorFeature$product: ", .e))
  }
  if (!is.null(x[["feature_id"]])) {
    .e <- checkmate::check_string(x[["feature_id"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("VectorFeature$feature_id: ", .e))
  }
  if (!is.null(x[["feature_type"]])) {
    .e <- checkmate::check_string(x[["feature_type"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("VectorFeature$feature_type: ", .e))
  }
  if (is.null(x[["geometry_type"]])) return("VectorFeature: required slot `geometry_type` is missing")
  if (!is.null(x[["geometry_type"]])) {
    .e <- checkmate::check_choice(x[["geometry_type"]], c("point", "multipoint", "linestring", "multilinestring", "polygon", "multipolygon", "geometrycollection"))
    if (!isTRUE(.e)) return(paste0("VectorFeature$geometry_type: ", .e))
  }
  if (is.null(x[["feature_geom_wkt"]])) return("VectorFeature: required slot `feature_geom_wkt` is missing")
  if (!is.null(x[["feature_geom_wkt"]])) {
    .e <- checkmate::check_string(x[["feature_geom_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("VectorFeature$feature_geom_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("VectorFeature$srid: ", .e))
  }
  if (!is.null(x[["feature_attributes"]])) {
    .e <- checkmate::check_string(x[["feature_attributes"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("VectorFeature$feature_attributes: ", .e))
  }
  if (!is.null(x[["length_m"]])) {
    .e <- checkmate::check_number(x[["length_m"]])
    if (!isTRUE(.e)) return(paste0("VectorFeature$length_m: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("VectorFeature$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("VectorFeature$processing_run: ", .e))
  }
  TRUE
}

assert_VectorFeature <- function(x) {
  res <- check_VectorFeature(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one LocationSet record
#' A user-supplied collection of locations — the `locs` argument, promoted to an entity. Carries the identifier field name (`locs_id`), the CRS the user 
check_LocationSet <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("LocationSet: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("LocationSet$id: ", .e))
  }
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("LocationSet$name: ", .e))
  }
  if (!is.null(x[["description"]])) {
    .e <- checkmate::check_string(x[["description"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("LocationSet$description: ", .e))
  }
  if (is.null(x[["locs_id_field"]])) return("LocationSet: required slot `locs_id_field` is missing")
  if (!is.null(x[["locs_id_field"]])) {
    .e <- checkmate::check_string(x[["locs_id_field"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("LocationSet$locs_id_field: ", .e))
  }
  if (is.null(x[["crs"]])) return("LocationSet: required slot `crs` is missing")
  if (!is.null(x[["crs"]])) {
    .e <- checkmate::check_string(x[["crs"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("LocationSet$crs: ", .e))
  }
  if (!is.null(x[["crs"]]) && !grepl("^[A-Za-z]+:[0-9]+$", x[["crs"]]))
    return("LocationSet$crs: does not match required pattern")
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("LocationSet$srid: ", .e))
  }
  if (!is.null(x[["geometry_type"]])) {
    .e <- checkmate::check_choice(x[["geometry_type"]], c("point", "multipoint", "linestring", "multilinestring", "polygon", "multipolygon", "geometrycollection"))
    if (!isTRUE(.e)) return(paste0("LocationSet$geometry_type: ", .e))
  }
  if (!is.null(x[["location_count"]])) {
    .e <- checkmate::check_int(x[["location_count"]])
    if (!isTRUE(.e)) return(paste0("LocationSet$location_count: ", .e))
  }
  if (is.null(x[["phi_status"]])) return("LocationSet: required slot `phi_status` is missing")
  if (!is.null(x[["phi_status"]])) {
    .e <- checkmate::check_choice(x[["phi_status"]], c("no_phi", "aggregated_no_phi", "phi_present"))
    if (!isTRUE(.e)) return(paste0("LocationSet$phi_status: ", .e))
  }
  if (!is.null(x[["provided_by"]])) {
    .e <- checkmate::check_string(x[["provided_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("LocationSet$provided_by: ", .e))
  }
  if (!is.null(x[["created_at"]])) {
    .e <- checkmate::check_string(x[["created_at"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("LocationSet$created_at: ", .e))
  }
  TRUE
}

assert_LocationSet <- function(x) {
  res <- check_LocationSet(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one Location record
#' One location in a set, with an optional validity interval. `valid_from` / `valid_to` exist because a residential address is not timeless. Address hist
check_Location <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("Location: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Location$id: ", .e))
  }
  if (is.null(x[["location_set"]])) return("Location: required slot `location_set` is missing")
  if (!is.null(x[["location_set"]])) {
    .e <- checkmate::check_string(x[["location_set"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("Location$location_set: ", .e))
  }
  if (is.null(x[["location_key"]])) return("Location: required slot `location_key` is missing")
  if (!is.null(x[["location_key"]])) {
    .e <- checkmate::check_string(x[["location_key"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Location$location_key: ", .e))
  }
  if (is.null(x[["location_geom_wkt"]])) return("Location: required slot `location_geom_wkt` is missing")
  if (!is.null(x[["location_geom_wkt"]])) {
    .e <- checkmate::check_string(x[["location_geom_wkt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("Location$location_geom_wkt: ", .e))
  }
  if (!is.null(x[["srid"]])) {
    .e <- checkmate::check_int(x[["srid"]])
    if (!isTRUE(.e)) return(paste0("Location$srid: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]])) {
    .e <- checkmate::check_number(x[["buffer_radius_m"]])
    if (!isTRUE(.e)) return(paste0("Location$buffer_radius_m: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]]) && x[["buffer_radius_m"]] < 0.0)
    return("Location$buffer_radius_m: below minimum 0.0")
  if (!is.null(x[["valid_from"]])) {
    .e <- checkmate::check_string(x[["valid_from"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("Location$valid_from: ", .e))
  }
  if (!is.null(x[["valid_to"]])) {
    .e <- checkmate::check_string(x[["valid_to"]], pattern = '^\\d{4}-\\d{2}-\\d{2}$')
    if (!isTRUE(.e)) return(paste0("Location$valid_to: ", .e))
  }
  if (!is.null(x[["target_geography_type"]])) {
    .e <- checkmate::check_choice(x[["target_geography_type"]], c("point_location", "buffered_point", "census_block_group", "census_tract", "zcta", "county", "state", "hydrologic_unit", "h3_hex", "grid_cell", "public_water_system"))
    if (!isTRUE(.e)) return(paste0("Location$target_geography_type: ", .e))
  }
  TRUE
}

assert_Location <- function(x) {
  res <- check_Location(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one ExtractionRequest record
#' A declarative, reproducible specification of an extraction: which variables, over which locations, for which time window, with which buffer, aggregati
check_ExtractionRequest <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("ExtractionRequest: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$id: ", .e))
  }
  if (!is.null(x[["name"]])) {
    .e <- checkmate::check_string(x[["name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$name: ", .e))
  }
  if (is.null(x[["location_set"]])) return("ExtractionRequest: required slot `location_set` is missing")
  if (!is.null(x[["location_set"]])) {
    .e <- checkmate::check_string(x[["location_set"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$location_set: ", .e))
  }
  if (is.null(x[["time_window_start"]])) return("ExtractionRequest: required slot `time_window_start` is missing")
  if (!is.null(x[["time_window_start"]])) {
    .e <- checkmate::check_string(x[["time_window_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$time_window_start: ", .e))
  }
  if (is.null(x[["time_window_end"]])) return("ExtractionRequest: required slot `time_window_end` is missing")
  if (!is.null(x[["time_window_end"]])) {
    .e <- checkmate::check_string(x[["time_window_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$time_window_end: ", .e))
  }
  if (!is.null(x[["temporal_grouping"]])) {
    .e <- checkmate::check_choice(x[["temporal_grouping"]], c("none", "hour", "day", "month", "year"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$temporal_grouping: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]])) {
    .e <- checkmate::check_number(x[["buffer_radius_m"]])
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$buffer_radius_m: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]]) && x[["buffer_radius_m"]] < 0.0)
    return("ExtractionRequest$buffer_radius_m: below minimum 0.0")
  if (is.null(x[["extraction_method"]])) return("ExtractionRequest: required slot `extraction_method` is missing")
  if (!is.null(x[["extraction_method"]])) {
    .e <- checkmate::check_choice(x[["extraction_method"]], c("nearest_cell", "bilinear", "inverse_distance_weighted_4_nearest_cells", "area_weighted_polygon_mean", "population_weighted_mean", "point_station_lookup", "nearest_station", "intersection_area_proportion", "raster_zonal_sum", "feature_density", "exact_extract"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$extraction_method: ", .e))
  }
  if (!is.null(x[["aggregation_method"]])) {
    .e <- checkmate::check_choice(x[["aggregation_method"]], c("instantaneous", "mean", "minimum", "maximum", "median", "sum", "count", "percentile", "cumulative", "area_weighted_mean", "population_weighted_mean", "mode", "class_proportion", "density", "nearest", "other"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$aggregation_method: ", .e))
  }
  if (!is.null(x[["weighting_product_variable"]])) {
    .e <- checkmate::check_string(x[["weighting_product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$weighting_product_variable: ", .e))
  }
  if (!is.null(x[["target_geography_type"]])) {
    .e <- checkmate::check_choice(x[["target_geography_type"]], c("point_location", "buffered_point", "census_block_group", "census_tract", "zcta", "county", "state", "hydrologic_unit", "h3_hex", "grid_cell", "public_water_system"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$target_geography_type: ", .e))
  }
  if (!is.null(x[["output_orientation"]])) {
    .e <- checkmate::check_choice(x[["output_orientation"]], c("wide", "long"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$output_orientation: ", .e))
  }
  if (!is.null(x[["materialization_mode"]])) {
    .e <- checkmate::check_choice(x[["materialization_mode"]], c("direct_remote_scan", "server_side_subset", "bounded_cache", "full_materialization"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$materialization_mode: ", .e))
  }
  if (!is.null(x[["source_priority_policy"]])) {
    .e <- checkmate::check_string(x[["source_priority_policy"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$source_priority_policy: ", .e))
  }
  if (!is.null(x[["status_mixing_policy"]])) {
    .e <- checkmate::check_choice(x[["status_mixing_policy"]], c("refuse", "allow_with_policy"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$status_mixing_policy: ", .e))
  }
  if (!is.null(x[["strict_extensivity_check"]])) {
    .e <- checkmate::check_choice(x[["strict_extensivity_check"]], c("strict", "permissive"))
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$strict_extensivity_check: ", .e))
  }
  if (!is.null(x[["requested_by"]])) {
    .e <- checkmate::check_string(x[["requested_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$requested_by: ", .e))
  }
  if (!is.null(x[["request_timestamp_utc"]])) {
    .e <- checkmate::check_string(x[["request_timestamp_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$request_timestamp_utc: ", .e))
  }
  if (is.null(x[["request_hash"]])) return("ExtractionRequest: required slot `request_hash` is missing")
  if (!is.null(x[["request_hash"]])) {
    .e <- checkmate::check_string(x[["request_hash"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$request_hash: ", .e))
  }
  if (!is.null(x[["request_hash"]]) && !grepl("^[0-9a-f]{64}$", x[["request_hash"]]))
    return("ExtractionRequest$request_hash: does not match required pattern")
  if (!is.null(x[["generated_sql"]])) {
    .e <- checkmate::check_string(x[["generated_sql"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ExtractionRequest$generated_sql: ", .e))
  }
  # rule 1: A buffer without an aggregation method is ambiguous — there are now many source values per location and nothin
  if ((!is.null(x[["buffer_radius_m"]]) && x[["buffer_radius_m"]] >= 1e-06)) {
    if (is.null(x[["aggregation_method"]])) return("ExtractionRequest: rule 1 — `aggregation_method` is required here")
  }
  # rule 2: Population weighting must name the population product it weighted by. "Population-weighted" with no stated pop
  if ((!is.null(x[["aggregation_method"]]) && as.character(x[["aggregation_method"]]) %in% c("population_weighted_mean"))) {
    if (is.null(x[["weighting_product_variable"]])) return("ExtractionRequest: rule 2 — `weighting_product_variable` is required here")
  }
  # rule 3: The AQS/AirNow rule, enforced at request time. Mixing preliminary and quality-controlled values into one resul
  if ((!is.null(x[["status_mixing_policy"]]) && identical(as.character(x[["status_mixing_policy"]]), "allow_with_policy"))) {
    if (is.null(x[["source_priority_policy"]])) return("ExtractionRequest: rule 3 — `source_priority_policy` is required here")
  }
  TRUE
}

assert_ExtractionRequest <- function(x) {
  res <- check_ExtractionRequest(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one AmbientValueAtLocation record
#' The output row: an ambient value assigned to a user location for a time interval, with the extraction that produced it recorded alongside. This is wha
check_AmbientValueAtLocation <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["location"]])) return("AmbientValueAtLocation: required slot `location` is missing")
  if (!is.null(x[["location"]])) {
    .e <- checkmate::check_string(x[["location"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$location: ", .e))
  }
  if (is.null(x[["extraction_request"]])) return("AmbientValueAtLocation: required slot `extraction_request` is missing")
  if (!is.null(x[["extraction_request"]])) {
    .e <- checkmate::check_string(x[["extraction_request"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$extraction_request: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]])) {
    .e <- checkmate::check_number(x[["buffer_radius_m"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$buffer_radius_m: ", .e))
  }
  if (!is.null(x[["buffer_radius_m"]]) && x[["buffer_radius_m"]] < 0.0)
    return("AmbientValueAtLocation$buffer_radius_m: below minimum 0.0")
  if (is.null(x[["extraction_method"]])) return("AmbientValueAtLocation: required slot `extraction_method` is missing")
  if (!is.null(x[["extraction_method"]])) {
    .e <- checkmate::check_choice(x[["extraction_method"]], c("nearest_cell", "bilinear", "inverse_distance_weighted_4_nearest_cells", "area_weighted_polygon_mean", "population_weighted_mean", "point_station_lookup", "nearest_station", "intersection_area_proportion", "raster_zonal_sum", "feature_density", "exact_extract"))
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$extraction_method: ", .e))
  }
  if (!is.null(x[["aggregation_method"]])) {
    .e <- checkmate::check_choice(x[["aggregation_method"]], c("instantaneous", "mean", "minimum", "maximum", "median", "sum", "count", "percentile", "cumulative", "area_weighted_mean", "population_weighted_mean", "mode", "class_proportion", "density", "nearest", "other"))
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$aggregation_method: ", .e))
  }
  if (!is.null(x[["contributing_value_count"]])) {
    .e <- checkmate::check_int(x[["contributing_value_count"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$contributing_value_count: ", .e))
  }
  if (!is.null(x[["coverage_fraction"]])) {
    .e <- checkmate::check_number(x[["coverage_fraction"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$coverage_fraction: ", .e))
  }
  if (!is.null(x[["coverage_fraction"]]) && x[["coverage_fraction"]] < 0.0)
    return("AmbientValueAtLocation$coverage_fraction: below minimum 0.0")
  if (!is.null(x[["coverage_fraction"]]) && x[["coverage_fraction"]] > 1.0)
    return("AmbientValueAtLocation$coverage_fraction: above maximum 1.0")
  if (!is.null(x[["distance_to_source_m"]])) {
    .e <- checkmate::check_number(x[["distance_to_source_m"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$distance_to_source_m: ", .e))
  }
  if (!is.null(x[["lag_days_applied"]])) {
    .e <- checkmate::check_int(x[["lag_days_applied"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$lag_days_applied: ", .e))
  }
  if (!is.null(x[["temporal_grouping"]])) {
    .e <- checkmate::check_choice(x[["temporal_grouping"]], c("none", "hour", "day", "month", "year"))
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$temporal_grouping: ", .e))
  }
  if (!is.null(x[["output_column_name"]])) {
    .e <- checkmate::check_string(x[["output_column_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$output_column_name: ", .e))
  }
  if (is.null(x[["id"]])) return("AmbientValueAtLocation: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$id: ", .e))
  }
  if (is.null(x[["product_variable"]])) return("AmbientValueAtLocation: required slot `product_variable` is missing")
  if (!is.null(x[["product_variable"]])) {
    .e <- checkmate::check_string(x[["product_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$product_variable: ", .e))
  }
  if (is.null(x[["canonical_variable"]])) return("AmbientValueAtLocation: required slot `canonical_variable` is missing")
  if (!is.null(x[["canonical_variable"]])) {
    .e <- checkmate::check_string(x[["canonical_variable"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$canonical_variable: ", .e))
  }
  if (!is.null(x[["asset"]])) {
    .e <- checkmate::check_string(x[["asset"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$asset: ", .e))
  }
  if (!is.null(x[["processing_run"]])) {
    .e <- checkmate::check_string(x[["processing_run"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$processing_run: ", .e))
  }
  if (!is.null(x[["value"]])) {
    .e <- checkmate::check_number(x[["value"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$value: ", .e))
  }
  if (!is.null(x[["value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$value_unit_ucum: ", .e))
  }
  if (!is.null(x[["native_value"]])) {
    .e <- checkmate::check_number(x[["native_value"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$native_value: ", .e))
  }
  if (!is.null(x[["native_value_unit_ucum"]])) {
    .e <- checkmate::check_string(x[["native_value_unit_ucum"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$native_value_unit_ucum: ", .e))
  }
  if (is.null(x[["valid_time_start"]])) return("AmbientValueAtLocation: required slot `valid_time_start` is missing")
  if (!is.null(x[["valid_time_start"]])) {
    .e <- checkmate::check_string(x[["valid_time_start"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$valid_time_start: ", .e))
  }
  if (!is.null(x[["valid_time_end"]])) {
    .e <- checkmate::check_string(x[["valid_time_end"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$valid_time_end: ", .e))
  }
  if (!is.null(x[["reference_time"]])) {
    .e <- checkmate::check_string(x[["reference_time"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$reference_time: ", .e))
  }
  if (!is.null(x[["retrieval_time_utc"]])) {
    .e <- checkmate::check_string(x[["retrieval_time_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$retrieval_time_utc: ", .e))
  }
  if (!is.null(x[["vertical_level"]])) {
    .e <- checkmate::check_number(x[["vertical_level"]])
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$vertical_level: ", .e))
  }
  if (!is.null(x[["vertical_level_unit"]])) {
    .e <- checkmate::check_string(x[["vertical_level_unit"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$vertical_level_unit: ", .e))
  }
  if (!is.null(x[["quality_flag"]])) {
    .e <- checkmate::check_string(x[["quality_flag"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$quality_flag: ", .e))
  }
  if (is.null(x[["data_status"]])) return("AmbientValueAtLocation: required slot `data_status` is missing")
  if (!is.null(x[["data_status"]])) {
    .e <- checkmate::check_choice(x[["data_status"]], c("preliminary", "provisional", "quality_controlled", "final", "forecast", "reanalysis", "superseded", "unknown"))
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$data_status: ", .e))
  }
  if (is.null(x[["source_system"]])) return("AmbientValueAtLocation: required slot `source_system` is missing")
  if (!is.null(x[["source_system"]])) {
    .e <- checkmate::check_string(x[["source_system"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$source_system: ", .e))
  }
  if (!is.null(x[["source_version"]])) {
    .e <- checkmate::check_string(x[["source_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$source_version: ", .e))
  }
  if (is.null(x[["null_semantics"]])) return("AmbientValueAtLocation: required slot `null_semantics` is missing")
  if (!is.null(x[["null_semantics"]])) {
    .e <- checkmate::check_choice(x[["null_semantics"]], c("present", "structural_null", "derived_null", "true_zero"))
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$null_semantics: ", .e))
  }
  if (!is.null(x[["superseded_by"]])) {
    .e <- checkmate::check_string(x[["superseded_by"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("AmbientValueAtLocation$superseded_by: ", .e))
  }
  TRUE
}

assert_AmbientValueAtLocation <- function(x) {
  res <- check_AmbientValueAtLocation(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one ToolRun record
#' One execution of one amadeus operation, with everything needed to decide whether it can be trusted and whether it can be skipped. "Whether it can be s
check_ToolRun <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("ToolRun: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ToolRun$id: ", .e))
  }
  if (is.null(x[["run_role"]])) return("ToolRun: required slot `run_role` is missing")
  if (!is.null(x[["run_role"]])) {
    .e <- checkmate::check_choice(x[["run_role"]], c("download", "process", "calculate", "register", "harmonize", "import", "export"))
    if (!isTRUE(.e)) return(paste0("ToolRun$run_role: ", .e))
  }
  if (is.null(x[["tool_name"]])) return("ToolRun: required slot `tool_name` is missing")
  if (!is.null(x[["tool_name"]])) {
    .e <- checkmate::check_string(x[["tool_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$tool_name: ", .e))
  }
  if (is.null(x[["tool_version"]])) return("ToolRun: required slot `tool_version` is missing")
  if (!is.null(x[["tool_version"]])) {
    .e <- checkmate::check_string(x[["tool_version"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$tool_version: ", .e))
  }
  if (!is.null(x[["function_name"]])) {
    .e <- checkmate::check_string(x[["function_name"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$function_name: ", .e))
  }
  if (!is.null(x[["run_arguments"]])) {
    .e <- checkmate::check_string(x[["run_arguments"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$run_arguments: ", .e))
  }
  if (!is.null(x[["container_image_repository"]])) {
    .e <- checkmate::check_string(x[["container_image_repository"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$container_image_repository: ", .e))
  }
  if (!is.null(x[["container_image_digest"]])) {
    .e <- checkmate::check_string(x[["container_image_digest"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$container_image_digest: ", .e))
  }
  if (!is.null(x[["container_image_digest"]]) && !grepl("^sha256:[0-9a-f]{64}$", x[["container_image_digest"]]))
    return("ToolRun$container_image_digest: does not match required pattern")
  if (is.null(x[["run_timestamp_utc"]])) return("ToolRun: required slot `run_timestamp_utc` is missing")
  if (!is.null(x[["run_timestamp_utc"]])) {
    .e <- checkmate::check_string(x[["run_timestamp_utc"]], pattern = '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$')
    if (!isTRUE(.e)) return(paste0("ToolRun$run_timestamp_utc: ", .e))
  }
  if (!is.null(x[["run_duration_seconds"]])) {
    .e <- checkmate::check_number(x[["run_duration_seconds"]])
    if (!isTRUE(.e)) return(paste0("ToolRun$run_duration_seconds: ", .e))
  }
  if (!is.null(x[["run_environment"]])) {
    .e <- checkmate::check_string(x[["run_environment"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$run_environment: ", .e))
  }
  if (!is.null(x[["input_file_sha256"]])) {
    .e <- checkmate::check_string(x[["input_file_sha256"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$input_file_sha256: ", .e))
  }
  if (!is.null(x[["input_file_sha256"]]) && !grepl("^[0-9a-f]{64}$", x[["input_file_sha256"]]))
    return("ToolRun$input_file_sha256: does not match required pattern")
  if (!is.null(x[["input_row_count"]])) {
    .e <- checkmate::check_int(x[["input_row_count"]])
    if (!isTRUE(.e)) return(paste0("ToolRun$input_row_count: ", .e))
  }
  if (!is.null(x[["output_file_sha256"]])) {
    .e <- checkmate::check_string(x[["output_file_sha256"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$output_file_sha256: ", .e))
  }
  if (!is.null(x[["output_file_sha256"]]) && !grepl("^[0-9a-f]{64}$", x[["output_file_sha256"]]))
    return("ToolRun$output_file_sha256: does not match required pattern")
  if (!is.null(x[["output_row_count"]])) {
    .e <- checkmate::check_int(x[["output_row_count"]])
    if (!isTRUE(.e)) return(paste0("ToolRun$output_row_count: ", .e))
  }
  if (is.null(x[["status"]])) return("ToolRun: required slot `status` is missing")
  if (!is.null(x[["status"]])) {
    .e <- checkmate::check_choice(x[["status"]], c("succeeded", "failed", "partial", "cancelled"))
    if (!isTRUE(.e)) return(paste0("ToolRun$status: ", .e))
  }
  if (!is.null(x[["log_excerpt"]])) {
    .e <- checkmate::check_string(x[["log_excerpt"]], na.ok = FALSE)
    if (!isTRUE(.e)) return(paste0("ToolRun$log_excerpt: ", .e))
  }
  if (!is.null(x[["materialization_mode"]])) {
    .e <- checkmate::check_choice(x[["materialization_mode"]], c("direct_remote_scan", "server_side_subset", "bounded_cache", "full_materialization"))
    if (!isTRUE(.e)) return(paste0("ToolRun$materialization_mode: ", .e))
  }
  if (!is.null(x[["bytes_transferred"]])) {
    .e <- checkmate::check_int(x[["bytes_transferred"]])
    if (!isTRUE(.e)) return(paste0("ToolRun$bytes_transferred: ", .e))
  }
  if (!is.null(x[["request_count"]])) {
    .e <- checkmate::check_int(x[["request_count"]])
    if (!isTRUE(.e)) return(paste0("ToolRun$request_count: ", .e))
  }
  # rule 1: A successful processing or calculation run must state how many rows it produced. Row-count parity against a fr
  if ((!is.null(x[["status"]]) && identical(as.character(x[["status"]]), "succeeded")) && (!is.null(x[["run_role"]]) && as.character(x[["run_role"]]) %in% c("process", "calculate"))) {
    if (is.null(x[["output_row_count"]])) return("ToolRun: rule 1 — `output_row_count` is required here")
  }
  # rule 2: Anything downstream of download must name what it consumed. Without this the chain has a hole and provenance t
  if ((!is.null(x[["run_role"]]) && as.character(x[["run_role"]]) %in% c("process", "calculate", "harmonize"))) {
    if (is.null(x[["upstream_runs"]])) return("ToolRun: rule 2 — `upstream_runs` is required here")
  }
  # rule 3: A failed run must carry diagnostic output. "A failed run is diagnosable and can resume without corrupting acce
  if ((!is.null(x[["status"]]) && identical(as.character(x[["status"]]), "failed"))) {
    if (is.null(x[["log_excerpt"]])) return("ToolRun: rule 3 — `log_excerpt` is required here")
  }
  TRUE
}

assert_ToolRun <- function(x) {
  res <- check_ToolRun(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Validate one ProvenanceChain record
#' A materialised walk of `upstream_runs` from a derived value back to its terminus, plus what the terminus is. Stored rather than always recomputed beca
check_ProvenanceChain <- function(x) {
  if (!is.list(x)) return("expected a named list")
  if (is.null(x[["id"]])) return("ProvenanceChain: required slot `id` is missing")
  if (!is.null(x[["id"]])) {
    .e <- checkmate::check_string(x[["id"]], min.chars = 1)
    if (!isTRUE(.e)) return(paste0("ProvenanceChain$id: ", .e))
  }
  if (is.null(x[["terminus_type"]])) return("ProvenanceChain: required slot `terminus_type` is missing")
  if (!is.null(x[["terminus_type"]])) {
    .e <- checkmate::check_choice(x[["terminus_type"]], c("raw_source_download", "remote_asset_scan", "pre_existing_curated_dataset", "externally_imported_dataset", "synthetic_data"))
    if (!isTRUE(.e)) return(paste0("ProvenanceChain$terminus_type: ", .e))
  }
  TRUE
}

assert_ProvenanceChain <- function(x) {
  res <- check_ProvenanceChain(x)
  if (!isTRUE(res)) stop(res, call. = FALSE)
  invisible(x)
}

#' Every generated validator, by class name
amadeus_validators <- list(
  "DataSource" = check_DataSource,
  "Product" = check_Product,
  "GridDefinition" = check_GridDefinition,
  "CanonicalVariable" = check_CanonicalVariable,
  "ProductVariable" = check_ProductVariable,
  "OmopConceptBinding" = check_OmopConceptBinding,
  "Asset" = check_Asset,
  "StationObservation" = check_StationObservation,
  "GridCellValue" = check_GridCellValue,
  "AreaValue" = check_AreaValue,
  "HexCellValue" = check_HexCellValue,
  "VectorFeature" = check_VectorFeature,
  "LocationSet" = check_LocationSet,
  "Location" = check_Location,
  "ExtractionRequest" = check_ExtractionRequest,
  "AmbientValueAtLocation" = check_AmbientValueAtLocation,
  "ToolRun" = check_ToolRun,
  "ProvenanceChain" = check_ProvenanceChain
)

#' Validate a data.frame of records against a class
#' @return a character vector of problems, empty if all rows pass
validate_amadeus_table <- function(df, class_name) {
  f <- amadeus_validators[[class_name]]
  if (is.null(f)) stop("unknown class: ", class_name, call. = FALSE)
  problems <- character(0)
  for (i in seq_len(nrow(df))) {
    rec <- as.list(df[i, , drop = FALSE])
    rec <- rec[!vapply(rec, function(v) length(v) == 0 || is.na(v[1]), logical(1))]
    res <- f(rec)
    if (!isTRUE(res)) problems <- c(problems, paste0("row ", i, ": ", res))
  }
  problems
}

