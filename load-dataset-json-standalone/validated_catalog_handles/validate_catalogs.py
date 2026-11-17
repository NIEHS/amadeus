#!/usr/bin/env python3
"""Validate the catalog artifacts against the maintained authoritative schema.

Requires PyYAML and LinkML. Run with the interpreter that provides
``linkml-validate``; no downloads or schema generation are performed.
"""

from __future__ import annotations

import argparse
import contextlib
import datetime
import hashlib
import importlib.metadata
import json
import io
import math
from pathlib import Path
import re
import runpy
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
import zipfile

import yaml


ROOT = Path(__file__).resolve().parents[1]
HERE = Path(__file__).resolve().parent
SPECS = ROOT / "dataset_spec_objects"
SCHEMA = ROOT / "amadeus_schema/src/amadeus_schema/schema/amadeus_schema.yaml"
DATASETS = ("gridmet", "aqs", "hms", "nlcd", "terraclimate", "koppen_geiger", "cropscape", "modis", "merra2", "population", "groads", "huc", "geos", "edgar", "narr", "prism", "drought", "ecoregion", "nei", "tri", "gmted", "olm", "improve")
CATALOG_COLLECTIONS = (
    "data_sources", "products", "grid_definitions", "canonical_variables", "product_variables", "assets"
)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path: Path) -> dict:
    return yaml.safe_load(path.read_text())


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def index(bundle: dict, collection: str) -> dict:
    rows = bundle.get(collection, [])
    result = {r["id"]: r for r in rows}
    require(len(result) == len(rows), f"Duplicate IDs in {collection}")
    return result


def workbook_cells(path: Path) -> dict:
    """Read actual saved workbook values independently of the audit report."""
    ns = {"m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
    result = {}
    with zipfile.ZipFile(path) as archive:
        strings = ["".join(si.itertext()) for si in ET.fromstring(
            archive.read("xl/sharedStrings.xml")).findall("m:si", ns)] if (
                "xl/sharedStrings.xml" in archive.namelist()) else []
        rels = {r.get("Id"): r.get("Target") for r in ET.fromstring(
            archive.read("xl/_rels/workbook.xml.rels"))}
        for sheet in ET.fromstring(archive.read("xl/workbook.xml")).findall("m:sheets/m:sheet", ns):
            target = rels[sheet.get("{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id")]
            part = target.lstrip("/") if target.startswith("/") else "xl/" + target
            cells = {}
            for cell in ET.fromstring(archive.read(part)).findall("m:sheetData/m:row/m:c", ns):
                value, inline = cell.find("m:v", ns), cell.find("m:is", ns)
                cells[cell.get("r")] = (
                    "".join(inline.itertext()) if inline is not None else
                    strings[int(value.text)] if cell.get("t") == "s" and value is not None else
                    value.text if value is not None else ""
                )
            result[sheet.get("name")] = cells
    return result


def import_coherence(bundle: dict) -> dict:
    """Run the authoritative helper; distinguish evidenced time/area scopes.

    The source helper is deliberately run unchanged. Its raw output remains
    visible; only known daily/monthly depth statements can be resolved by
    independent metadata. Any other message is an unresolved error.
    """
    helper = SCHEMA.parents[3] / "scripts/import_external.py"
    checker = runpy.run_path(str(helper), run_name="catalog_coherence_review")["stage_coherence"]
    with contextlib.redirect_stdout(io.StringIO()) as captured:
        raw = checker(bundle)
    cvs = index(bundle, "canonical_variables")
    products = index(bundle, "products")
    resolved = []
    unresolved = list(raw)
    for pv in bundle.get("product_variables", []):
        code = pv.get("amadeus_variable_code")
        if pv['id'].startswith(('amadeus:pvar/narr_psl_daily_','amadeus:pvar/prism_ppt_')):
            sys.path.insert(0,str(SPECS/'integration_20261107'))
            from verify_profiles import temporal_sum_evidence
            evidence = temporal_sum_evidence(pv,cvs.get(pv['canonical_variable'],{}),products.get(pv['product'],{}))
            message=f"{pv['id']}: aggregation 'sum' is illegal for intensive variable {pv['canonical_variable']}"
            if evidence and message in unresolved:
                unresolved.remove(message)
                resolved.append({'product_variable':pv['id'],'raw_message':message,**evidence,
                    'interpretation':'Provider temporal accumulation of a per-area amount; no spatial summation is authorized.'})
            continue
        if pv['id'] == f'amadeus:pvar/terraclimate.{code}' and code in {'aet','def','pet','ppt','q'}:
            cv = cvs.get(pv['canonical_variable'], {})
            product = products.get(pv['product'], {})
            document = ET.parse(SPECS / f'evidence/terraclimate_{code}.xml').getroot()
            node = document.find('.//grid')
            attrs = {a.get('name'):a.get('value') for a in node.findall('attribute')}
            table = re.sub('<[^>]+>', '', (SPECS/'evidence/terraclimate_variables.html').read_text())
            monthly = bool(re.search(re.escape(code)+r'\s*\([^)]*monthly total', table))
            evidenced = (monthly and node.get('name') == pv['native_name']
                and pv.get('aggregation_method') == 'sum' and pv.get('aggregation_window_iso') == 'P1M'
                and product.get('temporal_resolution') == 'monthly'
                and pv.get('native_units_ucum') == cv.get('units_ucum') == attrs.get('units') == 'mm'
                and cv.get('extensivity') == pv.get('extensivity', 'intensive') == 'intensive'
                and any(a.get('name') == 'time' and a.get('shape') == '12' for a in document.findall('axis')))
            message = f"{pv['id']}: aggregation 'sum' is illegal for intensive variable {cv.get('id')}"
            if evidenced and message in unresolved:
                unresolved.remove(message)
                resolved.append({'product_variable':pv['id'],'raw_message':message,
                    'source_metadata':f'dataset_spec_objects/evidence/terraclimate_{code}.xml',
                    'support_evidence':'dataset_spec_objects/evidence/terraclimate_variables.html',
                    'interpretation':'Verified monthly accumulated depth, spatially intensive; this does not authorize spatial summation.'})
            continue
        if pv["id"] != f"amadeus:pvar/gridmet.{code}" or code not in {"pr", "pet", "etr"}:
            continue
        cv = cvs.get(pv["canonical_variable"], {})
        product = products.get(pv["product"], {})
        metadata = ET.parse(SPECS / f"evidence/gridmet_{code}.xml").getroot().find(".//grid")
        attrs = {a.get("name"): a.get("value") for a in metadata.findall("attribute")}
        description = attrs.get("description", "").lower()
        evidence_matches = (
            pv.get("aggregation_method") == "sum"
            and pv.get("aggregation_window_iso") == "P1D"
            and pv.get("native_units_ucum") == cv.get("units_ucum") == attrs.get("units") == "mm"
            and cv.get("extensivity") == "intensive"
            and pv.get("extensivity", "intensive") == "intensive"
            and product.get("temporal_resolution") == "daily"
            and metadata.get("name") == pv["native_name"]
            and ("daily accumulated precipitation" in description
                 or "daily reference evapotranspiration" in description)
        )
        message = (f"{pv['id']}: aggregation 'sum' is illegal for intensive variable {cv.get('id')}")
        if evidence_matches and message in unresolved:
            unresolved.remove(message)
            resolved.append({
                "product_variable": pv["id"], "raw_message": message,
                "source_metadata": f"dataset_spec_objects/evidence/gridmet_{code}.xml",
                "interpretation": "Temporal daily depth accumulation; the quantity remains spatially intensive. This does not authorize spatial summation of depths.",
            })
    return {"checker": str(helper.relative_to(ROOT)), "checker_sha256": digest(helper),
            "raw_errors": raw, "raw_output": captured.getvalue().strip(),
            "resolved_temporal_spatial_scope_conflicts": resolved,
            "unresolved_errors": unresolved,
            "unmodified_upstream_checker_passed": not raw,
            "scope_aware_profile_check_passed": not unresolved}


def crs_requirements(bundle: dict) -> dict:
    """Resolve native definitions, never guess a backend SRID from missing data."""
    requirements = []
    for grid in bundle.get("grid_definitions", []):
        authority = grid.get("crs", "")
        unresolved_prefixes={'AMADEUS:100003':'merra2_','AMADEUS:100004':'geos_cf_v1_',
                             'AMADEUS:100005':'narr_psl_daily_','AMADEUS:100006':'noaa_eddi_conus_',
                             'AMADEUS:100007':'edgar_v81_global_'}
        if authority in unresolved_prefixes:
            expected_prefix=unresolved_prefixes[authority]
            require(grid['name'].startswith(expected_prefix), 'Unresolved CRS placeholder used outside its explicit scope')
            require('proj_string' not in grid and 'srid' not in grid, 'Unverified native datum/projection or engine SRID was silently assigned')
            requirements.append({'grid':grid['id'],'catalog_identifier':authority,
                'status':'unresolved placeholder, not a CRS definition','verified_database_srid':None,
                'database_geometry_ready':False,'required_action':'Obtain authoritative native datum/radius before coordinate transformation or geometry ingestion; reject all SRID/CRS fallback.'})
            continue
        if authority.startswith("AMADEUS:"):
            require(bool(grid.get("proj_string")), "Local CRS identifier lacks its full native definition")
            require("srid" not in grid, "A catalog-local CRS must not acquire an unverified engine SRID")
            if grid["id"] == "amadeus:grid/nlcd_annual_c1_conus_30m":
                albers = ET.parse(SPECS / "evidence/nlcd_native_metadata.xml").getroot().find(
                    ".//spref/horizsys/planar/mapproj/albers")
                parameters = dict(re.findall(r"\+(\w+)=([^\s]+)", grid["proj_string"]))
                require(parameters.get("proj") == "aea" and parameters.get("datum") == "WGS84"
                        and parameters.get("units") == "m", "NLCD projection type/datum/units changed")
                for key, tag in (("lat_0", "latprjo"), ("lon_0", "longcm"),
                                 ("x_0", "feast"), ("y_0", "fnorth")):
                    require(math.isclose(float(parameters.get(key, "nan")), float(albers.findtext(tag))),
                            f"NLCD projection parameter {key} differs from provider evidence")
                require(sorted(float(parameters.get(k, "nan")) for k in ("lat_1", "lat_2")) ==
                        sorted(float(p.text) for p in albers.findall("stdparll")),
                        "NLCD standard parallels differ from provider evidence")
            elif grid['crs'] == 'AMADEUS:100002':
                parameters = dict(re.findall(r'\+(\w+)=([^\s]+)', grid['proj_string']))
                require(parameters.get('proj') == 'sinu' and parameters.get('units') == 'm'
                        and float(parameters.get('R','nan')) == 6371007.181
                        and all(float(parameters.get(k,'nan')) == 0 for k in ('lon_0','x_0','y_0')),
                        'MODIS native spherical sinusoidal projection changed')
            requirements.append({"grid": grid["id"], "catalog_identifier": authority,
                                 "resolve_using": "GridDefinition.proj_string",
                                 "verified_database_srid": None,
                                 "database_geometry_ready": False,
                                 "required_action": "Resolve/transform the exact native projection or explicitly register a matching CRS in the target engine; reject implicit SRID fallback."})
        elif re.fullmatch(r"EPSG:\d+", authority):
            require(grid.get("srid") == int(authority.split(":")[1]), "CRS/SRID mismatch")
            requirements.append({"grid": grid["id"], "catalog_identifier": authority,
                                 "verified_database_srid": grid["srid"],
                                 "database_geometry_ready": True,
                                 "scope": "CRS metadata readiness only; ingestion has not been tested."})
        else:
            raise ValueError(f"Unresolved grid CRS: {authority}")
    return {"implicit_srid_fallback_allowed": False, "grids": requirements}


def require_geometry_ingestion_ready(bundle: dict) -> dict:
    """Fail closed before a consumer invokes spatial ingestion/transformation.

    Metadata validation can pass with unresolved grids. Consumers must invoke
    this separate gate; this function does not retrofit the package R adapters.
    """
    policy = crs_requirements(bundle)
    blocked = [r['grid'] for r in policy['grids'] if not r['database_geometry_ready']]
    require(not blocked, 'Geometry ingestion blocked by unresolved/unregistered native CRS: ' + ', '.join(blocked))
    return policy


def additional_semantic_checks(bundles: dict) -> list[str]:
    """Compare added profiles with independent provider/fixture metadata."""
    terra = bundles['terraclimate']
    require(len(terra['products']) == 1 and len(terra['product_variables']) == 14,
            'TerraClimate verified historical scope changed')
    p = terra['products'][0]
    require(p.get('product_version') == '1.1' and p.get('temporal_coverage_start') == '1950-01-01'
            and p.get('temporal_resolution') == 'monthly', 'TerraClimate version/coverage/support mismatch')
    require('String version "V1.1"' in (SPECS/'evidence/terraclimate_ppt.das').read_text(),
            'TerraClimate version evidence missing')
    cvs = index(terra, 'canonical_variables')
    for pv in terra['product_variables']:
        code = pv['amadeus_variable_code']
        doc = ET.parse(SPECS/f'evidence/terraclimate_{code}.xml').getroot()
        node = doc.find('.//grid')
        attrs = {a.get('name'):a.get('value') for a in node.findall('attribute')}
        require(pv['native_name'] == node.get('name') and pv.get('cf_standard_name') == attrs['standard_name'],
                'TerraClimate native/CF identity differs from header')
        require(pv.get('missing_value_sentinel') == attrs['_FillValue']
                and pv.get('native_value_scale_factor') == float(attrs['scale_factor'])
                and pv.get('native_value_offset') == float(attrs['add_offset']),
                'TerraClimate fill/packing differs from current v1.1 header')
        cv = cvs[pv['canonical_variable']]
        require(cv['extensivity'] == 'intensive', 'TerraClimate depth/state cannot be relabelled spatially extensive')
        if code in {'soil','swe','PDSI'}:
            require(pv.get('temporal_alignment') == 'end' and 'aggregation_method' not in pv,
                    'End-of-month model state was turned into an unsupported reduction')
        elif code in {'tmax','tmin'}:
            require(pv.get('aggregation_method') == 'mean' and 'monthly_mean_daily_' in cv['name'],
                    'Monthly mean daily temperature extreme was confused with an absolute monthly extreme')
    results=['PASS TerraClimate: fourteen v1.1 headers, monthly totals/means/end states, 1950 coverage and version-specific packing']

    koppen=bundles['koppen_geiger']
    require(len(koppen['products']) == 6 and len(koppen['product_variables']) == 12,
            'Köppen V1 period/resolution/companion scope changed')
    headers={x['file']:x['tiff_tags'] for x in json.loads((SPECS/'evidence/koppen_tiff_headers.json').read_text())}
    require(len(headers)==12, 'Köppen twelve native header records missing')
    require(len(re.findall(r'^\s*\d+:', (SPECS/'evidence/koppen_legend.txt').read_text(), re.M))==30,
            'Köppen native class legend incomplete')
    grids=index(koppen,'grid_definitions'); products=index(koppen,'products')
    for pv in koppen['product_variables']:
        tags=headers[pv['native_name']];p=products[pv['product']];g=grids[p['grid_definition']]
        require(g['n_columns']==tags['256'][0] and g['n_rows']==tags['257'][0]
                and g['resolution_x']==tags['33550'][0] and g['resolution_y']==tags['33550'][1],
                'Köppen grid differs from actual TIFF header')
        require(p['temporal_resolution']=='timeless' and 'temporal_coverage_start' not in p
                and 'temporal_coverage_end' not in p, 'Köppen static schema convention changed')
        require('V1' in p['product_version'] and '2023 V2' in p['description'], 'Köppen version distinction lost')
        if 'future' in p['name']:
            require('RCP8.5' in p['description'] and '2071–2100' in p['description'], 'V1 future scenario lost')
        require('missing_value_sentinel' not in pv, 'Köppen background sentinel was guessed')
    results.append('PASS Köppen–Geiger: all twelve TIFF headers, thirty-class legend, three exact grids and distinct V1 periods/scenario')

    cdl=bundles['cropscape'];p=cdl['products'][0];g=cdl['grid_definitions'][0]
    require(p['temporal_coverage_start']=='2008-01-01' and p['temporal_coverage_end']=='2023-12-31'
            and p['native_spatial_resolution_m']==30 and g['resolution_x']==g['resolution_y']==30,
            'Historical native 30 m CDL was confused with post-2023 native 10 m data')
    require(g['crs']=='EPSG:5070' and g['srid']==5070 and '+datum=NAD83' in g['proj_string'],
            'CDL NAD83 grid was confused with NLCD WGS84 Albers')
    require('origin_x' not in g and 'n_rows' not in g and 'n_columns' not in g,
            'Regional CDL subset geometry was promoted to national geometry')
    require(cdl['product_variables'][0].get('missing_value_sentinel')=='0', 'CDL native background policy lost')
    results.append('PASS CropScape: national-coverage/native-resolution distinction, NAD83 grid and categorical background semantics')

    modis=bundles['modis'];cvs=index(modis,'canonical_variables');products=index(modis,'products');grids=index(modis,'grid_definitions')
    require(modis['data_sources'][0]['requires_authentication'] is True, 'MODIS protected-data access condition lost')
    policy=json.loads((ROOT/'catalog_gap_resolution/policy.json').read_text())
    require(len(products)==(7 if policy['mod13a2'] else 6)
            and len(modis['product_variables'])==(64 if policy['mod13a2'] else 52), 'Verified MODIS product/band scope changed')
    native=json.loads((SPECS/'evidence/additional_native_headers.json').read_text())
    files={Path(x['file']).name.split('.')[0]:x for x in native}
    if policy['mod13a2']:
        mp=next(p for p in products.values() if p['name']=='mod13a2_061_1km')
        require(mp['temporal_resolution']=='multi_day' and mp['temporal_resolution_iso']=='P16D',
                'MOD13A2 must retain its 16-day composite support')
    require(len(files['MOD13A2']['subdatasets'])==12, 'Blocked MOD13A2 native evidence was discarded')
    for pv in modis['product_variables']:
        p=products[pv['product']];code=p['name'].split('_061_')[0].upper()
        band=next(x for x in files[code]['subdatasets'] if x['native_name']==pv['native_name'])
        attrs=band['metadata'];cv=cvs[pv['canonical_variable']]
        if attrs.get('_FillValue') not in {None,'NA','N/A'}:
            require(pv.get('missing_value_sentinel')==attrs['_FillValue'], 'MODIS fill differs from native SDS')
        else:
            require('missing_value_sentinel' not in pv, 'Synthetic GDAL nodata was substituted for provider NA')
        if 'scale_factor' in attrs:
            require(pv.get('native_value_scale_factor')==float(attrs['scale_factor'])
                    and pv.get('native_value_offset')==float(attrs.get('add_offset','0')),
                    'MODIS native scale/offset differs from header')
        if code=='MOD06_L2':
            require(p['spatial_support_type']=='other' and 'grid_definition' not in p
                    and p['temporal_resolution']=='subhourly' and p['temporal_resolution_iso']=='PT5M',
                    'Irregular cloud swath was treated as a regular raster or daily mean')
            require(pv.get('unit_conversion_formula')=='physical = native_value_scale_factor * (raw - native_value_offset)',
                    'MOD06 subtract-before-scale decoder was replaced with CF/GDAL affine convention')
        else:
            g=grids[p['grid_definition']]
            require(math.isclose(g['resolution_x'],band['geo_transform'][1],abs_tol=1e-4),
                    'MODIS native resolution group assigned to the wrong grid')
            require(g['crs']=='AMADEUS:100002' and 'srid' not in g, 'Unverified MODIS EPSG/SRID')
        if pv['native_name'] in {'AOD_QA','QC_Day','QC_Night','state_1km_1','QC_500m_1','gflags_1'}:
            require(cv['value_data_type']=='categorical' and cv['extensivity']=='categorical'
                    and 'aggregation_method' not in pv, 'QA bits were treated as continuous measurements')
        if pv['native_name']=='FineModeFraction':
            require('native_value_scale_factor' not in pv and pv.get('metadata_gaps')==['under_investigation'],
                    'Unverified fine-mode fraction multiplier supplied')
        if pv['native_name'].startswith('Optical_Depth'):
            require(cv['units_ucum']=='1' and 'pm25' not in cv['name'], 'Aerosol optical depth was equated to ground PM2.5')
    results.append('PASS MODIS: 64 native bands, resolution profiles, product-specific inverse/affine cloud decoders and 16-day support')
    current_review=SPECS/'catalog_updates_20261109'
    if (current_review/'active.json').exists():
        results.append(runpy.run_path(str(current_review/'verify.py'))['check'](bundles))
    return results


def semantic_checks() -> list[str]:
    results = []
    bundles = {dataset: load(SPECS / f"{dataset}.yaml") for dataset in DATASETS}
    global_catalog = load(HERE / "global_catalog.yaml")
    schema_documents = [load(p) for p in sorted(SCHEMA.parent.glob("*.yaml"))]
    classes = {k: v for s in schema_documents for k, v in (s.get("classes") or {}).items()}
    slots = {k: v for s in schema_documents for k, v in (s.get("slots") or {}).items()}
    containers = {
        "DataSource": "data_sources", "Product": "products", "GridDefinition": "grid_definitions",
        "CanonicalVariable": "canonical_variables", "ProductVariable": "product_variables",
        "OmopConceptBinding": "omop_concept_binding", "Asset": "assets",
    }
    for dataset, bundle in list(bundles.items()) + [("global_catalog", global_catalog)]:
        by_collection = {name: index(bundle, name) for name in CATALOG_COLLECTIONS}
        ids = [key for records in by_collection.values() for key in records]
        require(len(ids) == len(set(ids)), f"Cross-class duplicate IDs: {dataset}")
        for class_name, collection in containers.items():
            if collection not in by_collection:
                continue
            for row in by_collection[collection].values():
                for field, value in row.items():
                    target = slots.get(field, {}).get("range")
                    if target not in containers or target == "OmopConceptBinding":
                        continue
                    target_rows = index(bundle, containers[target])
                    for ref in value if isinstance(value, list) else [value]:
                        require(ref in target_rows, f"Unresolved {dataset}: {field} -> {ref}")
                require(not row.get("omop_concept_binding"), "No verified OMOP bindings are supplied")
        for row in by_collection["canonical_variables"].values():
            require(row.get("default_aggregation_method") != "other",
                    "An unspecified algorithm must not become an executable default")
            if row["value_data_type"] == "categorical":
                require(row["extensivity"] == "categorical", "Categorical extensivity mismatch")
                require("plausible_min" not in row and "plausible_max" not in row,
                        "Categorical code bounds must not become physical plausibility limits")
        for row in by_collection["products"].values():
            for field, wrapper, source_file in (
                ("amadeus_dataset_name", "download_data", "download.R"),
                ("amadeus_process_covariate", "process_covariates", "process.R"),
                ("amadeus_calculate_covariate", "calculate_covariates", "calculate_covariates.R"),
            ):
                if field not in row:
                    continue
                text = (ROOT / "R" / source_file).read_text()
                require(bool(re.search(r"\b" + re.escape(row[field]) + r"\s*=", text)),
                        f"Unknown {wrapper} dispatch: {row[field]}")
        results.append(f"PASS {dataset}: unique IDs, closed references, categorical constraints, dispatch declarations")
    for collection in CATALOG_COLLECTIONS:
        union = {}
        for bundle in bundles.values():
            for key, row in index(bundle, collection).items():
                require(key not in union or union[key] == row, f"Conflicting identity: {key}")
                union[key] = row
        require(index(global_catalog, collection) == union, f"Global union differs: {collection}")
    for dataset, spec in bundles.items():
        handle = load(HERE / f"{dataset}.yaml")
        expected = {k: v for k, v in spec.items()
                    if k in ("schema_version", "data_sources", "products", "grid_definitions")}
        require(handle == expected, f"Handle/spec drift: {dataset}")
    results.append(f"PASS global union and all {len(bundles)} discovery projections match their source specs exactly")

    gridmet = bundles["gridmet"]
    grid = gridmet["grid_definitions"][0]
    require(grid["crs"] == "EPSG:4326" and grid["n_columns"] == 1386 and grid["n_rows"] == 585,
            "gridMET grid identity/dimensions mismatch")
    axes = {a.get("name"): a for a in ET.parse(SPECS / "evidence/gridmet_tmmx.xml").getroot().findall("axis")}
    x, y = axes["lon"].find("values"), axes["lat"].find("values")
    require(math.isclose(grid["origin_x"], float(x.get("start")) - grid["resolution_x"] / 2),
            "gridMET cell-centre/corner convention mismatch")
    require(math.isclose(grid["origin_y"], float(y.get("start")) + grid["resolution_y"] / 2),
            "gridMET upper-left corner mismatch")
    daily_vars=[v for v in gridmet['product_variables'] if v['id'].startswith('amadeus:pvar/gridmet.')]
    for pv in daily_vars:
        code = pv["amadeus_variable_code"]
        node = ET.parse(SPECS / f"evidence/gridmet_{code}.xml").getroot().find(".//grid")
        attrs = {a.get("name"): a.get("value") for a in node.findall("attribute")}
        require(pv["native_name"] == node.get("name"), f"Incorrect netCDF variable: {code}")
        require(pv["missing_value_sentinel"] == attrs["_FillValue"], f"Incorrect fill: {code}")
        require(pv["native_value_scale_factor"] == float(attrs["scale_factor"]), f"Incorrect scale: {code}")
        require(pv["native_value_offset"] == float(attrs["add_offset"]), f"Incorrect offset: {code}")
        require(pv["cf_standard_name"] == attrs["standard_name"], f"Raw CF attribute changed: {code}")
        require("unsigned" in pv["harmonization_note"], f"Unsigned packing qualification missing: {code}")
    require("Mountain Standard Time" in gridmet["products"][0]["description"], "Missing fixed-MST qualification")
    direction = next(c for c in gridmet["canonical_variables"] if "direction" in c["name"])
    require("default_aggregation_method" not in direction, "Directions must not have an unverified aggregation default")
    for pv in daily_vars:
        code = pv["amadeus_variable_code"]
        cv = index(gridmet, "canonical_variables")[pv["canonical_variable"]]
        if code in {"bi", "erc", "fm100", "fm1000"}:
            require("aggregation_method" not in pv and "aggregation_window_iso" not in pv,
                    f"Unverified source statistic/window assigned to {code}")
            require("default_aggregation_method" not in cv, f"Unverified analysis default assigned to {code}")
        elif code == "th":
            require(pv.get("aggregation_method") == "mean", "Preserve the provider-labelled daily mean direction")
        elif code in {"pr", "pet", "etr"}:
            require(cv["extensivity"] == "intensive" and pv.get("extensivity", "intensive") == "intensive",
                    "Precipitation/ET depths must not be relabelled spatially extensive to satisfy an import check")
            require(pv["aggregation_method"] == "sum" and pv["aggregation_window_iso"] == "P1D",
                    "Temporal accumulated-depth semantics were lost")
    require(len(daily_vars) == 16, "Daily gridMET profile changed scope")
    results.append("PASS gridMET: provider attributes, 16 daily fields, centre/corner geometry, fixed MST and circular wind semantics")
    sys.path.insert(0,str(ROOT/'catalog_gap_resolution'))
    from verify_native import check_drought, check_decoders
    policy=json.loads((ROOT/'catalog_gap_resolution/policy.json').read_text())
    if policy['gridmet_drought']:
        results.append(check_drought(gridmet))
    else:
        require(len(gridmet['product_variables'])==16, 'Disabled drought profiles remain in the catalog')
    results.append(check_decoders(bundles['modis']))

    aqs = bundles["aqs"]
    require(aqs["data_sources"][0]["requires_authentication"] is False, "Bulk AQS is public")
    require(len(aqs["products"]) == 7 and len(aqs["canonical_variables"]) == 6, "AQS parameter scope changed")
    for product in aqs["products"]:
        require("crs" not in product and "srid" not in product, "AQS Datum is row-specific")
        require("amadeus_calculate_covariate" not in product, "AQS has no calculate dispatch")
        require("default_data_status" not in product, "No archive-wide certification assertion is justified")
        require(product.get("update_cadence_iso") == "P6M", "AQS bulk publication cadence must remain distinct from daily support")
        require(all(word in product["description"] for word in ("Sample Duration", "Pollutant Standard", "POC")),
                "AQS row selection qualifiers missing")
    results.append("PASS AQS: public bulk route, seven parameters, shared PM2.5 identity and explicit row-selection caveats")

    hms = bundles["hms"]
    require(hms["canonical_variables"][0]["value_data_type"] == "categorical", "HMS density is a class")
    require(hms["product_variables"][0]["native_units_ucum"] == "1", "HMS density is not surface ug/m3")
    require(hms["products"][0]["spatial_support_type"] == "polygon", "HMS smoke is not a raster")
    require("Start/End" in hms["products"][0]["description"], "HMS interval qualification missing")
    require(set(hms["products"][0]["data_genre"]) == {"wildfire_smoke", "air_pollution"},
            "HMS source themes were not reconciled")
    results.append("PASS HMS: polygon/class semantics and feature-time qualifications")

    nlcd = bundles["nlcd"]
    ng = nlcd["grid_definitions"][0]
    require(ng["crs"] == "AMADEUS:100001" and "srid" not in ng, "Do not invent an EPSG/database SRID")
    require("+datum=WGS84" in ng["proj_string"], "Annual NLCD native datum must remain WGS84")
    require(ng["resolution_x"] == ng["resolution_y"] == 30, "Annual NLCD grid spacing mismatch")
    source = ET.parse(SPECS / "evidence/nlcd_native_metadata.xml").getroot()
    require(ng["n_rows"] == int(source.findtext(".//rastinfo/rowcount")), "NLCD row count mismatch")
    require(ng["n_columns"] == int(source.findtext(".//rastinfo/colcount")), "NLCD column count mismatch")
    require(len(nlcd["products"]) == 6, "Annual NLCD profile scope changed")
    expected_fill = {"LndCov": "250", "LndChg": "9999", "LndCnf": "250",
                     "FctImp": "250", "ImpDsc": "250", "SpcChg": "9999"}
    for pv in nlcd["product_variables"]:
        require(pv["missing_value_sentinel"] == expected_fill[pv["native_name"]], "Annual/Legacy nodata confusion")
    for product in nlcd["products"]:
        require(product["crs"] == ng["crs"] and "srid" not in product, "Incorrect NLCD CRS inheritance")
        require("C1V1" in product["product_version"], "NLCD version pin lost")
    results.append("PASS NLCD: six C1V1 products, verified native projection, local CRS registration and product-specific nodata")
    results.extend(additional_semantic_checks(bundles))

    manifest = json.loads((SPECS / "source_manifest.json").read_text())
    require(manifest["schema"] == str(SCHEMA.relative_to(ROOT)), "Manifest authoritative baseline differs from validator")
    require(manifest["validation_policy"]["schema"] == manifest["schema"], "Validation policy baseline drift")
    for relative, expected in manifest["source_files_sha256"].items():
        require(digest(ROOT / relative) == expected, f"Source/schema changed since catalog verification: {relative}")
    for relative, expected in manifest["evidence_files_sha256"].items():
        require(digest(SPECS / relative) == expected, f"Provider evidence changed: {relative}")
    for relative, expected in manifest.get('gap_resolution',{}).get('implementation_sha256',{}).items():
        require(digest(ROOT / relative)==expected, f'Gap-resolution implementation changed: {relative}')
    results.append("PASS input schemas/workbooks/documentation and provider-evidence checksums")

    audit_path = SPECS / manifest["resource_reconciliation"]["report"]
    require(digest(audit_path) == manifest["resource_reconciliation"]["sha256"], "Resource audit checksum mismatch")
    audit = json.loads(audit_path.read_text())
    require(audit["authoritative_schema"] == manifest["schema"], "Resource audit baseline drift")
    source_cells = workbook_cells(ROOT / "environmental_datasets_table.xlsx")
    mapping_cells = workbook_cells(ROOT / "catalog_schema_excel_mapping_with_unmapped.xlsx")
    for trace in audit["field_trace"]:
        collection = containers[trace["target_class"]]
        require(trace["field"] in classes[trace["target_class"]]["slots"], "Trace points to an unattached field")
        records = index(bundles[trace["dataset"]], collection)
        for target in trace["objects"]:
            row = records[target["id"]]
            require(row.get(trace["field"]) == target["value"], "Resource trace value drift")
            require((trace["field"] in row) == target["field_present"], "Resource trace presence drift")
        for source in trace["excel_evidence"]:
            require(source_cells[source["sheet"]].get(source["cell"], "") == source["value"],
                    "Resource trace changed an Excel value")
        for mapping in trace["mapping_workbook_rows"]:
            row = str(mapping["row"])
            require(mapping_cells[mapping["sheet"]].get("B" + row) == mapping["item"],
                    "Mapping-workbook item reference mismatch")
            require(mapping_cells[mapping["sheet"]].get("C" + row) == mapping["status"],
                    "Mapping-workbook status reference mismatch")
    for record in audit["core_excel_cell_dispositions"]:
        require(source_cells[record["sheet"]].get(record["cell"], "") == record["value"],
                "Core Excel-cell disposition changed the source value")
    defined = {(p.name, kind, name)
               for p in sorted(SCHEMA.parent.glob("*.yaml"))
               for section, kind in (("classes", "Class"), ("enums", "Enum"), ("slots", "Slot"))
               for name in (load(p).get(section) or {})}
    traced = {(r["schema_file"], r["item_type"], r["item_name"])
              for r in audit["mapping_definitions"]}
    require(defined == traced and len(traced) == 286, "Resource audit misses a schema definition")
    require(len(audit["proposed_definition_dispositions"]) == 46 and audit["new_schema_definitions"] == 0,
            "Proposed versus implemented definitions were conflated")
    require(len(audit["hms_programme_contributions"]) == 21, "HMS programme table is incomplete")
    for history in audit["hms_programme_contributions"]:
        require(history["system_label"] in hms["data_sources"][0]["description"], "HMS history was not retained")
        if history["end_status"].startswith("not asserted"):
            require(history["end"] is None, "In use/chart endpoint became a retirement date")
    require(any(r["sheet"] == "Amadeus data sources" and r["row"] == 20
                for r in manifest["workbook_rows"]), "AQS bulk source row was omitted")
    results.append("PASS three-resource audit: actual Excel values, mapping rows/statuses, target fields, all definitions and scoped HMS history")
    sys.path.insert(0,str(ROOT/'catalog_five_dataset_extension'))
    from check_profiles import check
    try:
        geos_review = SPECS/'geos_review_20261109'
        if (geos_review/'active.json').exists():
            # Legacy checks asserted unknown GEOS packing. Keep those checks on
            # their immutable baseline, and validate current GEOS against native
            # evidence separately. The other four families remain current.
            import tarfile
            legacy_bundles = dict(bundles)
            with tarfile.open(geos_review/'before.tar.gz') as archive:
                legacy_bundles['geos'] = yaml.safe_load(archive.extractfile('dataset_spec_objects/geos.yaml').read())
            check(legacy_bundles)
            results.append('PASS existing MERRA2/population/GROADS/HUC checks; obsolete GEOS unknown-header assertions scoped to preserved baseline')
            results.append(runpy.run_path(str(geos_review/'verify.py'))['check'](bundles))
        else:
            results.append(check(bundles))
    except AssertionError as exc:
        raise ValueError('Five-dataset evidence check failed: '+str(exc)) from exc
    sys.path.insert(0,str(SPECS/'integration_20261107'))
    from verify_profiles import check as check_latest
    try:
        revalidation = json.loads((SPECS/'inventory_extension_20261107/workbook_revalidation.json').read_text())
        require(revalidation['path']=='environmental_datasets_table.xlsx', 'Unexpected reviewed input override')
        require(digest(ROOT/revalidation['path'])==revalidation['reviewed_sha256'], 'Reviewed workbook hash drift')
        reviewed_inputs={revalidation['path']:revalidation['reviewed_sha256']}
        if (SPECS/'geos_review_20261109/active.json').exists():
            current_review = json.loads((SPECS/'geos_review_20261109/before.json').read_text())
            reviewed_inputs['amadeus_schema/.git/config'] = current_review['readonly_files']['amadeus_schema/.git/config']
        results.append(check_latest(bundles,reviewed_inputs=reviewed_inputs))
    except AssertionError as exc:
        raise ValueError('EDGAR/NARR/PRISM/drought/ecoregion evidence check failed: '+str(exc)) from exc
    inventory_check = runpy.run_path(str(SPECS/'inventory_extension_20261107/verify_profiles.py'))['check']
    try:
        results.append(inventory_check(bundles))
    except AssertionError as exc:
        raise ValueError('NEI/TRI/GMTED/OLM/IMPROVE evidence check failed: '+str(exc)) from exc
    return results


def r_checks() -> dict:
    """Check the actual R objects and interpret the custom projection with PROJ."""
    rscript = shutil.which("Rscript")
    require(rscript is not None, "Rscript is needed to verify the supplied RDS objects")
    code = r'''
checks <- list()
read_catalog_yaml <- function(path) yaml::read_yaml(path)
if (file.exists('dataset_spec_objects/catalog_updates_20261109/active.json')) {
  source('dataset_spec_objects/catalog_updates_20261109/read_catalog.R')
}
for (dataset in c('gridmet', 'aqs', 'hms', 'nlcd', 'terraclimate', 'koppen_geiger', 'cropscape', 'modis', 'merra2', 'population', 'groads', 'huc', 'geos', 'edgar', 'narr', 'prism', 'drought', 'ecoregion', 'nei', 'tri', 'gmted', 'olm', 'improve')) {
  expected <- read_catalog_yaml(file.path('dataset_spec_objects', paste0(dataset,'.yaml')))
  actual <- readRDS(file.path('dataset_spec_objects', paste0(dataset,'.rds')))
  stopifnot(identical(expected, actual))
  for (asset in actual$assets) {
    if (!is.null(asset$size_bytes)) {
      stopifnot(is.finite(asset$size_bytes), asset$size_bytes == floor(asset$size_bytes))
    }
  }
  checks[[paste0(dataset,'_rds')]] <- 'PASS: RDS equals YAML-loaded named list'
}
grid <- readRDS('dataset_spec_objects/nlcd.rds')$grid_definitions[[1]]
crs <- sf::st_crs(grid$proj_string)
stopifnot(!is.na(crs), is.na(crs$epsg))
xy <- sf::st_coordinates(sf::st_transform(
  sf::st_sfc(sf::st_point(c(-96,23)),crs=4326),crs))
stopifnot(max(abs(xy)) < 1e-5)
checks$nlcd_projection <- 'PASS: exact WGS84 Albers parses; (-96,23) maps to (0,0); no EPSG match asserted'
grid <- readRDS('dataset_spec_objects/modis.rds')$grid_definitions[[1]]
crs <- sf::st_crs(grid$proj_string)
stopifnot(!is.na(crs), is.na(crs$epsg))
xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(0,0)),crs=4326),crs))
stopifnot(max(abs(xy)) < 1e-5)
checks$modis_projection <- 'PASS: native spherical sinusoidal parses; origin maps to (0,0); no EPSG/SRID assumed'
packages <- list(R=as.character(getRversion()),yaml=as.character(utils::packageVersion('yaml')),
                 sf=as.character(utils::packageVersion('sf')))
cat(jsonlite::toJSON(list(checks=checks,packages=packages),auto_unbox=TRUE))
'''
    proc = subprocess.run([rscript, "-"], input=code, cwd=ROOT, text=True, capture_output=True)
    require(proc.returncode == 0, f"R object/projection verification failed: {proc.stderr}")
    return json.loads(proc.stdout)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--report", type=Path, help="Write a JSON validation report")
    args = parser.parse_args()
    executable = Path(sys.executable).parent / "linkml-validate"
    if not executable.exists():
        found = shutil.which("linkml-validate")
        require(found is not None, "Run using an environment containing LinkML")
        executable = Path(found)
    files = sorted(SPECS.glob("*.yaml")) + sorted(HERE.glob("*.yaml"))
    results = []
    for path in files:
        command = [str(executable), "-s", str(SCHEMA), "-C", "AmadeusDatabase", str(path)]
        proc = subprocess.run(command, capture_output=True, text=True)
        output = (proc.stdout + proc.stderr).strip()
        passed = proc.returncode == 0 and "ERROR" not in output and "Traceback" not in output
        results.append({"file": str(path.relative_to(ROOT)), "passed": passed,
                        "sha256": digest(path), "validator_output": output})
        print(f'{"PASS" if passed else "FAIL"} {path.relative_to(ROOT)}', flush=True)
    errors = []
    r_results = {}
    upstream = {}
    crs_policy = {}
    try:
        checks = semantic_checks()
        global_catalog = load(HERE / "global_catalog.yaml")
        upstream = import_coherence(global_catalog)
        require(not upstream["unresolved_errors"], f"Unresolved authoritative import errors: {upstream['unresolved_errors']}")
        checks.append("PASS authoritative import review: raw checker output retained; only metadata-backed temporal/spatial conflicts resolved")
        crs_policy = crs_requirements(global_catalog)
        checks.append("PASS explicit CRS requirements: local identifiers cannot trigger a guessed numeric SRID")
        r_results = r_checks()
    except (ValueError, KeyError, OSError) as exc:
        checks = []
        errors.append(str(exc))
    for message in checks + errors:
        print(message, flush=True)
    passed = all(row["passed"] for row in results) and not errors
    report = {
        "validated_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "schema": str(SCHEMA.relative_to(ROOT)), "target_class": "AmadeusDatabase",
        "package_versions": {name: importlib.metadata.version(name)
                             for name in ("linkml", "linkml-runtime", "PyYAML")},
        "linkml_results": results, "semantic_checks": checks, "semantic_errors": errors,
        "r_object_and_projection_checks": r_results,
        "authoritative_import_coherence": upstream,
        "crs_execution_requirements": crs_policy,
        "passed": passed,
        "scope": "Metadata schema, references and stated scientific checks. No runtime data access or database ingestion is certified.",
        "open_verification_items": "../dataset_spec_objects/verification_gaps.json",
        "database_backend_execution_tested": False,
    }
    if args.report:
        args.report.write_text(json.dumps(report, indent=2) + "\n")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
