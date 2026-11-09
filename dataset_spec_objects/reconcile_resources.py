#!/usr/bin/env python3
"""Reconcile the three review resources with the four existing catalog specs.

Uses PyYAML plus the standard library. Reads the workbook crosswalk; never
promotes a proposed definition into the existing schema. No network requests.
"""
from __future__ import annotations
import datetime as dt
import hashlib
import json
from pathlib import Path
import re
import subprocess
import xml.etree.ElementTree as ET
import zipfile
import yaml

ROOT=Path(__file__).resolve().parents[1]
SPECS=ROOT/'dataset_spec_objects'
BUNDLES=ROOT/'validated_catalog_handles'
WORKBOOK=ROOT/'environmental_datasets_table.xlsx'
MAPPING=ROOT/'catalog_schema_excel_mapping_with_unmapped.xlsx'
DOC=ROOT/'catalog_dataset_integration.md'
SCHEMA_DIR=ROOT/'amadeus_schema_archived/src/amadeus_schema/schema'
DATASETS=('gridmet','aqs','hms','nlcd')
STAMP=dt.datetime.now(dt.timezone.utc).isoformat()
NS={'m':'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
protected={str(p.relative_to(ROOT)):sha(p) for p in [WORKBOOK,MAPPING,DOC,*SCHEMA_DIR.glob('*.yaml')]}

def read_workbook(path):
    result={}
    with zipfile.ZipFile(path) as z:
        strings=[''.join(x.itertext()) for x in ET.fromstring(z.read('xl/sharedStrings.xml')).findall('m:si',NS)] if 'xl/sharedStrings.xml' in z.namelist() else []
        rels={r.get('Id'):r.get('Target') for r in ET.fromstring(z.read('xl/_rels/workbook.xml.rels'))}
        def value(c):
            v=c.find('m:v',NS);i=c.find('m:is',NS)
            return ''.join(i.itertext()) if i is not None else strings[int(v.text)] if c.get('t')=='s' and v is not None else v.text if v is not None else ''
        for sh in ET.fromstring(z.read('xl/workbook.xml')).findall('m:sheets/m:sheet',NS):
            target=rels[sh.get('{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id')]
            part=target.lstrip('/') if target.startswith('/') else 'xl/'+target
            rows={int(row.get('r')):{re.sub(r'\d+','',c.get('r')):value(c) for c in row if value(c)} for row in ET.fromstring(z.read(part)).findall('m:sheetData/m:row',NS)}
            result[sh.get('name')]={'visibility':sh.get('state','visible'),'rows':rows}
        prop=ET.fromstring(z.read('xl/workbook.xml')).find('m:workbookPr',NS)
        date1904=prop is not None and prop.get('date1904') in ['1','true']
    return result,date1904

wb,date1904=read_workbook(WORKBOOK)
mp,_=read_workbook(MAPPING)
doc=DOC.read_text()
assert '## Remaining gaps in schema coverage and recommended placement' in doc
schemas={p.name:yaml.safe_load(p.read_text()) for p in SCHEMA_DIR.glob('*.yaml')}
classes={n:v for s in schemas.values() for n,v in (s.get('classes') or {}).items()}
slots={n:v for s in schemas.values() for n,v in (s.get('slots') or {}).items()}
enums={n:v for s in schemas.values() for n,v in (s.get('enums') or {}).items()}
existing=[];proposals={};mapping_rows=[]
for sheet,content in mp.items():
    for row,c in content['rows'].items():
        if c.get('C') not in ['Existing','Proposed']:continue
        entry={'worksheet':sheet,'row':row,'item_type':c['A'],'item_name':c['B'],'status':c['C'],'excel_sheet':c.get('D'),'excel_column':c.get('E'),'parent_class':c.get('F',''),'parent_enum':c.get('G',''),'relationship_type':c.get('H','')}
        if entry['status']=='Existing':
            section={'Class':'classes','Enum':'enums','Slot':'slots'}[entry['item_type']]
            assert entry['item_name'] in (schemas[sheet].get(section) or {}),(sheet,entry)
            existing.append((sheet,entry['item_type'],entry['item_name']))
        else:proposals.setdefault((entry['item_type'],entry['item_name']),[]).append(entry)
        mapping_rows.append(entry)
assert len(set(existing))==286 and len(proposals)==46
assert not any((e.get('excel_sheet') or '').startswith('Copy of') for e in mapping_rows)

def mapping_evidence(item,sheet=None,col=None,status='Existing'):
    entries=[e for e in mapping_rows if e['item_name']==item and e['status']==status]
    if sheet:entries=[e for e in entries if e['excel_sheet']==sheet]
    if col:entries=[e for e in entries if re.search(r'\('+re.escape(col)+r'; header',e.get('excel_column') or '')]
    return [{'sheet':e['worksheet'],'row':e['row'],'status':e['status'],'item':item} for e in entries]

def cell(sheet,addr):
    m=re.fullmatch(r'([A-Z]+)(\d+)',addr);return wb[sheet]['rows'][int(m[2])].get(m[1],'')
def ref(sheet,addr):return {'sheet':sheet,'cell':addr,'value':cell(sheet,addr)}
def source_row(sheet,row):return {'sheet':sheet,'row':row,'cells':{f'{col}{row}':v for col,v in wb[sheet]['rows'][row].items()}}
def append_block(text,tag,addition):
    marker=f' [Resource reconciliation: {tag}] '
    text=text.split(marker,1)[0]
    return text+marker+addition

specs={k:yaml.safe_load((SPECS/f'{k}.yaml').read_text()) for k in DATASETS}
config={
 'gridmet':{'ads':4,'eds':[52,79],'asp':[4],'asr':[3]},
 'aqs':{'ads':20,'eds':[2],'asp':[130],'asr':[116]},
 'hms':{'ads':15,'eds':[103],'asp':[126],'asr':[105]},
 'nlcd':{'ads':6,'eds':[13],'asp':[7,8],'asr':[11,12]},
}
# Capture source metadata at the publishing-programme scope. These remain
# attributed inventory assertions, not inferred SPDX identifiers/release dates.
for key,b in specs.items():
    c=config[key];r=c['ads'];s=b['data_sources'][0]
    terms=cell('Amadeus data sources',f'I{r}')
    status=cell('Amadeus data sources',f'H{r}')
    launch=cell('Amadeus data sources',f'J{r}')
    scope={
      'gridmet':'The inventory lists 2013, which also identifies the cited methods publication; an exact first data-release date is not established.',
      'aqs':'The inventory explicitly labels 1980 as record start; this is not a release date. The broader inventory has an approximate 1970s/1980s database-history statement.',
      'hms':'Programme operation from 2003 and smoke-polygon availability from August 2005 describe different scopes.',
      'nlcd':'The source row describes the Annual series launch in 2024, while the broader inventory and bibliography also contain Legacy NLCD. Neither the Legacy dates nor Legacy DOIs identify Annual C1V1.'
    }[key]
    text=f'Inventory usage/citation statement (Amadeus data sources!I{r}): {terms}. Inventory source status (H{r}): {status}; this is not observation QA, API availability or verified support for every product. Inventory first-release/history wording (J{r}): {launch}. {scope}'
    s['description']=append_block(s.get('description',''),'inventory rights and history',text)
    for p in b['products']:
        # Keep source completeness distinguishable from extent and modelled values.
        coverage=cell('Amadeus data sources',f'N{r}')
        completeness=f'Inventory coverage statement (Amadeus data sources!N{r}): {coverage}. This statement does not establish validity of every cell/date or fill observation gaps.'
        p['description']=append_block(p.get('description',''),'inventory completeness',completeness)

# Bibliography matching is explicit, never by shared row numbers across blocks.
aqs=specs['aqs']
aqs['data_sources'][0]['citation_apa']='U.S. Environmental Protection Agency. (2023). Air Quality System Data Mart [Internet database]. https://www.epa.gov/outdoor-air-quality-data'
for p in aqs['products']:
    p['update_cadence_iso']='P6M'
    p['description']=append_block(p['description'],'bulk publication cadence','Nominal twice-yearly bulk-file refresh (Amadeus data sources!K20), corroborated by the EPA AirData download page. P6M describes nominal publication frequency, not exact release dates or daily observation support. EPA page sections differ on month labels (June/December versus late May/November); use actual file modification metadata for availability. Environmental Datasets!G2 describes the broader database update stream and is not the bulk-file schedule.')
# The main inventory classifies smoke under Air Quality while the source row
# supplies the more specific Wildfire Smoke theme; both already have enum values.
hp=specs['hms']['products'][0]
hp['data_genre']=list(dict.fromkeys(hp['data_genre']+['air_pollution']))

# The contribution table is programme-wide (including years before smoke files).
# Do not force every platform/date into a smoke-only Product or new slot.
epoch=dt.datetime(1904,1,1) if date1904 else dt.datetime(1899,12,30)
def excel_date(value):return (epoch+dt.timedelta(days=float(value))).date().isoformat()
def text_date(value):return dt.datetime.strptime(value,'%Y-%b-%d').date().isoformat()
history=[]
for row in range(5,26):
    raw=wb['NOAA Hazard Mapping System']['rows'][row]
    start=excel_date(raw['C']);assert start==text_date(raw['E'])
    active=raw['D'].strip().lower()=='in use'
    end=None if active else excel_date(raw['D'])
    if end:assert end==text_date(raw['F'])
    history.append({'source':{'sheet':'NOAA Hazard Mapping System','row':row,'cells':{f'{col}{row}':raw.get(col,'') for col in 'BCDEF'}},'system_label':raw['B'],'start':start,'end':end,'end_status':'not asserted; source says In use' if active else 'source end date','scope':'HMS programme contribution; not independently verified as smoke-polygon participation','ignored_chart_endpoint':raw['F'] if active else None})
hs=specs['hms']['data_sources'][0]
history_text='Workbook-asserted HMS programme contribution table (NOAA Hazard Mapping System!B5:F25), including fire and other programme inputs; these are not verified start/end dates for the smoke-only product. '+'; '.join(f'{h["system_label"]}: {h["start"]} to '+(h['end'] or 'end not asserted (In use)') for h in history)+'. Chart date 46324 is not a retirement date. Trailing digits in source system labels are preserved without interpreting possible footnote markers.'
hs['description']=append_block(hs['description'],'programme observing-system history',history_text)
hp['description']=append_block(hp['description'],'history scope','The publishing DataSource retains the workbook programme contribution history. Do not treat that programme table as proof that each listed instrument contributed smoke polygons throughout those dates.')

# Source trace: record actual cells, crosswalk row, existing schema field,
# transformation/disposition, and the output object IDs and values.
trace=[]
def trace_field(dataset,collection,field,sources,decision,section,mapping_item=None,status='Existing',only_ids=None):
    rows=specs[dataset].get(collection,[])
    if only_ids:rows=[r for r in rows if r['id'] in only_ids]
    target_class={'data_sources':'DataSource','products':'Product','grid_definitions':'GridDefinition','product_variables':'ProductVariable','canonical_variables':'CanonicalVariable'}[collection]
    assert field in classes[target_class]['slots'],(target_class,field)
    evidence=[]
    for source in sources:
        col=re.sub(r'\d+','',source['cell'])
        evidence+=mapping_evidence(mapping_item or field,source['sheet'],col,status)
    if not evidence:evidence=mapping_evidence(mapping_item or field,status=status)
    evidence=list({(e['sheet'],e['row']):e for e in evidence}.values())
    trace.append({'dataset':dataset,'target_class':target_class,'field':field,'objects':[{'id':r['id'],'value':r.get(field),'field_present':field in r} for r in rows],'excel_evidence':sources,'mapping_workbook_rows':evidence,'integration_document_section':section,'decision':decision})

for key,c in config.items():
    row=c['ads'];s='Amadeus data sources';eds=c['eds'][0]
    trace_field(key,'data_sources','label',[ref(s,f'B{row}')],'Retain stable source identity and readable label; do not use workbook row numbers as globally stable IDs.','Existing catalog classes')
    trace_field(key,'data_sources','producer_institution',[ref('Environmental Datasets',f'D{eds}')],'Normalize institution names; current primary provider metadata supersedes historical inventory attribution.','Combined catalog mapping for the future `.R` workflow')
    trace_field(key,'data_sources','homepage',[ref(s,f'G{row}'),ref(s,f'AA{row}')],'Distinguish homepage, methods DOI, documentation and data endpoint; not every URL is an access URL.','Import and extension decisions')
    trace_field(key,'data_sources','amadeus_function_suffix',[ref(s,f'C{row}'),ref(s,f'Z{row}')],'Inventory suffix corroborated by actual local R dispatch/function declarations. GitHub is used for implementation context, not native scientific units.','Combined catalog mapping for the future `.R` workflow')
    trace_field(key,'data_sources','citation_apa',[ref('Amadeus Supp Refs',f'B{r}') for r in c['asr']]+[ref(s,f'T{row}'),ref(s,f'U{row}')],'Use bibliography references only at matching source/product scope. AQS now reuses the explicit 2023 Data Mart reference; Legacy NLCD citations are excluded from Annual C1V1.','Import and extension decisions')
    trace_field(key,'data_sources','description',[ref(s,f'I{row}'),ref(s,f'J{row}'),ref(s,f'H{row}')],'Preserve exact inventory usage, release/history and status assertions with qualifiers in an existing scoped field; do not create unimplemented slots or convert record start into release date.','Remaining gaps in schema coverage and recommended placement',mapping_item='first_release_description',status='Proposed')
    trace_field(key,'products','data_genre',[ref('Environmental Datasets',f'A{r}') for r in c['eds']]+[ref(s,f'E{row}'),ref(s,f'S{row}'),ref(s,f'X{row}')],'Normalize to existing DataGenreEnum values; smoke retains both wildfire_smoke and air_pollution themes. No new thematic subclass or vocabulary member is needed for these scoped products.','New enums and extensions to existing enums')
    trace_field(key,'products','native_format',[ref(s,f'D{row}'),ref(s,f'Q{row}'),ref(s,f'W{row}')],'Normalize with provider metadata; generic netCDF does not establish a NetCDF version. KML is an alternative HMS delivery outside the present shapefile profile. ZIP is packaging.','Combined catalog mapping for the future `.R` workflow')
    trace_field(key,'products','spatial_support_type',[ref(s,f'L{row}'),ref(s,f'AB{row}'),ref('Environmental Datasets',f'H{eds}')],'Select product-specific support. HMS fire points and smoke polygons must not be conflated. A nominal cell size does not establish a native grid.','Existing catalog classes')
    trace_field(key,'products','spatial_extent_descriptor',[ref(s,f'F{row}'),ref(s,f'Y{row}')],'Refine family-wide extent to the actual product scope; Annual NLCD CONUS does not inherit all Legacy regions.','Combined catalog mapping for the future `.R` workflow')
    trace_field(key,'products','native_spatial_resolution_descriptor',[ref(s,f'L{row}'),ref('Environmental Datasets',f'H{eds}')],'Preserve resolution wording; exact axes and CRS require provider metadata. No fixed resolution is assigned to smoke polygons.','Combined catalog mapping for the future `.R` workflow')
    trace_field(key,'products','temporal_resolution',[ref(s,f'M{row}'),ref(s,f'AN{row}'),ref('Environmental Datasets',f'I{eds}')],'Differentiate native support, daily archive packaging, annual map labels and bulk publication cadence. No new unsupported enum values are emitted.','Integration of `amadeus_common.yaml`')
    trace_field(key,'products','update_cadence_iso',[ref(s,f'K{row}'),ref('Environmental Datasets',f'G{eds}')],'AQS bulk route now has nominal P6M, supported by its route-specific row and EPA. GridMET keeps P1D. Do not assign Legacy 2–3-year cadence to Annual NLCD or a mutable update schedule to a pinned release; HMS exact publication intervals remain qualified text.','Remaining gaps in schema coverage and recommended placement')
    trace_field(key,'products','description',[ref(s,f'N{row}'),ref('Environmental Datasets',f'J{eds}')],'Retain attributed completeness/sampling qualifications in existing text; a footprint does not prove completeness. Proposed spatial_coverage fields remain unimplemented.','Remaining gaps in schema coverage and recommended placement',mapping_item='spatial_coverage',status='Proposed')

trace_field('hms','data_sources','description',[ref('NOAA Hazard Mapping System',f'{col}{row}') for row in range(5,26) for col in 'BCDEF'],'Preserve 21 complete system/date associations at programme scope. Validate C/E and numeric D/text F dates; In use has no asserted end, and chart sentinel F=46324 is excluded from dates. Product-scoped scalar proposals are not forced onto a programme-wide table.','Information captured through additional slots on existing classes',mapping_item='observing_system_history',status='Proposed')

# Record every core cell's disposition, including flags and conflicting labels.
# These are evidence dispositions, not new catalog classes or provider records.
core_evidence=[]
for key,c in config.items():
    for sheet,rows in [('Environmental Datasets',c['eds']),('Amadeus data sources',[c['ads']]),('Amadeus Source and Product',c['asp']),('Amadeus Supp Refs',c['asr'])]:
        for row in rows:
            for col,value in wb[sheet]['rows'][row].items():
                # Other independent ASR blocks are not bibliographic attributes.
                if sheet=='Amadeus Supp Refs' and col not in ['A','B','C']:continue
                a=f'{col}{row}'
                refs=[i for i,t in enumerate(trace) if t['dataset']==key and any(x['sheet']==sheet and x['cell']==a for x in t['excel_evidence'])]
                disposition='field trace recorded' if refs else 'corroborating inventory evidence; no additional assertion or field inferred'
                if sheet=='Environmental Datasets' and (len(col)>1 or col>='L') or sheet=='Amadeus data sources' and col not in list('BCDEFGHIJKLMNOPQRSTUVWXYZ')+['AA','AB','AN']:
                    disposition='classification indicator or source-row helper; corroborates descriptive support only, not a separate measured variable'
                core_evidence.append({'dataset':key,'sheet':sheet,'cell':a,'value':value,'disposition':disposition,'trace_entries':refs})

# Review all 46 candidates, without implying the older proposal table is an
# approved extension plan. The final narrowed gap review governs adoption.
proposal_dispositions=[]
rights={'license_statement','citation_requirement','access_restriction_note','first_release_description','first_release_year','provider_lifecycle_status','amadeus_support_status','ProviderLifecycleStatusEnum','AmadeusSupportStatusEnum'}
observing={'platform_name','instrument_name','contribution_start_date','contribution_end_date','observing_system_history'}
for (kind,name),entries in proposals.items():
    decision='Not implemented; optional candidate, not required or independently supported for the present four scoped profiles.'
    if name in rights:decision='Inventory wording is preserved in scoped descriptions and audit records; no new typed field, release date, SPDX code or observation QA state is inferred.'
    elif name in observing:decision='HMS programme-level paired history is retained in DataSource.description with attribution; source-only smoke contribution dates remain unverified. No scalar Product fields or new relationship class is introduced.'
    elif name in ['spatial_coverage','spatial_coverage_note','SpatialCoverageEnum']:decision='Completeness assertions retained as attributed Product.description text; no structured baseline field exists. Typed coverage filtering needs an approved catalog extension.'
    elif name=='update_frequency_description':decision='AQS regular bulk frequency is represented by existing update_cadence_iso=P6M; conflicting month labels remain in description. No parallel cadence slot is added.'
    elif name in ['spatial_unit_type','SpatialUnitTypeEnum']:decision='Existing SpatialSupportTypeEnum and descriptors suffice for these four scoped profiles; the final Markdown gap review does not justify a second geography vocabulary for these labels.'
    elif name in ['temporal_support_description','temporal_transform','TemporalTransformEnum']:decision='Reuse existing temporal support and aggregation fields, retaining necessary qualifications in descriptions/notes. Do not manufacture aggregation semantics from generic labels.'
    elif name=='native_grid_definition':decision='Both scoped raster families use one verified grid per product. Existing Product.grid_definition suffices; a variable override is unnecessary here. If needed later, the final Markdown review recommends reusing the existing slot on ProductVariable.'
    elif name in ['native_storage_type','NativeStorageTypeEnum','native_valid_min','native_valid_max','native_units_description']:decision='Provider-specific native encoding facts are retained in verified metadata evidence and harmonization notes; the MODIS workbook variable sheets do not provide native variable metadata for these four datasets. No canonical plausible bounds or semantic dtype are substituted.'
    elif name in ['processing_level','processing_level_description','ProcessingLevelEnum','vertical_coordinate_type','VerticalCoordinateTypeEnum','production_method','ProductionMethodEnum','product_dimension_count']:decision='Detailed satellite processing/level/dimension columns primarily describe other workbook products. Do not transfer MERRA/MODIS classifications to gridMET, AQS, HMS or NLCD by row proximity.'
    proposal_dispositions.append({'item_type':kind,'item_name':name,'decision':decision,'mapping_rows':[{'sheet':e['worksheet'],'row':e['row']} for e in entries],'integration_document_section':'Remaining gaps in schema coverage and recommended placement'})

scope_decisions=[
 {'source':'Environmental Datasets!B16:J16 and B21:J21','decision':'Tree Canopy Cover is a separate USFS/MRLC product family, repeated under two themes. Not inferred to be one of the six Annual NLCD C1V1 products; needs independent verification before registration.'},
 {'source':'Environmental Datasets!F13:G13; Amadeus Supp Refs!B11:C12; Amadeus Source and Product!B7:E8','decision':'Legacy years/cadence/DOIs do not describe the selected Annual C1V1 release. Retained as explicit exclusions, not copied to Annual Product DOI, version or release date.'},
 {'source':'Environmental Datasets!I2; Amadeus data sources!M20','decision':'Broader AQS inventory includes hourly data; the existing scopes cover daily summaries only. No implied hourly product is added.'},
 {'source':'Environmental Datasets!H103; Amadeus data sources!D15','decision':'HMS fire points and KML are distinct product/delivery scopes. The current spec covers raw smoke shapefiles; mixed support is not flattened into one product.'},
 {'source':'Environmental datasets taxonomy and Amadeus data taxonomy','decision':'Independent organization/license/classification lists are vocabulary evidence, never joined to dataset rows by row number. In ADT, only explicitly matched A:B and F:I blocks can identify source/function relationships; K:L is a separate keyed legend.'},
 {'source':'Amadeus data taxonomy!F4:I4; F11:I11; F14:I14; K3:L8','decision':'Corroborates named gridMET/NLCD/HMS calculation functions and broad methods via explicit names/category keys, not row-wise N:AA metadata. Actual dispatch strings are still verified against R code. AQS has no listed calculate route.'},
 {'source':'Four MODIS native-variable worksheets','decision':'Their Units/Data Type/Fill/Valid Range/Scale/Offset columns pertain to MODIS variables, not the four selected source families. Provider metadata is necessary for the selected native variables.'},
 {'source':'Both hidden Copy worksheets','decision':'Already established as duplicates of visible source columns; no duplicate product/source/citation records are imported.'},
 {'source':'Unmapped Columns worksheet','decision':'All entries reviewed. Chart columns, empty input fields, acronym glossary and independent calculation/category lists do not establish new metadata fields on the four scoped catalog products. Actual code-table row IDs are not global entity IDs.'},
]

# Existing Markdown guidance explicitly applied, with section references.
rules=[
 {'section':'Workbook coverage and reference key','application':'Visible source precedence; explicit bibliographic joins; independent taxonomy blocks; no chart/helper ingestion.'},
 {'section':'Existing catalog classes','application':'Preserve DataSource/Product/ProductVariable/CanonicalVariable spine; use existing GridDefinition references, no provider/category subclasses.'},
 {'section':'Proposed slots','application':'Candidate scope is reviewed, not automatically merged; rights/history/coverage fallback uses existing scoped description fields.'},
 {'section':'New enums and extensions to existing enums','application':'Normalize to existing genres/support/resolution/format enums. No unimplemented member is emitted.'},
 {'section':'Integration of `amadeus_common.yaml`','application':'Shared definitions remain single-source; native packing is separate from semantic value type, support is separate from output grouping, and metadata absence is not runtime state.'},
 {'section':'Import and extension decisions','application':'Do not confuse coverage/release dates, native/canonical bounds, primary identity/DOI, calendar/day-of-year, or a nominal cell size/full grid.'},
 {'section':'Remaining gaps in schema coverage and recommended placement','application':'Use this narrowed review over the older 46-candidate list; avoid duplicate geography enums, native-grid synonyms and per-product copies of shared level descriptions.'},
]
for rule in rules:assert rule['section'] in doc,rule
# Compare the snapshot of the original builder's inputs with what it actually
# parsed. This is an explicit historical audit, not a claim of prior automation.
original_usage={
 'catalog_schema_excel_mapping_with_unmapped.xlsx':'Reviewed in conversation and SHA-256 recorded; the initial generator did not parse its rows or use an executable crosswalk.',
 'catalog_dataset_integration.md':'Reviewed for class ownership and semantic safeguards; SHA-256 recorded. Its recommendations were applied manually, without section/field trace records.',
 'environmental_datasets_table.xlsx':'Selected visible rows were extracted into source_manifest.workbook_rows and some inventory metadata corroborated manually curated objects. Selection missed the explicitly named AQS bulk source row and HMS history; cells were not a comprehensive generation crosswalk.',
 'GitHub_and_providers':'GitHub/local R code supplied implementation dispatch/binding evidence. Native scientific details came primarily from provider XML/CSV/pages, with archived examples supplying reusable IDs. GitHub was not the sole scientific source.'}
report={'reviewed_utc':STAMP,'catalog_directory':str(BUNDLES.relative_to(ROOT)),'directory_note':'validated_catalog_bundles is not present; validated_catalog_handles contains the existing catalog bundles. No duplicate folder was created.','input_hashes':protected,'original_resource_use':original_usage,'current_resource_use':{'mapping_workbook':'All seven worksheets parsed; each existing definition checked against its defining YAML, candidate definitions separated, applicable field mappings linked by row.','integration_markdown':'Named design/gap sections explicitly applied; final narrowed gap review takes precedence over earlier candidate recommendations.','source_workbook':'All thirteen worksheets parsed, with visible/hidden state recorded; complete named source/bibliography rows for four datasets, programme-history table, taxonomy and source-scope exclusions reviewed.'},'mapping_inventory':{'existing_definitions':len(set(existing)),'proposed_definitions':len(proposals),'unmapped_columns':len(mp['Unmapped Columns']['rows'])-1},'mapping_definitions':[{'schema_file':a,'item_type':b,'item_name':c} for a,b,c in sorted(set(existing))],'design_rules':rules,'field_trace':trace,'core_excel_cell_dispositions':core_evidence,'proposed_definition_dispositions':proposal_dispositions,'source_scope_decisions':scope_decisions,'hms_programme_contributions':history,'new_schema_definitions':0,'change_summary':['Retain all four named bulk/source inventory rows, including AQS row 20, and explicit reference rows.','AQS publication cadence uses existing P6M; preserves conflicting month wording and separates daily observations from bulk updates.','Reuse the explicit AQS Data Mart bibliography entry.','Preserve rights/citation, release-history and lifecycle assertions with scope in existing descriptions.','Preserve 21 paired programme-level HMS history entries, excluding chart endpoint 46324 as an end date.','HMS gains existing air_pollution theme alongside wildfire_smoke.','Record all applicable crosswalk, exclusion and candidate-definition decisions; no proposed schema fields are emitted.']}
(SPECS/'resource_audit.json').write_text(json.dumps(report,indent=2,ensure_ascii=False)+'\n')

# Refresh YAML, discovery projections, global union, and R copies from one set
# of object definitions so the added inventory information cannot drift.
for key,b in specs.items():
    (SPECS/f'{key}.yaml').write_text('# Archived AmadeusDatabase metadata profile; source reconciliation documented in resource_audit.json.\n'+yaml.safe_dump(b,sort_keys=False,allow_unicode=True,width=100))
    h={k:v for k,v in b.items() if k in ['schema_version','data_sources','products','grid_definitions']}
    (BUNDLES/f'{key}.yaml').write_text('# Discovery projection of the corresponding dataset spec; identical stable IDs/metadata.\n'+yaml.safe_dump(h,sort_keys=False,allow_unicode=True,width=100))
g={'schema_version':specs['gridmet']['schema_version']}
for coll in ['data_sources','grid_definitions','products','canonical_variables','product_variables']:
    unique={}
    for b in specs.values():
        for r in b.get(coll,[]):
            assert r['id'] not in unique or unique[r['id']]==r,('Conflicting identity',r['id'])
            unique[r['id']]=r
    g[coll]=list(unique.values())
(BUNDLES/'global_catalog.yaml').write_text('# Full union of the four reconciled specs, retaining unique identifiers.\n'+yaml.safe_dump(g,sort_keys=False,allow_unicode=True,width=100))
subprocess.run(['Rscript','-e','for (dataset in c("gridmet","aqs","hms","nlcd")) {p <- file.path("dataset_spec_objects",paste0(dataset,".yaml")); saveRDS(yaml::read_yaml(p),sub("yaml$","rds",p),version=3)}'],cwd=ROOT,check=True)

# Augment the established audit manifest rather than add a catalog schema.
manifest_path=SPECS/'source_manifest.json';manifest=json.loads(manifest_path.read_text())
manifest['resource_reconciliation']={'reviewed_utc':STAMP,'report':'resource_audit.json','sha256':sha(SPECS/'resource_audit.json'),'field_traces':len(trace),'core_source_cells':len(core_evidence),'hms_programme_history_entries':len(history),'schema_definitions_added':0}
manifest['workbook_rows']=[
    source_row(sheet,row)
    for key,c in config.items()
    for sheet,rows in [
        ('Environmental Datasets',c['eds']),
        ('Amadeus data sources',[c['ads']]),
        ('Amadeus Source and Product',c['asp']),
        ('Amadeus Supp Refs',c['asr']),
    ]
    for row in rows
] + [source_row('NOAA Hazard Mapping System',row) for row in range(5,26)]
manifest_path.write_text(json.dumps(manifest,indent=2,ensure_ascii=False)+'\n')
# Add unresolved, precisely scoped source/model gaps without repeating entries.
gap_path=SPECS/'verification_gaps.json';gaps=json.loads(gap_path.read_text());gaps['resource_reviewed_utc']=STAMP
new=[
 {'dataset':'aqs','field':'bulk release months','reason':'The source row correctly distinguishes nominal twice-yearly bulk publication from daily database updates. The EPA page itself contains differing month descriptions.','impact':'P6M is nominal frequency only, not an exact calendar or availability guarantee.','next_action':'Use actual annual-file modification dates; resolve month-specific release claims with EPA if needed.'},
 {'dataset':'hms','field':'programme contribution history vs smoke support','reason':'The workbook B5:F25 table covers programme systems, including dates before smoke polygons began. In use and chart endpoint values do not establish final contribution dates.','impact':'Twenty-one paired source assertions are preserved in DataSource.description; none is asserted as a smoke-product-specific sensor/date relationship.','next_action':'Verify historical programme assertions and smoke-specific participation before typed contribution queries; do not introduce ambiguous scalar Product fields.'},
 {'dataset':'all','field':'first-release, rights, lifecycle and coverage vocabulary','reason':'All relevant inventory assertions are retained as attributed, scoped text. Dedicated proposed slots/enums have not been incorporated into the existing schema.','impact':'Metadata is preserved but not uniformly available as structured filter fields. Inventory claims do not become verified release dates, SPDX terms, implementation status or observation QA.','next_action':'Approve only justified extensions from the final Markdown gap review if typed queries are required; otherwise use the existing descriptions and audit trace.'},
 {'dataset':'nlcd','field':'legacy and Tree Canopy Cover scope','reason':'The workbook mixes legacy citations/years with Annual status and lists Tree Canopy Cover under multiple themes. Neither establishes a new verified Annual C1V1 product.','impact':'Legacy and canopy information is retained in scope decisions rather than copied into Annual records or registered twice.','next_action':'Verify and register these distinct product families only when expanding the stated scope.'},
]
bykey={(x['dataset'],x['field']):x for x in gaps['gaps']}
for item in new:bykey[(item['dataset'],item['field'])]=item
gaps['gaps']=list(bykey.values());gap_path.write_text(json.dumps(gaps,indent=2)+'\n')
assert all(sha(ROOT/p)==v for p,v in protected.items()),'Input resource/schema changed'
print(json.dumps({'field_traces':len(trace),'core_source_cells':len(core_evidence),'hms_history_entries':len(history),'proposed_definitions_reviewed':len(proposals),'existing_definitions_checked':len(set(existing)),'source_resources_unchanged':True},indent=2))

if __name__=='__main__':
    pass
