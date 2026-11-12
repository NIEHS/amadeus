# Maintainer-only release QA. Users receive these artifacts; never run this per request.
a<-commandArgs(TRUE)
script<-normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))
root<-dirname(dirname(script));sha<-function(p)digest::digest(file=p,algo='sha256')
source(file.path(root,'dev/check_approval.R'));approval<-check_approval(root)
pinned<-jsonlite::read_json(file.path(root,'schema/PROVENANCE.json'))
for(name in names(pinned$files))stopifnot(sha(file.path(root,'schema',name))==pinned$files[[name]])
contract<-jsonlite::read_json(file.path(root,'schema/contract.json'))
j<-jsonlite::read_json(file.path(root,'schema/amadeus.schema.json'))
registry<-read.csv(file.path(root,'inputs/variables.csv'),stringsAsFactors=FALSE)
required_columns<-c('code','label','native_ucum','accepted_source_units','database_tolerance')
stopifnot(all(required_columns%in%names(registry)),nrow(registry)==16L,
 !anyNA(registry[required_columns]),!anyDuplicated(registry$code),
 all(vapply(registry[c('code','label','native_ucum','accepted_source_units')],function(x)all(nzchar(trimws(x))),logical(1))),
 all(is.finite(as.numeric(registry$database_tolerance))),all(as.numeric(registry$database_tolerance)>=0))
for(cls in unlist(contract$collections))stopifnot(!is.null(contract$classes[[cls]]),!is.null(j[['$defs']][[cls]]))
for(f in list.files(root,pattern='\\.R$',recursive=TRUE,full.names=TRUE))parse(f)
for(f in list.files(file.path(root,'schema/source'),pattern='yaml$',full.names=TRUE))yaml::read_yaml(f)
invisible(jsonvalidate::json_validator(file.path(root,'schema/amadeus.schema.json'),engine='ajv'))
mapping<-jsonlite::read_json(file.path(root,'parquet/value-storage-contract.json'))
stopifnot(mapping$SQL_sha256==pinned$files$amadeus_duckdb.sql,mapping$schema_sha256==pinned$files$amadeus.schema.json)
fields<-vapply(mapping$columns,function(x)x$column_name,'')
stopifnot(!anyDuplicated(fields),setequal(fields,names(j[['$defs']]$AmbientValueAtLocation$properties)))
# Ensure the compiled runtime registry agrees with the prepared catalog.
dataset_metadata<-jsonlite::read_json(file.path(root,'catalog/gridmet.json'))
qualified<-Filter(function(x)isTRUE(x$qualified_in_current_profile),dataset_metadata$variables)
stopifnot(dataset_metadata$schema_sha256==pinned$files$amadeus.schema.json,
 setequal(vapply(qualified,function(x)x$code,''),registry$code))
for(x in qualified){i<-match(x$code,registry$code);stopifnot(x$label==registry$label[i],x$units_ucum==registry$native_ucum[i],x$accepted_source_units==registry$accepted_source_units[i],x$database_tolerance==registry$database_tolerance[i])}
# Runtime only needs fingerprints of executable contracts/config, not source YAML or authoring scripts.
files<-c('schema/PROVENANCE.json','schema/amadeus.schema.json','schema/amadeus_validators.R','schema/contract.json',
 'schema/amadeus_duckdb.sql','schema/amadeus_sedonadb.sql','schema/integrity.sql','inputs/variables.csv',
 'parquet/value-storage-contract.json','R/storage_boundary.R','R/schema_export.R','R/database.R',
 'R/parquet_experiment.R','R/export_gridmet.R','R/provenance_recorder.R','storage-contract/rules.json','storage-contract/value-view.sql','storage-contract/fields.json','storage-contract/request-policy.R.txt','catalog/gridmet.json','approval/catalog.json','approval/REVIEW-SNAPSHOT.json')
release<-list(revision='storage-boundary-publication-v2',build_qa_passed=TRUE,
 model_version=pinned$schema_version,runtime_files=setNames(lapply(files,function(f)sha(file.path(root,f))),files),
 publication_profile='DuckDB manifest v1; verified once per saved-store open; read-only new snapshots',
 approval_catalog_sha256=sha(file.path(root,'approval/catalog.json')),scientific_review='pending',production_approved=FALSE,
 dataset_profile='16 native daily gridMET variables, point extraction, one year per request',
 note='Stable consistency QA is completed before distribution. Runtime verifies critical bytes and actual request/data. No prevalidated future data is claimed.')
dir.create(file.path(root,'release'),showWarnings=FALSE)
jsonlite::write_json(release,file.path(root,'release/runtime-contract.json'),auto_unbox=TRUE,pretty=TRUE)
report<-list(build_qa_passed=TRUE,full_pinned_artifacts=length(pinned$files),runtime_fingerprints=length(files),
 registry_variables=nrow(registry),R_sources_parsed=TRUE,source_YAML_parsed=TRUE,JSON_Schema_compiled=TRUE,
 collection_classes_matched=TRUE,Parquet_fields_matched=length(fields),
 runtime_contract_sha256=sha(file.path(root,'release/runtime-contract.json')),
 LinkML_regenerated=FALSE,LinkML_import_lint_QA_executed=FALSE,physical_time_approved=FALSE,production_approved=FALSE)
jsonlite::write_json(report,file.path(root,'release/BUILD-RECEIPT.json'),auto_unbox=TRUE,pretty=TRUE)
if(length(a))jsonlite::write_json(report,a[1],auto_unbox=TRUE,pretty=TRUE)
print(report)
