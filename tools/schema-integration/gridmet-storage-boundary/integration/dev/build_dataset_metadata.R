# Maintainer build-time extraction only. No provider download or scientific processing.
script<-normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]));root<-dirname(dirname(script))
for(p in c('amadeus','jsonlite','digest'))stopifnot(requireNamespace(p,quietly=TRUE))
sha<-function(p)digest::digest(file=p,algo='sha256')
source(file.path(root,'dev/check_approval.R'));approval<-check_approval(root)
all<-amadeus::process_gridmet_codes('all')
stopifnot(is.matrix(all),ncol(all)==2L,!anyNA(all),!anyDuplicated(all[,2]))
mapping<-data.frame(code=all[,2],amadeus_label=all[,1],stringsAsFactors=FALSE)
stopifnot(identical(unname(amadeus::process_variable_codes(mapping$code,source='gridmet')),mapping$code))
profile_path<-file.path(root,'catalog-source/gridmet-reviewed-profile.csv')
profile<-read.csv(profile_path,stringsAsFactors=FALSE)
stopifnot(nrow(profile)==16L,!anyDuplicated(profile$code),all(profile$code%in%mapping$code))
qualified<-profile
qualified$label<-mapping$amadeus_label[match(profile$code,mapping$code)]
stopifnot(!anyNA(qualified$label))
routes<-c('download_gridmet','process_gridmet','calculate_gridmet')
exports<-getNamespaceExports('amadeus');stopifnot(all(routes%in%exports))
functions<-c('process_gridmet_codes','process_variable_codes',routes)
bodyhash<-setNames(lapply(functions,function(n)digest::digest(paste(deparse(body(getExportedValue('amadeus',n))),collapse='\n'),algo='sha256',serialize=FALSE)),functions)
entries<-lapply(seq_len(nrow(mapping)),function(i){
 code<-mapping$code[i];j<-match(code,profile$code)
 list(code=code,label=mapping$amadeus_label[i],qualified_in_current_profile=!is.na(j),
  units_ucum=if(is.na(j))NULL else profile$native_ucum[j],
  accepted_source_units=if(is.na(j))NULL else profile$accepted_source_units[j],
  database_tolerance=if(is.na(j))NULL else profile$database_tolerance[j],
  previous_pilot_label=if(is.na(j))NULL else profile$label[j],
  label_authority='installed amadeus::process_gridmet_codes("all"); exact returned spelling',
  units_and_tolerance_authority=if(is.na(j))'not supplied by this extraction'else'existing reviewed pilot profile; not extracted from Amadeus')
})
catalog<-list(revision='amadeus-package-metadata-v1',dataset='gridmet',
 approval_catalog_sha256=sha(file.path(root,'approval/catalog.json')),approval_policy='LinkML-defined catalog; bounded test qualification; scientific review pending; production false',reviewed_profile_sha256=sha(profile_path),package_version=as.character(packageVersion('amadeus')),schema_sha256=sha(file.path(root,'schema/amadeus.schema.json')),
 extraction=list(functions=as.list(functions),function_body_sha256=bodyhash,provider_data_downloaded=FALSE),
 scope=list(package_code_count=nrow(mapping),qualified_daily_code_count=nrow(profile),external_spreadsheet_imported=FALSE),
 routes=setNames(lapply(routes,function(n)list(exported=TRUE,argument_names=as.list(names(formals(getExportedValue('amadeus',n)))))),routes),
 field_authorities=list(code_and_label='installed Amadeus public mapping helpers',route_argument_names='installed Amadeus exported function formals',
  units_and_numeric_tolerances='reviewed existing pilot registry',source_native_name_units_grid='observed in actual NetCDF at runtime',
  product_CRS_cadence_and_descriptions='existing exporter profile; not newly derived from package',coverage_and_license='not extracted; do not infer'),variables=entries,
 note='Catalog instances/properties, not a replacement schema. Full package inventory is not onboarding qualification. Labels remain lowercase and retain package spelling. No source coverage, scientific ontology or drought-class interpretation is inferred.')
dir.create(file.path(root,'catalog'),showWarnings=FALSE)
write.csv(mapping,file.path(root,'catalog/amadeus-gridmet-inventory.csv'),row.names=FALSE)
write.csv(qualified,file.path(root,'inputs/variables.csv'),row.names=FALSE)
jsonlite::write_json(catalog,file.path(root,'catalog/gridmet.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=NA)
jsonlite::write_json(list(build_passed=TRUE,package_codes=nrow(mapping),qualified_codes=nrow(profile),catalog_sha256=sha(file.path(root,'catalog/gridmet.json')),compiled_registry_sha256=sha(file.path(root,'inputs/variables.csv')),reviewed_profile_sha256=sha(profile_path),external_imported=FALSE),file.path(root,'catalog/BUILD-RECEIPT.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Extracted',nrow(mapping),'Amadeus codes; retained',nrow(profile),'qualified daily variables.\n')
