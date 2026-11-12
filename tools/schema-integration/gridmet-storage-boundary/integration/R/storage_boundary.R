# Generic storage-boundary enforcement; no scientific dispatch or package hooks.
.storage_contract_cache <- new.env(parent=emptyenv())
schema_release <- function(root, verify=TRUE) {
  release <- jsonlite::read_json(file.path(root,'release/runtime-contract.json'))
  if (!isTRUE(release$build_qa_passed)) stop('Release has no successful build QA')
  if (verify) for(n in names(release$runtime_files)) {
    p <- file.path(root,n)
    if(!file.exists(p)||digest::digest(file=p,algo='sha256')!=release$runtime_files[[n]])stop('Storage release drift: ',n)
  }
  key <- normalizePath(root)
  if(!exists(key,envir=.storage_contract_cache,inherits=FALSE))assign(key,list(
    json=jsonvalidate::json_validator(file.path(root,'schema/amadeus.schema.json'),engine='ajv'),
    contract=jsonlite::read_json(file.path(root,'schema/contract.json')),
    registry=read.csv(file.path(root,'inputs/variables.csv'),stringsAsFactors=FALSE),
    dataset_metadata=jsonlite::read_json(file.path(root,'catalog/gridmet.json')),release=release),envir=.storage_contract_cache)
  get(key,envir=.storage_contract_cache,inherits=FALSE)
}
storage_validate <- function(doc,root,results,run_dir) {
  runtime<-schema_release(root,verify=TRUE)
  encode<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null='null',digits=NA))
  runtime$json(encode(doc),error=TRUE,greedy=TRUE)
  # Reuse existing sf engine for geometry; no DuckDB spatial extension is introduced.
  if(length(doc$locations)) {
    geometry<-sf::st_as_sfc(vapply(doc$locations,function(x)x$location_geom_wkt,''),crs=4326)
    if(any(sf::st_is_empty(geometry))||any(!sf::st_is_valid(geometry))||any(as.character(sf::st_geometry_type(geometry))!='POINT'))stop('Storage boundary: invalid point geometry')
  }
  for(n in names(runtime$contract$collections)) {
    cls<-runtime$contract$collections[[n]]
    inline<-names(runtime$contract$classes[[cls]]$inline)
    if(any(vapply(doc[[n]],function(x)any(names(x)%in%inline),TRUE)))stop('Storage boundary: unsupported inline records')
  }
  fields<-jsonlite::read_json(file.path(root,'storage-contract/fields.json'))
  con<-DBI::dbConnect(duckdb::duckdb(),dbdir=':memory:')
  on.exit(DBI::dbDisconnect(con,shutdown=TRUE))
  for(n in names(fields)) {
    records<-doc[[n]];cols<-unlist(fields[[n]])
    scalar<-function(z,f){x<-z[[f]];if(is.null(x)||is.list(x))NA_character_ else as.character(x)}
    frame<-as.data.frame(setNames(lapply(cols,function(f)vapply(records,scalar,'',f=f)),cols),stringsAsFactors=FALSE)
    DBI::dbWriteTable(con,n,frame)
    cls<-runtime$contract$collections[[n]]
    listfields<-unique(c(unlist(runtime$contract$classes[[cls]]$multiple),if(n=='tool_runs')'upstream_runs'))
    for(f in listfields) {
      edges<-lapply(records,function(x){v<-unlist(x[[f]]);if(!length(v))return(NULL);data.frame(owner=x$id,target=as.character(v),stringsAsFactors=FALSE)})
      edge<-do.call(rbind,edges);if(is.null(edge))edge<-data.frame(owner=character(),target=character())
      DBI::dbWriteTable(con,paste0(n,'__',f),edge)
    }
  }
  membership<-do.call(rbind,lapply(doc$extraction_requests,function(x)data.frame(request=x$id,variable=unlist(x$requested_product_variables))))
  if(is.null(membership))membership<-data.frame(request=character(),variable=character())
  DBI::dbWriteTable(con,'request_variables',membership)
  DBI::dbWriteTable(con,'expected',results)
  view<-paste(readLines(file.path(root,'storage-contract/value-view.sql'),warn=FALSE),collapse='\n')
  DBI::dbExecute(con,view)
  DBI::dbExecute(con,'CREATE TEMP TABLE before_write AS SELECT * FROM v')
  stage<-file.path(run_dir,'storage-staging');dir.create(stage,showWarnings=FALSE,recursive=TRUE)
  path<-file.path(stage,'values.parquet')
  if(file.exists(path))stop('Storage staging already exists; use a new run')
  DBI::dbExecute(con,paste0('COPY (SELECT * FROM v) TO ',DBI::dbQuoteString(con,path),' (FORMAT PARQUET)'))
  DBI::dbExecute(con,'DROP VIEW v')
  DBI::dbExecute(con,paste0('CREATE VIEW v AS SELECT * FROM read_parquet(',DBI::dbQuoteString(con,path),')'))
  # Exact symmetric difference across all mapped columns, including duplicate multiplicity.
  for(sql in c('SELECT count(*) FROM (SELECT * FROM before_write EXCEPT ALL SELECT * FROM v)',
               'SELECT count(*) FROM (SELECT * FROM v EXCEPT ALL SELECT * FROM before_write)'))
    if(DBI::dbGetQuery(con,sql)[[1]][1]!=0)stop('Parquet full-column readback changed values')
  rules<-jsonlite::read_json(file.path(root,'storage-contract/rules.json'))
  for(n in names(rules)) {
    bad<-DBI::dbGetQuery(con,rules[[n]])[[1]][1]
    if(is.na(bad)||bad!=0)stop('Storage SQL rejected: ',n,' (',bad,')')
  }
  jsonlite::write_json(list(boundary_checks_passed=TRUE,sql_rules=length(rules),
    structural_json_schema=TRUE,geometry=TRUE,full_column_Parquet_readback=TRUE,
    parquet_sha256=digest::digest(file=path,algo='sha256'),publication_accepted=FALSE),
    file.path(stage,'boundary-checks.json'),pretty=TRUE,auto_unbox=TRUE)
  invisible(TRUE)
}

# Publication integrity: post-calculation only. These helpers do not import Amadeus science.
.publication_sha <- function(path) digest::digest(file=path,algo='sha256')
.publication_path <- function(run_dir,relative) {
  if(length(relative)!=1L || grepl('(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)',relative))stop('Unsafe publication relative path')
  base<-normalizePath(run_dir,mustWork=TRUE)
  path<-normalizePath(file.path(base,relative),mustWork=TRUE)
  if(!startsWith(path,paste0(base,'/')))stop('Publication path escapes run directory')
  path
}
.publication_release <- function(root) {
  release<-jsonlite::read_json(file.path(root,'release/runtime-contract.json'))
  if(!isTRUE(release$build_qa_passed))stop('Release has no successful build QA')
  if(!length(release$runtime_files))stop('Missing release fingerprints')
  for(f in names(release$runtime_files))if(.publication_sha(file.path(root,f))!=release$runtime_files[[f]])stop('Publication release drift: ',f)
  release
}
storage_publish_manifest <- function(root,run_dir,engines=character()) {
  .publication_release(root)
  if(length(engines)&&!identical(engines,'duckdb'))stop('Publication profile qualifies persistent DuckDB only')
  base_files<-c('schema-instance.json','acceptance.json','results.csv','source-metadata.json',
    'direct-output-context.json','provenance/execution.rds','provenance/events.json','provenance/seal.json')
  files<-base_files
  if(length(engines)) {
    files<-c(files,'database/duckdb/gridmet.duckdb','database/duckdb/receipt.json')
    db<-jsonlite::read_json(file.path(run_dir,'database/duckdb/receipt.json'))
    if(identical(db$storage_route,'parquet'))files<-c(files,'database/duckdb/accepted-values.parquet')
    if(file.exists(file.path(run_dir,'database/duckdb/gridmet.duckdb.wal')))stop('Uncheckpointed DuckDB WAL; cannot publish')
  }
  acceptance<-jsonlite::read_json(file.path(run_dir,'acceptance.json'))
  if(!isTRUE(acceptance$accepted)||!identical(acceptance$runtime_contract_sha256,.publication_sha(file.path(root,'release/runtime-contract.json')))||!identical(acceptance$schema_sha256,.publication_sha(file.path(root,'schema/amadeus.schema.json')))||!identical(acceptance$instance_sha256,.publication_sha(file.path(run_dir,'schema-instance.json'))))stop('Cannot publish unaccepted or mismatched release/instance')
  entries<-setNames(lapply(files,function(f){p<-.publication_path(run_dir,f);before<-file.info(p);hash<-.publication_sha(p);after<-file.info(p)
    if(before$size!=after$size||before$mtime!=after$mtime)stop('Publication file changed while hashing: ',f)
    list(bytes=as.numeric(after$size),sha256=hash)}),files)
  manifest<-list(revision='publication-manifest-v1',engines=as.list(engines),
    instance_sha256=acceptance$instance_sha256,schema_sha256=acceptance$schema_sha256,
    runtime_contract_sha256=.publication_sha(file.path(root,'release/runtime-contract.json')),
    files=entries,verification_scope='Owned published snapshot bytes; no provider reread; unsigned local receipt chain',
    immutable_policy='New run directory; no overwrite; read-only published files; coordinated privileged edits not independently attested',production_approved=FALSE)
  path<-file.path(run_dir,'publication-manifest.json')
  if(file.exists(path))stop('Publication manifest already exists')
  jsonlite::write_json(manifest,path,auto_unbox=TRUE,pretty=TRUE,digits=NA)
  modes<-Sys.chmod(c(vapply(files,function(f).publication_path(run_dir,f),''),path),mode='0444')
  if(!all(modes))stop('Could not mark publication files read-only; no workflow completion')
  list(sha256=.publication_sha(path),files=length(files),policy='verify bytes at each store open; no trusted stat-only cache')
}
storage_verify_publication <- function(root,run_dir,engine='duckdb') {
  started<-unname(proc.time()[3]);.publication_release(root)
  if(!identical(engine,'duckdb'))stop('Only persisted DuckDB publication is qualified')
  wf<-jsonlite::read_json(file.path(run_dir,'workflow-receipt.json'))
  if(!isTRUE(wf$completed)||!identical(wf$selected_engine,'duckdb')||!isTRUE(wf$schema_accepted))stop('No completed DuckDB publication')
  manifest_path<-file.path(run_dir,'publication-manifest.json')
  if(is.null(wf$publication_manifest_sha256)||!file.exists(manifest_path)||.publication_sha(manifest_path)!=wf$publication_manifest_sha256)stop('Missing or changed publication manifest')
  manifest<-jsonlite::read_json(manifest_path)
  if(!identical(manifest$revision,'publication-manifest-v1')||!identical(unlist(manifest$engines,use.names=FALSE),'duckdb'))stop('Publication engine/revision mismatch')
  runtime_sha<-.publication_sha(file.path(root,'release/runtime-contract.json'))
  if(!identical(manifest$runtime_contract_sha256,runtime_sha)||!identical(wf$runtime_contract_sha256,runtime_sha))stop('Publication belongs to a different runtime release')
  required<-c('schema-instance.json','acceptance.json','results.csv','source-metadata.json','direct-output-context.json',
   'provenance/execution.rds','provenance/events.json','provenance/seal.json','database/duckdb/gridmet.duckdb','database/duckdb/receipt.json')
  allowed<-c(required,'database/duckdb/accepted-values.parquet')
  if(!all(required%in%names(manifest$files))||!all(names(manifest$files)%in%allowed)||anyDuplicated(names(manifest$files)))stop('Incomplete/unknown publication file coverage')
  if(file.exists(file.path(run_dir,'database/duckdb/gridmet.duckdb.wal')))stop('DuckDB WAL present; published snapshot is not immutable')
  bytes<-0
  for(f in names(manifest$files)){
    p<-.publication_path(run_dir,f);entry<-manifest$files[[f]];bytes<-bytes+as.numeric(file.info(p)$size)
    if(file.info(p)$size!=entry$bytes||.publication_sha(p)!=entry$sha256)stop('Published snapshot changed: ',f)
  }
  acceptance<-jsonlite::read_json(file.path(run_dir,'acceptance.json'))
  db<-jsonlite::read_json(file.path(run_dir,'database/duckdb/receipt.json'))
  if(!isTRUE(acceptance$accepted)||!isTRUE(db$all_within_declared_tolerances)||!isTRUE(db$persistent_reopen_passed)||
     !identical(manifest$instance_sha256,acceptance$instance_sha256)||!identical(manifest$files[['schema-instance.json']]$sha256,acceptance$instance_sha256)||!identical(acceptance$runtime_contract_sha256,runtime_sha)||!identical(wf$instance_sha256,acceptance$instance_sha256)||
     !identical(db$accepted_instance_sha256,acceptance$instance_sha256)||wf$accepted_records!=db$records||db$records!=acceptance$records||
     !identical(manifest$schema_sha256,acceptance$schema_sha256)||!identical(manifest$schema_sha256,.publication_sha(file.path(root,'schema/amadeus.schema.json'))))stop('Publication receipt/instance mismatch')
  if(identical(db$storage_route,'parquet')){
    if(!'database/duckdb/accepted-values.parquet'%in%names(manifest$files)||!identical(db$parquet_sha256,manifest$files[['database/duckdb/accepted-values.parquet']]$sha256))stop('Parquet publication binding mismatch')
  }
  list(verified=TRUE,files=length(manifest$files),bytes=bytes,verification_elapsed_seconds=unname(proc.time()[3])-started,
    scope='Once per database open; raw source files not re-read; unsigned local receipt chain')
}
