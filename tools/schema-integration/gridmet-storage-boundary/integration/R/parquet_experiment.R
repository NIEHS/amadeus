# Optional Parquet storage experiment. Core Amadeus and existing adapters unchanged.
a<-commandArgs(TRUE)
if(!length(a)%in%c(2L,3L,4L))stop('Usage: Rscript R/parquet_experiment.R ACCEPTED_RUN NEW_OUTPUT [all|rowwise|inmemory_bulk|parquet]')
script<-normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))
root<-dirname(dirname(script));run_dir<-normalizePath(a[1]);out<-a[2];mode<-if(length(a)>=3L)a[3]else'all'
profile<-if(length(a)==4L)a[4]else'comparison'
stopifnot(profile%in%c('comparison','operational'))
operational<-profile=='operational'
if(operational&&mode=='all')stop('Operational loading selects one route, not all comparison routes')
stopifnot(mode%in%c('all','rowwise','inmemory_bulk','parquet'))
if(dir.exists(out))stop('Output exists; choose a NEW experiment directory')
if(mode=='all') {
 dir.create(out,recursive=TRUE)
 # Two repetitions, rotated mode order; independent R processes. No science rerun.
 plan<-data.frame(repetition=rep(1:2,each=3),mode=c('rowwise','inmemory_bulk','parquet','parquet','rowwise','inmemory_bulk'))
 write.csv(plan,file.path(out,'plan.csv'),row.names=FALSE)
 for(i in seq_len(nrow(plan))) {
  case<-file.path(out,paste0(plan$mode[i],'-',plan$repetition[i]))
  case_start<-proc.time()[3]
  status<-system2(file.path(R.home('bin'),'Rscript'),vapply(c(script,run_dir,case,plan$mode[i]),shQuote,''))
  plan$external_wall_seconds[i]<-unname(proc.time()[3]-case_start)
  write.csv(plan,file.path(out,'plan.csv'),row.names=FALSE)
  if(status!=0)stop('Experiment failed; inspect ',case)
 }
 paths<-list.files(out,pattern='^experiment-receipt.json$',recursive=TRUE,full.names=TRUE)
 rows<-lapply(paths,function(p){x<-jsonlite::read_json(p);data.frame(mode=x$mode,records=x$records,
   worker_seconds=x$total_seconds_before_report,asset_verification_seconds=x$asset_verification_seconds,
   value_load_seconds=x$value_load_seconds,parquet_export_seconds=x$parquet_export_seconds,
   parquet_validation_seconds=x$parquet_validation_seconds,db_checks_seconds=x$db_checks_seconds,
   filtered_query_seconds=x$filtered_query_seconds,parquet_bytes=x$parquet_bytes,
   same_columns_CSV_bytes=x$same_columns_CSV_bytes,main_R_peak_RSS_MiB=x$main_R_peak_RSS_MiB,
   SQL_checks=x$SQL_checks,maximum_database_difference=x$maximum_database_difference,
   maximum_parquet_numeric_difference=if(is.null(x$maximum_parquet_numeric_difference))NA_real_ else x$maximum_parquet_numeric_difference,receipt=p)})
 write.csv(do.call(rbind,rows),file.path(out,'comparison.csv'),row.names=FALSE)
 cat('Storage comparison:',normalizePath(file.path(out,'comparison.csv')),'\n')
 quit(save='no',status=0)
}
worker_start<-unname(proc.time()[3])
for(p in c('DBI','duckdb','jsonlite','digest'))if(!requireNamespace(p,quietly=TRUE))stop('Missing optional experiment dependency: ',p)
dir.create(out,recursive=TRUE);out<-normalizePath(out)
env<-new.env(parent=globalenv());sys.source(file.path(root,'R/performance.R'),env)
perf<-env$performance_monitor(root,paste0('storage-',mode));perf$attach(out)
main<-function(){
 on.exit(perf$finish(),add=TRUE)
 engine<-'duckdb';sha<-function(p)digest::digest(file=p,algo='sha256')
 perf$stage('verify_accepted_assets');t<-proc.time()[3]
 receipt<-jsonlite::read_json(file.path(run_dir,'acceptance.json'))
 path<-file.path(run_dir,'schema-instance.json');provenance<-jsonlite::read_json(file.path(root,'schema/PROVENANCE.json'))
 stopifnot(isTRUE(receipt$accepted),sha(path)==receipt$instance_sha256,
           sha(file.path(root,'schema/amadeus.schema.json'))==receipt$schema_sha256)
 doc<-jsonlite::read_json(path)
 for(asset in doc$assets)stopifnot(file.exists(asset$local_path),sha(asset$local_path)==asset$sha256,file.info(asset$local_path)$size==asset$size_bytes)
 for(n in c('amadeus_duckdb.sql','integrity.sql','contract.json'))stopifnot(sha(file.path(root,'schema',n))==provenance$files[[n]])
 mapping<-jsonlite::read_json(file.path(root,'parquet/value-storage-contract.json'))
 stopifnot(mapping$SQL_sha256==provenance$files$amadeus_duckdb.sql,mapping$schema_sha256==receipt$schema_sha256)
 asset_time<-unname(proc.time()[3]-t)
 request<-readRDS(file.path(run_dir,'request.rds'));before<-readRDS(file.path(run_dir,'source-existed-before.rds'))
 perf$context(request,before,'duckdb')
 contract<-jsonlite::read_json(file.path(root,'schema/contract.json'))
 perf$stage('database_prepare')
 con<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(out,'gridmet.duckdb'))
 on.exit(if(DBI::dbIsValid(con)){try(DBI::dbRollback(con),silent=TRUE);DBI::dbDisconnect(con,shutdown=TRUE)},add=TRUE)
 execute<-function(sql,params=NULL)if(is.null(params))DBI::dbExecute(con,sql)else DBI::dbExecute(con,sql,params=params)
 fetch<-function(sql)DBI::dbGetQuery(con,sql)
statements <- function(p) {
  text <- gsub(
    '--[^\n]*',
    '',
    paste(readLines(p, warn = FALSE), collapse = '\n')
  )
  Filter(nzchar, trimws(strsplit(text, ';', fixed = TRUE)[[1]]))
}
quote_name <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
literal <- function(x) {
  if (is.null(x) || length(x) != 1 || is.list(x) || is.na(x)) {
    stop('Unexpected SQL scalar')
  }
  if (is.logical(x)) {
    return(if (x) 'TRUE' else 'FALSE')
  }
  if (is.numeric(x)) {
    if (!is.finite(x)) {
      stop('Nonfinite SQL value')
    }
    return(format(x, digits = 17, scientific = FALSE, trim = TRUE))
  }
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

ddl <- Filter(
  function(s) grepl('^CREATE TABLE', s, ignore.case = TRUE),
  statements(file.path(root, 'schema', paste0('amadeus_', engine, '.sql')))
)
table_name <- function(sql) strsplit(sql, '"', fixed = TRUE)[[1]][2]
tables <- vapply(ddl, table_name, '')
dependencies <- setNames(
  lapply(seq_along(ddl), function(i) {
    refs <- regmatches(
      ddl[i],
      gregexpr(
        'REFERENCES[[:space:]]+"?[A-Za-z][A-Za-z0-9_]*"?',
        ddl[i],
        perl = TRUE
      )
    )[[1]]
    setdiff(gsub('"', '', sub('^REFERENCES[[:space:]]+', '', refs)), tables[i])
  }),
  tables
)
order <- character()
left <- tables
while (length(left)) {
  ready <- left[vapply(
    left,
    function(t) all(dependencies[[t]] %in% order),
    logical(1)
  )]
  if (!length(ready)) {
    stop('Unsupported cyclic DDL dependencies')
  }
  order <- c(order, ready)
  left <- setdiff(left, ready)
}

 DBI::dbBegin(con)
 for(sql in ddl)execute(sql)
    cols <- fetch(
      "SELECT table_name,column_name,ordinal_position FROM information_schema.columns WHERE table_schema NOT IN ('information_schema','pg_catalog') ORDER BY table_name,ordinal_position"
    )
    rows <- setNames(vector('list', length(tables)), tables)
    for (collection in names(contract$collections)) {
      cls <- contract$collections[[collection]]
      for (record in doc[[collection]]) {
        scalar <- list()
        for (field in names(record)) {
          value <- record[[field]]
          if (is.null(value)) {
            next
          }
          if (is.list(value)) {
            junction <- paste(cls, field, sep = '_')
            if (!junction %in% tables) {
              stop('Unsupported inline/multivalued mapping: ', cls, '.', field)
            }
            columns <- cols$column_name[cols$table_name == junction]
            parent <- paste0(cls, '_id')
            child <- setdiff(columns, parent)
            if (length(child) != 1 || !parent %in% columns) {
              stop('Ambiguous junction mapping')
            }
            for (v in value) {
              rows[[junction]] <- c(
                rows[[junction]],
                list(setNames(list(record$id, v), c(parent, child)))
              )
            }
          } else {
            if (!field %in% cols$column_name[cols$table_name == cls]) {
              stop('Unknown scalar SQL column: ', cls, '.', field)
            }
            scalar[[field]] <- value
          }
        }
        rows[[cls]] <- c(rows[[cls]], list(scalar))
      }
    }

 perf$stage('value_table_mapping')
 cls<-'AmbientValueAtLocation';fields<-vapply(mapping$columns,function(x)x$column_name,'')
 stage_types<-setNames(vapply(mapping$columns,function(x)x$parquet_stage_type,''),fields)
 nullable<-setNames(vapply(mapping$columns,function(x)x$is_nullable=='YES',logical(1)),fields)
 # Start with the generated table's typed empty template, then preserve accepted numbers in R doubles.
 template<-fetch('SELECT * FROM "AmbientValueAtLocation" WHERE FALSE')
 stopifnot(identical(names(template),fields))
 n<-length(rows[[cls]])
 value_df<-as.data.frame(lapply(template,function(x)x[rep(NA_integer_,n)]),check.names=FALSE)
 for(i in seq_len(n))for(field in names(rows[[cls]][[i]])){
  v<-rows[[cls]][[i]][[field]]
  if(inherits(value_df[[field]],'POSIXct'))v<-as.POSIXct(v,format='%Y-%m-%dT%H:%M:%SZ',tz='UTC')
  if(inherits(value_df[[field]],'Date'))v<-as.Date(v)
  value_df[[field]][i]<-v
 }
 validate_values<-function(actual,types=NULL){
  if(!identical(names(actual),fields))stop('Parquet column contract mismatch')
  if(!is.null(types)&&!identical(unname(types),unname(stage_types)))stop('Parquet physical type mismatch')
  if(nrow(actual)!=n||anyDuplicated(actual$id)||!setequal(actual$id,value_df$id))stop('Parquet key/count mismatch')
  for(f in fields[!nullable])if(anyNA(actual[[f]]))stop('Parquet required value absent: ',f)
  if(any(!is.finite(actual$value))||any(!is.finite(actual$native_value)))stop('Parquet nonfinite value')
  actual<-actual[match(value_df$id,actual$id),,drop=FALSE];rownames(actual)<-NULL
  if(!isTRUE(all.equal(value_df,actual,tolerance=0,check.attributes=FALSE)))stop('Parquet records differ from accepted records')
  invisible(TRUE)
 }
 validate_values(value_df)
 csv_bytes<-NA_real_
 if(!operational){
  write.csv(value_df,file.path(out,'same-columns-values.csv'),row.names=FALSE)
  csv_bytes<-file.info(file.path(out,'same-columns-values.csv'))$size
 }
 inserted<-0L;load_time<-0;export_time<-0;readback_time<-0;parquet_bytes<-0;parquet_difference<-NA_real_;negative<-NULL
 list_columns<-paste(quote_name(fields),collapse=',')
 qfile<-function(p)paste0("'",gsub("'","''",normalizePath(p,mustWork=FALSE),fixed=TRUE),"'")
 # All routes use the same metadata/junction mapping and the same target DDL.
 for(table in order){
  if(table!=cls||mode=='rowwise'){
   perf$stage(if(table==cls)'value_load'else'metadata_load')
   t<-proc.time()[3]
   for(row in rows[[table]]){
    sql<-paste0('INSERT INTO ',quote_name(table),' (',paste(quote_name(names(row)),collapse=','),') VALUES (',paste(rep('?',length(row)),collapse=','),')')
    execute(sql,unname(row));inserted<-inserted+1L
   }
   if(table==cls)load_time<-unname(proc.time()[3]-t)
  }else if(mode=='inmemory_bulk'){
   perf$stage('value_load');t<-proc.time()[3]
   DBI::dbAppendTable(con,cls,value_df);load_time<-unname(proc.time()[3]-t);inserted<-inserted+n
  }else{
   perf$stage('parquet_export');t<-proc.time()[3]
   DBI::dbWriteTable(con,'_parquet_values_stage',value_df,temporary=TRUE,field.types=stage_types)
   parquet<-file.path(out,'accepted-values.parquet')
   execute(paste0('COPY (SELECT ',list_columns,' FROM _parquet_values_stage) TO ',qfile(parquet),' (FORMAT PARQUET, COMPRESSION ZSTD)'))
   export_time<-unname(proc.time()[3]-t);parquet_bytes<-file.info(parquet)$size
   perf$stage('parquet_readback_validation');t<-proc.time()[3]
   actual<-fetch(paste0('SELECT * FROM read_parquet(',qfile(parquet),')'))
   actual_types<-fetch(paste0('DESCRIBE SELECT * FROM read_parquet(',qfile(parquet),')'))$column_type
   validate_values(actual,actual_types)
   parquet_difference<-max(abs(actual$value-value_df$value))
   readback_time<-unname(proc.time()[3]-t)
   perf$stage('value_load');t<-proc.time()[3]
   execute(paste0('INSERT INTO ',quote_name(cls),' (',list_columns,') SELECT ',list_columns,' FROM read_parquet(',qfile(parquet),')'))
   load_time<-unname(proc.time()[3]-t);inserted<-inserted+n
  }
 }
 perf$stage('SQL_and_fidelity_checks');t<-proc.time()[3]
 checks<-statements(file.path(root,'schema/integrity.sql'))
 for(sql in checks)if(as.numeric(fetch(sql)[1,1])!=0)stop('SQL integrity violation: ',sql)
 query<-paste('SELECT v.id,p.amadeus_variable_code AS variable,l.location_key AS site_id,CAST(v.valid_time_start AS VARCHAR) AS date_label,v.value,v.value_unit_ucum,v.native_value,v.asset,v.processing_run FROM "AmbientValueAtLocation" v JOIN "ProductVariable" p ON v.product_variable=p.id JOIN "Location" l ON v.location=l.id ORDER BY v.id')
 actual<-fetch(query);expected<-doc$ambient_values_at_location;ix<-match(actual$id,vapply(expected,function(x)x$id,''))
 if(nrow(actual)!=n||anyNA(ix)||anyDuplicated(actual$id))stop('Database identity mismatch')
 registry<-read.csv(file.path(root,'inputs/variables.csv'));tol<-registry$database_tolerance[match(actual$variable,registry$code)]
 error<-abs(actual$value-vapply(expected[ix],function(v)v$value,0))
 if(any(error>tol)||any(actual$value_unit_ucum!=vapply(expected[ix],function(v)v$value_unit_ucum,'')))stop('Database numeric/unit mismatch')
 db_time<-unname(proc.time()[3]-t)
 query_time<-0
 if(!operational){
 perf$stage('filtered_query');t<-proc.time()[3]
 filtered<-fetch(paste0('SELECT id,value FROM "AmbientValueAtLocation" WHERE product_variable=',literal(value_df$product_variable[1]),' AND location=',literal(value_df$location[1]),' ORDER BY id'))
 query_time<-unname(proc.time()[3]-t)
 if(mode=='parquet'){
  # Direct file query joins the already loaded metadata tables; it does not inherit target constraints.
  pq_query<-sub('FROM "AmbientValueAtLocation" v',paste0('FROM read_parquet(',qfile(parquet),') v'),query,fixed=TRUE)
  direct<-fetch(pq_query);stopifnot(identical(actual$id,direct$id),all(abs(actual$value-direct$value)<=tol))
  write.csv(direct,file.path(out,'direct-parquet-results.csv'),row.names=FALSE)
  writeLines(pq_query,file.path(out,'direct-parquet-query.sql'))
 }
 }
 if(mode=='parquet'){
  # Negative fixtures are opt-in qualification, outside the default repeated comparison.
  if(!operational && Sys.getenv('GRIDMET_PARQUET_QA','0')=='1') {
  perf$stage('separate_negative_QA')
  fixtures<-list(missing_column=value_df[,setdiff(fields,'value_unit_ucum'),drop=FALSE],
    duplicate_key=rbind(value_df,value_df[1,,drop=FALSE]),nonfinite=value_df,wrong_unit=value_df,wrong_reference=value_df,wrong_type=value_df)
  fixtures$nonfinite$value[1]<-NaN;fixtures$wrong_unit$value_unit_ucum[1]<-'invalid-unit'
  fixtures$wrong_reference$processing_run[1]<-'different-run'
  fixtures$wrong_type$value<-as.character(fixtures$wrong_type$value)
  tests<-list()
  for(name in names(fixtures)){
   temp<-file.path(out,paste0('invalid-',name,'.parquet'))
   bad_types<-stage_types[names(fixtures[[name]])]
   if(name=='wrong_type')bad_types['value']<-'VARCHAR'
   DBI::dbWriteTable(con,'_invalid_values',fixtures[[name]],temporary=TRUE,overwrite=TRUE,field.types=bad_types)
   execute(paste0('COPY _invalid_values TO ',qfile(temp),' (FORMAT PARQUET, COMPRESSION ZSTD)'))
   bad<-fetch(paste0('SELECT * FROM read_parquet(',qfile(temp),')'))
   bad_types_actual<-fetch(paste0('DESCRIBE SELECT * FROM read_parquet(',qfile(temp),')'))$column_type
   why<-tryCatch({validate_values(bad,bad_types_actual);NULL},error=conditionMessage)
   if(is.null(why))stop('Invalid Parquet fixture unexpectedly accepted: ',name)
   tests[[name]]<-list(rejected=TRUE,reason=why)
  }
  negative<-tests
  jsonlite::write_json(tests,file.path(out,'parquet-negative-tests.json'),auto_unbox=TRUE,pretty=TRUE)
  }
 }
 # Check before commit in this experiment; original adapter publication behavior is unchanged.
 engine_version <- as.character(fetch('SELECT version()')[[1]])
 DBI::dbCommit(con)
 write.csv(actual,file.path(out,'results.csv'),row.names=FALSE)
 DBI::dbDisconnect(con,shutdown=TRUE)
 perf$stage('persistent_reopen')
 reopened<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(out,'gridmet.duckdb'),read_only=TRUE)
 stopifnot(DBI::dbGetQuery(reopened,'SELECT COUNT(*) n FROM "AmbientValueAtLocation"')$n==n)
 DBI::dbDisconnect(reopened,shutdown=TRUE)
 stopifnot(sha(path)==receipt$instance_sha256)
 hwm<-if(file.exists('/proc/self/status'))grep('^VmHWM:',readLines('/proc/self/status'),value=TRUE)else character()
 report<-list(experiment_helper_sha256=sha(script),mode=mode,source_run=run_dir,source_schema_accepted=TRUE,source_instance_sha256=receipt$instance_sha256,
 parquet_sha256=if(mode=='parquet')sha(parquet)else NULL,
 storage_mapping_revision=mapping$mapping_revision,storage_mapping_sha256=sha(file.path(root,'parquet/value-storage-contract.json')),
 records=n,inserted_rows=inserted,SQL_checks=length(checks),all_within_declared_tolerances=TRUE,persistent_reopen_passed=TRUE,
 maximum_database_difference=max(error),maximum_parquet_numeric_difference=parquet_difference,
 asset_verification_seconds=asset_time,value_load_seconds=load_time,parquet_export_seconds=export_time,
 parquet_validation_seconds=readback_time,db_checks_seconds=db_time,filtered_query_seconds=query_time,
 parquet_bytes=parquet_bytes,same_columns_CSV_bytes=csv_bytes,
 main_R_peak_RSS_MiB=if(length(hwm))as.numeric(gsub('[^0-9]','',hwm))/1024 else NA_real_,
 total_seconds_before_report=unname(proc.time()[3])-worker_start,
 total_scope='Worker from dependency loading to report; includes mapping, metadata, checks, queries and reopen; parent plan.csv also records external process wall time',
 negative_tests_passed=if(is.null(negative))0L else length(negative),
 profile='Accepted saved records; DuckDB core/WKT only; no science rerun or Parquet replacement of primary schema acceptance',
 production_approved=FALSE)
 if(!operational)jsonlite::write_json(report,file.path(out,'experiment-receipt.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA,na='null')
 write.csv(cols, file.path(out, 'columns.csv'), row.names = FALSE)
 writeLines(query, file.path(out, 'results.sql'))
 jsonlite::write_json(list(engine = 'duckdb', engine_version = engine_version, records = n,
   inserted_rows = inserted, integrity_queries = length(checks), all_within_declared_tolerances = TRUE,
   maximum_numeric_storage_difference = max(error), accepted_instance_sha256 = receipt$instance_sha256,
   persistent_reopen_passed = TRUE, storage_route = mode, execution_profile = profile,
   parquet_sha256 = if(mode=='parquet')sha(parquet)else NULL,
   storage_mapping_revision = mapping$mapping_revision,
   storage_mapping_sha256 = sha(file.path(root, 'parquet/value-storage-contract.json')),
   profile = 'Core SQL/WKT', production_approved = FALSE),
   file.path(out, 'receipt.json'), auto_unbox = TRUE, pretty = TRUE, digits = NA)
 perf$succeed()
 cat(mode,':',n,'values;',length(checks),'SQL checks passed\n')
}
withCallingHandlers(main(),error=function(e)perf$failure(conditionMessage(e)))
