# Post-measurement audit. Never run concurrently with timed cases.
b<-dirname(dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))))
obs<-data.frame(case=1:2,arm='candidate',exit_status=0,run=vapply(c('final-small','final-full'),function(x)readLines(file.path(b,'outputs',x,'LATEST.txt'),n=1),''))
documents<-lapply(obs$run,function(run)jsonlite::read_json(file.path(run,'schema-instance.json')))
source_paths<-unique(unlist(lapply(documents,function(doc)vapply(Filter(function(x)identical(x$native_format,'netcdf4_cf'),doc$assets),function(x)x$local_path,''))))
source_manifest<-data.frame(path=normalizePath(source_paths),sha256=vapply(source_paths,function(p)digest::digest(file=p,algo='sha256'),''),stringsAsFactors=FALSE)
sha<-function(p)digest::digest(file=p,algo='sha256')
encode<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,digits=NA,null='null'))
rows<-list()
obs<-obs[obs$arm!='ordinary',]
for(i in seq_len(nrow(obs))){
 row<-obs[i,];run<-row$run
 if(row$exit_status!=0||!nzchar(run)){rows[[i]]<-data.frame(case=row$case,arm=row$arm,assets_verified=FALSE,call_metadata_match=FALSE,ordinary_RDS_match=FALSE,recording_copy_matches=FALSE,maximum_RDS_CSV_difference=NA);next}
 doc<-jsonlite::read_json(file.path(run,'schema-instance.json'));calls<-readRDS(file.path(run,'calls.rds'));results<-read.csv(file.path(run,'results.csv'),stringsAsFactors=FALSE)
 assets_ok<-TRUE;call_ok<-TRUE;ordinary_ok<-TRUE;maxdiff<-0
 for(asset in doc$assets){
  n<-match(asset$local_path,source_manifest$path)
  actual<-if(is.na(n))sha(asset$local_path)else source_manifest$sha256[n]
  if(actual!=asset$sha256||file.info(asset$local_path)$size!=asset$size_bytes)assets_ok<-FALSE
 }
 for(call in calls){
  tool<-Filter(function(x)endsWith(x$id,paste0('/run/',call$key)),doc$tool_runs)
  if(length(tool)!=1){call_ok<-FALSE;next};tool<-tool[[1]]
  if(tool$function_name!=call$function_name||tool$run_role!=call$role||tool$tool_version!=as.character(packageVersion('amadeus')))call_ok<-FALSE
  args<-jsonlite::fromJSON(tool$run_arguments,simplifyVector=FALSE)
  for(n in names(call$arguments))if(!identical(encode(args[[n]]),encode(call$arguments[[n]])))call_ok<-FALSE
  if(tool$run_timestamp_utc!=format(call$started,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')||abs(tool$run_duration_seconds-as.numeric(difftime(call$ended,call$started,units='secs')))>1e-8)call_ok<-FALSE
  if(call$role=='calculate'){
   ordinary<-as.data.frame(readRDS(call$output));tab<-results[results$call_key==call$key,];code<-tab$variable[1]
   col<-grep(paste0('^',code,'(_|$)'),names(ordinary),value=TRUE);ix<-match(paste(tab$site_id,tab$date),paste(ordinary$site_id,as.character(as.Date(ordinary$time))))
   if(length(col)!=1||anyNA(ix)||tool$output_row_count!=nrow(ordinary))ordinary_ok<-FALSE
   else {delta<-max(abs(tab$value-ordinary[[col]][ix]));maxdiff<-max(maxdiff,delta);if(!is.finite(delta)||delta>1e-10)ordinary_ok<-FALSE}
  }
 }
 recording_ok<-NA
 if(row$arm=='candidate'){
  rec<-jsonlite::read_json(file.path(run,'workflow-receipt.json'))$direct_output_context
  recording_ok<-sha(file.path(run,'provenance/seal.json'))==rec$recording_seal_sha256
  seal<-jsonlite::read_json(file.path(run,'provenance/seal.json'))
  recording_ok<-recording_ok&&sha(file.path(run,'provenance/events.json'))==seal$events_sha256&&sha(file.path(run,'provenance/execution.rds'))==seal$execution_sha256
 }
 con<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(run,'database/duckdb/gridmet.duckdb'),read_only=TRUE)
 stored<-DBI::dbGetQuery(con,'SELECT id,value,native_value,value_unit_ucum,native_value_unit_ucum FROM "AmbientValueAtLocation"')
 DBI::dbDisconnect(con,shutdown=TRUE)
 value_records<-setNames(doc$ambient_values_at_location,vapply(doc$ambient_values_at_location,function(x)x$id,''))
 pvs<-setNames(doc$product_variables,vapply(doc$product_variables,function(x)x$id,''))
 registry<-read.csv(file.path(b,'integration/inputs/variables.csv'),stringsAsFactors=FALSE)
 db_ok<-nrow(stored)==length(value_records);dbdiff<-0
 for(j in seq_len(nrow(stored))){
   expected<-value_records[[stored$id[j]]]
   if(is.null(expected)){db_ok<-FALSE;next}
   code<-pvs[[expected$product_variable]]$amadeus_variable_code
   tolerance<-registry$database_tolerance[match(code,registry$code)]
   delta<-max(abs(c(stored$value[j]-expected$value,stored$native_value[j]-expected$native_value)))
   dbdiff<-max(dbdiff,delta)
   if(!is.finite(delta)||delta>tolerance||stored$value_unit_ucum[j]!=expected$value_unit_ucum||stored$native_value_unit_ucum[j]!=expected$native_value_unit_ucum)db_ok<-FALSE
 }
 rows[[i]]<-data.frame(case=row$case,arm=row$arm,assets_verified=assets_ok,call_metadata_match=call_ok,ordinary_RDS_match=ordinary_ok,recording_copy_matches=recording_ok,maximum_RDS_CSV_difference=maxdiff,actual_saved_database_values_match=db_ok,maximum_saved_database_difference=dbdiff)
}
write.csv(do.call(rbind,rows),file.path(b,'reports/case-audits.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(b,'reports/audit-session.txt'))
