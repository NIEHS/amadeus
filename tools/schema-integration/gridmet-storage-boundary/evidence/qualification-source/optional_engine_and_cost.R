b<-dirname(dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))));root<-file.path(b,'integration');run<-readLines(file.path(b,'outputs/final-small/LATEST.txt'),n=1)
source(file.path(root,'R/provenance_recorder.R'));source(file.path(root,'R/export_gridmet.R'));source(file.path(root,'R/storage_boundary.R'))
core<-readRDS(file.path(run,'provenance/execution.rds'));execution<-c(core,list(recording=list(directory=file.path(run,'provenance'),seal_sha256=digest::digest(file=file.path(run,'provenance/seal.json'),algo='sha256'))))
request<-readRDS(file.path(run,'request.rds'));calls<-readRDS(file.path(run,'calls.rds'));calc<-Filter(function(c)c$role=='calculate',calls);proc<-Filter(function(c)c$role=='process',calls)
calculated<-setNames(lapply(calc,function(c)readRDS(c$output)),vapply(calc,function(c)c$key,''));processed<-setNames(lapply(proc,function(c)c$output),vapply(proc,function(c)c$key,''))
x<-export_gridmet(calculated,processed,request,execution,file.path(b,'outputs/schema-only'),engine='none',storage='rowwise',root=root)
wf<-jsonlite::read_json(file.path(x$run_dir,'workflow-receipt.json'));manifest<-jsonlite::read_json(file.path(x$run_dir,'publication-manifest.json'));stopifnot(isTRUE(wf$completed),wf$selected_engine=='none',length(manifest$engines)==0L,!dir.exists(file.path(x$run_dir,'database')))
msg<-tryCatch({storage_verify_publication(root,x$run_dir);''},error=conditionMessage);stopifnot(grepl('No completed DuckDB publication',msg))
jsonlite::write_json(list(schema_only_still_supported=TRUE,no_database_created=TRUE,saved_DB_query_verification_rejected=TRUE,scope='Re-export of actual recorded outputs; not new scientific execution'),file.path(b,'reports/schema-only-regression.json'),auto_unbox=TRUE,pretty=TRUE)
rows<-list()
for(w in c('final-small','final-full')){
 rr<-readLines(file.path(b,'outputs',w,'LATEST.txt'),n=1)
 for(rep in 1:3){v<-storage_verify_publication(root,rr);rows[[length(rows)+1]]<-data.frame(workload=w,repetition=rep,files=v$files,bytes=v$bytes,verification_seconds=v$verification_elapsed_seconds,scope='Warm-file-cache integrity verification only; excludes R startup, DB connect/query and science; no claim of full-workflow speed')}
}
write.csv(do.call(rbind,rows),file.path(b,'reports/store-open-verification-cost.csv'),row.names=FALSE)
