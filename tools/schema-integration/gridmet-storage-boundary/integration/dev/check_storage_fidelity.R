# Separate qualification: all 32 columns against accepted source records.
a<-commandArgs(TRUE)
if(length(a)!=2L)stop('Usage: Rscript dev/check_storage_fidelity.R EXPERIMENT_ROOT REPORT.csv')
paths<-list.files(a[1],pattern='^experiment-receipt.json$',recursive=TRUE,full.names=TRUE)
report<-list()
for(path in paths){
 r<-jsonlite::read_json(path);case<-dirname(path)
 doc<-jsonlite::read_json(file.path(r$source_run,'schema-instance.json'))
 stopifnot(digest::digest(file=file.path(r$source_run,'schema-instance.json'),algo='sha256')==r$source_instance_sha256)
 con<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(case,'gridmet.duckdb'),read_only=TRUE)
 x<-DBI::dbGetQuery(con,'SELECT * FROM "AmbientValueAtLocation" ORDER BY id')
 types<-DBI::dbGetQuery(con,"SELECT column_name,data_type FROM information_schema.columns WHERE table_name='AmbientValueAtLocation'")
 source<-doc$ambient_values_at_location;ix<-match(x$id,vapply(source,function(v)v$id,''));stopifnot(!anyNA(ix),length(ix)==length(source),!anyDuplicated(x$id))
 source<-source[ix]
 for(field in names(x)){
  expected<-lapply(source,function(v)v[[field]])
  absent<-vapply(expected,is.null,logical(1));stopifnot(all(is.na(x[[field]][absent])))
  present<-!absent
  if(any(present)){
   actual<-x[[field]][present];wanted<-unlist(expected[present],use.names=FALSE)
   if(inherits(actual,'POSIXct'))stopifnot(identical(format(actual,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),wanted))
   else if(is.numeric(actual)){
    tolerance<-if(types$data_type[match(field,types$column_name)]%in%c('FLOAT','REAL','DOUBLE'))1e-4 else 0
    stopifnot(all(is.finite(actual)),all(abs(actual-as.numeric(wanted))<=tolerance))
   }else stopifnot(identical(as.character(actual),as.character(wanted)))
  }
 }
 DBI::dbDisconnect(con,shutdown=TRUE)
 report[[length(report)+1L]]<-data.frame(case=case,mode=r$mode,records=nrow(x),columns_checked=ncol(x),all_passed=TRUE)
}
write.csv(do.call(rbind,report),a[2],row.names=FALSE)
cat('All-column database fidelity passed for',length(report),'cases\n')
