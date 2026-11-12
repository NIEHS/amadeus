# Optional independent reader qualification; Arrow is NOT an experiment dependency.
a<-commandArgs(TRUE)
if(length(a)!=2L)stop('Usage: Rscript dev/check_arrow_reader.R EXPERIMENT_ROOT REPORT.csv')
if(!requireNamespace('arrow',quietly=TRUE))stop('Optional Arrow reader not installed; primary experiment does not require it')
paths<-list.files(a[1],pattern='^accepted-values.parquet$',recursive=TRUE,full.names=TRUE)
report<-list()
for(p in paths){
 con<-DBI::dbConnect(duckdb::duckdb())
 expected<-DBI::dbGetQuery(con,paste0('SELECT * FROM read_parquet(',as.character(DBI::dbQuoteString(con,normalizePath(p))),')'))
 x<-as.data.frame(arrow::read_parquet(p))
 # TIMESTAMP without timezone needs the declared nominal-UTC read/display convention.
 # Check underlying numbers BEFORE normalizing display; never shift or repair data.
 timestamp_fields<-names(x)[vapply(x,inherits,logical(1),'POSIXct')]
 defaults<-vapply(timestamp_fields,function(f)paste(attr(x[[f]],'tzone'),collapse='|'),'')
 for(f in timestamp_fields){
  stopifnot(identical(as.numeric(x[[f]]),as.numeric(expected[[f]])))
  attr(x[[f]],'tzone')<-'UTC'
 }
 stopifnot(identical(names(x),names(expected)),isTRUE(all.equal(x,expected,tolerance=0,check.attributes=FALSE)))
 DBI::dbDisconnect(con,shutdown=TRUE)
 report[[length(report)+1L]]<-data.frame(file=p,rows=nrow(x),exact_semantic_readback=TRUE,arrow_version=as.character(packageVersion('arrow')),timestamp_numbers_unchanged=TRUE,explicit_UTC_display=TRUE,reader_default_timezone=paste(unique(defaults),collapse='|'))
}
write.csv(do.call(rbind,report),a[2],row.names=FALSE)
cat('Independent Arrow readback passed for',length(report),'Parquet files\n')
