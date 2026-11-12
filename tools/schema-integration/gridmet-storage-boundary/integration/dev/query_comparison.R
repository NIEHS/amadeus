# Development query benchmark only; direct DBI/Parquet reads do not certify a publication. For verified published queries use R/database.R RUN duckdb SQL.
# Post-load query experiment; warm repetitions, not an OS-cache-cold benchmark.
a<-commandArgs(TRUE)
if(length(a)!=1L)stop('Usage: Rscript dev/query_comparison.R PARQUET_CASE_DIRECTORY')
case<-normalizePath(a[1]);r<-jsonlite::read_json(file.path(case,'experiment-receipt.json'))
stopifnot(r$mode=='parquet',digest::digest(file=file.path(case,'accepted-values.parquet'),algo='sha256')==r$parquet_sha256)
con<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(case,'gridmet.duckdb'),read_only=TRUE)
sql_string<-function(x)as.character(DBI::dbQuoteString(con,as.character(x)))
p<-sql_string(normalizePath(file.path(case,'accepted-values.parquet')))
one<-DBI::dbGetQuery(con,'SELECT product_variable,location FROM "AmbientValueAtLocation" ORDER BY id LIMIT 1')
base<-paste0('SELECT id,value,value_unit_ucum,valid_time_start FROM %s WHERE product_variable=',sql_string(one$product_variable),' AND location=',sql_string(one$location),' ORDER BY id')
queries<-list(database=sprintf(base,'"AmbientValueAtLocation"'),parquet=sprintf(base,paste0('read_parquet(',p,')')))
# Compression control: equivalent-column gzipped CSV, generated outside loading timings.
csv_path<-file.path(case,'same-columns-values.csv');gz_path<-paste0(csv_path,'.gz')
input<-file(csv_path,'rb');output<-gzfile(gz_path,'wb',compression=6)
repeat {chunk<-readBin(input,'raw',n=65536L);if(!length(chunk))break;writeBin(chunk,output)}
close(input);close(output)
fields<-DBI::dbGetQuery(con,"SELECT column_name,data_type FROM information_schema.columns WHERE table_name='AmbientValueAtLocation' ORDER BY ordinal_position")
fields$data_type[fields$data_type%in%c('REAL','FLOAT','DOUBLE')]<-'DOUBLE'
columns<-paste(vapply(seq_len(nrow(fields)),function(i)paste0(sql_string(fields$column_name[i]),':',sql_string(fields$data_type[i])),''),collapse=',')
csv_source<-paste0('read_csv(',sql_string(normalizePath(gz_path)),", header=true, nullstr='NA', columns={",columns,'})')
queries$gzip_csv<-sprintf(base,csv_source)
results<-list();times<-list()
for(i in seq_len(8))for(kind in if(i%%2L)c('database','parquet','gzip_csv')else c('gzip_csv','parquet','database')){
 t<-Sys.time();answer<-DBI::dbGetQuery(con,queries[[kind]])
 times[[length(times)+1L]]<-data.frame(repetition=i,kind=kind,elapsed_seconds=as.numeric(difftime(Sys.time(),t,units='secs')),rows=nrow(answer))
 results[[kind]]<-answer
}
stopifnot(identical(results$database$id,results$parquet$id),identical(results$database$value_unit_ucum,results$parquet$value_unit_ucum),all(abs(results$database$value-results$parquet$value)<=1e-4))
stopifnot(identical(results$database$id,results$gzip_csv$id),identical(results$database$value_unit_ucum,results$gzip_csv$value_unit_ucum),all(abs(results$database$value-results$gzip_csv$value)<=1e-4))
write.csv(do.call(rbind,times),file.path(case,'query-comparison.csv'),row.names=FALSE)
for(kind in names(queries)){
 writeLines(queries[[kind]],file.path(case,paste0('filtered-',kind,'.sql')))
 explain<-DBI::dbGetQuery(con,paste('EXPLAIN ANALYZE',queries[[kind]]))
 writeLines(explain$explain_value,file.path(case,paste0('filtered-',kind,'-explain.txt')))
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('Eight interleaved query repetitions per database/Parquet/gzip-CSV route passed; no cold-cache claim:',case,'\n')
