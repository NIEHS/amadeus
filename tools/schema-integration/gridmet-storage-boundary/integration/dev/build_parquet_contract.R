a<-commandArgs(TRUE);root<-normalizePath(a[1])
p<-jsonlite::read_json(file.path(root,'schema/PROVENANCE.json'))
ddlpath<-file.path(root,'schema/amadeus_duckdb.sql')
stopifnot(digest::digest(file=ddlpath,algo='sha256')==p$files$amadeus_duckdb.sql)
sql<-gsub('--[^\n]*','',paste(readLines(ddlpath),collapse='\n'))
ddl<-Filter(function(z)grepl('^CREATE TABLE',z,ignore.case=TRUE),trimws(strsplit(sql,';',fixed=TRUE)[[1]]))
con<-DBI::dbConnect(duckdb::duckdb())
for(z in ddl)DBI::dbExecute(con,z)
f<-DBI::dbGetQuery(con,"SELECT column_name,data_type,is_nullable FROM information_schema.columns WHERE table_name='AmbientValueAtLocation' ORDER BY ordinal_position")
f$parquet_stage_type<-ifelse(f$data_type%in%c('FLOAT','REAL','DOUBLE'),'DOUBLE',ifelse(grepl('VARCHAR',f$data_type),'VARCHAR',f$data_type))
stopifnot(all(f$parquet_stage_type%in%c('DOUBLE','INTEGER','BIGINT','VARCHAR','BOOLEAN','TIMESTAMP','DATE')))
jsonlite::write_json(list(mapping_revision='parquet-value-table-v1',class='AmbientValueAtLocation',
 schema_sha256=p$files$amadeus.schema.json,SQL_sha256=p$files$amadeus_duckdb.sql,
 note='Build-derived physical mapping; retain accepted numeric values as DOUBLE in Parquet even where target SQL uses FLOAT. No scientific conversion.',
 columns=f),file.path(root,'parquet/value-storage-contract.json'),auto_unbox=TRUE,pretty=TRUE,dataframe='rows')
DBI::dbDisconnect(con,shutdown=TRUE)
cat('Built physical mapping from pinned DDL:',nrow(f),'columns\n')
