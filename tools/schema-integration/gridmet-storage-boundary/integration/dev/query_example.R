# Development query benchmark only; direct DBI/Parquet reads do not certify a publication. For verified published queries use R/database.R RUN duckdb SQL.
a<-commandArgs(TRUE)
if(length(a)!=1L)stop('Usage: Rscript dev/query_example.R EXAMPLE_DIRECTORY')
p<-normalizePath(a[1]);con<-DBI::dbConnect(duckdb::duckdb(),dbdir=file.path(p,'gridmet.duckdb'),read_only=TRUE)
file<-as.character(DBI::dbQuoteString(con,normalizePath(file.path(p,'accepted-values.parquet'))))
sql<-paste0('SELECT p.amadeus_variable_code AS variable,l.location_key AS site_id,v.valid_time_start,v.value,v.value_unit_ucum FROM read_parquet(',file,') v JOIN "ProductVariable" p ON v.product_variable=p.id JOIN "Location" l ON v.location=l.id ORDER BY variable,site_id,v.valid_time_start LIMIT 20')
print(DBI::dbGetQuery(con,sql))
print(DBI::dbGetQuery(con,paste0('DESCRIBE SELECT * FROM read_parquet(',file,')')))
DBI::dbDisconnect(con,shutdown=TRUE)
