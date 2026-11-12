a<-commandArgs(TRUE);stopifnot(length(a)==1L);output<-a[1];dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
packages<-c('amadeus','terra','sf','ncdf4','jsonvalidate','jsonlite','yaml','digest','DBI','duckdb')
versions<-setNames(lapply(packages,function(p)tryCatch(as.character(packageVersion(p)),error=function(e)NULL)),packages)
functions<-c('download_data','process_covariates','calculate_covariates','download_gridmet','process_gridmet','calculate_gridmet','process_gridmet_codes','process_variable_codes')
hashes<-setNames(lapply(functions,function(n)tryCatch(digest::digest(paste(deparse(body(getExportedValue('amadeus',n))),collapse='\n'),algo='sha256',serialize=FALSE),error=function(e)NULL)),functions)
description<-tryCatch(as.list(utils::packageDescription('amadeus')),error=function(e)list())
provenance<-description[intersect(names(description),c('Package','Version','Built','RemoteType','RemoteHost','RemoteRepo','RemoteUsername','RemoteSha','GithubSHA1'))]
jsonlite::write_json(list(R=R.version.string,platform=R.version$platform,system=as.list(Sys.info()),timezone=Sys.getenv('TZ'),packages=versions,amadeus_install=provenance,public_function_body_sha256=hashes,
 terra_libraries=tryCatch(terra::gdal(lib='all'),error=function(e)NULL),sf_libraries=tryCatch(sf::sf_extSoftVersion(),error=function(e)NULL),scope='Observed environment; not transitive lockfile or independently reproduced stack'),output,auto_unbox=TRUE,pretty=TRUE,null='null')
