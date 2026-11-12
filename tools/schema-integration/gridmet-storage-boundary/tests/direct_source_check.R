b<-dirname(dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))));root<-file.path(b,'integration');a<-commandArgs(TRUE);run<-if(length(a))normalizePath(a[1])else readLines(file.path(b,'outputs/final-full/LATEST.txt'),n=1);report_dir<-if(length(a)>=2L)a[2]else file.path(b,'reports');dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
results<-read.csv(file.path(run,'results.csv'),stringsAsFactors=FALSE);sites<-NULL;profile<-read.csv(file.path(root,'inputs/variables.csv'),stringsAsFactors=FALSE);metadata<-jsonlite::read_json(file.path(run,'source-metadata.json'));request<-readRDS(file.path(run,'request.rds'));sites<-request$locations
rows<-list();summaries<-list()
check_code<-function(code){
 source_path<-file.path(request$download_dir,code,paste0(code,'_',request$year,'.nc'));stopifnot(digest::digest(file=source_path,algo='sha256')==metadata[[code]]$sha256);nc<-ncdf4::nc_open(source_path);on.exit(ncdf4::nc_close(nc))
 var<-nc$var[[metadata[[code]]$source_variable]];stopifnot(!is.null(var));dims<-vapply(var$dim,function(x)x$name,'')
 ix<-which(dims%in%c('lon','longitude'));iy<-which(dims%in%c('lat','latitude'));it<-which(dims%in%c('day','time'));stopifnot(length(ix)==1L,length(iy)==1L,length(it)==1L,length(dims)==3L)
 dx<-var$dim[[ix]];dy<-var$dim[[iy]];dt<-var$dim[[it]]
 u<-dt$units;stopifnot(grepl('^(days|hours|seconds) since ',u))
 origin<-sub('^(days|hours|seconds) since ','',u);origin<-sub(' UTC$','',origin);origin<-sub('Z$','',origin);origin<-gsub('T',' ',origin,fixed=TRUE)
 base<-as.POSIXct(origin,tz='UTC',tryFormats=c('%Y-%m-%d %H:%M:%OS','%Y-%m-%d'));stopifnot(!is.na(base))
 calendar<-ncdf4::ncatt_get(nc,dt$name,'calendar');cal<-if(isTRUE(calendar$hasatt))calendar$value else 'standard (CF default)'
 stopifnot(cal%in%c('standard','gregorian','proleptic_gregorian','standard (CF default)'))
 factor<-if(startsWith(u,'days'))86400 else if(startsWith(u,'hours'))3600 else 1
 instants<-base+dt$vals*factor;labels<-as.character(as.Date(instants,tz='UTC'))
 lon<-dx$vals;if(all(lon>=0)&&max(lon)>180)lon<-((lon+180)%%360)-180
 tab<-results[results$variable==code,];tol<-profile$database_tolerance[match(code,profile$code)]
 for(j in seq_len(nrow(tab))){
  r<-tab[j,];site<-sites[match(r$site_id,sites$site_id),];ti<-which(labels==r$date);stopifnot(length(ti)==1L)
  xi<-which.min(abs(lon-site$lon));yi<-which.min(abs(dy$vals-site$lat));start<-rep(1L,3);start[ix]<-xi;start[iy]<-yi;start[it]<-ti
  v<-as.numeric(ncdf4::ncvar_get(nc,var$ name,start=start,count=rep(1L,3),raw_datavals=FALSE))
  delta<-abs(v-r$value)
  rows[[length(rows)+1]]<<-data.frame(variable=code,site_id=r$site_id,date=r$date,source_grid_lon=lon[xi],source_grid_lat=dy$vals[yi],CF_time_coordinate=format(instants[ti],'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),CF_calendar=cal,source_value=v,amadeus_value=r$value,absolute_difference=delta,tolerance=tol,passed=is.finite(delta)&&delta<=tol,scope='Independent ncdf4 nearest-coordinate read with CF scale/offset decoding; not interval/provider-truth approval')
 }
}
for(code in unique(results$variable)){
 msg<-tryCatch({check_code(code);''},error=function(e)conditionMessage(e))
 summaries[[length(summaries)+1]]<-data.frame(variable=code,supported_read_completed=!nzchar(msg),message=msg)
 write.csv(do.call(rbind,rows),file.path(report_dir,'direct-source-values.csv'),row.names=FALSE)
 write.csv(do.call(rbind,summaries),file.path(report_dir,'direct-source-coverage.csv'),row.names=FALSE)
}
stopifnot(length(rows)==nrow(results),all(vapply(rows,function(x)x$passed,TRUE)),all(vapply(summaries,function(x)x$supported_read_completed,TRUE)))
cat('Direct source checks:',length(rows),'values matched within stated tolerances\n')
