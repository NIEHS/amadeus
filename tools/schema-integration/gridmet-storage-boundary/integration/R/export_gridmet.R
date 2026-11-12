# External post-calculation writer. No download/process/calculate calls or package hooks.
.gridmet_export_root <- if (!is.null(sys.frame(1)$ofile)) dirname(dirname(normalizePath(sys.frame(1)$ofile))) else NULL
export_gridmet <- function(calculated, processed, request, execution, output,
                           source_observation = NULL, engine = 'duckdb',
                           storage = 'parquet', root = .gridmet_export_root) {
  export_started <- unname(proc.time()[3])
  if (is.null(root)) stop('Supply root=integration folder when loading with sys.source')
  root <- normalizePath(root)
  for(p in c('amadeus','terra','sf','jsonlite','yaml','digest','jsonvalidate','ncdf4','DBI','duckdb'))
    if(!requireNamespace(p,quietly=TRUE))stop('Post-calculation writer requires ',p)
  if(!engine%in%c('none','duckdb')||!storage%in%c('rowwise','inmemory_bulk','parquet'))stop('Unqualified engine/storage selection')
  if(engine=='none'&&storage!='rowwise')stop('engine none requires storage rowwise; DuckDB is still used for boundary enforcement')
  if(!is.list(execution)||!length(execution$calls)||is.null(execution$package_version))stop('Actual caller execution context is required; history is not inferred from results')
  if(execution$package_version!=as.character(utils::packageVersion('amadeus')))stop('Caller package version differs from current installation; resolve before export')
  observer <- new.env(parent=globalenv())
  sys.source(file.path(root,'R/provenance_recorder.R'),observer)
  recorded <- observer$verify_recorded_provenance(execution,request,calculated,processed)
  if(!is.null(source_observation)&&!identical(source_observation,recorded$source_observation))stop('Source observation differs from sealed recording')
  source_observation <- recorded$source_observation
  calls<-execution$calls
  if(anyDuplicated(vapply(calls,function(x)x$key,'')))stop('Duplicate execution key')
  for(c in calls) {
    if(!all(c('key','role','function_name','started','ended','arguments')%in%names(c))||
       !c$role%in%c('download','process','calculate')||
       !inherits(c$started,'POSIXt')||!inherits(c$ended,'POSIXt')||c$ended<c$started)
      stop('Incomplete/invalid execution context; no fabricated times or calls')
  }
  for(c in Filter(function(c)c$role=='calculate',calls)) {
    args<-c$arguments
    grouping<-if('.by_time'%in%names(args))args[['.by_time']]else args[['by_time']]
    ordinary_daily<-is.null(grouping)||identical(grouping,'NULL; existing daily gridMET route')
    if(!c$function_name%in%c('calculate_covariates','calculate_gridmet')||
       !all(c('radius','fun','geom')%in%names(args))||!identical(as.numeric(args$radius),0)||
       !identical(args$fun,'mean')||!identical(args$geom,FALSE)||!ordinary_daily)
      stop('Writer profile supports native daily point extraction only; no relabeling of other Amadeus analyses')
  }
  if(!is.list(calculated)||!is.list(processed)||!length(calculated)||!length(processed))stop('Provide named lists of ordinary calculated and processed outputs')
  if(is.null(names(calculated))||is.null(names(processed)))stop('Output lists must be named by their actual execution call keys')
  if(!setequal(names(calculated),vapply(Filter(function(c)c$role=='calculate',calls),function(c)c$key,''))||
     !setequal(names(processed),vapply(Filter(function(c)c$role=='process',calls),function(c)c$key,'')))stop('Supplied outputs must match actual process/calculate execution keys exactly')
  if(!all(c('variables','dates','locations','download_dir')%in%names(request)))stop('Pass existing variables, dates, locations and download_dir in request')
  request$dates<-as.Date(request$dates);request$year<-as.integer(format(request$dates[1],'%Y'))
  if(is.null(request$phi_status))request$phi_status<-'no_phi'
  request$download_dir<-normalizePath(request$download_dir)
  # All integration code loads HERE, after caller-owned scientific execution.
  env<-new.env(parent=globalenv())
  sys.source(file.path(root,'R/storage_boundary.R'),env)
  sys.source(file.path(root,'R/schema_export.R'),env)
  env$schema_release(root,verify=TRUE)
  output<-normalizePath(output,mustWork=FALSE);dir.create(output,recursive=TRUE,showWarnings=FALSE)
  run_id<-paste0('direct-',substr(digest::digest(list(Sys.time(),Sys.getpid(),runif(1)),algo='sha256'),1,16))
  run_dir<-file.path(output,run_id);if(dir.exists(run_dir))stop('Run exists')
  dir.create(run_dir);for(n in c('processed','ordinary'))dir.create(file.path(run_dir,n))
  rows<-list();raster_map<-list()
  # Serialize supplied objects without recalculating or renaming their dates.
  for(i in seq_along(calls)){
    c<-calls[[i]]
    if(c$role=='process'){
      if(!c$key%in%names(processed))stop('Missing processed output: ',c$key)
      x<-processed[[c$key]];if(is.character(x)&&length(x)==1)x<-terra::rast(x)
      if(!inherits(x,'SpatRaster'))stop('Expected SpatRaster or raster path')
      p<-file.path(run_dir,'processed',paste0(sprintf('%03d',i),'.tif'))
      terra::writeRaster(x,p,datatype='FLT8S',overwrite=FALSE);c$output<-p
      raster_map[[c$key]]<-p
    }else if(c$role=='calculate'){
      if(!c$key%in%names(calculated))stop('Missing calculated output: ',c$key)
      tab<-as.data.frame(calculated[[c$key]])
      code<-if(!is.null(c$variable))c$variable else c$arguments$variable
      column<-grep(paste0('^',code,'(_|$)'),names(tab),value=TRUE)
      if(length(column)!=1||!all(c('site_id','time')%in%names(tab)))stop('Ordinary result needs site_id, time and one native variable column')
      p<-file.path(run_dir,'ordinary',paste0(sprintf('%03d',i),'.rds'));saveRDS(calculated[[c$key]],p);c$output<-p
      rows[[length(rows)+1L]]<-data.frame(variable=code,site_id=as.character(tab$site_id),date=as.character(as.Date(tab$time)),value=as.numeric(tab[[column]]),call_key=c$key)
    }
    calls[[i]]<-c
  }
  results<-do.call(rbind,rows);if(is.null(results))stop('No calculation outputs')
  write.csv(results,file.path(run_dir,'results.csv'),row.names=FALSE)
  saveRDS(request,file.path(run_dir,'request.rds'));saveRDS(calls,file.path(run_dir,'calls.rds'))
  write.csv(request$locations,file.path(run_dir,'requested-sites.csv'),row.names=FALSE)
  before<-setNames(as.list(rep(NA,length(request$variables))),request$variables)
  saveRDS(before,file.path(run_dir,'source-existed-before.rds'))
  source_scope<-if(is.null(source_observation))'post_calculation_only'else'recorded_before_processing; not independently attested'
  if(is.null(source_observation))source_observation<-setNames(lapply(request$variables,function(code){p<-file.path(request$download_dir,code,paste0(code,'_',request$year,'.nc'));list(path=normalizePath(p),sha256=digest::digest(file=p,algo='sha256'))}),request$variables)
  jsonlite::write_json(source_observation,file.path(run_dir,'source-observation.json'),auto_unbox=TRUE,pretty=TRUE)
  context<-list(recorded_context_integrity_verified=TRUE,recording_seal_sha256=recorded$seal_sha256,
    observer_trust=recorded$trust,interface='recorded-direct-output-v1',execution_context='explicit start/finish brackets; not independently attested',source_identity_scope=source_scope,
    source_unchanged_during_science_established=FALSE,scientific_calls_performed_by_writer=FALSE,
    serialization_performed_after_calculation=TRUE,call_package_version=execution$package_version,
    provenance_complete=FALSE,production_approved=FALSE)
  jsonlite::write_json(context,file.path(run_dir,'direct-output-context.json'),auto_unbox=TRUE,pretty=TRUE)
  dir.create(file.path(run_dir,'provenance'))
  for(n in c('execution.rds','events.json','seal.json'))if(!file.copy(file.path(recorded$directory,n),file.path(run_dir,'provenance',n)))stop('Could not preserve recording')
  # Record API provenance limitations in acceptance/completion, not only documentation.
  env$.direct_output_context<-context
  settings<-list(engine=engine,storage=storage,output=output)
  perf<-list(stage=function(...)invisible(NULL))
  receipt<-env$schema_write_and_load(root,run_dir,request,results,calls,before,settings,raster_map,perf)
  peak <- NULL
  if(file.exists('/proc/self/status')) {
    line <- grep('^VmHWM:',readLines('/proc/self/status'),value=TRUE)
    if(length(line))peak <- as.numeric(strsplit(trimws(sub('^VmHWM:','',line[1])),'[[:space:]]+')[[1]][1])/1024
  }
  tryCatch(jsonlite::write_json(list(export_elapsed_seconds=unname(proc.time()[3])-export_started,
    main_process_high_water_MiB=peak,scope='export elapsed only; memory high-water includes earlier work in this R process; not attributable to export alone'),
    file.path(run_dir,'direct-output-performance.json'),auto_unbox=TRUE,pretty=TRUE,null='null'),error=function(e)warning('Optional export telemetry unavailable: ',conditionMessage(e)))
  invisible(list(run_dir=run_dir,receipt=receipt,source_identity_scope=source_scope))
}
