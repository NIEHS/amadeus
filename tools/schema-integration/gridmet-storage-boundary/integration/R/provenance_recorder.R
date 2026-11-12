# Explicit observation only. Never calls Amadeus science, changes inputs, or validates a schema.
.provenance_sha <- function(p) digest::digest(file=p,algo='sha256')
.provenance_object_sha <- function(x) digest::digest(x,algo='sha256',serialize=TRUE)
.provenance_function_sha <- function(name) digest::digest(paste(deparse(body(getExportedValue('amadeus',name))),collapse='\n'),algo='sha256',serialize=FALSE)
.provenance_output <- function(x) {
  if(is.character(x)&&length(x)==1L&&file.exists(x))return(list(kind='file',sha256=.provenance_sha(x),bytes=as.numeric(file.info(x)$size)))
  if(inherits(x,'SpatRaster')){
    metadata<-list(dim=dim(x),extent=as.vector(terra::ext(x)),crs=terra::crs(x),names=names(x),time=as.character(terra::time(x)))
    return(list(kind='raster_metadata_only',sha256=.provenance_object_sha(metadata)))
  }
  list(kind='R_object',sha256=.provenance_object_sha(x))
}
record_provenance <- function(request,directory) {
  for(p in c('digest','jsonlite','amadeus'))if(!requireNamespace(p,quietly=TRUE))stop('Recorder requires ',p)
  if(dir.exists(directory)||file.exists(directory))stop('Use a new recording directory; no overwrite')
  dir.create(directory,recursive=TRUE);directory<-normalizePath(directory)
  e<-new.env(parent=emptyenv());e$calls<-list();e$events<-list();e$previous<-'GENESIS';e$sealed<-FALSE;e$sources<-NULL;e$source_epoch<-NULL
  package_version<-as.character(utils::packageVersion('amadeus'))
  request_sha<-.provenance_object_sha(request)
  append<-function(kind,data){
    if(e$sealed)stop('Recording already sealed')
    payload<-list(sequence=length(e$events)+1L,kind=kind,observed_epoch=as.numeric(Sys.time()),data=data)
    encoded<-as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,digits=NA,null='null'))
    hash<-digest::digest(paste(e$previous,encoded,sep='\n'),algo='sha256',serialize=FALSE)
    event<-list(previous=e$previous,payload_json=encoded,sha256=hash)
    e$events[[length(e$events)+1L]]<-event;e$previous<-hash
    jsonlite::write_json(e$events,file.path(directory,'events.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
  }
  append('recording_started',list(package_version=package_version,request_sha256=request_sha))
  start<-function(key,role,function_name,arguments,parent=NULL,variable=NULL){
    if(!is.null(e$calls[[key]]))stop('Duplicate recording key')
    hash<-.provenance_function_sha(function_name)
    c<-list(key=key,role=role,function_name=function_name,arguments=arguments,
      started=Sys.time(),function_body_sha256=hash)
    if(!is.null(parent))c$parent<-parent
    if(!is.null(variable))c$variable<-variable
    e$calls[[key]]<-c;append('call_started',list(key=key,role=role,function_name=function_name,arguments=arguments,body_sha256=hash))
    invisible(NULL)
  }
  finish<-function(key,output=NULL){
    c<-e$calls[[key]]
    if(is.null(c)||!is.null(c$ended))stop('Unknown or already finished recording key')
    c$ended<-Sys.time();c$outcome<-'succeeded';c$output_evidence<-.provenance_output(output)
    e$calls[[key]]<-c;append('call_finished',list(key=key,outcome=c$outcome,output_evidence=c$output_evidence))
    invisible(NULL)
  }
  fail<-function(key,error){
    c<-e$calls[[key]];if(is.null(c)||!is.null(c$ended))stop('Unknown or already finished recording key')
    c$ended<-Sys.time();c$outcome<-'failed';c$error<-as.character(error);e$calls[[key]]<-c
    append('call_failed',list(key=key,error=as.character(error)));invisible(NULL)
  }
  observe_sources<-function(paths){
    if(is.null(names(paths))||anyDuplicated(names(paths)))stop('Supply named source paths')
    e$source_epoch<-as.numeric(Sys.time())
    e$sources<-setNames(lapply(paths,function(p)list(path=normalizePath(p),sha256=.provenance_sha(p))),names(paths))
    append('source_observation',list(observed_epoch=e$source_epoch,files=e$sources));invisible(NULL)
  }
  seal<-function(){
    if(e$sealed)stop('Recording already sealed')
    if(!length(e$calls)||any(vapply(e$calls,function(c)is.null(c$ended)||c$outcome!='succeeded',TRUE)))stop('Incomplete or failed scientific operation; no successful seal')
    append('recording_sealed',list(calls=length(e$calls)))
    core<-list(package_version=package_version,calls=unname(e$calls),request_sha256=request_sha,
      source_observation=e$sources,source_observed_epoch=e$source_epoch)
    saveRDS(core,file.path(directory,'execution.rds'))
    manifest<-list(revision='explicit-observer-v1',execution_sha256=.provenance_sha(file.path(directory,'execution.rds')),
      events_sha256=.provenance_sha(file.path(directory,'events.json')),last_event_sha256=e$previous,
      trust='Unkeyed local evidence; not independent execution attestation')
    jsonlite::write_json(manifest,file.path(directory,'seal.json'),auto_unbox=TRUE,pretty=TRUE)
    e$sealed<-TRUE
    c(core,list(recording=list(directory=directory,seal_sha256=.provenance_sha(file.path(directory,'seal.json')))))
  }
  list(start=start,finish=finish,fail=fail,observe_sources=observe_sources,seal=seal,directory=directory)
}
# Runs only in the post-calculation writer, not between scientific calls.
verify_recorded_provenance <- function(execution,request,calculated,processed) {
  ref<-execution$recording;if(is.null(ref))stop('Recorded execution evidence is required by this candidate')
  directory<-normalizePath(ref$directory)
  if(.provenance_sha(file.path(directory,'seal.json'))!=ref$seal_sha256)stop('Recording seal changed')
  seal<-jsonlite::read_json(file.path(directory,'seal.json'))
  if(.provenance_sha(file.path(directory,'execution.rds'))!=seal$execution_sha256||.provenance_sha(file.path(directory,'events.json'))!=seal$events_sha256)stop('Recorded evidence bytes changed')
  core<-readRDS(file.path(directory,'execution.rds'));provided<-execution;provided$recording<-NULL
  if(!identical(core,provided))stop('Supplied execution context differs from sealed recording')
  if(.provenance_object_sha(request)!=core$request_sha256)stop('Request differs from recorded request')
  previous<-'GENESIS'
  for(event in jsonlite::read_json(file.path(directory,'events.json'))){
    expected<-digest::digest(paste(previous,event$payload_json,sep='\n'),algo='sha256',serialize=FALSE)
    if(previous!=event$previous||expected!=event$sha256)stop('Recording event chain broken')
    previous<-event$sha256
  }
  if(previous!=seal$last_event_sha256)stop('Recording final event mismatch')
  if(core$package_version!=as.character(utils::packageVersion('amadeus')))stop('Execution package identity changed')
  process<-Filter(function(c)c$role=='process',core$calls)
  if(!length(process)||is.null(core$source_observation)||!setequal(names(core$source_observation),request$variables)||
     is.null(core$source_observed_epoch)||core$source_observed_epoch>=min(vapply(process,function(c)as.numeric(c$started),0.0)))stop('Source evidence was not recorded before processing')
  for(c in core$calls){
    if(c$function_body_sha256!=.provenance_function_sha(c$function_name))stop('Installed function differs from recorded function')
    if(c$role%in%c('calculate','process')){
      x<-if(c$role=='calculate')calculated[[c$key]]else processed[[c$key]]
      if(identical(c$output_evidence$kind,'raster_metadata_only')&&is.character(x)&&length(x)==1L)x<-terra::rast(x)
      if(is.null(x)||!identical(.provenance_output(x),c$output_evidence))stop('Supplied output differs from recorded output: ',c$key)
    }
  }
  list(directory=directory,source_observation=core$source_observation,
    seal_sha256=ref$seal_sha256,trust=seal$trust)
}
