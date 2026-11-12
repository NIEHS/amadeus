# Optional external telemetry only. Never changes Amadeus, accepts data, or repairs results.
performance_monitor <- function(root, variant) {
  e <- new.env(parent=emptyenv())
  e$started <- Sys.time();e$clock <- unname(proc.time()[3]);e$cpu <- proc.time()
  e$current <- 'startup_and_request';e$stage_start <- e$clock;e$stage_cpu<-e$cpu
  e$events<-list();e$issues<-character();e$run<-NULL;e$output<-file.path(root,'outputs')
  e$request<-NULL;e$before<-NULL;e$finished<-FALSE;e$success<-FALSE;e$error<-NULL
  e$work<-tempfile('gridmet-telemetry-');dir.create(e$work)
  e$samples<-file.path(e$work,'samples.csv');e$stop<-file.path(e$work,'STOP')
  memory_requested <- Sys.getenv('GRIDMET_MONITOR_MEMORY','1') != '0'
  e$memory_status<-if(!memory_requested)'disabled'else'unsupported; timing remains available'
  safe <- function(f) tryCatch(f(),error=function(err) {
    e$issues<-unique(c(e$issues,conditionMessage(err)));invisible(NULL)
  })
  stage <- function(name) safe(function() {
    now<-unname(proc.time()[3]);p<-proc.time()
    e$events[[length(e$events)+1L]]<-data.frame(stage=e$current,
      elapsed_seconds=now-e$stage_start,main_R_cpu_seconds=sum((p-e$stage_cpu)[1:2]))
    e$current<-name;e$stage_start<-now;e$stage_cpu<-p
  })
  if(memory_requested && file.exists('/proc/self/status') && nzchar(Sys.which('sh'))) safe(function() {
    interval<-suppressWarnings(as.numeric(Sys.getenv('GRIDMET_MONITOR_INTERVAL','1')))
    if(!is.finite(interval)||interval<0.2)interval<-1
    shell<-file.path(root,'R/performance_sampler.sh')
    if(!file.exists(shell))stop('Memory sampler unavailable; continuing with timing')
    system2(Sys.which('sh'),vapply(c(shell,as.character(Sys.getpid()),e$samples,e$stop,as.character(interval)),shQuote,''),
            wait=FALSE,stdout=FALSE,stderr=FALSE)
    e$memory_status<-'Linux procfs sampling requested';e$interval<-interval
  })
  peak_hwm <- function() {
    f<-'/proc/self/status'
    if(!file.exists(f))return(NULL)
    z<-grep('^VmHWM:',readLines(f,warn=FALSE),value=TRUE)
    if(!length(z))return(NULL)
    as.numeric(gsub('[^0-9]','',z))/1024
  }
  finish <- function() {
    if(e$finished)return(invisible(NULL))
    e$finished<-TRUE
    safe(function() {
      stage(if(e$success)'complete'else'failed')
      file.create(e$stop)
      sample<-NULL
      if(file.exists(e$samples)) sample<-tryCatch(read.csv(e$samples),error=function(x)NULL)
      if(!is.null(sample)&&nrow(sample))e$memory_status<-'Linux procfs sampled'
      destination<-if(!is.null(e$run))e$run else file.path(e$output,
        paste0('monitoring-failed-',Sys.getpid(),'-',format(e$started,'%Y%m%dT%H%M%S')))
      dir.create(destination,recursive=TRUE,showWarnings=FALSE)
      events<-do.call(rbind,e$events)
      write.csv(events,file.path(destination,'stage-timings.csv'),row.names=FALSE)
      read_receipt<-function(p) {
        if(!file.exists(p)||!requireNamespace('jsonlite',quietly=TRUE))return(NULL)
        tryCatch(jsonlite::read_json(p),error=function(x)NULL)
      }
      acceptance<-read_receipt(file.path(destination,'acceptance.json'))
      database<-list()
      for(engine in c('duckdb','sedonadb')) {
        q<-read_receipt(file.path(destination,'database',engine,'receipt.json'))
        if(!is.null(q))database[[engine]]<-list(records=q$records,integrity_queries=q$integrity_queries,
          fidelity=q$all_within_declared_tolerances,persistent_reopen=q$persistent_reopen_passed)
      }
      packages<-c('amadeus','terra','sf','ncdf4','jsonlite','jsonvalidate','digest','duckdb','sedonadb')
      versions<-setNames(lapply(packages,function(p)tryCatch(as.character(utils::packageVersion(p)),error=function(x)NULL)),packages)
      source_bytes<-NULL
      if(!is.null(e$request)) {
        paths<-file.path(e$request$download_dir,e$request$variables,paste0(e$request$variables,'_',e$request$year,'.nc'))
        source_bytes<-sum(file.info(paths)$size,na.rm=TRUE)
      }
      commit<-NULL
      if(nzchar(Sys.which('git'))) {
        q<-suppressWarnings(system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE,stderr=FALSE))
        if(length(q)==1L&&grepl('^[a-f0-9]{40}$',q))commit<-q
      }
      fingerprints<-list()
      if(requireNamespace('digest',quietly=TRUE)) {
        for(name in c('example_gridmet.R','R/schema_export.R','R/storage_boundary.R','R/database.R','R/performance.R','R/performance_sampler.sh'))
          if(file.exists(file.path(root,name)))fingerprints[[name]]<-digest::digest(file=file.path(root,name),algo='sha256')
      }
      summary<-list(monitor_revision='external-telemetry-v1',variant=variant,
        status=if(e$success)'succeeded'else'failed',error=e$error,
        started_utc=format(e$started,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),
        finished_utc=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),
        elapsed_seconds=unname(proc.time()[3])-e$clock,
        elapsed_scope='monitor initialization to finalization; interpreter startup and final summary writing excluded',
        stage_elapsed_sum_seconds=sum(events$elapsed_seconds),
        main_R_peak_RSS_MiB=peak_hwm(),main_R_peak_method='Linux VmHWM when available; includes monitoring in main process',
        memory_sampling_status=e$memory_status,sampling_interval_seconds=if(!is.null(e$interval))e$interval else NULL,
        memory_samples=if(!is.null(sample))nrow(sample)else 0L,
        sampled_main_R_peak_RSS_MiB=if(!is.null(sample)&&nrow(sample))max(sample$main_R_RSS_MiB,na.rm=TRUE)else NULL,
        sampled_workload_tree_RSS_sum_peak_MiB=if(!is.null(sample)&&nrow(sample))max(sample$workload_tree_RSS_sum_MiB,na.rm=TRUE)else NULL,
        memory_limitations='Sampling may miss peaks. Tree sum excludes telemetry subtree and can double-count shared memory. Not unique physical RAM, file size or R object size.',
        request=if(is.null(e$request))NULL else list(variables=as.list(e$request$variables),sites=nrow(e$request$locations),
          start=as.character(e$request$dates[1]),end=as.character(e$request$dates[2]),
          inclusive_days=as.integer(diff(e$request$dates))+1L,expected_values=length(e$request$variables)*nrow(e$request$locations)*(as.integer(diff(e$request$dates))+1L)),
        acquisition=list(existed_before_call=e$before,selected_source_bytes_after_call=source_bytes,
          note='Existence indicates requested reuse; it does not prove freshness, actual network bytes or source integrity'),
        schema_accepted=if(is.null(acceptance))FALSE else isTRUE(acceptance$accepted),
        accepted_records=if(is.null(acceptance))NULL else acceptance$records,
        integration_revision=if(is.null(acceptance))NULL else acceptance$integration_revision,
        selected_engine=if(!is.null(e$engine))e$engine else NULL,database=database,
        code=list(git_commit=commit,file_SHA256=fingerprints),R_version=R.version.string,platform=R.version$platform,
        packages=versions,monitoring_issues=as.list(e$issues),production_approved=FALSE)
      if(requireNamespace('jsonlite',quietly=TRUE)) {
        jsonlite::write_json(summary,file.path(destination,'performance.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=NA)
      } else {
        write.csv(data.frame(status=summary$status,elapsed_seconds=summary$elapsed_seconds,
          monitoring_issue='jsonlite unavailable; CSV fallback only'),file.path(destination,'performance-fallback.csv'),row.names=FALSE)
      }
      if(Sys.getenv('GRIDMET_MONITOR_KEEP_SAMPLES','0')=='1'&&file.exists(e$samples))
        file.copy(e$samples,file.path(destination,'memory-samples.csv'),overwrite=TRUE)
    })
    if(length(e$issues))cat('Monitoring note (scientific status unchanged): ',paste(e$issues,collapse='; '),'\n',file=stderr())
    invisible(NULL)
  }
  list(stage=stage,finish=finish,
    output=function(path)safe(function(){e$output<-path}),
    attach=function(path)safe(function(){e$run<-path}),
    context=function(request,before,engine)safe(function(){e$request<-request;e$before<-before;e$engine<-engine}),
    failure=function(message)safe(function(){e$error<-message}),
    succeed=function()safe(function(){e$success<-TRUE}))
}
