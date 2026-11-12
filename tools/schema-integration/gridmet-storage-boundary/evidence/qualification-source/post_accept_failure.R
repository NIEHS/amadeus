b<-dirname(dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))));run<-readLines(file.path(b,'outputs/final-small/LATEST.txt'),n=1)
parent<-file.path(b,'faults/final-post-accept-load');dir.create(parent);writeLines(run,file.path(parent,'LATEST.txt'))
rows<-list()
for(mode in c('blocked-load','fresh-retry')){
 case<-file.path(parent,mode);dir.create(case)
 for(n in c('request.rds','calls.rds','source-existed-before.rds','results.csv','source-observation.json','direct-output-context.json'))stopifnot(file.copy(file.path(run,n),case))
 invisible(file.copy(file.path(run,'provenance'),case,recursive=TRUE))
 if(mode=='blocked-load')file.create(file.path(case,'database')) # owned conflicting path; no production files modified
 log<-file.path(case,'console.log');status<-system2(file.path(R.home('bin'),'Rscript'),vapply(c(file.path(b,'tests/post_accept_failure_worker.R'),b,case),shQuote,''),stdout=log,stderr=log)
 outcome<-jsonlite::read_json(file.path(case,'worker-outcome.json'))
 acceptance<-file.exists(file.path(case,'acceptance.json'));complete<-file.exists(file.path(case,'workflow-receipt.json'));latest<-readLines(file.path(parent,'LATEST.txt'))
 if(mode=='blocked-load')passed<-status!=0&&!outcome$success&&acceptance&&!complete&&identical(latest,run)
 else passed<-status==0&&outcome$success&&acceptance&&complete&&identical(latest,case)
 rows[[length(rows)+1]]<-data.frame(test=mode,passed=passed,schema_acceptance_present=acceptance,workflow_completion_present=complete,error=outcome$error,scope='Lower writer fixture: intentional path conflict then fresh-directory retry; no scientific rerun or recorder attestation claim')
 write.csv(do.call(rbind,rows),file.path(b,'reports/post-accept-failure.csv'),row.names=FALSE)
 stopifnot(passed)
}
