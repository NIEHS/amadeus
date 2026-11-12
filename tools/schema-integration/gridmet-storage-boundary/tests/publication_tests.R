b<-dirname(dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))));root<-file.path(b,'integration')
source(file.path(root,'R/storage_boundary.R'))
a<-commandArgs(TRUE);if(length(a)!=2L)stop('Usage: Rscript tests/publication_tests.R SNAPSHOT NEW_SCRATCH');run<-normalizePath(a[1],mustWork=TRUE);scratch<-a[2];if(file.exists(scratch))stop('Use a NEW scratch directory');dir.create(scratch,recursive=TRUE);for(n in c('faults','reports','verification'))dir.create(file.path(scratch,n));rows<-list();cases<-list();sha<-function(p)digest::digest(file=p,algo='sha256')
edit_json<-function(t,relative,fn){p<-file.path(t,relative);Sys.chmod(p,'0644');x<-jsonlite::read_json(p);jsonlite::write_json(fn(x),p,auto_unbox=TRUE,pretty=TRUE,digits=NA);Sys.chmod(p,'0444')}
append_byte<-function(t,f){p<-file.path(t,f);Sys.chmod(p,'0644');cat('\n',file=p,append=TRUE);Sys.chmod(p,'0444')}
run_case<-function(name,mutate=NULL,accept=FALSE,other_root=root){
 t<-file.path(scratch,'faults',paste0('final-',name));dir.create(t);invisible(file.copy(list.files(run,all.files=TRUE,no..=TRUE,full.names=TRUE),t,recursive=TRUE))
 if(!is.null(mutate))mutate(t)
 msg<-'';result<-tryCatch({storage_verify_publication(other_root,t);TRUE},error=function(e){msg<<-conditionMessage(e);FALSE})
 rows[[length(rows)+1]]<<-data.frame(test=name,expected_acceptance=accept,accepted=result,passed=result==accept,message=msg)
 cases[[name]]<<-t
 write.csv(do.call(rbind,rows),file.path(scratch,'reports/publication-tests.csv'),row.names=FALSE)
}
run_case('unaltered-relocated-snapshot',accept=TRUE)
run_case('database-value-edited',function(t){p<-file.path(t,'database/duckdb/gridmet.duckdb');Sys.chmod(p,'0644');con<-DBI::dbConnect(duckdb::duckdb(),dbdir=p);DBI::dbExecute(con,'UPDATE "AmbientValueAtLocation" SET value=value+1 WHERE id=(SELECT id FROM "AmbientValueAtLocation" LIMIT 1)');DBI::dbDisconnect(con,shutdown=TRUE);Sys.chmod(p,'0444')})
run_case('database-truncated',function(t){p<-file.path(t,'database/duckdb/gridmet.duckdb');Sys.chmod(p,'0644');writeBin(as.raw(1:10),p);Sys.chmod(p,'0444')})
run_case('parquet-bytes-edited',function(t)append_byte(t,'database/duckdb/accepted-values.parquet'))
run_case('database-receipt-edited',function(t)edit_json(t,'database/duckdb/receipt.json',function(x){x$records<-999;x}))
run_case('accepted-json-edited',function(t)append_byte(t,'schema-instance.json'))
run_case('acceptance-edited',function(t)append_byte(t,'acceptance.json'))
run_case('recording-events-edited',function(t)append_byte(t,'provenance/events.json'))
run_case('manifest-edited',function(t)append_byte(t,'publication-manifest.json'))
run_case('workflow-incomplete',function(t)edit_json(t,'workflow-receipt.json',function(x){x$completed<-FALSE;x}))
run_case('workflow-wrong-engine',function(t)edit_json(t,'workflow-receipt.json',function(x){x$selected_engine<-'none';x}))
run_case('workflow-wrong-instance',function(t)edit_json(t,'workflow-receipt.json',function(x){x$instance_sha256<-paste(rep('a',64),collapse='');x}))
run_case('workflow-missing-manifest-binding',function(t)edit_json(t,'workflow-receipt.json',function(x){x$publication_manifest_sha256<-NULL;x}))
run_case('workflow-missing',function(t){Sys.chmod(file.path(t,'workflow-receipt.json'),'0644');unlink(file.path(t,'workflow-receipt.json'))})
run_case('manifest-missing',function(t){Sys.chmod(file.path(t,'publication-manifest.json'),'0644');unlink(file.path(t,'publication-manifest.json'))})
run_case('parquet-missing',function(t){Sys.chmod(file.path(t,'database/duckdb/accepted-values.parquet'),'0644');unlink(file.path(t,'database/duckdb/accepted-values.parquet'))})
run_case('database-wal-present',function(t)writeLines('owned-test-fixture',file.path(t,'database/duckdb/gridmet.duckdb.wal')))
run_case('same-name-different-db',function(t){p<-file.path(t,'database/duckdb/gridmet.duckdb');Sys.chmod(p,'0644');unlink(p);con<-DBI::dbConnect(duckdb::duckdb(),dbdir=p);DBI::dbExecute(con,'CREATE TABLE unrelated(x INTEGER)');DBI::dbDisconnect(con,shutdown=TRUE);Sys.chmod(p,'0444')})
# No arbitrary path/symlink read even if a caller recalculates the local manifest digest.
run_case('manifest-path-escape',function(t){edit_json(t,'publication-manifest.json',function(x){x$files[['../outside']]<-list(bytes=0,sha256=paste(rep('a',64),collapse=''));x});edit_json(t,'workflow-receipt.json',function(x){x$publication_manifest_sha256<-sha(file.path(t,'publication-manifest.json'));x})})
# Critical signature coverage cannot be removed merely by rehashing the local manifest.
run_case('manifest-omits-database',function(t){edit_json(t,'publication-manifest.json',function(x){x$files[['database/duckdb/gridmet.duckdb']]<-NULL;x});edit_json(t,'workflow-receipt.json',function(x){x$publication_manifest_sha256<-sha(file.path(t,'publication-manifest.json'));x})})
run_case('resealed-wrong-instance-binding',function(t){edit_json(t,'schema-instance.json',function(x){x$product_variables[[1]]$label<-'owned test edit';x});edit_json(t,'publication-manifest.json',function(x){x$files[['schema-instance.json']]$sha256<-sha(file.path(t,'schema-instance.json'));x$files[['schema-instance.json']]$bytes<-as.numeric(file.info(file.path(t,'schema-instance.json'))$size);x});edit_json(t,'workflow-receipt.json',function(x){x$publication_manifest_sha256<-sha(file.path(t,'publication-manifest.json'));x})})
run_case('resealed-wrong-acceptance-release',function(t){edit_json(t,'acceptance.json',function(x){x$runtime_contract_sha256<-paste(rep('a',64),collapse='');x});edit_json(t,'publication-manifest.json',function(x){x$files[['acceptance.json']]$sha256<-sha(file.path(t,'acceptance.json'));x$files[['acceptance.json']]$bytes<-as.numeric(file.info(file.path(t,'acceptance.json'))$size);x});edit_json(t,'workflow-receipt.json',function(x){x$publication_manifest_sha256<-sha(file.path(t,'publication-manifest.json'));x})})
# Recheck false/missing successful-build status against corrected helper, not production root mutation.
for(mode in c('false','missing')){
 rr<-file.path(scratch,'faults',paste0('final-release-',mode));dir.create(rr);invisible(file.copy(list.files(root,all.files=TRUE,no..=TRUE,full.names=TRUE),rr,recursive=TRUE))
 p<-file.path(rr,'release/runtime-contract.json');x<-jsonlite::read_json(p);x$build_qa_passed<-if(mode=='false')FALSE else NULL;jsonlite::write_json(x,p,auto_unbox=TRUE,pretty=TRUE)
 run_case(paste0('release-status-',mode),other_root=rr)
}
# Actual supported CLI must reject edited DB before returning rows.
for(n in c('unaltered-relocated-snapshot','database-value-edited')){
 log<-file.path(scratch,'verification',paste0('query-',n,'.log'))
 status<-system2(file.path(R.home('bin'),'Rscript'),vapply(c(file.path(root,'R/database.R'),cases[[n]],'duckdb','SELECT COUNT(*) AS n FROM "AmbientValueAtLocation"'),shQuote,''),stdout=log,stderr=log)
 rows[[length(rows)+1]]<-data.frame(test=paste0('CLI-',n),expected_acceptance=n=='unaltered-relocated-snapshot',accepted=status==0,passed=(status==0)==(n=='unaltered-relocated-snapshot'),message=paste(readLines(log),collapse='; '))
}
write.csv(do.call(rbind,rows),file.path(scratch,'reports/publication-tests.csv'),row.names=FALSE)
stopifnot(all(do.call(rbind,rows)$passed));cat(length(rows),'publication checks passed\n')
