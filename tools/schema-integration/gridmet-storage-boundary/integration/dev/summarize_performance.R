a<-commandArgs(TRUE)
if(length(a)!=2L)stop('Usage: Rscript dev/summarize_performance.R OUTPUT_ROOT REVIEW.csv')
paths<-list.files(a[1],pattern='^performance.json$',recursive=TRUE,full.names=TRUE)
if(!length(paths))stop('No performance summaries found')
rows<-lapply(paths,function(p){
 x<-jsonlite::read_json(p);scalar<-function(v)if(is.null(v))NA else v
 stages<-read.csv(file.path(dirname(p),'stage-timings.csv'))
 elapsed<-function(prefix)sum(stages$elapsed_seconds[startsWith(stages$stage,prefix)])
 data.frame(variant=x$variant,status=x$status,variables=if(is.null(x$request))NA else length(x$request$variables),
 sites=scalar(x$request$sites),days=scalar(x$request$inclusive_days),expected_values=scalar(x$request$expected_values),
 source_bytes=scalar(x$acquisition$selected_source_bytes_after_call),
 sources_existed_before=if(is.null(x$acquisition$existed_before_call))NA else all(unlist(x$acquisition$existed_before_call)),
 engine=scalar(x$selected_engine),elapsed_seconds=x$elapsed_seconds,
 download_seconds=elapsed('download'),process_seconds=elapsed('process_call'),calculate_seconds=elapsed('calculate_call'),
 schema_export_seconds=elapsed('schema_export'),database_seconds=elapsed('database_'),
 main_R_peak_RSS_MiB=scalar(x$main_R_peak_RSS_MiB),sampled_tree_RSS_sum_peak_MiB=scalar(x$sampled_workload_tree_RSS_sum_peak_MiB),
 accepted=x$schema_accepted,accepted_records=scalar(x$accepted_records),error=scalar(x$error),summary_path=p)
})
write.csv(do.call(rbind,rows),a[2],row.names=FALSE)
cat('Wrote',length(rows),'run summaries to',a[2],'\n')
