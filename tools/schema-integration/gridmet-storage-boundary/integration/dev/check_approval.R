# Build-only approval policy. No scientific dispatch or new runtime helper.
check_approval <- function(root) {
 sha<-function(p)digest::digest(file=p,algo='sha256')
 schema<-file.path(root,'approval/approval.schema.json')
 source<-file.path(root,'approval/approval_schema.yaml')
 generation<-jsonlite::read_json(file.path(root,'approval/GENERATION.json'))
 stopifnot(generation$source_sha256==sha(source),generation$json_schema_sha256==sha(schema))
 a<-yaml::read_yaml(file.path(root,'approval/catalog.yaml'))
 encode<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null='null',digits=NA))
 jsonvalidate::json_validate(encode(a),schema,engine='ajv',error=TRUE)
 stopifnot(a$dataset=='gridmet',identical(a$production_approved,FALSE))
 codes<-vapply(a$policies,function(x)x$code,'');stopifnot(!anyDuplicated(codes))
 evnames<-vapply(a$evidence,function(x)x$id,'');stopifnot(!anyDuplicated(evnames))
 for(e in a$evidence){stopifnot(e$file=='evidence/VERIFICATION.json');stopifnot(sha(file.path(root,'approval',e$file))==e$sha256)}
 ev<-jsonlite::read_json(file.path(root,'approval/evidence/VERIFICATION.json'))
 stopifnot(isTRUE(ev$all_four_pairs_native_metadata_graph_match),isTRUE(ev$postrun_source_RDS_database_audits_passed),ev$maximum_cross_arm_native_difference==0)
 # Independent membership extracted from archived, actually observed paired comparisons.
 pairs<-read.csv(file.path(root,'approval/evidence/pair-equivalence.csv'))
 audits<-read.csv(file.path(root,'approval/evidence/case-audits.csv'))
 stopifnot(nrow(pairs)==4,nrow(audits)==8,all(tolower(as.character(pairs$native_values_and_bindings_match))=='true'),all(audits$actual_saved_database_values_match))
 q<-Filter(function(x)x$status=='qualified',a$policies)
 profile<-read.csv(file.path(root,'catalog-source/gridmet-reviewed-profile.csv'),stringsAsFactors=FALSE)
 stopifnot(setequal(profile$code,vapply(q,function(x)x$code,'')))
 for(x in q){
  i<-match(x$code,profile$code)
  stopifnot(x$evidence_id%in%evnames,nzchar(x$scope),x$conversion=='identity',x$native_unit==x$output_unit,
   x$native_unit==profile$native_ucum[i],x$source_unit_aliases==profile$accepted_source_units[i],
   x$database_tolerance==profile$database_tolerance[i])
 }
 # The seal binds all authoring inputs and evidence to a reviewed candidate snapshot.
 lock<-jsonlite::read_json(file.path(root,'approval/REVIEW-SNAPSHOT.json'))
 for(f in names(lock$files))stopifnot(sha(file.path(root,'approval',f))==lock$files[[f]])
 jsonlite::write_json(a,file.path(root,'approval/catalog.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=NA)
 invisible(a)
}
