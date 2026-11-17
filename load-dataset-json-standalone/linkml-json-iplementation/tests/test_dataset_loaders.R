# End-to-end API tests: use private inputs/outputs, preserve existing valid JSON.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
here <- dirname(dirname(normalizePath(script)))
repo <- dirname(here)
source(file.path(here, "R/load_dataset_json.R"))
source(file.path(repo, "dataset_spec_objects/catalog_updates_20261109/read_catalog.R"))
source(file.path(here, "R/convert_catalog.R"))
schema <- jsonlite::read_json(file.path(here, "output/amadeus_schema.schema.json"), simplifyVector = FALSE)
# JSON is a YAML subset: serialize modified fixtures without yaml::write_yaml's
# singleton-sequence collapse, so each negative control isolates one defect.
write_fixture <- function(doc, path) writeLines(catalog_json_text(doc, schema), path, useBytes = TRUE)
work <- tempfile(pattern = "dataset loader tests ")
dir.create(work)
input <- file.path(work, "input files")
dir.create(input)
file.copy(file.path(repo, "global_dataset_catalogs/aqs.yaml"), file.path(input, "aqs.yaml"))
original <- read_catalog_yaml(file.path(input, "aqs.yaml"))
output <- file.path(work, "output files", "aqs.json")
report <- sub(".json", ".validation.json", output, fixed = TRUE)
checks <- 0L
check <- function(condition) { stopifnot(condition); checks <<- checks + 1L }
load_test <- function(...) load_aqs_json(input_dir = input, output_file = output, quiet = TRUE, ...)
error_from <- function(code) tryCatch({force(code); stop("Expected failure did not occur")}, error = identity)
path <- load_test()
check(file.exists(path) && identical(attr(path, "validation_report"), report))
document <- jsonlite::read_json(path, simplifyVector = FALSE)
check(length(document$product_variables) == 7L)
check(identical(jsonlite::read_json(report)$status, "PASS"))
before <- unname(tools::md5sum(output))
# Every invalid input must preserve the last accepted JSON byte-for-byte.
rejected <- function(doc, expected) {
  write_fixture(doc, file.path(input, "aqs.yaml"))
  e <- error_from(load_test())
  check(grepl(expected, conditionMessage(e), ignore.case = TRUE))
  check(identical(unname(tools::md5sum(output)), before))
  r <- jsonlite::read_json(report, simplifyVector = FALSE)
  check(identical(r$status, "FAIL"))
}
x <- original; x$products[[1]]$id <- NULL
rejected(x, "required property")
x <- original; x$data_sources[[1]]$requires_authentication <- "not_a_boolean"
rejected(x, "boolean")
x <- original; x$product_variables[[1]]$metadata_gaps <- list("complete")
rejected(x, "complete")
x <- original; x$product_variables[[1]]$canonical_variable <- "amadeus:var/nonexistent"
rejected(x, "Unresolved")
writeLines("products: [", file.path(input, "aqs.yaml"))
e <- error_from(load_test())
check(grepl("failed during prepare", conditionMessage(e), fixed = TRUE))
check(identical(unname(tools::md5sum(output)), before))
check(identical(jsonlite::read_json(report)$status, "FAIL"))
write_fixture(original, file.path(input, "aqs.yaml"))
bad_schema <- file.path(work, "broken_schema.yaml")
writeLines("classes: [", bad_schema)
e <- error_from(load_test(schema_yaml = bad_schema))
check(grepl("failed during prepare", conditionMessage(e), fixed = TRUE))
check(identical(unname(tools::md5sum(output)), before))
e <- error_from(load_test(overwrite = FALSE))
check(grepl("already exists", conditionMessage(e), fixed = TRUE))
check(identical(unname(tools::md5sum(output)), before))
# Recovery from a previous failed call must replace its report with PASS.
load_test()
check(identical(jsonlite::read_json(report)$status, "PASS"))
# A nullable optional collection is allowed and must stay null, not become [].
x <- original; x["assets"] <- list(NULL)
write_fixture(x, file.path(input, "aqs.yaml"))
load_test()
nullable <- jsonlite::read_json(output, simplifyVector = FALSE)
check("assets" %in% names(nullable) && is.null(nullable$assets))
write_fixture(original, file.path(input, "aqs.yaml"))
e <- error_from(load_dataset_json("unknown_dataset", input_dir = input,
  output_file = file.path(work, "unknown.json"), quiet = TRUE))
check(grepl("Available datasets: aqs", conditionMessage(e), fixed = TRUE))
check(!file.exists(file.path(work, "unknown.json")))
e <- error_from(load_dataset_json("../aqs", quiet = TRUE))
check(grepl("without paths", conditionMessage(e), fixed = TRUE))
# Any valid new dataset filename can reuse the factory without registry edits.
file.copy(file.path(input,"aqs.yaml"), file.path(input,"another_dataset.yaml"))
load_another_dataset_json <- make_dataset_json_loader("another_dataset")
new_output <- load_another_dataset_json(input_dir = input,
  output_file = file.path(work,"another.json"), quiet = TRUE)
check(file.exists(new_output))
# Defaults remain tied to the sourced repository, even after changing cwd.
old <- getwd();setwd(work)
p <- load_aqs_json(output_file = file.path(work,"from_another_cwd.json"), quiet = TRUE)
setwd(old)
check(file.exists(p))
unlink(work, recursive = TRUE)
cat("PASS:", checks, "dataset-loader integration checks\n")
