# Portable catalog conversion

See the standalone [setup, development, and test guide](../README.md). All paths
in this subtree resolve against the standalone folder, with no parent project
dependency. The original [loader guide](R/README_load_dataset_json.md) is included
as a source snapshot; proposed integrations there remain future work.

## 7. Generate one dataset with reusable R functions

```r
source("linkml-json-iplementation/R/load_dataset_json.R")
path <- load_dataset_json("merra2", quiet = TRUE)
catalog <- jsonlite::read_json(path, simplifyVector = FALSE)
```

Optional arguments: `input_dir` selects dataset YAML files; `schema_yaml` selects
the shared LinkML model; `output_file` chooses a JSON destination outside the
protected input trees; `python` selects an installed interpreter; `overwrite`
controls replacement; `quiet` suppresses success messages. Defaults are anchored
to the source file's location, while explicit relative paths use the caller's
working directory. `load_dataset_json("global_catalog")` is unsupported: use the
included `run_pipeline.R` for global union checks. Expected reports, complete
build commands, and validation limits are documented in the standalone guide.
