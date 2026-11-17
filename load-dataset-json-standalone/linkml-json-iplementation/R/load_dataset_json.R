# Public R API for on-demand, validated dataset JSON generation.
# Source this file once; no JSON generation happens until a loader is called.

load_dataset_json <- local({
  # source() knows this file even when the caller's working directory differs.
  origins <- vapply(sys.frames(), function(frame) {
    value <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.null(value)) "" else as.character(value)[1L]
  }, character(1))
  origins <- origins[nzchar(origins)]
  if (!length(origins)) {
    stop("Load this API using source('.../R/load_dataset_json.R')")
  }
  api_file <- normalizePath(tail(origins, 1L), mustWork = TRUE)
  repository <- normalizePath(
    file.path(dirname(api_file), "..", ".."),
    mustWork = TRUE
  )
  converter <- new.env(parent = baseenv())
  sys.source(
    file.path(repository, "linkml-json-iplementation/R/convert_catalog.R"),
    converter
  )

  scalar_string <- function(value, name) {
    if (!is.character(value) || length(value) != 1L ||
      is.na(value) || !nzchar(value)) {
      stop(name, " must be one nonempty string", call. = FALSE)
    }
  }
  find_python <- function(python) {
    if (is.null(python)) {
      python <- Sys.getenv("AMADEUS_CATALOG_PYTHON", unset = "")
      if (!nzchar(python)) {
        candidates <- c(
          file.path(repository, ".venv-catalog", "bin", "python"),
          file.path(repository, ".venv-catalog", "Scripts", "python.exe")
        )
        existing <- candidates[file.exists(candidates)]
        python <- if (length(existing)) existing[1L] else Sys.which("python3")
      }
    }
    scalar_string(python, "python")
    if (!file.exists(python)) python <- Sys.which(python)
    if (!nzchar(python) || !file.exists(python)) {
      stop(
        paste0(
          "Python interpreter not found; provide python= pointing to ",
          "the LinkML environment"
        ),
        call. = FALSE
      )
    }
    # Resolving the executable symlink would bypass a virtual environment.
    file.path(normalizePath(dirname(python), mustWork = TRUE), basename(python))
  }

  #' Generate and validate a dataset catalog JSON file using its LinkML model.
  #'
  #' @param dataset character(1). YAML filename stem, e.g. "aqs" or "gridmet".
  #' @param input_dir character(1). Folder containing reviewed dataset YAML.
  #' @param schema_yaml character(1). LinkML umbrella schema, not a data
  #'   catalog.
  #' @param output_file character(1) or NULL. Final JSON path; defaults to
  #'   linkml-json-iplementation/output/single-dataset/<dataset>.json.
  #' @param python character(1) or NULL. Interpreter in the LinkML environment.
  #' @param overwrite logical(1). Whether a successfully validated file may
  #'   replace existing output. Invalid input never replaces the final JSON.
  #' @param quiet logical(1). Suppress the success message.
  #' @return Invisibly, the generated JSON path. Attribute "validation_report"
  #'   identifies the validation receipt. Failures raise an R error.
  function(dataset,
           input_dir = file.path(repository, "global_dataset_catalogs"),
           schema_yaml = file.path(
             repository,
             paste0(
               "amadeus_schema/src/amadeus_schema/schema/",
               "amadeus_schema.yaml"
             )
           ),
           output_file = NULL, python = NULL, overwrite = TRUE, quiet = FALSE) {
    scalar_string(dataset, "dataset")
    dataset <- tolower(dataset)
    if (!grepl("^[a-z][a-z0-9_]*$", dataset)) {
      stop(
        "dataset must be a YAML filename stem, without paths or extensions",
        call. = FALSE
      )
    }
    if (dataset == "global_catalog") {
      stop(
        "Use run_pipeline.R for the global catalog and complete union checks",
        call. = FALSE
      )
    }
    scalar_string(input_dir, "input_dir")
    scalar_string(schema_yaml, "schema_yaml")
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
      stop("overwrite must be TRUE or FALSE", call. = FALSE)
    }
    if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
      stop("quiet must be TRUE or FALSE", call. = FALSE)
    }
    for (package in c("yaml", "jsonlite")) {
      if (!requireNamespace(package, quietly = TRUE)) {
        stop("Install R package: ", package, call. = FALSE)
      }
    }
    if (is.null(output_file)) {
      output_file <- file.path(
        repository, "linkml-json-iplementation/output/single-dataset",
        paste0(dataset, ".json")
      )
    }
    scalar_string(output_file, "output_file")
    if (!grepl("[.]json$", output_file, ignore.case = TRUE)) {
      stop("output_file must end with .json", call. = FALSE)
    }
    output <- normalizePath(output_file, mustWork = FALSE)
    # normalizePath may leave a nonexistent relative path relative on Unix.
    absolute <- function(path) {
      if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
        path <- file.path(getwd(), path)
      }
      normalizePath(path, mustWork = FALSE)
    }
    output <- absolute(output)
    report <- sub("[.]json$", ".validation.json", output, ignore.case = TRUE)
    input_dir <- absolute(input_dir)
    schema_yaml <- absolute(schema_yaml)
    protected <- vapply(
      c(
        "global_dataset_catalogs", "validated_catalog_handles", "amadeus_schema"
      ),
      function(x) {
        normalizePath(file.path(repository, x), mustWork = TRUE)
      }, character(1)
    )
    if (any(vapply(
      protected,
      function(x) {
        output == x || startsWith(output, paste0(x, .Platform$file.sep))
      },
      logical(1)
    ))) {
      stop(
        "Choose output_file outside the input catalog/schema directories",
        call. = FALSE
      )
    }
    python <- find_python(python)
    bridge <- file.path(
      repository, "linkml-json-iplementation/python/dataset_bridge.py"
    )
    work <- tempfile(pattern = paste0("amadeus-json-", dataset, "-"))
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE), add = TRUE)
    invoke <- function(command) {
      args <- c(
        bridge, command, "--dataset", dataset, "--input-dir", input_dir,
        "--schema-yaml", schema_yaml, "--work-dir", work,
        "--output-file", output, "--report-file", report
      )
      if (overwrite) args <- c(args, "--overwrite")
      log <- file.path(work, paste0(command, ".log"))
      status <- suppressWarnings(system2(
        python, vapply(args, shQuote, character(1)),
        stdout = log, stderr = log
      ))
      if (status != 0L) {
        detail <- paste(readLines(log, warn = FALSE), collapse = "\n")
        stop("Dataset '", dataset, "' failed during ", command, ":\n", detail,
          paste0(
            "\nCheck dependencies with: python -m pip install -r ",
            "linkml-json-iplementation/requirements.txt"
          ),
          call. = FALSE
        )
      }
    }
    prepared <- FALSE
    tryCatch(
      {
        invoke("prepare")
        prepared <- TRUE
        plan <- jsonlite::read_json(
          file.path(work, "dataset_plan.json"),
          simplifyVector = FALSE
        )
        converter$convert_catalog(
          plan$input, plan$schema_file, plan$candidate_file,
          file.path(
            repository,
            "dataset_spec_objects/catalog_updates_20261109/read_catalog.R"
          )
        )
        jsonlite::write_json(
          list(
            R_version = as.character(getRversion()),
            packages = list(
              yaml = as.character(utils::packageVersion("yaml")),
              jsonlite = as.character(utils::packageVersion("jsonlite"))
            )
          ), file.path(work, "r_conversion_report.json"),
          auto_unbox = TRUE, pretty = TRUE
        )
        invoke("publish")
      },
      error = function(e) {
        # Preserve detailed bridge diagnostics. Record R conversion errors
        # if the bridge has not already produced a failure receipt for
        # this invocation.

        prior <- if (file.exists(report)) {
          tryCatch(jsonlite::read_json(report), error = function(e) NULL)
        } else {
          NULL
        }
        if (prepared && (is.null(prior) || !identical(prior$status, "FAIL"))) {
          dir.create(dirname(report), recursive = TRUE, showWarnings = FALSE)
          jsonlite::write_json(list(
            status = "FAIL", dataset = dataset,
            phase = "R_conversion_or_dependency", error = conditionMessage(e),
            output_accepted = FALSE
          ), report, pretty = TRUE, auto_unbox = TRUE)
        }
        stop(conditionMessage(e), call. = FALSE)
      }
    )
    if (!file.exists(output)) {
      stop("Final JSON was not published: ", output, call. = FALSE)
    }
    if (!quiet) {
      message("Validated JSON: ", output, "\nValidation report: ", report)
    }
    result <- output
    attr(result, "validation_report") <- report
    invisible(result)
  }
})

# Factory for any additional dataset: no duplicated conversion/validation code.
make_dataset_json_loader <- function(dataset) {
  force(dataset)
  function(...) load_dataset_json(dataset = dataset, ...)
}

load_aqs_json <- make_dataset_json_loader("aqs")
load_geos_json <- make_dataset_json_loader("geos")
load_gridmet_json <- make_dataset_json_loader("gridmet")
load_merra2_json <- make_dataset_json_loader("merra2")
