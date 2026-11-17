# Schema-aware serialization, using jsonlite for JSON escaping and primitives.
# This file defines functions only; sourcing it performs no conversion.

resolve_json_rule <- function(rule, schema) {
  seen <- character()
  while (!is.null(rule[["$ref"]])) {
    ref <- rule[["$ref"]]
    if (!startsWith(ref, "#/") || ref %in% seen) {
      stop("Unsupported or cyclic JSON Schema reference: ", ref)
    }
    seen <- c(seen, ref)
    tokens <- strsplit(substring(ref, 3L), "/", fixed = TRUE)[[1L]]
    resolved <- schema
    for (token in tokens) {
      token <- gsub("~1", "/", gsub("~0", "~", token, fixed = TRUE), fixed = TRUE)
      resolved <- resolved[[token]]
    }
    if (is.null(resolved)) stop("Unresolved JSON Schema reference: ", ref)
    rule <- resolved
  }
  rule
}

catalog_json_text <- function(document, schema) {
  # A scalar is explicitly serialized as a scalar. Only schema-declared arrays
  # become arrays, so singleton sequences and empty collections remain arrays.
  key_cache <- new.env(parent = emptyenv())
  quote_key <- function(key) {
    if (!exists(key, envir = key_cache, inherits = FALSE)) {
      assign(key, as.character(jsonlite::toJSON(key, auto_unbox = TRUE)), key_cache)
    }
    get(key, envir = key_cache, inherits = FALSE)
  }
  encode <- function(value, rule, path) {
    rule <- resolve_json_rule(rule, schema)
    if (is.null(value)) return("null")
    types <- unlist(rule$type, use.names = FALSE)
    if ("array" %in% types) {
      values <- as.list(value)
      encoded <- vapply(seq_along(values), function(i) {
        encode(values[[i]], rule$items, paste0(path, "/", i - 1L))
      }, character(1))
      return(paste0("[", paste(encoded, collapse = ","), "]"))
    }
    if ("object" %in% types) {
      if (!is.list(value) || (length(value) && is.null(names(value)))) {
        stop(path, ": expected a named object")
      }
      keys <- names(value)
      if (length(keys) && (any(!nzchar(keys)) || anyDuplicated(keys))) {
        stop(path, ": empty or duplicate object key")
      }
      encoded <- vapply(keys, function(key) {
        property <- rule$properties[[key]]
        if (is.null(property)) stop(path, ": undeclared field ", key)
        paste0(quote_key(key), ":", encode(value[[key]], property, paste0(path, "/", key)))
      }, character(1))
      return(paste0("{", paste(encoded, collapse = ","), "}"))
    }
    if (is.list(value) || length(value) != 1L || is.na(value)) {
      stop(path, ": expected one finite scalar; use explicit NULL for JSON null")
    }
    if (is.numeric(value) && !is.finite(value)) stop(path, ": non-finite number")
    if (is.numeric(value)) {
      # jsonlite's default numeric printer can round grid coordinates to 15
      # digits. Seventeen significant digits round-trip an IEEE-754 double.
      number <- sprintf("%.17g", as.double(value))
      decimal <- Sys.localeconv()[["decimal_point"]]
      if (decimal != ".") number <- sub(decimal, ".", number, fixed = TRUE)
      return(number)
    }
    as.character(jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA,
                                  na = "null", null = "null"))
  }
  jsonlite::prettify(encode(document, schema, ""), indent = 2)
}

convert_catalog <- function(input, schema_file, output, reader_file, schema = NULL) {
  if (normalizePath(input, mustWork = TRUE) == normalizePath(output, mustWork = FALSE)) {
    stop("Input and output must differ")
  }
  reader <- new.env(parent = baseenv())
  sys.source(reader_file, envir = reader)
  document <- reader$read_catalog_yaml(input)
  if (is.null(schema)) schema <- jsonlite::read_json(schema_file, simplifyVector = FALSE)
  text <- catalog_json_text(document, schema)
  # Syntax-check the serialized text before publishing the individual output.
  if (!jsonlite::validate(text)) stop("Invalid JSON produced for ", input)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".json-", tmpdir = dirname(output))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  if (!file.copy(temporary, output, overwrite = TRUE)) stop("Unable to write ", output)
  invisible(output)
}
