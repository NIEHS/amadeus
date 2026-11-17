# Serialization regressions that generic auto_unbox=TRUE converters can miss.
args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
here <- dirname(dirname(normalizePath(script)))
repo <- dirname(here)
schema_file <- if (length(args)) args[1] else file.path(here, "output", "amadeus_schema.schema.json")
source(file.path(here, "R", "convert_catalog.R"))
source(file.path(repo, "dataset_spec_objects/catalog_updates_20261109/read_catalog.R"))
schema <- jsonlite::read_json(schema_file, simplifyVector = FALSE)
checks <- 0L
check <- function(condition) {
  stopifnot(condition)
  checks <<- checks + 1L
}
# Singleton and empty arrays, scalar strings/numbers/booleans, explicit null,
# absent fields, Unicode, quotes, backslashes, and leading-zero strings.
doc <- list(schema_version = "0.1.0",
            data_sources = list(list(id = "amadeus:source/001", name = "001",
                                    requires_authentication = FALSE,
                                    description = "µg/m³: \"quoted\" \\ path\nnext")),
            products = list(), grid_definitions = NULL,
            product_variables = list(list(id = "v", product = "p", canonical_variable = "c",
              native_name = "001", native_units_ucum = "1", native_value_offset = -5,
              metadata_gaps = list())))
x <- jsonlite::fromJSON(catalog_json_text(doc, schema), simplifyVector = FALSE)
check(is.list(x$data_sources) && length(x$data_sources) == 1L)
check(is.list(x$products) && length(x$products) == 0L)
check("grid_definitions" %in% names(x) && is.null(x$grid_definitions))
check(!"assets" %in% names(x))
check(identical(x$data_sources[[1]]$requires_authentication, FALSE))
check(identical(x$data_sources[[1]]$name, "001"))
check(identical(x$data_sources[[1]]$description, doc$data_sources[[1]]$description))
check(identical(x$product_variables[[1]]$native_units_ucum, "1"))
check(x$product_variables[[1]]$native_value_offset == -5)
check(is.list(x$product_variables[[1]]$metadata_gaps) && length(x$product_variables[[1]]$metadata_gaps) == 0L)
doc$product_variables[[1]]$metadata_gaps <- list("under_investigation")
x <- jsonlite::fromJSON(catalog_json_text(doc, schema), simplifyVector = FALSE)
check(is.list(x$product_variables[[1]]$metadata_gaps) && length(x$product_variables[[1]]$metadata_gaps) == 1L)
# Native grid spacing/origins must round-trip without 15-digit rounding.
doc$grid_definitions <- list(list(id = "g", crs = "EPSG:4326",
  resolution_x = 0.041666666666671404, resolution_y = 0.008333333333333333,
  origin_x = -180.00013888888893))
x <- jsonlite::fromJSON(catalog_json_text(doc, schema), simplifyVector = FALSE)
check(identical(x$grid_definitions[[1]]$resolution_x, doc$grid_definitions[[1]]$resolution_x))
check(identical(x$grid_definitions[[1]]$origin_x, doc$grid_definitions[[1]]$origin_x))
# Large integer YAML values must not be truncated to R's 32-bit range.
temporary <- tempfile(fileext = ".yaml")
writeLines(c('schema_version: "0.1.0"', 'assets:', '  - id: a',
             '    product: p', '    size_bytes: 5000000000'), temporary)
x <- read_catalog_yaml(temporary)
converted <- jsonlite::fromJSON(catalog_json_text(x, schema), simplifyVector = FALSE)
check(converted$assets[[1]]$size_bytes == 5000000000)
writeLines(c('schema_version: "0.1.0"', 'assets:', '  - id: a',
             '    product: p', '    size_bytes: 9007199254740991'), temporary)
x <- read_catalog_yaml(temporary)
converted <- jsonlite::fromJSON(catalog_json_text(x, schema), simplifyVector = FALSE)
check(converted$assets[[1]]$size_bytes == 2^53 - 1)
writeLines('value: 9007199254740993', temporary)
check(inherits(try(read_catalog_yaml(temporary), silent = TRUE), "try-error"))
unlink(temporary)
# No silent replacement of invalid non-finite data or undeclared properties.
doc$product_variables[[1]]$native_value_offset <- Inf
check(inherits(try(catalog_json_text(doc, schema), silent = TRUE), "try-error"))
check(inherits(try(catalog_json_text(list(unknown_field = "x"), schema), silent = TRUE), "try-error"))
cat("PASS:", checks, "R serialization checks\n")
