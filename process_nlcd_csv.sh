#!/usr/bin/env bash
set -euo pipefail

# Download, process, and calculate an NLCD product for locations in a CSV.
# Usage: ./process_nlcd_csv.sh locations.csv ./data 2021 "Land Cover"
# The CSV must contain id, lon, and lat columns.
# The product argument is optional and defaults to "Land Cover".

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
CONTAINER="${SCRIPT_DIR}/tests/testthat/container/ama_container.sif"
CSV_FILE="${1:-locations.csv}"
DATA_DIR="${2:-./data}"
YEAR="${3:-2021}"
PRODUCT_INPUT="${4:-Land Cover}"
LOC_RADIUS="${LOC_RADIUS:-1000}"

case "${PRODUCT_INPUT,,}" in
    "land cover")
        PRODUCT="Land Cover"
        PRODUCT_SLUG="land_cover"
        CLASS_NAMES="mrlc"
        ;;
    "land cover change")
        PRODUCT="Land Cover Change"
        PRODUCT_SLUG="land_cover_change"
        CLASS_NAMES="mrlc"
        ;;
    "land cover confidence")
        PRODUCT="Land Cover Confidence"
        PRODUCT_SLUG="land_cover_confidence"
        CLASS_NAMES="code"
        ;;
    "fractional impervious surface")
        PRODUCT="Fractional Impervious Surface"
        PRODUCT_SLUG="fractional_impervious_surface"
        CLASS_NAMES="code"
        ;;
    "impervious descriptor")
        PRODUCT="Impervious Descriptor"
        PRODUCT_SLUG="impervious_descriptor"
        CLASS_NAMES="mrlc"
        ;;
    "spectral change day of year")
        PRODUCT="Spectral Change Day of Year"
        PRODUCT_SLUG="spectral_change_day_of_year"
        CLASS_NAMES="mrlc"
        ;;
    *)
        echo "ERROR: Unknown NLCD product: $PRODUCT_INPUT" >&2
        echo "Valid products are:" >&2
        echo "  Land Cover" >&2
        echo "  Land Cover Change" >&2
        echo "  Land Cover Confidence" >&2
        echo "  Fractional Impervious Surface" >&2
        echo "  Impervious Descriptor" >&2
        echo "  Spectral Change Day of Year" >&2
        exit 1
        ;;
esac

case "$CSV_FILE" in
    /*) csv_candidate="$CSV_FILE" ;;
    *) csv_candidate="${PWD}/${CSV_FILE}" ;;
esac

case "$DATA_DIR" in
    /*) data_candidate="$DATA_DIR" ;;
    *) data_candidate="${PWD}/${DATA_DIR}" ;;
esac

if ! command -v apptainer >/dev/null 2>&1; then
    echo "ERROR: apptainer is not in PATH." >&2
    exit 1
fi

if [ ! -f "$CONTAINER" ]; then
    echo "ERROR: Apptainer image not found: $CONTAINER" >&2
    exit 1
fi

if [ ! -f "$csv_candidate" ]; then
    echo "ERROR: CSV file not found: $CSV_FILE" >&2
    exit 1
fi

if ! printf '%s' "$YEAR" | grep -Eq '^[0-9]{4}$'; then
    echo "ERROR: YEAR must be a four-digit year." >&2
    exit 1
fi

if ! printf '%s' "$LOC_RADIUS" | grep -Eq '^[0-9]+([.][0-9]+)?$'; then
    echo "ERROR: LOC_RADIUS must be a non-negative number." >&2
    exit 1
fi

CSV_FILE_ABS=$(cd "$(dirname "$csv_candidate")" && pwd -P)/$(basename "$csv_candidate")
mkdir -p "$data_candidate"
DATA_DIR_ABS=$(cd "$data_candidate" && pwd -P)

echo "============================================"
echo "NLCD processing"
echo "CSV file : $CSV_FILE_ABS"
echo "Data dir : $DATA_DIR_ABS"
echo "Year     : $YEAR"
echo "Product  : $PRODUCT"
echo "Radius   : $LOC_RADIUS"
echo "============================================"

apptainer exec \
    --bind "${SCRIPT_DIR}:/project:ro" \
    --bind "${CSV_FILE_ABS}:/input/locations.csv:ro" \
    --bind "${DATA_DIR_ABS}:/output" \
    "$CONTAINER" \
    Rscript -e "setwd('/project'); devtools::load_all(quiet = TRUE); withr::local_options(
      list(sf_use_s2 = FALSE)
    ); loc <- utils::read.csv(
      '/input/locations.csv',
      stringsAsFactors = FALSE,
      check.names = FALSE
    ); required <- c('id', 'lon', 'lat'); missing <- setdiff(required, names(loc));
    if (length(missing) > 0L) {
      stop('CSV is missing required column(s): ', paste(missing, collapse = ', '))
    }; if (nrow(loc) == 0L) stop('CSV contains no locations.');
    locs <- terra::vect(loc, geom = c('lon', 'lat'), crs = 'EPSG:4326');
    product_dir <- file.path('/output', '${PRODUCT_SLUG}');
    download_nlcd(
      product = '${PRODUCT}',
      year = ${YEAR},
      directory_to_save = product_dir,
      acknowledgement = TRUE
    ); nlcd <- process_nlcd(path = product_dir, year = ${YEAR});
    result <- suppressMessages(calculate_nlcd(
      from = nlcd,
      locs = locs,
      locs_id = 'id',
      radius = ${LOC_RADIUS},
      mode = 'exact',
      class_names = '${CLASS_NAMES}',
      geom = FALSE
    )); output_file <- file.path(
      '/output',
      'nlcd_${YEAR}_${PRODUCT_SLUG}_results.csv'
    );
    utils::write.csv(result, output_file, row.names = FALSE);
    message('Saved result: ', output_file); print(result)"

echo "NLCD processing completed successfully."
