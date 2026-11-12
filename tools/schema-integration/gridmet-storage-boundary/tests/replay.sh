#!/bin/sh
# Qualification convenience only; not imported/required by Amadeus or the integration.
# Ordinary caller -> automatic environment capture -> verified query -> independent source QA.
if [ "$#" -ne 6 ]; then
  echo 'Usage: sh tests/replay.sh NEW_OUTPUT VARIABLES START END SITES_CSV RAW_CACHE' >&2
  exit 2
fi
review_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
case $1 in /*) output_path=$1 ;; *) output_path=./$1 ;; esac
if [ -e "$output_path" ]; then echo 'Choose a NEW output directory; no overwrite' >&2; exit 2; fi
mkdir -p -- "$output_path" || exit 2
output_path=$(CDPATH= cd -- "$output_path" && pwd -P)
status=0
TZ=UTC Rscript "$review_root/examples/ordinary_with_recording.R" "$review_root/integration" "$output_path" "$2" "$3" "$4" "$5" "$6" > "$output_path/replay.log" 2>&1 || status=$?
TZ=UTC Rscript "$review_root/tests/capture_environment.R" "$output_path/environment.json" > "$output_path/environment.log" 2>&1 || echo 'Environment capture failed; inspect environment.log' >&2
if [ "$status" -eq 0 ]; then
  IFS= read -r published_run < "$output_path/LATEST.txt"
  Rscript "$review_root/integration/R/database.R" "$published_run" duckdb 'SELECT COUNT(*) AS records FROM "AmbientValueAtLocation"' > "$output_path/query.log" 2>&1 || status=$?
  if [ "$status" -eq 0 ]; then
    Rscript "$review_root/tests/direct_source_check.R" "$published_run" "$output_path/source-checks" > "$output_path/source-check.log" 2>&1 || status=$?
  fi
fi
printf 'qualification_exit_status=%s\nIndependent human sign-off is not implied.\nLogs: %s\n' "$status" "$output_path" | tee "$output_path/replay-summary.txt"
exit "$status"
