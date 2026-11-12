#!/bin/sh
# Linux-only optional sampler. Never signals the scientific process.
parent=$1
samples=$2
stop_file=$3
interval=$4
printf 'epoch_seconds,main_R_RSS_MiB,workload_tree_RSS_sum_MiB\n' > "$samples" || exit 0
rss_kib() {
  awk '/^VmRSS:/ {print $2; found=1; exit} END {if (!found) print 0}' "/proc/$1/status" 2>/dev/null
}
tree_kib() {
  task_pid=$1
  [ "$task_pid" = "$$" ] && return
  [ -r "/proc/$task_pid/status" ] || return
  rss_kib "$task_pid"
  if [ -r "/proc/$task_pid/task/$task_pid/children" ]; then
    for child_pid in $(cat "/proc/$task_pid/task/$task_pid/children" 2>/dev/null); do
      tree_kib "$child_pid"
    done
  fi
}
while [ -r "/proc/$parent/status" ] && [ ! -e "$stop_file" ]; do
  main=$(rss_kib "$parent")
  total=$(tree_kib "$parent" | awk '{s+=$1} END {print s+0}')
  stamp=$(date +%s)
  awk -v t="$stamp" -v m="${main:-0}" -v s="${total:-0}" 'BEGIN {printf "%s,%.6f,%.6f\n",t,m/1024,s/1024}' >> "$samples"
  sleep "$interval"
done
