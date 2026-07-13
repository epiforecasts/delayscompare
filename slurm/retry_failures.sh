#!/bin/bash
# Log the TIMEOUT/FAILED array tasks of a completed job to results/rerun_log.tsv,
# mapped to disease/scenario/gt/inc/tp AND the sampler settings that job used, and
# print the comma-separated task ids to resubmit. Recording the settings inline makes
# the log a self-contained "what failed at which settings" record, and comparing
# tasks across rounds tells transient failures (a cell fails once then succeeds) from
# systematic ones (a cell fails at every setting).
#
# Usage: slurm/retry_failures.sh <job_id> <round> <adapt_delta> <max_treedepth> <wall_h>
#   run from project root
set -euo pipefail

job=$1
round=$2
adapt_delta=${3:-NA}
max_treedepth=${4:-NA}
wall_h=${5:-NA}
log=results/rerun_log.tsv
ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)

if [ ! -f "$log" ]; then
  printf 'round\ttimestamp\tjob_id\tadapt_delta\tmax_treedepth\twall_h\ttask\tdisease\tscenario\tgt\tinc\ttp\tstate\n' > "$log"
fi

ids=""
while IFS='|' read -r jid state _; do
  t=$(printf '%s' "$jid" | grep -oE '_[0-9]+$' | tr -d '_')
  [ -z "$t" ] && continue
  cell=$(( (t - 1) / 8 + 1 )); tp=$(( (t - 1) % 8 + 1 ))
  if   [ "$cell" -le 108 ]; then d=ebola;   l=$cell;
  elif [ "$cell" -le 216 ]; then d=covid;   l=$((cell - 108));
  else                            d=cholera; l=$((cell - 216)); fi
  if   [ "$l" -le 36 ]; then s=constRt;   cb=$l;
  elif [ "$l" -le 72 ]; then s=incdecRt;  cb=$((l - 36));
  else                       s=casestudy; cb=$((l - 72)); fi
  gt=$(( (cb - 1) / 6 + 1 )); inc=$(( (cb - 1) % 6 + 1 ))
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$round" "$ts" "$job" "$adapt_delta" "$max_treedepth" "$wall_h" \
    "$t" "$d" "$s" "$gt" "$inc" "$tp" "$state" >> "$log"
  ids="${ids}${ids:+,}${t}"
done < <(sacct -j "$job" --format=JobID,State -p 2>/dev/null | grep -E '^[0-9]+_[0-9]+\|(TIMEOUT|FAILED)')

printf '%s\n' "$ids"
