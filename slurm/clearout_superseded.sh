#!/bin/bash
# Clear superseded cholera and Ebola result files after the delay-parameter
# update (cholera GT -> Gamma(shape=0.5, rate=0.1); Ebola GT -> 10.9 mean;
# Ebola IP -> 8.5 mean). Those runs must be re-generated.
#
# COVID results are unaffected and are kept. Input data under data/ is never
# touched. Run from the project root (locally or on the cluster).
#
#   ./slurm/clearout_superseded.sh            # dry run: list files + size to free
#   ./slurm/clearout_superseded.sh --delete   # actually remove them
#
# Override the results location with RESULTS_DIR=/path ./slurm/clearout_superseded.sh
set -euo pipefail

RESULTS_DIR="${RESULTS_DIR:-results}"
DELETE=0
[ "${1:-}" = "--delete" ] && DELETE=1

if [ ! -d "$RESULTS_DIR" ]; then
  echo "No results directory at '$RESULTS_DIR' (set RESULTS_DIR or run from project root)."
  exit 1
fi

mapfile -t files < <(find "$RESULTS_DIR" -type f \( -iname '*cholera*' -o -iname '*ebola*' \) 2>/dev/null)

echo "Superseded cholera/Ebola result files under '$RESULTS_DIR': ${#files[@]}"
if [ "${#files[@]}" -eq 0 ]; then
  echo "Nothing to do."
  exit 0
fi
du -ch "${files[@]}" 2>/dev/null | tail -1 | sed 's/\ttotal/\ttotal to free/'

if [ "$DELETE" -eq 1 ]; then
  printf '%s\0' "${files[@]}" | xargs -0 -r rm -f
  echo "Deleted ${#files[@]} files."
else
  printf '  %s\n' "${files[@]:0:15}"
  [ "${#files[@]}" -gt 15 ] && echo "  ... and $(( ${#files[@]} - 15 )) more"
  echo "(dry run — re-run with --delete to remove)"
fi
