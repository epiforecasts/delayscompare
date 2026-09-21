#!/bin/bash
#SBATCH --job-name=cs_lit_weekly
#SBATCH --output=slurm/logs/cs_lit_weekly_%A_%a.out
#SBATCH --error=slurm/logs/cs_lit_weekly_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# The "literature" condition (gt=4, inc=4: imposed literature values, no
# estimation) at dense weekly origins, matched to the GT-learn weekly array
# (02_run_scenarios_widepriors_weekly.sh), for an origin-matched estimate-vs-
# impose forecast comparison. One task per origin.
#
# Usage: FREQ_FC=1 MAX_TP=24 sbatch --array=1-24%20 \
#          slurm/02_run_casestudy_literature_weekly.sh <disease>

DISEASE=${1:-covid}

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TP=$SLURM_ARRAY_TASK_ID
RT_OPTS="latest"

echo "=========================================="
echo "Disease: $DISEASE  Condition: literature (gt=4 inc=4)  Timepoint: $TP"
echo "FREQ_FC=${FREQ_FC:-4}  MAX_TP=${MAX_TP:-8}"
echo "=========================================="

Rscript scripts/06d_scenariorun_casestudy.R 4 4 $RT_OPTS $DISEASE "${TP}-${TP}"

echo "Done: $DISEASE literature tp=$TP"
