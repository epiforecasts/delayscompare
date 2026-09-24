#!/bin/bash
#SBATCH --job-name=cs_weekly
#SBATCH --output=slurm/logs/cs_weekly_%A_%a.out
#SBATCH --error=slurm/logs/cs_weekly_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# One impose-arm cell (fixed GT/INC, no estimation) at dense weekly origins. Used
# for both "literature" (gt=4 inc=4) and "no_input" (gt=1 inc=1), matched to the
# learn-arm weekly array (02_run_scenarios_widepriors_weekly.sh), for a five-
# condition, origin-matched Q2 comparison all at weekly density. One task per
# origin.
#
# Usage: FREQ_FC=1 MAX_TP=24 sbatch --array=1-24%20 \
#          slurm/02_run_casestudy_weekly.sh <disease> <gt> <inc>

DISEASE=${1:-covid}
GT=${2:?must set GT (e.g. 4 for literature, 1 for no_input)}
INC=${3:?must set INC (e.g. 4 for literature, 1 for no_input)}

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
echo "Disease: $DISEASE  GT: $GT  INC: $INC  Timepoint: $TP"
echo "FREQ_FC=${FREQ_FC:-4}  MAX_TP=${MAX_TP:-8}"
echo "=========================================="

Rscript scripts/06d_scenariorun_casestudy.R $GT $INC $RT_OPTS $DISEASE "${TP}-${TP}"

echo "Done: $DISEASE gt=$GT inc=$INC tp=$TP"
