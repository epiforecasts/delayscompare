#!/bin/bash
#SBATCH --job-name=wide_single
#SBATCH --output=slurm/logs/wide_single_%A_%a.out
#SBATCH --error=slurm/logs/wide_single_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=32gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Wide-prior regime-2 rerun: 1 timepoint × 1 vary per task. weight_prior=FALSE only.
# Tasks per disease: N_tp * 3 vary.
#   covid:   6 tp × 3 = 18 tasks  (array=1-18)
#   ebola:   8 tp × 3 = 24 tasks  (array=1-24)
#   cholera: 8 tp × 3 = 24 tasks  (array=1-24)
#
# Usage: sbatch --array=1-24%20 slurm/02_run_scenarios_widepriors_single.sh <disease>

DISEASE=${1:-covid}

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# 3 tasks per timepoint: gt, inc, both
TP=$(( ((TASK_ID - 1) / 3) + 1 ))
WITHIN=$(( (TASK_ID - 1) % 3 ))

case $WITHIN in
    0) VARY="gt"   ;;
    1) VARY="inc"  ;;
    2) VARY="both" ;;
esac

RT_OPTS="latest"
SCRIPT="scripts/06f_scenariorun_widepriors.R"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Vary: $VARY"
echo "Timepoint: $TP"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $DISEASE $VARY $RT_OPTS $TP $TP

echo "Done: $DISEASE vary=$VARY tp=$TP"
