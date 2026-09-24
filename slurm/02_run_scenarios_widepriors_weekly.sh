#!/bin/bash
#SBATCH --job-name=wide_weekly
#SBATCH --output=slurm/logs/wide_weekly_%A_%a.out
#SBATCH --error=slurm/logs/wide_weekly_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Dense-origin learn arm (vary=gt, inc, or both), one task per forecast origin,
# spaced FREQ_FC weeks apart, capped at MAX_TP origins. covid's inc/both are
# expected to stay mostly non-convergent regardless of density (the observation
# delay is not identifiable under exponential growth, tp2-6 already failed at
# monthly resolution, §1); ebola/cholera were fully tractable monthly and should
# scale to weekly. vary=gt tests whether learning the generation time helps
# independently of local trend (recovery diagnostic: the growth-rate/R
# relationship constrains the GT mean even under constant Rt), and whether the
# benefit is instead driven by the direction of prior misspecification
# (over-long priors barely correct).
#
# Usage: FREQ_FC=1 MAX_TP=24 sbatch --array=1-24%20 \
#          slurm/02_run_scenarios_widepriors_weekly.sh <disease> <vary>

DISEASE=${1:-covid}
VARY=${2:-gt}

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TP=$SLURM_ARRAY_TASK_ID
RT_OPTS="latest"
SCRIPT="scripts/06f_scenariorun_widepriors.R"

echo "=========================================="
echo "Disease: $DISEASE  Vary: $VARY  Timepoint: $TP"
echo "FREQ_FC=${FREQ_FC:-4}  MAX_TP=${MAX_TP:-8}"
echo "=========================================="

Rscript $SCRIPT $DISEASE $VARY $RT_OPTS $TP $TP

echo "Done: $DISEASE vary=$VARY tp=$TP"
