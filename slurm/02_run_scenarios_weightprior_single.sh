#!/bin/bash
#SBATCH --job-name=wp_single
#SBATCH --output=slurm/logs/wp_single_%A_%a.out
#SBATCH --error=slurm/logs/wp_single_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=32gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Run weightprior scenarios: 1 timepoint × 1 vary × 1 weight_prior per task
# Array: N_tp * 6 jobs per disease (N_tp timepoints × 3 vary × 2 wp)
#   covid:   6 tp × 6 = 36 tasks  (array=1-36)
#   ebola:   8 tp × 6 = 48 tasks  (array=1-48)
#   cholera: 8 tp × 6 = 48 tasks  (array=1-48)
#
# Usage: sbatch --array=1-48%40 slurm/02_run_scenarios_weightprior_single.sh <disease>

DISEASE=${1:-covid}

# Get project root
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# 6 tasks per timepoint: gt/TRUE, gt/FALSE, inc/TRUE, inc/FALSE, both/TRUE, both/FALSE
TP=$(( ((TASK_ID - 1) / 6) + 1 ))
WITHIN=$(( (TASK_ID - 1) % 6 ))

case $WITHIN in
    0) VARY="gt";   WP="TRUE"  ;;
    1) VARY="gt";   WP="FALSE" ;;
    2) VARY="inc";  WP="TRUE"  ;;
    3) VARY="inc";  WP="FALSE" ;;
    4) VARY="both"; WP="TRUE"  ;;
    5) VARY="both"; WP="FALSE" ;;
esac

RT_OPTS="latest"
SCRIPT="scripts/06e_scenariorun_weightprior.R"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Vary: $VARY"
echo "weight_prior: $WP"
echo "Timepoint: $TP"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $DISEASE $VARY $WP $RT_OPTS $TP $TP

echo "Done: $DISEASE vary=$VARY wp=$WP tp=$TP"
