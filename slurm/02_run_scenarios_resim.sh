#!/bin/bash
#SBATCH --job-name=resim
#SBATCH --output=slurm/logs/resim_%A_%a.out
#SBATCH --error=slurm/logs/resim_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Run resim scenarios: 1 GT x 1 INC per task
# Array: 36 jobs = 6 GT x 6 INC per disease
#
# Usage: sbatch --array=1-36%40 slurm/02_run_scenarios_resim.sh <disease>

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

if (( TASK_ID < 1 || TASK_ID > 36 )); then
  echo "Error: SLURM_ARRAY_TASK_ID must be in [1,36], got ${TASK_ID}" >&2
  exit 2
fi

GT=$(( ((TASK_ID - 1) / 6) + 1 ))
INC=$(( ((TASK_ID - 1) % 6) + 1 ))

RT_OPTS="latest"
SCRIPT="scripts/06a_scenariorun_resim.R"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "GT level: $GT"
echo "INC level: $INC"
echo "RT opts: $RT_OPTS"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE

echo "Done: $DISEASE resim gt=$GT inc=$INC"
