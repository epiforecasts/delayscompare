#!/bin/bash
#SBATCH --job-name=resim
#SBATCH --output=slurm/logs/resim_%A_%a.out
#SBATCH --error=slurm/logs/resim_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Run resim (re-simulated disease-like) scenarios, one estimate_infections fit per
# (GT, INC, timepoint) task. 288 tasks per disease = 36 cells (6 GT x 6 INC) x 8
# timepoints. Timepoints over-provisioned to 8; an out-of-range timepoint is a clean
# no-op (sim_scenarios returns empty, runner writes nothing).
#
# Usage: sbatch --array=1-288%40 slurm/02_run_scenarios_resim.sh <disease>
# Sampler overridable via --export=ALL,WARMUP=...,ADAPT_DELTA=...,SAMPLES=...

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

if (( TASK_ID < 1 || TASK_ID > 288 )); then
  echo "Error: SLURM_ARRAY_TASK_ID must be in [1,288], got ${TASK_ID}" >&2
  exit 2
fi

# Split into a cell (1-36) and a timepoint (1-8), tp innermost so a cell's 8
# timepoints are contiguous.
CELL_ID=$(( ((TASK_ID - 1) / 8) + 1 ))
TP=$(( ((TASK_ID - 1) % 8) + 1 ))

GT=$(( ((CELL_ID - 1) / 6) + 1 ))
INC=$(( ((CELL_ID - 1) % 6) + 1 ))

RT_OPTS="latest"
SCRIPT="scripts/06a_scenariorun_resim.R"

echo "=========================================="
echo "Task: $TASK_ID  Cell: $CELL_ID  Timepoint: $TP"
echo "Disease: $DISEASE  GT: $GT  INC: $INC"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE "${TP}-${TP}"

echo "Done: $DISEASE resim gt=$GT inc=$INC tp=$TP"
