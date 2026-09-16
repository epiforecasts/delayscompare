#!/bin/bash
#SBATCH --job-name=turnpt
#SBATCH --output=slurm/logs/turnpt_%A_%a.out
#SBATCH --error=slurm/logs/turnpt_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Turning-point mechanism test. For one trajectory (flat/bend/turn) sweep BOTH delay
# axes and run all dense forecast origins (FREQ_FC=2 -> ~14d spacing, MAX_TP=20), so
# per-origin R-estimation error and forecast CRPS can be plotted against local dR.
#
# 11 misspecification cells: GT axis (gt=1..6, inc=4) + delay axis (gt=4, inc=1,2,3,5,6).
# Array = 11 cells x NREP replicates. tp="all" -> every origin in one job.
# Usage: NREP=3 sbatch --array=1-33 --export=ALL,NREP=3,DATA_TAG=turn,FREQ_FC=2,MAX_TP=20,WARMUP=500,HORIZON=42 \
#          slurm/02_run_scenarios_turningpoint.sh <disease>

DISEASE=${1:-covid}
NREP=${NREP:-3}
DATA_TAG=${DATA_TAG:?must set DATA_TAG=flat|bend|turn}

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then cd "$SLURM_SUBMIT_DIR/.." || exit 1; else cd "$SLURM_SUBMIT_DIR" || exit 1; fi
echo "Working directory: $(pwd)"
module load R

# 11 cells as parallel (gt,inc) arrays: 6 GT-axis then 5 delay-axis
GT_LIST=(1 2 3 4 5 6 4 4 4 4 4)
INC_LIST=(4 4 4 4 4 4 1 2 3 5 6)
NCELL=11

TASK_ID=$SLURM_ARRAY_TASK_ID
NTASKS=$(( NCELL * NREP ))
if (( TASK_ID < 1 || TASK_ID > NTASKS )); then
  echo "Error: array task must be in [1,${NTASKS}], got ${TASK_ID}" >&2; exit 2
fi

CELL=$(( (TASK_ID - 1) % NCELL ))
REP=$(( (TASK_ID - 1) / NCELL + 1 ))
GT=${GT_LIST[$CELL]}
INC=${INC_LIST[$CELL]}

RT_OPTS="latest"
SCRIPT="scripts/06h_scenariorun_horizon.R"

echo "=========================================="
echo "Task $TASK_ID  cell $CELL  GT=$GT INC=$INC  REP=$REP  tag=$DATA_TAG disease=$DISEASE"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE "all" $REP "$DATA_TAG"

echo "Done: $DISEASE $DATA_TAG gt=$GT inc=$INC rep=$REP"
