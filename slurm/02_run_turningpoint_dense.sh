#!/bin/bash
#SBATCH --job-name=tpdense
#SBATCH --output=slurm/logs/tpdense_%A_%a.out
#SBATCH --error=slurm/logs/tpdense_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Dense-origin turning-point fits, one fit per task. Each line of TASK_FILE is:
#   disease tag rep origin_day gt inc
# with tag "-" for the disease-like replicates. Fits are saved under run label hzd so
# they stay apart from the regular-schedule runs. The array size limit is below the
# number of lines, so OFFSET shifts the array index into the file.
# Usage: sbatch --array=1-4000%150 --export=ALL,TASK_FILE=slurm/tasks_turningpoint_dense.txt,OFFSET=0 \
#          slurm/02_run_turningpoint_dense.sh

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then cd "$SLURM_SUBMIT_DIR/.." || exit 1; else cd "$SLURM_SUBMIT_DIR" || exit 1; fi

TASK_FILE=${TASK_FILE:?must set TASK_FILE}
OFFSET=${OFFSET:-0}
LINE=$(( SLURM_ARRAY_TASK_ID + OFFSET ))
read -r DISEASE DATA_TAG REP DAY GT INC < <(sed -n "${LINE}p" "$TASK_FILE")
[[ "$DATA_TAG" == "-" ]] && DATA_TAG=""

export HORIZON=42 WARMUP=500 ORIGIN_DAYS=$DAY RUN_LABEL=hzd

module load R
echo "Line $LINE: $DISEASE ${DATA_TAG:-resim} rep=$REP day=$DAY gt=$GT inc=$INC"
Rscript scripts/06h_scenariorun_horizon.R "$GT" "$INC" latest "$DISEASE" all "$REP" "$DATA_TAG"
echo "Done: line $LINE"
