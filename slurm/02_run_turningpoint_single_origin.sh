#!/bin/bash
#SBATCH --job-name=turnpt1
#SBATCH --output=slurm/logs/turnpt1_%A_%a.out
#SBATCH --error=slurm/logs/turnpt1_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Turning-point fits one origin per task, for cells whose all-origins job
# (02_run_scenarios_turningpoint.sh) ran out of time before saving anything.
# Same settings as that job. Each line of TASK_FILE is: disease tag rep tp
# Usage: sbatch --array=1-<lines> --export=ALL,TASK_FILE=slurm/tasks.txt,GT=4,INC=4 \
#          slurm/02_run_turningpoint_single_origin.sh

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then cd "$SLURM_SUBMIT_DIR/.." || exit 1; else cd "$SLURM_SUBMIT_DIR" || exit 1; fi

TASK_FILE=${TASK_FILE:?must set TASK_FILE}
GT=${GT:?must set GT}
INC=${INC:?must set INC}
read -r DISEASE DATA_TAG REP TP < <(sed -n "${SLURM_ARRAY_TASK_ID}p" "$TASK_FILE")

export FREQ_FC=2 MAX_TP=20 WARMUP=500 HORIZON=42

module load R
echo "Task $SLURM_ARRAY_TASK_ID: $DISEASE $DATA_TAG rep=$REP tp=$TP gt=$GT inc=$INC"
Rscript scripts/06h_scenariorun_horizon.R "$GT" "$INC" latest "$DISEASE" "${TP}-${TP}" "$REP" "$DATA_TAG"
echo "Done: $DISEASE $DATA_TAG rep=$REP tp=$TP"
