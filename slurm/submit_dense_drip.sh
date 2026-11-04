#!/bin/bash
#SBATCH --job-name=tpdrip
#SBATCH --output=slurm/logs/tpdrip_%j.out
#SBATCH --error=slurm/logs/tpdrip_%j.err
#SBATCH --time=72:00:00
#SBATCH --mem=1gb
#SBATCH --ntasks=1
#SBATCH --nodes=1
set -uo pipefail

# Submits the disease-like dense fits in chunks of 2000 task lines once fewer than
# MAX_QUEUED of our job records are in the queue. The cluster-wide MaxJobCount (10000)
# counts each pending array task, so submitting everything at once would be refused
# and would leave other users unable to submit.
# Usage: sbatch slurm/submit_dense_drip.sh

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then cd "$SLURM_SUBMIT_DIR/.." || exit 1; else cd "$SLURM_SUBMIT_DIR" || exit 1; fi

TASK_FILE=slurm/tasks_turningpoint_dense.txt
FIRST=6732                       # disease-like lines are FIRST+1 .. end of file
LAST=$(wc -l < "$TASK_FILE")
CHUNK=2000
MAX_QUEUED=3000

offset=$FIRST
while (( offset < LAST )); do
  queued=$(squeue -u "$USER" -h -r | wc -l)
  if (( queued < MAX_QUEUED )); then
    n=$(( LAST - offset < CHUNK ? LAST - offset : CHUNK ))
    if id=$(sbatch --parsable --array=1-${n}%100 \
              --export=ALL,TASK_FILE=$TASK_FILE,OFFSET=$offset slurm/02_run_turningpoint_dense.sh); then
      echo "$(date -Is) submitted lines $((offset + 1))-$((offset + n)) as job $id ($queued queued)"
      offset=$(( offset + n ))
      continue
    fi
    echo "$(date -Is) sbatch refused at offset $offset, retrying"
  fi
  sleep 900
done
echo "$(date -Is) all disease-like lines submitted"
