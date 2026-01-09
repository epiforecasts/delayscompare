#!/bin/bash
#SBATCH --job-name=cholera_cs
#SBATCH --output=slurm/logs/cholera_cs_%A_%a.out
#SBATCH --error=slurm/logs/cholera_cs_%A_%a.err
#SBATCH --time=72:00:00
#SBATCH --mem=32gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-192%30

# Rerun cholera casestudy INC 3-6 split by individual timepoint
# 6 GT × 4 INC × 8 timepoints = 192 jobs
# Longer time (72h) and more memory (32gb) for difficult combinations

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID
DISEASE="cholera"
RT_OPTS="latest"

# 192 jobs = 6 GT × 4 INC × 8 timepoints
# TASK_ID = (GT-1)*32 + (INC_IDX)*8 + TP
# where INC_IDX = 0,1,2,3 for INC = 3,4,5,6

TASK_IDX=$((TASK_ID - 1))
GT=$(( (TASK_IDX / 32) + 1 ))
REMAINDER=$((TASK_IDX % 32))
INC_IDX=$((REMAINDER / 8))
INC=$((INC_IDX + 3))  # INC 3,4,5,6
TP=$(( (REMAINDER % 8) + 1 ))  # Timepoints 1-8

TP_RANGE="${TP}-${TP}"

echo "=========================================="
echo "Cholera Casestudy (timeout rerun)"
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "GT level: $GT"
echo "INC level: $INC"
echo "Timepoint: $TP"
echo "RT opts: $RT_OPTS"
echo "=========================================="

Rscript scripts/06d_scenariorun_casestudy.R $GT $INC $RT_OPTS $DISEASE $TP_RANGE

echo "Done: $DISEASE casestudy gt=$GT inc=$INC tp=$TP"
