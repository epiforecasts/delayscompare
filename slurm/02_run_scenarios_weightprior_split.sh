#!/bin/bash
#SBATCH --job-name=wp_split
#SBATCH --output=slurm/logs/wp_split_%A_%a.out
#SBATCH --error=slurm/logs/wp_split_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=32gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Run weightprior scenarios split by timepoint
# Each original job (36 GT×INC combos) is split into 4 parts (2 timepoints each)
# Array: 144 jobs = 36 GT×INC × 4 timepoint chunks
# Usage: sbatch --array=1-144%40 slurm/02_run_scenarios_weightprior_split.sh <disease>
#   disease: covid, ebola, or cholera

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

# 144 jobs = 36 GT×INC combos × 4 timepoint chunks
# TASK_ID 1-4 = GT=1,INC=1 chunks 1-4
# TASK_ID 5-8 = GT=1,INC=2 chunks 1-4
# etc.

COMBO_ID=$(( ((TASK_ID - 1) / 4) + 1 ))  # 1-36
CHUNK=$(( ((TASK_ID - 1) % 4) + 1 ))      # 1-4

# Determine GT and INC from COMBO_ID (1-36)
GT=$(( ((COMBO_ID - 1) / 6) + 1 ))
INC=$(( ((COMBO_ID - 1) % 6) + 1 ))

# Timepoint ranges: chunk 1=tp1-2, chunk 2=tp3-4, chunk 3=tp5-6, chunk 4=tp7-8
TP_START=$(( (CHUNK - 1) * 2 + 1 ))
TP_END=$(( CHUNK * 2 ))

RT_OPTS="latest"
SCRIPT="scripts/06e_scenariorun_weightprior.R"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Scenario: weightprior"
echo "GT level: $GT"
echo "INC level: $INC"
echo "Timepoint chunk: $CHUNK (tp $TP_START-$TP_END)"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE $TP_START $TP_END

echo "Done: $DISEASE weightprior gt=$GT inc=$INC tp=$TP_START-$TP_END"
