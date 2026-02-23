#!/bin/bash
#SBATCH --job-name=casestudy
#SBATCH --output=slurm/logs/casestudy_%A_%a.out
#SBATCH --error=slurm/logs/casestudy_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-108%40
set -euo pipefail

# Run ONLY case study analyses (real data)
# Array: 108 jobs = 3 diseases × 6 gt_levels × 6 inc_levels
# GT and INC vary independently
# Limited to 40 concurrent (LSHTM limit)
#
# Jobs 1-36:   ebola   (gt 1-6 × inc 1-6)
# Jobs 37-72:  covid   (gt 1-6 × inc 1-6)
# Jobs 73-108: cholera (gt 1-6 × inc 1-6)

# Get project root from SLURM_SUBMIT_DIR
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# Determine disease (36 jobs per disease)
if [ $TASK_ID -le 36 ]; then
    DISEASE="ebola"
    LOCAL_ID=$TASK_ID
elif [ $TASK_ID -le 72 ]; then
    DISEASE="covid"
    LOCAL_ID=$((TASK_ID - 36))
else
    DISEASE="cholera"
    LOCAL_ID=$((TASK_ID - 72))
fi

# Determine GT and INC from LOCAL_ID (1-36)
# LOCAL_ID = (GT-1)*6 + INC, so:
# GT = ((LOCAL_ID-1) / 6) + 1
# INC = ((LOCAL_ID-1) % 6) + 1
GT=$(( ((LOCAL_ID - 1) / 6) + 1 ))
INC=$(( ((LOCAL_ID - 1) % 6) + 1 ))

RT_OPTS="latest"

echo "=========================================="
echo "Case Study Analysis"
echo "Disease: $DISEASE"
echo "GT level: $GT"
echo "INC level: $INC"
echo "RT opts: $RT_OPTS"
echo "=========================================="

Rscript scripts/06d_scenariorun_casestudy.R $GT $INC $RT_OPTS $DISEASE

echo "Done: $DISEASE casestudy gt=$GT inc=$INC"
