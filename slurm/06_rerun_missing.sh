#!/bin/bash
#SBATCH --job-name=rerun_missing
#SBATCH --output=slurm/logs/rerun_missing_%A_%a.out
#SBATCH --error=slurm/logs/rerun_missing_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-60%30

# Rerun missing jobs:
# 1. Cholera casestudy INC 3-6 (6 GT × 4 INC = 24 jobs)
# 2. Cholera weightprior all (6 GT × 6 INC = 36 jobs)
# Total: 60 jobs

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID
RT_OPTS="latest"
DISEASE="cholera"

# Jobs 1-24: Cholera casestudy INC 3-6
# Jobs 25-60: Cholera weightprior all

if [ $TASK_ID -le 24 ]; then
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
    SCENARIO="casestudy"

    # 24 = 6 GT × 4 INC (INC 3,4,5,6)
    LOCAL_ID=$((TASK_ID - 1))
    GT=$(( (LOCAL_ID / 4) + 1 ))
    INC=$(( (LOCAL_ID % 4) + 3 ))  # INC 3,4,5,6
else
    SCRIPT="scripts/06e_scenariorun_weightprior.R"
    SCENARIO="weightprior"

    # 36 = 6 GT × 6 INC
    LOCAL_ID=$((TASK_ID - 25))
    GT=$(( (LOCAL_ID / 6) + 1 ))
    INC=$(( (LOCAL_ID % 6) + 1 ))
fi

echo "=========================================="
echo "Rerun Missing Jobs"
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Scenario: $SCENARIO"
echo "GT level: $GT"
echo "INC level: $INC"
echo "RT opts: $RT_OPTS"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE

echo "Done: $DISEASE $SCENARIO gt=$GT inc=$INC"
