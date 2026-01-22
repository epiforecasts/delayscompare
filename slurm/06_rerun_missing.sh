#!/bin/bash
set -euo pipefail
#SBATCH --job-name=rerun_missing
#SBATCH --output=slurm/logs/rerun_missing_%A_%a.out
#SBATCH --error=slurm/logs/rerun_missing_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-168%40

# Rerun missing jobs:
# 1. Cholera casestudy INC 3-6 (6 GT × 4 INC = 24 jobs)
# 2. Cholera weightprior all (6 GT × 6 INC = 36 jobs)
# 3. Resim ALL diseases (3 diseases × 6 GT × 6 INC = 108 jobs) - never run before!
# Total: 168 jobs

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID
RT_OPTS="latest"

# Jobs 1-24: Cholera casestudy INC 3-6
# Jobs 25-60: Cholera weightprior all
# Jobs 61-96: Ebola resim (36 jobs)
# Jobs 97-132: COVID resim (36 jobs)
# Jobs 133-168: Cholera resim (36 jobs)

if [ $TASK_ID -le 24 ]; then
    DISEASE="cholera"
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
    SCENARIO="casestudy"

    # 24 = 6 GT × 4 INC (INC 3,4,5,6)
    LOCAL_ID=$((TASK_ID - 1))
    GT=$(( (LOCAL_ID / 4) + 1 ))
    INC=$(( (LOCAL_ID % 4) + 3 ))  # INC 3,4,5,6

elif [ $TASK_ID -le 60 ]; then
    DISEASE="cholera"
    SCRIPT="scripts/06e_scenariorun_weightprior.R"
    SCENARIO="weightprior"

    # 36 = 6 GT × 6 INC
    LOCAL_ID=$((TASK_ID - 25))
    GT=$(( (LOCAL_ID / 6) + 1 ))
    INC=$(( (LOCAL_ID % 6) + 1 ))

elif [ $TASK_ID -le 96 ]; then
    DISEASE="ebola"
    SCRIPT="scripts/06a_scenariorun_resim.R"
    SCENARIO="resim"

    LOCAL_ID=$((TASK_ID - 61))
    GT=$(( (LOCAL_ID / 6) + 1 ))
    INC=$(( (LOCAL_ID % 6) + 1 ))

elif [ $TASK_ID -le 132 ]; then
    DISEASE="covid"
    SCRIPT="scripts/06a_scenariorun_resim.R"
    SCENARIO="resim"

    LOCAL_ID=$((TASK_ID - 97))
    GT=$(( (LOCAL_ID / 6) + 1 ))
    INC=$(( (LOCAL_ID % 6) + 1 ))

else
    DISEASE="cholera"
    SCRIPT="scripts/06a_scenariorun_resim.R"
    SCENARIO="resim"

    LOCAL_ID=$((TASK_ID - 133))
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
