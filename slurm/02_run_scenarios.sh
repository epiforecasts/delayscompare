#!/bin/bash
#SBATCH --job-name=scenarios
#SBATCH --output=slurm/logs/scenarios_%A_%a.out
#SBATCH --error=slurm/logs/scenarios_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-324%40
set -euo pipefail

# Run all scenario analyses with independent GT and INC variation
# Array structure: 324 jobs = 3 diseases × 3 scenario_types × 6 GT × 6 INC
# Limited to 40 concurrent jobs (LSHTM max)
#
# Per disease: 108 jobs (3 scenarios × 36 GT×INC combinations)
# Jobs 1-108:    ebola
# Jobs 109-216:  covid
# Jobs 217-324:  cholera

# Get project root from SLURM_SUBMIT_DIR
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# Determine disease (108 jobs per disease)
if [ $TASK_ID -le 108 ]; then
    DISEASE="ebola"
    LOCAL_ID=$TASK_ID
elif [ $TASK_ID -le 216 ]; then
    DISEASE="covid"
    LOCAL_ID=$((TASK_ID - 108))
else
    DISEASE="cholera"
    LOCAL_ID=$((TASK_ID - 216))
fi

# Within each disease: 36 jobs per scenario type (6 GT × 6 INC)
# LOCAL_ID 1-36=constRt, 37-72=incdecRt, 73-108=casestudy
if [ $LOCAL_ID -le 36 ]; then
    SCENARIO="constRt"
    COMBO_ID=$LOCAL_ID
    SCRIPT="scripts/06b_scenariorun_constRt.R"
elif [ $LOCAL_ID -le 72 ]; then
    SCENARIO="incdecRt"
    COMBO_ID=$((LOCAL_ID - 36))
    SCRIPT="scripts/06c_scenariorun_incdecRt.R"
else
    SCENARIO="casestudy"
    COMBO_ID=$((LOCAL_ID - 72))
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
fi

# Determine GT and INC from COMBO_ID (1-36)
# COMBO_ID = (GT-1)*6 + INC
GT=$(( ((COMBO_ID - 1) / 6) + 1 ))
INC=$(( ((COMBO_ID - 1) % 6) + 1 ))

RT_OPTS="latest"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Scenario: $SCENARIO"
echo "GT level: $GT"
echo "INC level: $INC"
echo "RT opts: $RT_OPTS"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE

echo "Done: $DISEASE $SCENARIO gt=$GT inc=$INC"
