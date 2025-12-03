#!/bin/bash
#SBATCH --job-name=scenarios
#SBATCH --output=slurm/logs/scenarios_%A_%a.out
#SBATCH --error=slurm/logs/scenarios_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-432%40

# Run all scenario analyses with independent GT and INC variation
# Array structure: 432 jobs = 3 diseases × 4 scenario_types × 6 GT × 6 INC
# Limited to 40 concurrent jobs (LSHTM max)
#
# Per disease: 144 jobs (4 scenarios × 36 GT×INC combinations)
# Jobs 1-144:    ebola
# Jobs 145-288:  covid
# Jobs 289-432:  cholera

# Get project root from SLURM_SUBMIT_DIR
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.."
else
    cd "$SLURM_SUBMIT_DIR"
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# Determine disease (144 jobs per disease)
if [ $TASK_ID -le 144 ]; then
    DISEASE="ebola"
    LOCAL_ID=$TASK_ID
elif [ $TASK_ID -le 288 ]; then
    DISEASE="covid"
    LOCAL_ID=$((TASK_ID - 144))
else
    DISEASE="cholera"
    LOCAL_ID=$((TASK_ID - 288))
fi

# Within each disease: 36 jobs per scenario type (6 GT × 6 INC)
# LOCAL_ID 1-36=constRt, 37-72=incdecRt, 73-108=casestudy, 109-144=weightprior
if [ $LOCAL_ID -le 36 ]; then
    SCENARIO="constRt"
    COMBO_ID=$LOCAL_ID
    SCRIPT="scripts/06b_scenariorun_constRt.R"
elif [ $LOCAL_ID -le 72 ]; then
    SCENARIO="incdecRt"
    COMBO_ID=$((LOCAL_ID - 36))
    SCRIPT="scripts/06c_scenariorun_incdecRt.R"
elif [ $LOCAL_ID -le 108 ]; then
    SCENARIO="casestudy"
    COMBO_ID=$((LOCAL_ID - 72))
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
else
    SCENARIO="weightprior"
    COMBO_ID=$((LOCAL_ID - 108))
    SCRIPT="scripts/06e_scenariorun_weightprior.R"
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
