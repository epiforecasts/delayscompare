#!/bin/bash
#SBATCH --job-name=scenarios
#SBATCH --output=slurm/logs/scenarios_%A_%a.out
#SBATCH --error=slurm/logs/scenarios_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-2592%40
set -euo pipefail

# One SLURM task per (disease, scenario_type, GT, INC, timepoint) = one estimate_infections fit.
# 2592 tasks = 324 cells (3 diseases × 3 scenario_types × 6 GT × 6 INC) × 8 timepoints.
# Timepoints are over-provisioned to 8; an out-of-range timepoint is a clean no-op
# (sim_scenarios returns empty and the runner writes nothing). 40 concurrent (LSHTM max).

# Get project root from SLURM_SUBMIT_DIR
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

# Split the task id into a cell (1-324) and a timepoint (1-8), tp innermost so a
# cell's 8 timepoints are contiguous.
CELL_ID=$(( ((SLURM_ARRAY_TASK_ID - 1) / 8) + 1 ))
TP=$(( ((SLURM_ARRAY_TASK_ID - 1) % 8) + 1 ))

# Determine disease (108 cells per disease)
if [ $CELL_ID -le 108 ]; then
    DISEASE="ebola"
    LOCAL_ID=$CELL_ID
elif [ $CELL_ID -le 216 ]; then
    DISEASE="covid"
    LOCAL_ID=$((CELL_ID - 108))
else
    DISEASE="cholera"
    LOCAL_ID=$((CELL_ID - 216))
fi

# Within each disease: 36 cells per scenario type (6 GT × 6 INC)
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

# GT and INC from COMBO_ID (1-36): COMBO_ID = (GT-1)*6 + INC
GT=$(( ((COMBO_ID - 1) / 6) + 1 ))
INC=$(( ((COMBO_ID - 1) % 6) + 1 ))

RT_OPTS="latest"

echo "=========================================="
echo "Task: $SLURM_ARRAY_TASK_ID  Cell: $CELL_ID  Timepoint: $TP"
echo "Disease: $DISEASE  Scenario: $SCENARIO  GT: $GT  INC: $INC"
echo "Script: $SCRIPT"
echo "=========================================="

# constRt/incdecRt take rt_level/trend in slot 5 then the timepoint in slot 6;
# casestudy takes the timepoint directly in slot 5.
if [ "$SCENARIO" = "casestudy" ]; then
    Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE "${TP}-${TP}"
else
    Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE both "${TP}-${TP}"
fi

echo "Done: $DISEASE $SCENARIO gt=$GT inc=$INC tp=$TP"
