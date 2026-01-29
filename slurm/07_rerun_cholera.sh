#!/bin/bash
set -euo pipefail
#SBATCH --job-name=cholera_rerun
#SBATCH --output=slurm/logs/cholera_%A_%a.out
#SBATCH --error=slurm/logs/cholera_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=0-251

# Re-run ALL cholera analyses with updated max cap (4x base mean)
# 7 script types × 36 GT×INC combinations = 252 jobs
# Script types: const_low, const_high, inc, dec, casestudy, resim, weightprior
# Note: weightprior runs both TRUE/FALSE in single execution

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

module load R

# Calculate indices: 36 combinations per script type
SCRIPT_IDX=$((SLURM_ARRAY_TASK_ID / 36))
COMBO_IDX=$((SLURM_ARRAY_TASK_ID % 36))
GT=$((COMBO_IDX / 6 + 1))
INC=$((COMBO_IDX % 6 + 1))

RT_OPTS="latest"
DISEASE="cholera"

case $SCRIPT_IDX in
    0) # const_low
        SCRIPT="06b_scenariorun_constRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE low"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE low
        ;;
    1) # const_high
        SCRIPT="06b_scenariorun_constRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE high"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE high
        ;;
    2) # inc
        SCRIPT="06c_scenariorun_incdecRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc
        ;;
    3) # dec
        SCRIPT="06c_scenariorun_incdecRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    4) # casestudy
        SCRIPT="06d_scenariorun_casestudy.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE
        ;;
    5) # resim
        SCRIPT="06a_scenariorun_resim.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE
        ;;
    6) # weightprior (runs both TRUE and FALSE)
        SCRIPT="06e_scenariorun_weightprior.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE
        ;;
esac

echo "Done: script=$SCRIPT_IDX gt=$GT inc=$INC"
