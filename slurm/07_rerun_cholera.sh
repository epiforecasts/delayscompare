#!/bin/bash
#SBATCH --job-name=cholera_rerun
#SBATCH --output=slurm/logs/cholera_%A_%a.out
#SBATCH --error=slurm/logs/cholera_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=0-359

# Re-run ALL cholera analyses with updated max cap (4x base mean)
# This covers: 4 sim scenarios + casestudy + resim + 2 weightprior = 8 types
# Each type has 36 GT×INC combinations (6×6)
# Total: 8 × 36 = 288 jobs... but we'll do scripts×gt×inc = 10×36 = 360

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

module load R

# Scripts to run (10 total)
SCRIPTS=(
    "06_scenariorun_constRt.R const_low"
    "06_scenariorun_constRt.R const_high"
    "06b_scenariorun_incdecRt.R inc"
    "06b_scenariorun_incdecRt.R dec"
    "06d_scenariorun_casestudy.R casestudy"
    "06a_scenariorun_resim.R resim"
    "06e_scenariorun_weightprior.R weightprior_TRUE"
    "06e_scenariorun_weightprior.R weightprior_FALSE"
)

# Calculate indices
# 36 combinations per script type, 8 script types (but some need special handling)
SCRIPT_IDX=$((SLURM_ARRAY_TASK_ID / 36))
COMBO_IDX=$((SLURM_ARRAY_TASK_ID % 36))
GT=$((COMBO_IDX / 6 + 1))
INC=$((COMBO_IDX % 6 + 1))

RT_OPTS="latest"
DISEASE="cholera"

case $SCRIPT_IDX in
    0) # const_low
        SCRIPT="06_scenariorun_constRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE const_low"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE const_low
        ;;
    1) # const_high
        SCRIPT="06_scenariorun_constRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE const_high"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE const_high
        ;;
    2) # inc
        SCRIPT="06b_scenariorun_incdecRt.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc
        ;;
    3) # dec
        SCRIPT="06b_scenariorun_incdecRt.R"
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
    6) # weightprior TRUE
        SCRIPT="06e_scenariorun_weightprior.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE TRUE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE TRUE
        ;;
    7) # weightprior FALSE
        SCRIPT="06e_scenariorun_weightprior.R"
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE FALSE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE FALSE
        ;;
    8|9) # Extra slots (288 jobs needed, array goes to 359)
        echo "Slot $SCRIPT_IDX not used, exiting"
        exit 0
        ;;
esac

echo "Done: script=$SCRIPT_IDX gt=$GT inc=$INC"
