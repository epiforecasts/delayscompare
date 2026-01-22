#!/bin/bash
set -euo pipefail
#SBATCH --job-name=cholera_retry
#SBATCH --output=slurm/logs/cholera_retry_%A_%a.out
#SBATCH --error=slurm/logs/cholera_retry_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=0-9

# Rerun failed/timed-out cholera jobs with 48h limit
# These jobs timed out at 24h in the previous run

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

module load R

RT_OPTS="latest"
DISEASE="cholera"

# Map array index to failed job parameters
case $SLURM_ARRAY_TASK_ID in
    0) # const_low gt=2 inc=5
        SCRIPT="06b_scenariorun_constRt.R"
        GT=2; INC=5
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE low"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE low
        ;;
    1) # const_high gt=1 inc=2
        SCRIPT="06b_scenariorun_constRt.R"
        GT=1; INC=2
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE high"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE high
        ;;
    2) # inc gt=2 inc=6
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=2; INC=6
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE inc
        ;;
    3) # dec gt=1 inc=4
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=1; INC=4
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    4) # dec gt=2 inc=1
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=2; INC=1
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    5) # dec gt=2 inc=3
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=2; INC=3
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    6) # dec gt=4 inc=1
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=4; INC=1
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    7) # dec gt=6 inc=6
        SCRIPT="06c_scenariorun_incdecRt.R"
        GT=6; INC=6
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE dec
        ;;
    8) # casestudy gt=2 inc=4
        SCRIPT="06d_scenariorun_casestudy.R"
        GT=2; INC=4
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE
        ;;
    9) # resim gt=1 inc=2
        SCRIPT="06a_scenariorun_resim.R"
        GT=1; INC=2
        echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
        Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE
        ;;
esac

echo "Done: task=$SLURM_ARRAY_TASK_ID"
