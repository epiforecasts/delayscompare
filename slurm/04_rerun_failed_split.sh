#!/bin/bash
set -euo pipefail
#SBATCH --job-name=rerun_split
#SBATCH --output=slurm/logs/rerun_%A_%a.out
#SBATCH --error=slurm/logs/rerun_%A_%a.err
#SBATCH --time=48:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-24%24

# Rerun failed covid jobs, split by sub-scenario
# constRt jobs (155,156,159,160,162,166) × 2 (low/high) = 12 jobs
# incdecRt jobs (190,191,197,204) × 2 (inc/dec) = 8 jobs
# casestudy jobs (228,240) × 2 (tp1-4/tp5-8) = 4 jobs
# Total: 24 jobs

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# Define the failed jobs and their splits
case $TASK_ID in
    # constRt jobs - low
    1)  GT=2; INC=5; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    2)  GT=2; INC=6; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    3)  GT=3; INC=3; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    4)  GT=3; INC=4; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    5)  GT=3; INC=6; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    6)  GT=4; INC=4; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="low" ;;
    # constRt jobs - high
    7)  GT=2; INC=5; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    8)  GT=2; INC=6; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    9)  GT=3; INC=3; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    10) GT=3; INC=4; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    11) GT=3; INC=6; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    12) GT=4; INC=4; SCRIPT="scripts/06b_scenariorun_constRt.R"; SPLIT="high" ;;
    # incdecRt jobs - inc
    13) GT=2; INC=4; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="inc" ;;
    14) GT=2; INC=5; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="inc" ;;
    15) GT=3; INC=5; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="inc" ;;
    16) GT=4; INC=6; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="inc" ;;
    # incdecRt jobs - dec
    17) GT=2; INC=4; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="dec" ;;
    18) GT=2; INC=5; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="dec" ;;
    19) GT=3; INC=5; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="dec" ;;
    20) GT=4; INC=6; SCRIPT="scripts/06c_scenariorun_incdecRt.R"; SPLIT="dec" ;;
    # casestudy jobs - timepoints 1-4
    21) GT=2; INC=6; SCRIPT="scripts/06d_scenariorun_casestudy.R"; SPLIT="1-4" ;;
    22) GT=4; INC=6; SCRIPT="scripts/06d_scenariorun_casestudy.R"; SPLIT="1-4" ;;
    # casestudy jobs - timepoints 5-8
    23) GT=2; INC=6; SCRIPT="scripts/06d_scenariorun_casestudy.R"; SPLIT="5-8" ;;
    24) GT=4; INC=6; SCRIPT="scripts/06d_scenariorun_casestudy.R"; SPLIT="5-8" ;;
esac

DISEASE="covid"
RT_OPTS="latest"

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Script: $SCRIPT"
echo "GT level: $GT"
echo "INC level: $INC"
echo "Split: $SPLIT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE $SPLIT

echo "Done: $DISEASE gt=$GT inc=$INC split=$SPLIT"
