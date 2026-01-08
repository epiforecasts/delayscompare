#!/bin/bash
#SBATCH --job-name=rerun_tp
#SBATCH --output=slurm/logs/rerun_tp_%A_%a.out
#SBATCH --error=slurm/logs/rerun_tp_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-56%40

# Rerun remaining timeout jobs split by individual timepoints
# Each job now runs just 1-2 timepoints instead of all 8
#
# Timeout jobs that need rerunning:
# constRt high: GT=2 INC=5, GT=2 INC=6, GT=3 INC=3, GT=4 INC=4 (4 combos × 8 tp = 32)
# incdecRt dec: GT=2 INC=4, GT=2 INC=5 (2 combos × 8 tp = 16)
# casestudy: GT=2 INC=6 tp5-8 (1 combo × 4 tp = 4) - split into individual
# Plus casestudy GT=4 INC=6 tp5-8 (1 combo × 4 tp = 4)
# Total: 32 + 16 + 4 + 4 = 56 jobs

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

DISEASE="covid"
RT_OPTS="latest"

# Jobs 1-32: constRt high (4 GT/INC combos × 8 timepoints)
# Jobs 33-48: incdecRt dec (2 GT/INC combos × 8 timepoints)
# Jobs 49-52: casestudy GT=2 INC=6 (4 timepoints: 5,6,7,8)
# Jobs 53-56: casestudy GT=4 INC=6 (4 timepoints: 5,6,7,8)

if [ $TASK_ID -le 32 ]; then
    SCRIPT="scripts/06b_scenariorun_constRt.R"
    SPLIT="high"
    COMBO=$(( (TASK_ID - 1) / 8 ))
    TP=$(( ((TASK_ID - 1) % 8) + 1 ))
    case $COMBO in
        0) GT=2; INC=5 ;;
        1) GT=2; INC=6 ;;
        2) GT=3; INC=3 ;;
        3) GT=4; INC=4 ;;
    esac
    TP_RANGE="${TP}-${TP}"
elif [ $TASK_ID -le 48 ]; then
    SCRIPT="scripts/06c_scenariorun_incdecRt.R"
    SPLIT="dec"
    LOCAL_ID=$((TASK_ID - 32))
    COMBO=$(( (LOCAL_ID - 1) / 8 ))
    TP=$(( ((LOCAL_ID - 1) % 8) + 1 ))
    case $COMBO in
        0) GT=2; INC=4 ;;
        1) GT=2; INC=5 ;;
    esac
    TP_RANGE="${TP}-${TP}"
elif [ $TASK_ID -le 52 ]; then
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
    GT=2; INC=6
    LOCAL_ID=$((TASK_ID - 48))
    TP=$((LOCAL_ID + 4))  # timepoints 5,6,7,8
    TP_RANGE="${TP}-${TP}"
    SPLIT=$TP_RANGE
else
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
    GT=4; INC=6
    LOCAL_ID=$((TASK_ID - 52))
    TP=$((LOCAL_ID + 4))  # timepoints 5,6,7,8
    TP_RANGE="${TP}-${TP}"
    SPLIT=$TP_RANGE
fi

echo "=========================================="
echo "Job: $TASK_ID"
echo "Disease: $DISEASE"
echo "Script: $SCRIPT"
echo "GT level: $GT"
echo "INC level: $INC"
echo "Split/TP: $SPLIT / $TP_RANGE"
echo "=========================================="

if [[ $SCRIPT == *"casestudy"* ]]; then
    Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE $TP_RANGE
else
    Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE $SPLIT $TP_RANGE
fi

echo "Done: $DISEASE gt=$GT inc=$INC tp=$TP_RANGE"
