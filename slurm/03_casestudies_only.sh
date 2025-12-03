#!/bin/bash
#SBATCH --job-name=casestudy
#SBATCH --output=slurm/logs/casestudy_%A_%a.out
#SBATCH --error=slurm/logs/casestudy_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-36%36

# Run ONLY case study analyses (real data - most important)
# Array: 36 jobs = 3 diseases × 6 gt_levels × 2 rt_opts
# Limited to 36 concurrent (within LSHTM's 40 job limit)
#
# Jobs 1-12:  ebola   (gt 1-6 latest, gt 1-6 project)
# Jobs 13-24: covid   (gt 1-6 latest, gt 1-6 project)
# Jobs 25-36: cholera (gt 1-6 latest, gt 1-6 project)

# Get project root from SLURM_SUBMIT_DIR
# If submitted from slurm/, go up one level; if from root, stay there
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.."
else
    cd "$SLURM_SUBMIT_DIR"
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID

# Determine disease
if [ $TASK_ID -le 12 ]; then
    DISEASE="ebola"
    LOCAL_ID=$TASK_ID
elif [ $TASK_ID -le 24 ]; then
    DISEASE="covid"
    LOCAL_ID=$((TASK_ID - 12))
else
    DISEASE="cholera"
    LOCAL_ID=$((TASK_ID - 24))
fi

# Determine gt and rt_opts (1-6 = latest, 7-12 = project)
if [ $LOCAL_ID -le 6 ]; then
    GT=$LOCAL_ID
    RT_OPTS="latest"
else
    GT=$((LOCAL_ID - 6))
    RT_OPTS="project"
fi

echo "=========================================="
echo "Case Study Analysis"
echo "Disease: $DISEASE"
echo "GT level: $GT"
echo "RT opts: $RT_OPTS"
echo "=========================================="

# INC defaults to same as GT (both misspecified by same amount)
INC=$GT
Rscript scripts/06d_scenariorun_casestudy.R $GT $INC $RT_OPTS $DISEASE

echo "Done: $DISEASE casestudy gt=$GT rt_opts=$RT_OPTS"
