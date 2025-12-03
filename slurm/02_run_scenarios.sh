#!/bin/bash
#SBATCH --job-name=scenarios
#SBATCH --output=logs/scenarios_%A_%a.out
#SBATCH --error=logs/scenarios_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=1-72%40

# Run scenario analyses
# Array structure: 72 jobs = 3 diseases × 4 scenario_types × 6 gt_levels
# Limited to 40 concurrent jobs (LSHTM max)
#
# Jobs 1-24:   ebola   (const=1-6, incdec=7-12, casestudy=13-18, weightprior=19-24)
# Jobs 25-48:  covid   (const=25-30, incdec=31-36, casestudy=37-42, weightprior=43-48)
# Jobs 49-72:  cholera (const=49-54, incdec=55-60, casestudy=61-66, weightprior=67-72)

cd $SLURM_SUBMIT_DIR/..

module load R

# Parse array task ID
TASK_ID=$SLURM_ARRAY_TASK_ID

# Determine disease (1-24=ebola, 25-48=covid, 49-72=cholera)
if [ $TASK_ID -le 24 ]; then
    DISEASE="ebola"
    LOCAL_ID=$TASK_ID
elif [ $TASK_ID -le 48 ]; then
    DISEASE="covid"
    LOCAL_ID=$((TASK_ID - 24))
else
    DISEASE="cholera"
    LOCAL_ID=$((TASK_ID - 48))
fi

# Determine scenario type and gt (within each disease: 4 types × 6 gt = 24 jobs)
# 1-6=constRt, 7-12=incdecRt, 13-18=casestudy, 19-24=weightprior
if [ $LOCAL_ID -le 6 ]; then
    SCENARIO="constRt"
    GT=$LOCAL_ID
    SCRIPT="scripts/06b_scenariorun_constRt.R"
elif [ $LOCAL_ID -le 12 ]; then
    SCENARIO="incdecRt"
    GT=$((LOCAL_ID - 6))
    SCRIPT="scripts/06c_scenariorun_incdecRt.R"
elif [ $LOCAL_ID -le 18 ]; then
    SCENARIO="casestudy"
    GT=$((LOCAL_ID - 12))
    SCRIPT="scripts/06d_scenariorun_casestudy.R"
else
    SCENARIO="weightprior"
    GT=$((LOCAL_ID - 18))
    SCRIPT="scripts/06e_scenariorun_weightprior.R"
fi

RT_OPTS="latest"  # Can also use "project"

echo "=========================================="
echo "Job: $SLURM_ARRAY_TASK_ID"
echo "Disease: $DISEASE"
echo "Scenario: $SCENARIO"
echo "GT level: $GT"
echo "RT opts: $RT_OPTS"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $RT_OPTS $DISEASE

echo "Done: $DISEASE $SCENARIO gt=$GT"
