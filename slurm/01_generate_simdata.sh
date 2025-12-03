#!/bin/bash
#SBATCH --job-name=simdata
#SBATCH --output=logs/simdata_%A_%a.out
#SBATCH --error=logs/simdata_%A_%a.err
#SBATCH --time=01:00:00
#SBATCH --mem=8gb
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --array=1-6

# Generate simulation data for all diseases
# Array index: 1-3 = constRt, 4-6 = incdecRt
# 1=ebola_const, 2=covid_const, 3=cholera_const
# 4=ebola_incdec, 5=covid_incdec, 6=cholera_incdec

cd $SLURM_SUBMIT_DIR/..

module load R

DISEASES=("ebola" "covid" "cholera")

if [ $SLURM_ARRAY_TASK_ID -le 3 ]; then
    # Constant Rt scenarios
    IDX=$((SLURM_ARRAY_TASK_ID - 1))
    DISEASE=${DISEASES[$IDX]}
    echo "Generating constant Rt data for $DISEASE"
    Rscript scripts/05b_simulatedata_constRt.R $DISEASE
else
    # Increasing/decreasing Rt scenarios
    IDX=$((SLURM_ARRAY_TASK_ID - 4))
    DISEASE=${DISEASES[$IDX]}
    echo "Generating inc/dec Rt data for $DISEASE"
    Rscript scripts/05c_simulatedata_incdecRt.R $DISEASE
fi

echo "Done: $DISEASE"
