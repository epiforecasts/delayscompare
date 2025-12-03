#!/bin/bash
#SBATCH --job-name=epinow2
#SBATCH --output=logs/single_%J.out
#SBATCH --error=logs/single_%J.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1

# Run a single scenario analysis
# Usage: sbatch run_single.sh <script> <gt> <rt_opts> <disease>
# Example: sbatch run_single.sh 06d_scenariorun_casestudy.R 4 latest covid

cd $SLURM_SUBMIT_DIR/..

module load R

SCRIPT=$1
GT=$2
RT_OPTS=$3
DISEASE=$4

echo "Running: Rscript scripts/$SCRIPT $GT $RT_OPTS $DISEASE"
Rscript scripts/$SCRIPT $GT $RT_OPTS $DISEASE

echo "Done"
