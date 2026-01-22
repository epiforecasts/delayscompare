#!/bin/bash
set -euo pipefail
#SBATCH --job-name=epinow2
#SBATCH --output=slurm/logs/single_%J.out
#SBATCH --error=slurm/logs/single_%J.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1

# Run a single scenario analysis
# Usage: sbatch run_single.sh <script> <gt> <inc> <rt_opts> <disease>
# Example: sbatch run_single.sh 06d_scenariorun_casestudy.R 4 4 latest covid

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

module load R

SCRIPT=$1
GT=$2
INC=$3
RT_OPTS=$4
DISEASE=$5

echo "Running: Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE"
Rscript scripts/$SCRIPT $GT $INC $RT_OPTS $DISEASE

echo "Done"
