#!/bin/bash
#SBATCH --job-name=diag_recovery
#SBATCH --output=slurm/logs/diag_recovery_%A_%a.out
#SBATCH --error=slurm/logs/diag_recovery_%A_%a.err
#SBATCH --time=02:00:00
#SBATCH --mem=8gb
#SBATCH --cpus-per-task=2
set -euo pipefail

# Recovery / learning diagnostic grid (288 cells). One cell per array task.
# Submit with:
#   sbatch --array=1-288%40 slurm/diag_recovery.sbatch
# Then aggregate:
#   Rscript scripts/diag_grid_aggregate.R

cd ~/delayscompare
Rscript scripts/diag_gt_recovery_grid.R
