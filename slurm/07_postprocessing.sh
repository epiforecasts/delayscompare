#!/bin/bash
#SBATCH --job-name=postprocess
#SBATCH --output=slurm/logs/postprocess_%j.out
#SBATCH --error=slurm/logs/postprocess_%j.err
#SBATCH --time=02:00:00
#SBATCH --mem=32gb
#SBATCH --ntasks=1
#SBATCH --nodes=1
set -euo pipefail

# Run results processing and figure generation

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

echo "=== Running results processing ==="
Rscript scripts/07_resultsprocessing.R

echo "=== Generating resim figures ==="
Rscript scripts/fig_resim.R

echo "=== Generating casestudy figures ==="
Rscript scripts/fig_casestudy.R

echo "=== Generating weightprior figures ==="
Rscript scripts/fig_weightprior.R

echo "=== Generating fig1 (simulated scenarios) ==="
Rscript scripts/fig1.R

echo "=== All postprocessing complete ==="
