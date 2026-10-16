#!/bin/bash
#SBATCH --job-name=tp_proper
#SBATCH --output=slurm/logs/tp_proper_%A_%a.out
#SBATCH --error=slurm/logs/tp_proper_%A_%a.err
#SBATCH --time=02:00:00
#SBATCH --mem=8gb
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --array=0-86
set -euo pipefail

# One task per replicate: disease x step trajectory x 3 replicates (tasks 0-26),
# then disease x 20 disease-like replicates (tasks 27-86)

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

DISEASES=(ebola covid cholera)
SCENS=(flat bend turn)
ID=$SLURM_ARRAY_TASK_ID
if (( ID < 27 )); then
    DISEASE=${DISEASES[$((ID / 9))]}
    SCEN=${SCENS[$(((ID / 3) % 3))]}
    REP=$((ID % 3 + 1))
else
    J=$((ID - 27))
    DISEASE=${DISEASES[$((J / 20))]}
    SCEN=resim
    REP=$((J % 20 + 1))
fi

module load R
Rscript scripts/score_turningpoint_proper.R "$DISEASE" "$SCEN" "$REP"
