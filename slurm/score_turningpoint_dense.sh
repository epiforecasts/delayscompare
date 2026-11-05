#!/bin/bash
#SBATCH --job-name=tpd_score
#SBATCH --output=slurm/logs/tpd_score_%A_%a.out
#SBATCH --error=slurm/logs/tpd_score_%A_%a.err
#SBATCH --time=04:00:00
#SBATCH --mem=8gb
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --array=0-65
set -euo pipefail

# Scores the dense-origin fits (run label hzd), one task per replicate: disease x step
# trajectory x 3 replicates (tasks 0-35), then disease x disease-like replicates 1-10
# (tasks 36-65)

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then cd "$SLURM_SUBMIT_DIR/.." || exit 1; else cd "$SLURM_SUBMIT_DIR" || exit 1; fi

DISEASES=(ebola covid cholera)
SCENS=(bend turn bendup turnup)
ID=$SLURM_ARRAY_TASK_ID
if (( ID < 36 )); then
    DISEASE=${DISEASES[$((ID / 12))]}
    SCEN=${SCENS[$(((ID / 3) % 4))]}
    REP=$((ID % 3 + 1))
else
    J=$((ID - 36))
    DISEASE=${DISEASES[$((J / 10))]}
    SCEN=resim
    REP=$((J % 10 + 1))
fi

module load R
Rscript scripts/score_turningpoint_proper.R "$DISEASE" "$SCEN" "$REP" hzd
