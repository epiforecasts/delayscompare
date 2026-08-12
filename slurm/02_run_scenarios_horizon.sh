#!/bin/bash
#SBATCH --job-name=horizon
#SBATCH --output=slurm/logs/horizon_%A_%a.out
#SBATCH --error=slurm/logs/horizon_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
set -euo pipefail

# Horizon-sweep power/sensitivity experiment. One estimate_infections fit per
# (GT, replicate, timepoint) on a replicate disease-like dataset, INC held correct
# (=4), forecast HORIZON extended (default 42d) so CRPS can be scored at 1-6 weeks.
#
# Reduced grid: 6 GT x NREP replicates x 8 timepoints per disease. INC fixed at 4.
# Timepoints over-provisioned to 8; an out-of-range tp is a clean no-op.
#
# Array size = 6 * NREP * 8. For NREP=20 that is 960 tasks per disease.
# Usage: NREP=20 sbatch --array=1-960%40 --export=ALL,NREP=20,WARMUP=500,HORIZON=42 \
#          slurm/02_run_scenarios_horizon.sh <disease>
# Sampler overridable via the same --export list (ADAPT_DELTA, SAMPLES, ...).

DISEASE=${1:-covid}
NREP=${NREP:-20}
INC=4

# Get project root
if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi
echo "Working directory: $(pwd)"

module load R

TASK_ID=$SLURM_ARRAY_TASK_ID
NTASKS=$(( 6 * NREP * 8 ))

if (( TASK_ID < 1 || TASK_ID > NTASKS )); then
  echo "Error: SLURM_ARRAY_TASK_ID must be in [1,${NTASKS}], got ${TASK_ID}" >&2
  exit 2
fi

# tp innermost (contiguous), then replicate, then GT outermost.
TP=$(( ((TASK_ID - 1) % 8) + 1 ))
REP=$(( ((TASK_ID - 1) / 8) % NREP + 1 ))
GT=$(( ((TASK_ID - 1) / (8 * NREP)) + 1 ))

RT_OPTS="latest"
SCRIPT="scripts/06h_scenariorun_horizon.R"

echo "=========================================="
echo "Task: $TASK_ID  GT: $GT  REP: $REP  Timepoint: $TP"
echo "Disease: $DISEASE  INC: $INC (fixed)  Horizon: ${HORIZON:-42}"
echo "Script: $SCRIPT"
echo "=========================================="

Rscript $SCRIPT $GT $INC $RT_OPTS $DISEASE "${TP}-${TP}" $REP

echo "Done: $DISEASE horizon gt=$GT inc=$INC tp=$TP rep=$REP"
