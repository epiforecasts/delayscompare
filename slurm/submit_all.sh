#!/bin/bash
# Master script to submit all analyses to LSHTM HPC
# Usage: ./submit_all.sh

cd "$(dirname "$0")"
mkdir -p logs

echo "=== Submitting delay misspecification analyses ==="
echo "LSHTM HPC cluster"
echo ""

# Step 1: Generate simulation data (if needed)
echo "Step 1: Submitting simulation data generation..."
SIMDATA_JOB=$(sbatch --parsable 01_generate_simdata.sh)
echo "  Job ID: $SIMDATA_JOB (6 array tasks)"

# Step 2: Run scenario analyses (depends on simdata)
echo "Step 2: Submitting scenario analyses..."
SCENARIO_JOB=$(sbatch --parsable --dependency=afterok:$SIMDATA_JOB 02_run_scenarios.sh)
echo "  Job ID: $SCENARIO_JOB (72 array tasks, max 40 concurrent)"

echo ""
echo "=== Submission complete ==="
echo ""
echo "Monitor with:"
echo "  squeue -u $USER"
echo ""
echo "Check logs in:"
echo "  slurm/logs/"
echo ""
echo "Estimated completion: ~24-48 hours"
