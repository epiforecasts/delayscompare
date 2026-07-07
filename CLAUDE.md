# Project Instructions

## SLURM Job Submission

The `#SBATCH` directives in `slurm/` scripts are now correctly placed before `set -euo pipefail`, so SLURM parses them properly. You can submit jobs without repeating resource requirements on the command line:

```bash
# This works - SLURM reads directives from the script
sbatch slurm/02_run_scenarios.sh

# You can still override on the command line if needed
sbatch --mem=64gb --time=48:00:00 --array=1-324%40 slurm/02_run_scenarios.sh
```
