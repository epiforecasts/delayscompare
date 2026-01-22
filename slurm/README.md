# SLURM Job Submission Scripts (LSHTM HPC)

Configured for the LSHTM HPC cluster: https://hpcinfo.lshtm.ac.uk/

## Quick Start

```bash
cd slurm
mkdir -p logs

# Option 1: Run everything
./submit_all.sh

# Option 2: Run only case studies (recommended - most important results)
sbatch 03_casestudies_only.sh

# Option 3: Run a single analysis (gt=4, inc=4, rt_opts=latest, disease=covid)
sbatch run_single.sh 06d_scenariorun_casestudy.R 4 4 latest covid
```

## Scripts Overview

| Script | Description | Array Size | Est. Time |
|--------|-------------|------------|-----------|
| `01_generate_simdata.sh` | Generate simulation data | 6 jobs | ~1 hr total |
| `02_run_scenarios.sh` | All scenario analyses | 72 jobs | ~24 hrs each |
| `03_casestudies_only.sh` | Case studies only | 36 jobs | ~24 hrs each |
| `run_single.sh` | Single analysis | 1 job | ~24 hrs |

## LSHTM HPC Specifics

- **Max concurrent jobs:** 40 (array jobs limited with `%40`)
- **Max wall time:** 168 hours (7 days)
- **R module:** `module load R`
- **Default memory:** 1GB per core (we request 16GB)
- **Cores:** 4 per job (matches `mc.cores` in R scripts)

## Job Structure

### Full Analysis (`02_run_scenarios.sh`)
- **72 array jobs** = 3 diseases × 4 scenario types × 6 gt levels
- Scenario types: constRt, incdecRt, casestudy, weightprior
- Each job runs 6 incubation levels × 2 timepoints = 12 EpiNow2 fits
- Limited to 40 concurrent jobs

### Case Studies Only (`03_casestudies_only.sh`)
- **36 array jobs** = 3 diseases × 6 gt levels × 2 rt_opts
- Runs on real outbreak data (Ebola, COVID, Cholera)
- Limited to 36 concurrent jobs

## Monitoring

```bash
# Check job status
squeue -u $USER

# Check specific job
scontrol show job <JOBID>

# View logs
tail -f logs/casestudy_<JOBID>_<ARRAYID>.out

# Cancel all jobs
scancel -u $USER

# Check completed job info
sacct -j <JOBID>
```

## Output Files

Results saved to `results/` with naming pattern:
```
res_<disease>_<scenario>_<rt_opts>_<type><gt><date>.rds
```

Output types per run:
- `*_id*.rds` - Scenario identifiers
- `*_samples*.rds` - Posterior samples for cases (~3MB each)
- `*_R*.rds` - Posterior samples for Rt (~11MB each)
- `*_summary*.rds` - Full EpiNow2 fit objects (~250KB each)
- `*_warnings*.rds` - Model warnings
- `*_timing*.rds` - Runtime information

## Troubleshooting

**Job killed (time limit):** Increase `--time` up to 168:00:00

**Out of memory:** Increase `--mem` (nodes have 320GB-1024GB)

**R package errors:** Ensure packages installed in user library:
```r
install.packages("EpiNow2", repos = c("https://epiforecasts.r-universe.dev", getOption("repos")))
```

## Email Notifications (optional)

Add to any script:
```bash
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=your.email@lshtm.ac.uk
```
