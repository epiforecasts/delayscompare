# delayscompare

A research project comparing the impact of misspecified delay distributions on epidemic forecasting using EpiNow2. This repository contains simulation studies and real-world case studies examining how incorrect assumptions about generation intervals, incubation periods, and reporting delays affect real-time reproduction number (Rt) estimates and case forecasts.

## Overview

This project investigates how misspecification of epidemiological delay distributions affects forecast accuracy across different epidemic scenarios. The analysis uses the EpiNow2 R package to:

- Simulate epidemic data with known delay distributions
- Estimate infections and Rt under various delay misspecification scenarios
- Compare forecast performance across different diseases and epidemic trajectories
- Evaluate the impact of prior weighting on delay estimation

## Project Structure

```
delayscompare/
├── R/                      # Core functions
│   ├── funcs_data.R       # Data processing functions
│   ├── funcs_plots.R      # Plotting functions
│   ├── funcs_rtraj.R      # R trajectory simulation functions
│   ├── generate_scores_func.R  # Scoring metrics
│   └── scenario_loop.R    # Main scenario simulation wrapper
├── scripts/               # Analysis scripts
│   ├── 01_packages.R      # Package dependencies
│   ├── *_simulatedata.R   # Data simulation scripts
│   ├── *_scenariorun.R    # Scenario execution scripts
│   └── 07_resultsprocessing_*.R  # Results processing
├── data/                  # Input data files
├── results/               # Analysis outputs
├── scenario_loop.R        # Main scenario functions
├── plots_baseline.R       # Baseline plotting script
└── plot_runtimes.R        # Runtime analysis
```

## Case Studies

The analysis includes three epidemic case studies:

1. **COVID-19** (England, 2021 Delta wave)

2. **Ebola** (Sierra Leone, 2014)

3. **Cholera** (Yemen, 2016)

## Scenarios

Each case study examines delay misspecification scenarios:

- **No delay**: All delays set to zero (Fixed(0))
- **Very low**: 0.25× correct delay mean
- **Low**: 0.8× correct delay mean
- **Correct**: 1× correct delay mean (ground truth)
- **High**: 1.25× correct delay mean
- **Very high**: 2× correct delay mean

Additional scenarios tested:
- Different Rt projection methods (`rt_opts`: "latest" vs "project")
- Under-reporting (observation scale: 0.3 vs 1.0)
- Prior weight specifications for delay distributions

## Requirements

### R Packages

- EpiNow2
- incidence2
- readxl
- dplyr, tidyr, purrr
- here
- ggplot2
- scoringutils
- viridis, RColorBrewer, cowplot

Install packages with:
```r
pkgs <- c("EpiNow2", "incidence2", "readxl", "dplyr", "tidyr",
          "purrr", "here", "ggplot2", "scoringutils", "viridis",
          "RColorBrewer", "cowplot")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = c("https://epiforecasts.r-universe.dev",
                          getOption("repos")))
```

## Usage

### Running Scenario Analysis

Basic scenario execution:
```r
source("scripts/01_packages.R")
source("R/scenario_loop.R")

# Run COVID-19 scenario (example)
res <- sim_scenarios(
  case_data = covid_data,
  var = 1,  # Generation time scenario
  gen_mean = 3.6,
  gen_sd = 3.1,
  gen_max = 30,
  inc_mean = 5.2,
  inc_sd = 1.52,
  inc_max = 30,
  rep_mean = 4.4,
  rep_sd = 5.6,
  rep_max = 30,
  freq_fc = 4,        # Forecast frequency (every 4 weeks)
  weeks_inc = 12,     # Use 12 weeks of data
  rt_opts_choice = "latest",
  obs_scale = 1
)
```

### Running with Prior Weights

For uncertainty in delay distributions:
```r
res <- sim_weightprior(
  case_data = data,
  var = 1,
  gen_mean_mean = 3.6,
  gen_mean_sd = 0.5,
  gen_sd_mean = 3.1,
  gen_sd_sd = 0.3,
  gen_max = 30,
  # ... other parameters
  weight_prior = TRUE 
)
```

### Command Line Execution

Scripts accept command line arguments for parallel execution:
```bash
Rscript scripts/casestudy_covid_scenariorun.R 1
```

## Output

Results are saved as RDS files in the `results/` directory:

- `*_samples*.rds`: Posterior samples for case predictions
- `*_R*.rds`: Posterior samples for Rt estimates
- `*_id*.rds`: Scenario identifiers
- `*_summary*.rds`: Summary statistics
- `*_warnings*.rds`: Model warnings

Plots are generated showing:
- Impact of delay misspecification on CRPS for Rt and case forecasts
- Timeseries of best- and worst-performing forecasts
- Runtime comparisons

## Configuration

The analysis uses 8 timepoints per case study to balance computational efficiency with temporal coverage. Timepoints are spaced every 4 weeks throughout the epidemic period.

MCMC settings:
- Samples: 3000
- Adapt delta: 0.99
- Max treedepth: 20
- Forecast horizon: 14 days

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
