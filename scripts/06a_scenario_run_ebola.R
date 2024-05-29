library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load data ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data")
ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS"))

# In required format for EpiNow2

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenarios ##
ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))
res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           gen_max=50,
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           inc_max=50,
                           rep_meanlog=ebola_rep_params$meanlog,
                           rep_sdlog=ebola_rep_params$sdlog,
                           rep_max=50,
                           freq_fc=4,
                           weeks_inc=12,
                           obs_scale=0.83)

#save_latest(res_ebola[[1]], here("results"), "res_ebola")
save_latest(res_ebola[[2]], here("results"), "res_ebola_id")
save_latest(res_ebola[[3]], here("results"), "res_ebola_warnings")

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), "res_ebola_samples")

