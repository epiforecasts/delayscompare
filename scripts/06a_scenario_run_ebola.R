source("~/delayscompare/scripts/01_packages.R")
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

res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           rep_meanlog=ebola_reporting_delay$mean_mean,
                           rep_sdlog=ebola_reporting_delay$sd_mean,
                           freq_fc=8,
                           weeks_inc=12,
                           obs_scale=0.83)

save_latest(res_ebola[[1]], here("results"), "res_ebola")
save_latest(res_ebola[[2]], here("results"), "res_ebola_id")
save_latest(res_ebola[[3]], here("results"), "res_ebola_warnings")

## Saving samples only ##

ebola_samples <- data.frame()
for(i in 1:length(res_ebola)){
  samples_scen <- res_ebola[[1]]res_ebola[[i]][res_ebola[[i]]$variable=="reported_cases"] |>
    mutate(model="EpiNow2")
# Add ID
samples_scen$result_list <- i

# Bind to dataframe
ebola_samples <- rbind(ebola_samples, samples_scen)
}

ebola_samples <- ebola_samples |>
  rename(prediction=value)

save_latest(ebola_samples, here("results"), "res_ebola_samples")

