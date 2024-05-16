source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))

## Load data ##

ebola_sim_data <- readRDS(here("data", paste0("ebola_sim_data", "2024-04-23", ".rds")))
ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.rds"))

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
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           rep_meanlog=ebola_rep_params$meanlog,
                           rep_sdlog=ebola_rep_params$sdlog,
                           freq_fc=8,
                           weeks_inc=12,
                           obs_scale=0.83)

saveRDS(res_ebola[[1]], here("results", paste0("res_ebola", Sys.Date(), ".rds")))
saveRDS(res_ebola[[2]], here("results", paste0("res_ebola_id", Sys.Date(), ".rds")))
saveRDS(res_ebola[[3]], here("results", paste0("res_ebola_warnings", Sys.Date(), ".rds")))

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

saveRDS(ebola_samples, here("results", paste0("res_ebola_samples", Sys.Date(), ".rds")))

