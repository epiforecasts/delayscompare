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

saveRDS(res_ebola[[1]], here("results", paste0("res_ebola", Sys.Date(), ".rds")))
saveRDS(res_ebola[[2]], here("results", paste0("res_ebola_id", Sys.Date(), ".rds")))
saveRDS(res_ebola[[3]], here("results", paste0("res_ebola_warnings", Sys.Date(), ".rds")))

## Saving samples only ##

ebola_samples <- lapply(1:length(res_ebola[[1]]), function(i) {
  res_ebola[[1]][[i]][variable=="reported_cases"]
}) |>
  bind_rows(.id = "result_list") |>
  mutate(model = "EpiNow2", result_list = as.integer(result_list)) |>
  rename(prediction=value)

saveRDS(ebola_samples, here("results", paste0("res_ebola_samples", Sys.Date(), ".rds")))

