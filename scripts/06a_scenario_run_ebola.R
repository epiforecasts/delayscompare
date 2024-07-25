library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

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
                           gt,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           gen_max=50,
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           inc_max=60,
                           rep_meanlog=ebola_rep_params$meanlog,
                           rep_sdlog=ebola_rep_params$sdlog,
                           rep_max=50,
                           freq_fc=4,
                           weeks_inc=12,
                           obs_scale=0.83)

#save_latest(res_ebola[[1]], here("results"), "res_ebola")
save_latest(res_ebola[[2]], here("results"), paste0("res_ebola_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebola_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_ebola_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebola_R", gt))

## Saving Rt estimates only 
res_R_estimates <- res_ebola[[4]] |>
  filter(type=="estimate")
save_latest(res_R_estimates, here("results"), paste0("res_ebola_R_est", gt))

ebola_rt_samples <- list()
for(i in c(1:6)){
  ebola_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_ebola_R", gt)) |>
    filter(type=="estimate")
  ebola_rt_samples[[i]]$gt <- i
  save_latest(ebola_rt_samples[[i]], here("results"), paste0("res_ebola_R_est", i))
}



