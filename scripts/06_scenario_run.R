library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load data ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data")
ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.rds"))
covid_sim_data <- read_latest(here("data"), "covid_sim_data")

# In required format for EpiNow2

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_cases <- covid_sim_data |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Ebola ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           rep_meanlog=ebola_reporting_delay$mean_mean,
                           rep_sdlog=ebola_reporting_delay$sd_mean,
                           freq_fc=4,
                           weeks_inc=12,
                           obs_scale=0.83)

save_latest(lates_ebola[[1]], here("results"), "res_ebola")
save_latest(res_ebola[[2]], here("results"), "res_ebola_id")

## COVID-19 ##

res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Sherratt et al. 2021 - surveillance paper
                           inc_mean=5.2,
                           inc_sd=1.52, # from Sherratt et al. 2021 - surveillance paper
                           rep_meanlog=convert_to_logmean(4.4, 5.6),
                           rep_sdlog=convert_to_logsd(4.4, 5.6), # MAXIMUM IS JUST A PLACEHOLDER # from Sherratt et al. 2021 - surveillance paper
                           freq_fc=4,
                           weeks_inc=12,
                           obs_scale=0.4)
save_latest(res_covid[[1]], here("results"), "res_covid")
save_latest(res_covid[[2]], here("results"), "res_covid_id")
