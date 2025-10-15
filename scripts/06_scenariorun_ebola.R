library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "05_simulate_data.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

#### EBOLA ####

## Loading data ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data")

## Delays
ebola_gen_mean <- 16.2
ebola_gen_sd <- 9.4
ebola_gen_max <- 45
ebola_inc_mean <- 11.4
ebola_inc_sd <- 8.1
ebola_inc_max <- 45
ebola_rep_mean <- 0
ebola_rep_sd <- 0
ebola_rep_max <-0
ebola_underreport <- 0.83

# In required format for EpiNow2

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 1 - rt_opts=latest, under-reporting=yes ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
                           var=gt,
                           gen_mean=ebola_gen_mean,
                           gen_sd=ebola_gen_sd,
                           gen_max=ebola_gen_max,
                           inc_mean=ebola_inc_mean,
                           inc_sd=ebola_inc_sd,
                           inc_max=ebola_inc_max,
                           rep_mean=ebola_rep_mean,
                           rep_sd=ebola_rep_sd,
                           rep_max=ebola_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=ebola_underreport,
                           report_freq="day")

save_latest(res_ebola[[1]], here("results"), paste0("res_ebola_sim_latest_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebola_sim_latest_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebola_sim_latest_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebola_sim_latest_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebola_sim_latest_warnings", gt))

## Run scenario 2 - rt_opts=project, under-reporting=yes ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
                           gt,
                           gen_mean=ebola_gen_mean,
                           gen_sd=ebola_gen_sd,
                           gen_max=ebola_gen_max,
                           inc_mean=ebola_inc_mean,
                           inc_sd=ebola_inc_sd,
                           inc_max=ebola_inc_max,
                           rep_mean=ebola_rep_mean,
                           rep_sd=ebola_rep_sd,
                           rep_max=ebola_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="project",
                           obs_scale=ebola_underreport)

save_latest(res_ebola[[1]], here("results"), paste0("res_ebola_sim_project_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebola_sim_project_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebola_sim_project_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebola_sim_project_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebola_sim_project_warnings", gt))
