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
                           rt_opts_choice="latest",
                           obs_scale=ebola_underreport)

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

#### COVID ####

## Loading data ##

covid_sim_data <- read_latest(here("data"), "covid_sim_data")

## Delays
covid_gen_mean <- 3.6
covid_gen_sd <- 3.1
covid_gen_max <- 30
covid_inc_mean <- 5.2
covid_inc_sd <- 1.52
covid_inc_max <- 30
covid_rep_mean <- 4.4
covid_rep_sd <- 5.6
covid_rep_max <-30
covid_underreport <- 0.4

# In required format for EpiNow2

covid_sim_data_cases <- covid_sim_data |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 1 - rt_opts=latest, under-reporting=yes ##

res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gt,
                           gen_mean=covid_gen_mean,
                           gen_sd=covid_gen_sd,
                           gen_max=covid_gen_max,
                           inc_mean=covid_inc_mean,
                           inc_sd=covid_inc_sd,
                           inc_max=covid_inc_max,
                           rep_mean=covid_rep_mean,
                           rep_sd=covid_rep_sd,
                           rep_max=covid_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=covid_underreport)

save_latest(res_covid[[1]], here("results"), paste0("res_covid_sim_latest_samples", gt))
save_latest(res_covid[[2]], here("results"), paste0("res_covid_sim_latest_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covid_sim_latest_R", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covid_sim_latest_summary", gt))
save_latest(res_covid[[5]], here("results"), paste0("res_covid_sim_latest_warnings", gt))

## Run scenario 2 - rt_opts=project, under-reporting=yes ##

res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gt,
                           gen_mean=covid_gen_mean,
                           gen_sd=covid_gen_sd,
                           gen_max=covid_gen_max,
                           inc_mean=covid_inc_mean,
                           inc_sd=covid_inc_sd,
                           inc_max=covid_inc_max,
                           rep_mean=covid_rep_mean,
                           rep_sd=covid_rep_sd,
                           rep_max=covid_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="project",
                           obs_scale=covid_underreport)

save_latest(res_covid[[1]], here("results"), paste0("res_covid_sim_project_samples", gt))
save_latest(res_covid[[2]], here("results"), paste0("res_covid_sim_project_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covid_sim_project_R", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covid_sim_project_summary", gt))
save_latest(res_covid[[5]], here("results"), paste0("res_covid_sim_project_warnings", gt))

#### CHOLERA ####

## Loading data ##

cholera_sim_data <- read_latest(here("data"), "cholera_sim_data")

## Delays
cholera_gen_mean <- 8.51
cholera_gen_sd <- 0.54
cholera_gen_max <- 50
cholera_inc_mean <- 1.77
cholera_inc_sd <- 1.08
cholera_inc_max <- 30
cholera_rep_mean <- 4.4
cholera_rep_sd <- 0.67
cholera_rep_max <-30
cholera_underreport <- 0.28

# In required format for EpiNow2

cholera_sim_data_cases <- cholera_sim_data |> filter(variable=="reported_cases")
cholera_sim_data_cases <- cholera_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 1 - rt_opts=latest, under-reporting=yes ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_cases,
                           gt,
                           gen_mean=cholera_gen_mean,
                           gen_sd=cholera_gen_sd,
                           gen_max=cholera_gen_max,
                           inc_mean=cholera_inc_mean,
                           inc_sd=cholera_inc_sd,
                           inc_max=cholera_inc_max,
                           rep_mean=cholera_rep_mean,
                           rep_sd=cholera_rep_sd,
                           rep_max=cholera_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=cholera_underreport)

save_latest(res_cholera[[1]], here("results"), paste0("res_cholera_sim_latest_samples", gt))
save_latest(res_cholera[[2]], here("results"), paste0("res_cholera_sim_latest_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholera_sim_latest_R", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholera_sim_latest_summary", gt))
save_latest(res_cholera[[5]], here("results"), paste0("res_cholera_sim_latest_warnings", gt))

## Run scenario 2 - rt_opts=project, under-reporting=yes ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_cases,
                           gt,
                           gen_mean=cholera_gen_mean,
                           gen_sd=cholera_gen_sd,
                           gen_max=cholera_gen_max,
                           inc_mean=cholera_inc_mean,
                           inc_sd=cholera_inc_sd,
                           inc_max=cholera_inc_max,
                           rep_mean=cholera_rep_mean,
                           rep_sd=cholera_rep_sd,
                           rep_max=cholera_rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="project",
                           obs_scale=cholera_underreport)

save_latest(res_cholera[[1]], here("results"), paste0("res_cholera_sim_project_samples", gt))
save_latest(res_cholera[[2]], here("results"), paste0("res_cholera_sim_project_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholera_sim_project_R", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholera_sim_project_summary", gt))
save_latest(res_cholera[[5]], here("results"), paste0("res_cholera_sim_project_warnings", gt))




