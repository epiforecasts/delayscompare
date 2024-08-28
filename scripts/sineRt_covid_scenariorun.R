library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

############### SCENARIOS #################

## Loading data ##

covid_sim_data_sine <- read_latest(here("data"), "covid_sim_data_sine")
covid_sim_data_sine_ur <- read_latest(here("data"), "covid_sim_data_sine_ur")

# In required format for EpiNow2

covid_sim_data_sine_cases <- covid_sim_data_sine |> filter(variable=="reported_cases")
covid_sim_data_sine_cases <- covid_sim_data_sine_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_sine_cases_ur <- covid_sim_data_sine_ur |> filter(variable=="reported_cases")
covid_sim_data_sine_cases_ur <- covid_sim_data_sine_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 17 - rt_opts=latest, under-reporting=no ##

res_covid <- sim_scenarios(case_data=covid_sim_data_sine_cases,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, 
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen17_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen17_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen17_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen17_R", gt)) 

## Run scenario 18 - rt_opts=latest, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_sine_cases_ur,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, 
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.3)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen18_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen18_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen18_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen18_R", gt)) 

## Run scenario 19 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_sine_cases,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen19_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen19_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen19_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen19_R", gt)) 

## Run scenario 20 - rt_opts=project, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_sine_cases_ur,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1,
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen20_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen20_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen20_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen20_R", gt)) 
  
