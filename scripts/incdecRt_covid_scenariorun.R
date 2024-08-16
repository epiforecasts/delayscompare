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

#### Increasing Rt ####

## Loading data ##

covid_sim_data_inc <- read_latest(here("data"), "covid_sim_data_inc")
covid_sim_data_inc_ur <- read_latest(here("data"), "covid_sim_data_inc_ur")

# In required format for EpiNow2

covid_sim_data_inc_cases <- covid_sim_data_inc |> filter(variable=="reported_cases")
covid_sim_data_inc_cases <- covid_sim_data_inc_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_inc_cases_ur <- covid_sim_data_inc_ur |> filter(variable=="reported_cases")
covid_sim_data_inc_cases_ur <- covid_sim_data_inc_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 9 - rt_opts=latest, under-reporting=no ##

res_covid <- sim_scenarios(case_data=covid_sim_data_inc_cases,
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

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen9_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen9_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen9_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen9_R", gt)) 

## Run scenario 10 - rt_opts=latest, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_inc_cases_ur,
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

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen10_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen10_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen10_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen10_R", gt)) 

## Run scenario 11 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
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
  
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen11_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen11_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen11_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen11_R", gt)) 

## Run scenario 12 - rt_opts=project, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_inc_cases_ur,
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
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen12_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen12_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen12_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen12_R", gt)) 
  
  #### Decreasing Rt ####
  
  ## Loading data ##

  covid_sim_data_dec <- read_latest(here("data"), "covid_sim_data_dec")
  covid_sim_data_dec_ur <- read_latest(here("data"), "covid_sim_data_dec_ur")
  
# In required format for EpiNow2

covid_sim_data_dec_cases <-  covid_sim_data_dec |> filter(variable=="reported_cases")
covid_sim_data_dec_cases <- covid_sim_data_dec_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_dec_cases_ur <- covid_sim_data_dec_ur |> filter(variable=="reported_cases")
covid_sim_data_dec_cases_ur <- covid_sim_data_dec_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 13 - rt_opts=latest, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_dec_cases,
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
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen13_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen13_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen13_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen13_R", gt)) 

## Run scenario 14 - rt_opts=latest, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_dec_cases_ur,
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
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen14_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen14_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen14_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen14_R", gt)) 

## Run scenario 15 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_dec_cases,
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
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen15_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen15_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen15_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen15_R", gt)) 

## Run scenario 16 - rt_opts=project, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_dec_cases_ur,
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
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen16_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen16_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen16_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen16_R", gt)) 

