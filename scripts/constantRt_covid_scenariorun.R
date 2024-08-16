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

covid_sim_data_const_low <- read_latest(here("data"), "covid_sim_data_const_low")
covid_sim_data_const_low_ur <- read_latest(here("data"), "covid_sim_data_const_low_ur")

# In required format for EpiNow2

covid_sim_data_cases <- covid_sim_data_const_low |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_cases_ur <- covid_sim_data_const_low_ur |> filter(variable=="reported_cases")
covid_sim_data_cases_ur <- covid_sim_data_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 1 - rt_opts=latest, under-reporting=no ##

res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Park et al. 2019
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Aylward et al. 2014 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen1_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen1_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen1_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen1_R", gt)) 

## Run scenario 2 - rt_opts=latest, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases_ur,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, 
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Aylward et al. 2014 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.3)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen2_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen2_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen2_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen2_R", gt)) 

## Run scenario 3 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen3_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen3_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen3_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen3_R", gt)) 

## Run scenario 4 - rt_opts=project, under-reporting=yes ##

  
  res_covid <- sim_scenarios(case_data=covid_sim_data_cases_ur,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen4_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen4_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen4_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen4_R", gt)) 
  
  ## Loading data ##

  covid_sim_data_const_hi <- read_latest(here("data"), "covid_sim_data_const_hi")
  covid_sim_data_const_hi_ur <- read_latest(here("data"), "covid_sim_data_const_hi_ur")
  
# In required format for EpiNow2

covid_sim_data_cases <- covid_sim_data_const_hi |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_sim_data_cases_ur <- covid_sim_data_const_hi_ur |> filter(variable=="reported_cases")
covid_sim_data_cases_ur <- covid_sim_data_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 5 - rt_opts=latest, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=1)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen5_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen5_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen5_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen5_R", gt)) 

## Run scenario 6 - rt_opts=latest, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases_ur,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, # from Park et al. 2019
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=0.3)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen6_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen6_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen6_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen6_R", gt)) 

## Run scenario 7 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1,
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen7_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen7_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen7_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen7_R", gt)) 

## Run scenario 8 - rt_opts=project, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=covid_sim_data_cases_ur,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, # from Park et al. 2019
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen8_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen8_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen8_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen8_R", gt)) 

