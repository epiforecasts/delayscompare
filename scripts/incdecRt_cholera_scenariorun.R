library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

############### SCENARIOS #################

#### Increasing Rt ####

## Loading data ##

cholera_sim_data_inc <- read_latest(here("data"), "cholera_sim_data_inc")
cholera_sim_data_inc_ur <- read_latest(here("data"), "cholera_sim_data_inc_ur")

# In required format for EpiNow2

cholera_sim_data_inc_cases <- cholera_sim_data_inc |> filter(variable=="reported_cases")
cholera_sim_data_inc_cases <- cholera_sim_data_inc_cases |>
  select(date, value) |>
  rename(confirm=value)

cholera_sim_data_inc_cases_ur <- cholera_sim_data_inc_ur |> filter(variable=="reported_cases")
cholera_sim_data_inc_cases_ur <- cholera_sim_data_inc_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Parameters ##

gen_mean=8.51
gen_sd=0.54
gen_max=30
inc_mean=1.77
inc_sd=1.08
inc_max=30
rep_mean=4.4
rep_sd=0.67
rep_max=30
freq_fc=4
weeks_inc=12


## Run scenario 9 - rt_opts=latest, under-reporting=no ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_inc_cases,
                           gt,
                           gen_mean=gen_mean,
                           gen_sd=gen_sd, 
                           gen_max=gen_max,
                           inc_mean=inc_mean,
                           inc_sd=inc_sd, 
                           inc_max=inc_max,
                           rep_mean=rep_mean,
                           rep_sd=rep_sd,
                           rep_max=rep_max,
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen9_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen9_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen9_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen9_R", gt)) 

## Run scenario 10 - rt_opts=latest, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_inc_cases_ur,
                           gt,
                           gen_mean=gen_mean,
                           gen_sd=gen_sd, 
                           gen_max=gen_max,
                           inc_mean=inc_mean,
                           inc_sd=inc_sd, 
                           inc_max=inc_max,
                           rep_mean=rep_mean,
                           rep_sd=rep_sd,
                           rep_max=rep_max,
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=0.28)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen10_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen10_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen10_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen10_R", gt)) 

## Run scenario 11 - rt_opts=project, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_inc_cases,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd, 
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd, 
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen11_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen11_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen11_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen11_R", gt)) 

## Run scenario 12 - rt_opts=project, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_inc_cases_ur,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd, 
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd, 
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen12_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen12_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen12_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen12_R", gt)) 
  
  #### Decreasing Rt ####
  
  ## Loading data ##

  cholera_sim_data_dec <- read_latest(here("data"), "cholera_sim_data_dec")
  cholera_sim_data_dec_ur <- read_latest(here("data"), "cholera_sim_data_dec_ur")
  
# In required format for EpiNow2

cholera_sim_data_dec_cases <- cholera_sim_data_dec |> filter(variable=="reported_cases")
cholera_sim_data_dec_cases <- cholera_sim_data_dec_cases |>
  select(date, value) |>
  rename(confirm=value)

cholera_sim_data_dec_cases_ur <- cholera_sim_data_dec_ur |> filter(variable=="reported_cases")
cholera_sim_data_dec_cases_ur <- cholera_sim_data_dec_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

cholera_rep_params <- get_parameters(fix_dist(cholera_reporting_delay))

## Run scenario 13 - rt_opts=latest, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_dec_cases,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd, 
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd,  
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="latest",
                             obs_scale=1)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen13_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen13_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen13_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen13_R", gt)) 

## Run scenario 14 - rt_opts=latest, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_dec_cases_ur,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd, 
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd, 
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="latest",
                             obs_scale=0.28)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen14_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen14_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen14_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen14_R", gt)) 

## Run scenario 15 - rt_opts=project, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_dec_cases,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd,
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd, 
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen15_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen15_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen15_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen15_R", gt)) 

## Run scenario 16 - rt_opts=project, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_dec_cases_ur,
                             gt,
                             gen_mean=gen_mean,
                             gen_sd=gen_sd, 
                             gen_max=gen_max,
                             inc_mean=inc_mean,
                             inc_sd=inc_sd, 
                             inc_max=inc_max,
                             rep_mean=rep_mean,
                             rep_sd=rep_sd,
                             rep_max=rep_max,
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen16_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen16_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen16_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen16_R", gt)) 

