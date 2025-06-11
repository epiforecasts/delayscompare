library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

############### SCENARIOS #################

## Loading data ##

cholera_sim_data_const_low <- read_latest(here("data"), "cholera_sim_data_const_low")
cholera_sim_data_const_low_ur <- read_latest(here("data"), "cholera_sim_data_const_low_ur")

# In required format for EpiNow2

cholera_sim_data_low_cases <- cholera_sim_data_const_low |> filter(variable=="reported_cases")
cholera_sim_data_low_cases <- cholera_sim_data_low_cases |>
  select(date, value) |>
  rename(confirm=value)

cholera_sim_data_low_cases_ur <- cholera_sim_data_const_low_ur |> filter(variable=="reported_cases")
cholera_sim_data_low_cases_ur <- cholera_sim_data_low_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Parameter values ##
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

## Run scenario 1 - rt_opts=latest, under-reporting=no ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_low_cases,
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

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen1_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen1_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen1_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen1_R", gt)) 

## Run scenario 2 - rt_opts=latest, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_low_cases_ur,
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

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen2_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen2_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen2_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen2_R", gt)) 

## Run scenario 3 - rt_opts=project, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_low_cases,
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
  
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen3_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen3_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen3_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen3_R", gt)) 

## Run scenario 4 - rt_opts=project, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_low_cases_ur,
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
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen4_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen4_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen4_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen4_R", gt)) 
  
  ## Loading data ##

  cholera_sim_data_const_hi <- read_latest(here("data"), "cholera_sim_data_const_hi")
  cholera_sim_data_const_hi_ur <- read_latest(here("data"), "cholera_sim_data_const_hi_ur")
  
# In required format for EpiNow2

cholera_sim_data_hi_cases <- cholera_sim_data_const_hi |> filter(variable=="reported_cases")
cholera_sim_data_hi_cases <- cholera_sim_data_hi_cases |>
  select(date, value) |>
  rename(confirm=value)

cholera_sim_data_hi_cases_ur <- cholera_sim_data_const_hi_ur |> filter(variable=="reported_cases")
cholera_sim_data_hi_cases_ur <- cholera_sim_data_hi_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

cholera_rep_params <- get_parameters(fix_dist(cholera_reporting_delay))

## Run scenario 5 - rt_opts=latest, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_hi_cases,
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
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen5_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen5_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen5_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen5_R", gt)) 

## Run scenario 6 - rt_opts=latest, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_hi_cases_ur,
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
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen6_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen6_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen6_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen6_R", gt)) 

## Run scenario 7 - rt_opts=project, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_hi_cases,
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
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen7_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen7_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen7_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen7_R", gt)) 

## Run scenario 8 - rt_opts=project, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_hi_cases_ur,
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
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen8_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen8_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen8_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen8_R", gt)) 

