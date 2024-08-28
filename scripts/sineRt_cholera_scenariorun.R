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

cholera_sim_data_sine <- read_latest(here("data"), "cholera_sim_data_sine")
cholera_sim_data_sine_ur <- read_latest(here("data"), "cholera_sim_data_sine_ur")

# In required format for EpiNow2

cholera_sim_data_sine_cases <- cholera_sim_data_sine |> filter(variable=="reported_cases")
cholera_sim_data_sine_cases <- cholera_sim_data_sine_cases |>
  select(date, value) |>
  rename(confirm=value)

cholera_sim_data_sine_cases_ur <- cholera_sim_data_sine_ur |> filter(variable=="reported_cases")
cholera_sim_data_sine_cases_ur <- cholera_sim_data_sine_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

cholera_rep_params <- get_parameters(fix_dist(cholera_reporting_delay))

## Run scenario 17 - rt_opts=latest, under-reporting=no ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_sine_cases,
                           gt,
                           gen_mean=5,
                           gen_sd=8, 
                           gen_max=30,
                           inc_mean=1.4,
                           inc_sd=1.98, 
                           inc_max=30,
                           rep_meanlog=cholera_rep_params$meanlog,
                           rep_sdlog=cholera_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen17_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen17_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen17_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen17_R", gt)) 

## Run scenario 18 - rt_opts=latest, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_sine_cases_ur,
                           gt,
                           gen_mean=5,
                           gen_sd=8, 
                           gen_max=30,
                           inc_mean=1.4,
                           inc_sd=1.98, 
                           inc_max=30,
                           rep_meanlog=cholera_rep_params$meanlog,
                           rep_sdlog=cholera_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.28)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen18_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen18_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen18_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen18_R", gt)) 

## Run scenario 19 - rt_opts=project, under-reporting=no ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_sine_cases,
                             gt,
                             gen_mean=5,
                             gen_sd=8, 
                             gen_max=30,
                             inc_mean=1.4,
                             inc_sd=1.98, 
                             inc_max=30,
                             rep_meanlog=cholera_rep_params$meanlog,
                             rep_sdlog=cholera_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen19_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen19_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen19_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen19_R", gt)) 

## Run scenario 20 - rt_opts=project, under-reporting=yes ##

  res_cholera <- sim_scenarios(case_data=cholera_sim_data_sine_cases_ur,
                             gt,
                             gen_mean=5,
                             gen_sd=8, 
                             gen_max=30,
                             inc_mean=1.4,
                             inc_sd=1.98, 
                             inc_max=30,
                             rep_meanlog=cholera_rep_params$meanlog,
                             rep_sdlog=cholera_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen20_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen20_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen20_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen20_R", gt)) 
  
